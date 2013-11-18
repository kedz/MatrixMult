import x10.util.Timer;
import x10.util.ArrayList;
import x10.util.HashMap;
import x10.util.concurrent.AtomicLong;
import x10.util.concurrent.AtomicDouble;
import x10.lang.System;

/**
 * This is the class that provides the solve() method.
 *
 * The assignment is to replace the contents of the solve() method
 * below with code that actually works :-)
 */
public class Solver {
    
    /**
     * Constructor function for initializing the global solution vector when creating
     * a PlaceLocalHandle.
     * 
     * @param   size - The size of the rail to initialize.
     * @return  A rail of 'size' Doubles initialized to 0.0 
     */   
    public def makeSolutionFragment(size:long) : Rail[Double] {
        return new Rail[Double](size, (i:long) => 0.0);
    }

    /**
     * Constructor function for initializing matrix fragments when creating
     * a PlaceLocalHandle for the web page matrix.
     * 
     * @param   size - The number of matrix rows 
     *                 (a.k.a the size of the rail to initialize.)
     * @return  A rail of 'size' empty MatrixRow objects 
     */   
    public def makeFragment(size:long) : Rail[MatrixRow] {
        return new Rail[MatrixRow](size, (i:long) => new MatrixRow());
    }

    /**
     * Constructor function for making a map of a local matrix row index to
     * the global matrix row index.
     *
     * @param offset    The global index that will be 0 locally.
     * @param size      The number of rows to map.
     *
     * @return  A map from the local matrix row index to a global matrix row
     *          index.
     */
    public def makeIndexMap(offset:long, numItems:long):HashMap[Long, Long] {
        val map:HashMap[Long, Long] = new HashMap[Long, Long]();
        for (var i:long = 0; i < numItems; i++)
            map.put(i, offset+i);
        return map;
    }
    
    /**
     * solve(webGraph: Rail[WebNode], dampingFactor: double,epsilon:Double)
     * 
     * Returns an approximation of the page rank of the given web graph
     */
    public def solve(webGraph: Rail[WebNode], dampingFactor: Double, epsilon:Double) : Rail[Double] {
        
        Console.OUT.println("Number of pages: "+webGraph.size);
        Console.OUT.println("Damping Factor: "+dampingFactor);
        Console.OUT.println("Number of Threads: "+Runtime.NTHREADS);
        if (PlaceGroup.WORLD.numPlaces() != 2) {
            Console.OUT.println("FATAL ERROR: Solver must have two places to run. Currently there are "+
                PlaceGroup.WORLD.numPlaces() + " place(s)");
            throw new Error("X10_NPLACES must be set to 2.");
        }
        
        val n: double = webGraph.size;
        val beta = (1.0-dampingFactor) / n ;
    
        /** INITIALIZATION */
        
        // Get the runtimes number of threads.
        val nthreads = Runtime.NTHREADS;
        
        // Process input graph into a matrix.
        val sparseMatrix = graphToMatrix(webGraph);
       
        // The sizes of our matrix fragments. If there are an uneven amount of web pages,
        // these numbers will differ by 1.
        val size1 : long = webGraph.size / 2;
        val size2 : long = webGraph.size - size1;
        
        // Our places in the world. (It's a small world.)
        val place1 = PlaceGroup.WORLD(0);
        val place2 = PlaceGroup.WORLD(1);

        // Plh to a map of local matrix fragment indices to what their actual matrix row index is.
        val indexMap = 
            PlaceLocalHandle.make[HashMap[Long, Long]](PlaceGroup.WORLD,
              () => (place1.id == here.id) ? makeIndexMap(0,size1) : makeIndexMap(size1, size2));

        // Plh to a matrix (a Rail of MatrixRows). Locally each matrix represents 1/2 of the whole
        // global matrix.
        val matrixFragments = 
            PlaceLocalHandle.make[Rail[MatrixRow] ](PlaceGroup.WORLD,
              () =>(place1.id == here.id) ? makeFragment(size1) : makeFragment(size2));

        // Plh to the global solution vector. After each iteration, this vector is identical
        // at both locations. 
        val solutions:Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>1.0/n);
        var gSolutionVar:PlaceLocalHandle[Rail[Double]] = 
            PlaceLocalHandle.make[Rail[Double]](PlaceGroup.WORLD,
              () => solutions);
      

        // Plh to the global new solution vector. After each iteration, this vector is identical
        // at both locations.
        val newSolutions:Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>0.0);
        var gNewSolutionVar:PlaceLocalHandle[Rail[Double]] = 
            PlaceLocalHandle.make[Rail[Double]](PlaceGroup.WORLD,
              () => newSolutions);
    
        val gDist = PlaceLocalHandle.make[AtomicDouble](PlaceGroup.WORLD, 
              () => new AtomicDouble(0.0));

        // Initialize the matrix fragments 
        val blockSize = (sparseMatrix.size/nthreads + 1);
        
        finish for (var i:long = 0; i < sparseMatrix.size; i += blockSize) {
            val iVal = i;
            val end = (iVal + blockSize) > sparseMatrix.size ? sparseMatrix.size : (iVal + blockSize);
            async{
                for(var k:long = iVal; k < end; k ++) {
                    val kVal = k;
                    val row = sparseMatrix(k);
                    if (k < size1) {
                        at (place1){
                            matrixFragments()(kVal) = row;
                        }
                    } else {
                        at (place2){
                            matrixFragments()(kVal-size1) = row;
                        }
                    }
                }
            }
        }

        /** MAIN LOOP */

        var iter:long = 0;
        val eps = epsilon / 10.0;

        while(true) {

            val gSolution = gSolutionVar;
            val gNewSolution = gNewSolutionVar;

            // At each place, give each thread a chunk of rows from the matrix to work on. 
            finish {
                for (val p in PlaceGroup.WORLD) {
                    at (p) {
 
                        async{

                            gDist().set(0.0);
                            // Size of local matrix
                            val fragSize = matrixFragments().size;
                            // Number of rows each thread will work on.
                            val chunkSize = fragSize/nthreads+1;

                            // Assign each thread a chunk.
                            finish for( var k:long = 0; k < fragSize ; k+= chunkSize) {
                                val start = k;
                                val end = (start + chunkSize) > fragSize ? fragSize : (start + chunkSize); 
                                
                                async{

                                    var locDist: double = 0.0;
                                    
                                    val blkUpdate:Rail[double] = new Rail[double](chunkSize);
                                    val otherPlace = (here.id == place1.id()) ? place2 : place1;
                                    val oldPlace = here.id;
                                    var sum:double = 0.0;

                                    //Calculate the base array entry
                                    for (val j in gSolution().range()) {
                                        sum += gSolution()(j) ;
                                    }
                                    sum = sum * beta;
                                    
                                    // Calculate each row
                                    for (var i:long = start; i < end ; i++) {
                                        var rowUpdate:double = 0.0;
                                        var curr:NodeProb = matrixFragments()(i).last;
                                        var total:double = sum;

                                        //Sparse matrix. Perform computation only when there are entries
                                        while( curr != null  ){
                                            total += gSolution()(curr.id)*dampingFactor*curr.prob;
                                            curr = curr.next;
                                        }
                                        
                                        val gIndex = indexMap().get(i).value;
                                        blkUpdate(i%chunkSize) = total;
                                        gNewSolution()(gIndex) = total;
                                        locDist += Math.pow(total-gSolution()(gIndex), 2);

                                    }                                   
                                        
                                    //val distUpdate = locDist.get();
                                    gDist().addAndGet(locDist);

                                    val gstart = indexMap().get(start).value;
                                    val gend = indexMap().get(end-1).value;

                                    at ( otherPlace ) {
                                        for( var i:long = gstart ; i <= gend ; i++ ){
                                            gNewSolution()( i ) = blkUpdate(i-gstart);
                                        }
                                    }
                                }
                            }
                            
                        }
                    }
                
                }
            }
            
            // Update the distance here with the distance there.
            //val thisDist = gDist().get();
            val thisPlace = here;
            at (here.next()) {
                val otherDist = gDist().get();
                at (thisPlace) gDist().addAndGet(otherDist);
                //gDist().getAndAdd(thisDist);
            }
                    
            // swap references to the old and new solutions
            val swap = gSolutionVar;
            gSolutionVar = gNewSolutionVar;
            gNewSolutionVar = swap;
            
            //val dist = distance(gSolution(), gNewSolution());
            val dist = Math.sqrt(gDist().get());
            Console.OUT.println("Old Solution vctr: "+gSolution());
            Console.OUT.println("New Solution vctr: "+gNewSolution());
            
            if (dist < eps ) {
                break;
            }
            
            //Console.OUT.println("ParDist: "+Math.sqrt(gDist().get()));
            Console.OUT.println("Distance: "+dist+" > "+epsilon+"\n");
        }    

        return gSolutionVar();
    }

    public def graphToMatrix(webGraph: Rail[WebNode]) : Rail[ MatrixRow ]  {
  
        var SMatrix: Rail[MatrixRow] = new  Rail[ MatrixRow ](webGraph.size, (i:long)=> new MatrixRow());

        val totalLinks:double = webGraph.size;

        for (wn in webGraph) {

            numLinks: double = wn.links.size();
            
            if (numLinks >0) {

                for (lwn in wn.links) {
                    val prob: double = 1.0 / numLinks;
                    val newNode = new NodeProb(wn.id-1, prob, SMatrix(lwn.id-1).last);
                    SMatrix(lwn.id-1).last = newNode;
                }
                
            } else {
                for (i in webGraph.range()) {
                    val prob: double = 1.0 / totalLinks;
                    val newNode = new NodeProb(wn.id-1, prob, SMatrix( i ).last);
                    SMatrix( i ).last = newNode;
                }

            }

        }

        return SMatrix;

    }


    // pretty print for debugging.
    public def prettyFragmentPrint(sparseMatrix: Rail[HashMap[Long, NodeProb]], n:long) {

        for (val i in sparseMatrix.range()) {
    
            for (val j in new LongRange(0,n-1)) {
                if (sparseMatrix(i).containsKey(j)) {
                    val prob: double = sparseMatrix(i).get(j).value.prob;
                    Console.OUT.print(String.format("%0.3f ", new Rail[Any](1, (c:long) => prob))); 
                
                } else 
                    Console.OUT.print("0.000 ");
    
            }
            Console.OUT.println();

        }
    }

    // pretty print for debugging.
    public def prettyPrint(sparseMatrix: Rail[HashMap[Long, NodeProb]]) {

        for (val i in sparseMatrix.range()) {
    
            for (val j in sparseMatrix.range()) {
                if (sparseMatrix(i).containsKey(j)) {
                    val prob: double = sparseMatrix(i).get(j).value.prob;
            } else 
                Console.OUT.print("0.000 ");
    
            }
            Console.OUT.println();

        }


    }


        // Linked list version of a sparse matrix row.
        private class MatrixRow {
            var last:NodeProb;

            def this() {
                this.last = null;
            }
        }

        // Sparse matrix node
        public class NodeProb {
            // Matrix column number
            val id: long;
            // page probability
            val prob: double;
            // next nonempty column in the current row.
            var next: NodeProb;

            def this(id: long, prob: double) {
                this.id = id;
                this.prob = prob;
            this.next = null;
        }

        def this(id: long, prob: double, next : NodeProb) {
            this.id = id;
            this.prob = prob;
            this.next = next;
        }

    }


    public def distance(v1:Rail[Double], v2:Rail[Double]) : double {

        var sum: double = 0.0;

        for (i in v1.range()) {
            sum += Math.pow(v1(i) - v2(i), 2);  
        }

        return Math.sqrt(sum);

    }

}
