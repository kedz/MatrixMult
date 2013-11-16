import x10.util.Timer;
import x10.util.ArrayList;
import x10.util.HashMap;

/**
 * This is the class that provides the solve() method.
 *
 * The assignment is to replace the contents of the solve() method
 * below with code that actually works :-)
 */
public class Solver
{
    
    public def makeFragment(size:long): Rail[HashMap[Long, NodeProb]] {
        return new Rail[HashMap[Long, NodeProb]](size, (i:long)=> new HashMap[Long, NodeProb]());
    }

    public def makeSolutionFragment(size:long) : Rail[Double] {
        return new Rail[Double](size, (i:long) => 0.0);
    }

    public def makeIndexMap(offset:long, numItems:long):HashMap[Long, Long] {
        val map:HashMap[Long, Long] = new HashMap[Long, Long]();
        for (var i:long = 0; i < numItems; i++) {
            map.put(i, offset+i);

        }
        return map;
    }
    
    /**
	   * solve(webGraph: Rail[WebNode], dampingFactor: double,epsilon:Double)
	   * 
     * Returns an approximation of the page rank of the given web graph
     */
    public def solve(webGraph: Rail[WebNode], dampingFactor: Double, epsilon:Double) : Rail[Double] {
        
        val n: double = webGraph.size;
    	 
        var extra: long = 0;
        
        val sparseMatrix = graphToMatrix(webGraph);
        val solutions:Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>1.0/webGraph.size);
       
        Console.OUT.println("Matrix size: "+n+"x"+n);
        Console.OUT.println("DampingFactor: "+dampingFactor);

        val size1 : long = webGraph.size / 2;
        val size2 : long = webGraph.size - size1;
        
        val place1 = PlaceGroup.WORLD(0);
        val place2 = PlaceGroup.WORLD(1);
       
        val indexMap = 
                    PlaceLocalHandle.make[HashMap[Long, Long]](PlaceGroup.WORLD,
                        () =>(place1.id == here.id) ? makeIndexMap(0,size1) : makeIndexMap(size1, size2));

        val matrixFragments = 
                    PlaceLocalHandle.make[Rail[HashMap[Long, NodeProb]]](PlaceGroup.WORLD,
                        () =>(place1.id == here.id) ? makeFragment(size1) : makeFragment(size2));

        var gSolutionVar:PlaceLocalHandle[Rail[Double]] = 
                    PlaceLocalHandle.make[Rail[Double]](PlaceGroup.WORLD,
                        () => solutions);
        

        var gNewSolutionVar:PlaceLocalHandle[Rail[Double]] = 
                    PlaceLocalHandle.make[Rail[Double]](PlaceGroup.WORLD,
                        () => new Rail[Double](webGraph.size, (i:long) => 0.0));
     

        for (i in sparseMatrix.range()) {
            val row = sparseMatrix(i);
            if (i < size1) {
                at (place1) matrixFragments()(i) = row;
            } else {
                at (place2) matrixFragments()(i-size1) = row;
            }
        }


        while(true) {
            val gSolution = gSolutionVar;
            val gNewSolution = gNewSolutionVar;

            

            finish {
                for (val p in PlaceGroup.WORLD) {
                    at (p) {
                        async{
                            val fragSize = matrixFragments().size; 
                            for (var i:long = 0; i < fragSize; i++) {
                                var rowUpdate:double = 0.0;

                                for (val j in gSolution().range()) {
                                    var sum: double = (1-dampingFactor) / n;
                                    if (matrixFragments()(i).containsKey(j)) {
                                        sum += dampingFactor * matrixFragments()(i).get(j).value.prob;
                                    }

                                    rowUpdate += (gSolution()(j) * sum);
                                }
                                
                                val gIndex = indexMap().get(i).value;
                                val gRowUpdate = rowUpdate / 1.0;
                                at (place1) gNewSolution()(gIndex) = gRowUpdate;
                                at (place2) gNewSolution()(gIndex) = gRowUpdate;

                            }
                            
                        }
                    }
                
                }
            }
            

            val swap = gSolutionVar;
            gSolutionVar = gNewSolutionVar;
            gNewSolutionVar = swap;
            
            val dist = distance(gSolution(), gNewSolution());
            Console.OUT.println("Old Solution vctr: "+gSolution());
            Console.OUT.println("New Solution vctr: "+gNewSolution());
            
            if (dist < epsilon) {
                Console.OUT.println("Distance: "+dist + " < " +epsilon);
                Console.OUT.println("EXTRA ITERATION: "+extra);                
                extra++;

                if (extra >= 100) {


                    Console.OUT.println("Printing vector: \n\n");
                    for (val i in gSolution().range()) {
                        Console.OUT.println(gSolution()(i));
                    }
                    Console.OUT.println("\n\n--------\n\n");

                
                    break;
                }
            }
            
            Console.OUT.println("Distance: "+dist+" > "+epsilon+"\n");
                    
        }    

        return gSolutionVar();
    }
    

    public def graphToMatrix(webGraph: Rail[WebNode]) : Rail[HashMap[Long, NodeProb]] {
  
        var sparseMatrix: Rail[HashMap[Long, NodeProb]] = new  Rail[HashMap[Long, NodeProb]](webGraph.size, (i:long)=> new HashMap[Long, NodeProb]());
        val totalLinks:double = webGraph.size;

        Console.OUT.println("Graph size");
        Console.OUT.println(webGraph.size);
        for (wn in webGraph) {
            numLinks: double = wn.links.size();
      
            if (numLinks >0) {
                for (lwn in wn.links) {
                    val prob: double = 1.0 / numLinks;        
                    sparseMatrix(lwn.id-1).put(wn.id-1, new NodeProb(wn.id-1, prob));        
                }
                
            } else {
                for (i in webGraph.range()) {
                    val prob: double = 1.0 / totalLinks;        
                    sparseMatrix(i).put(wn.id-1, new NodeProb(wn.id-1, prob));        
                }

            }

        }
    
    
        return sparseMatrix;

    }


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


    public class NodeProb {
        val id: long;
        val prob: double;
        def this(id: long, prob: double) {
            this.id = id;
            this.prob = prob;
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
