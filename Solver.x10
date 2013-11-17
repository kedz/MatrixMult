import x10.util.Timer;
import x10.util.ArrayList;
import x10.util.HashMap;
import x10.util.concurrent.AtomicLong;

/**
 * This is the class that provides the solve() method.
 *
 * The assignment is to replace the contents of the solve() method
 * below with code that actually works :-)
 */
public class Solver
{
    
	public def makeSolutionFragment(size:long) : Rail[Double] {
        return new Rail[Double](size, (i:long) => 0.0);
    }
	
    public def makeFragment(size:long): Rail[ MatrixRow ] {
       return new Rail[ MatrixRow ](size, (i:long)=> new MatrixRow());
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
		val	nthreads = 4;
        
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

		/*
        val matrixFragments = 
                    PlaceLocalHandle.make[Rail[HashMap[Long, NodeProb]]](PlaceGroup.WORLD,
                        () =>(place1.id == here.id) ? makeFragment(size1) : makeFragment(size2));
		*/
		val matrixFragments = 
                    PlaceLocalHandle.make[Rail[ MatrixRow ] ](PlaceGroup.WORLD,
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

		var iter:long = 0;

        while(true) {
            val gSolution = gSolutionVar;
            val gNewSolution = gNewSolutionVar;
			val iterV = iter;

            finish {
                for (val p in PlaceGroup.WORLD) {
                    at (p) {
					 
                        async{
							val fragSize = matrixFragments().size;
							val chunkSize = fragSize/nthreads+1;
											
							finish for( var k:long = 0; k < fragSize ; k+= chunkSize){
								val start = k;
								val end = (start + chunkSize) > fragSize ? fragSize : (start + chunkSize); 
							
								async{
								
									var blkUpdate:Rail[double] = new Rail[double]( chunkSize );
									val otherPlace = (here.id == place1.id()) ? place2 : place1;
									val oldPlace = here.id;
									val beta = (1-dampingFactor) / n ;
									var sum:double = 0.0;
									
									//Calculate the base array entry
									for (val j in gSolution().range()) {
											sum += beta*gSolution()(j) ;
									}
									
									for (var i:long = start; i < end ; i++) {
										var rowUpdate:double = 0.0;
										var curr:NodeProb = matrixFragments()(i).last;
										var total:double = sum;
										
										//Sparse matrix. Perform computation only when there are entries
										while( curr != null  ){
											total += gSolution()(curr.id)*dampingFactor*curr.prob;
											curr = curr.next;
										}
										rowUpdate = total;
										
										val gIndex = indexMap().get(i).value;
										val gRowUpdate = rowUpdate / 1.0;
										blkUpdate( i%chunkSize ) = rowUpdate/1.0;
										gNewSolution()(gIndex) = gRowUpdate;
										
										at (here) gNewSolution()(gIndex) = gRowUpdate;
									
									}
									
									val XferBlk = blkUpdate;
									val gstart = indexMap().get(start).value;
									val gend = indexMap().get(end-1).value;
									
									at ( otherPlace )
										for( var i:long = gstart ; i <= gend ; i++ ){
											gNewSolution()( i ) = XferBlk(i-gstart);
										}
									
								}
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
            
            iter++;
			Console.OUT.println("Distance: "+dist+" > "+epsilon+"\n");
                    
        }    

        return gSolutionVar();
    }

    public def graphToMatrix(webGraph: Rail[WebNode]) : Rail[ MatrixRow ]  {
  
        //var sparseMatrix: Rail[HashMap[Long, NodeProb]] = new  Rail[HashMap[Long, NodeProb]](webGraph.size, (i:long)=> new HashMap[Long, NodeProb]());
        var SMatrix: Rail[MatrixRow] = new  Rail[ MatrixRow ](webGraph.size, (i:long)=> new MatrixRow());
		var prevNode: Rail[NodeProb] = new  Rail[ NodeProb ](webGraph.size, (i:long)=> null );
	
        val totalLinks:double = webGraph.size;

        Console.OUT.println("Graph size");
        Console.OUT.println(webGraph.size);
        for (wn in webGraph) {
			
            numLinks: double = wn.links.size();
            
			if (numLinks >0) {
						
                for (lwn in wn.links) {
					val prob: double = 1.0 / numLinks;
					val newNode = new NodeProb(wn.id-1, prob, prevNode(lwn.id-1));
                    SMatrix(lwn.id-1).pages.put(wn.id-1, newNode);	
					prevNode(lwn.id-1) = newNode;
					SMatrix(lwn.id-1).last = newNode;
                }
                
            } else {
                for (i in webGraph.range()) {
                    val prob: double = 1.0 / totalLinks;
					val newNode = 
					new NodeProb(wn.id-1, prob, prevNode( i ));
                    SMatrix(i).pages.put(wn.id-1, newNode );   
					prevNode( i ) = newNode;
					SMatrix( i ).last = newNode;
                }

            }

        }
		/*
		//Link-List the nodes
		for( i in SMatrix.range() ){
		
			var prev:NodeProb = null;
			for( j in SMatrix.range() ){
				
				if( SMatrix(i).pages.containsKey(j) ){
					//Console.OUT.println("Found one at index:( "+i+" , "+j+")\n");
					var curr:NodeProb = SMatrix(i).pages.get(j).value;
					curr.next = prev;
					prev = curr;
					SMatrix(i).num++;
				}
				SMatrix(i).last = prev;
			}
		}*/
        return SMatrix;

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
	
	private class MatrixRow{
		var last:NodeProb;
		var num:long;
		var pages:HashMap[Long, NodeProb];
		
		def this( ) {
            this.last = null;
			this.num = 0;
			this.pages = new HashMap[Long, NodeProb]() ;
        }
		
		public def setPages( row:HashMap[Long, NodeProb]  ){
			this.pages = row;
		}
		
		public def setLast( nd:NodeProb ){
			this.last = nd;
		}
		
	}

    public class NodeProb {
        val id: long;
        val prob: double;
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