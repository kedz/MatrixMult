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
    
    /**
	   * solve(webGraph: Rail[WebNode], dampingFactor: double,epsilon:Double)
	   * 
     * Returns an approximation of the page rank of the given web graph
     */
    public def solve(webGraph: Rail[WebNode], dampingFactor: Double, epsilon:Double) : Rail[Double] {
        
        val n: double = webGraph.size;
    	  
    	  val sparseMatrix = graphToMatrix(webGraph);
        var solutions:Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>1.0/webGraph.size);
       
        Console.OUT.println("Matrix size: "+n+"x"+n);
        Console.OUT.println("DampingFactor: "+dampingFactor);

        val size1 : long = webGraph.size / 2;
        val size2 : long = webGraph.size - size1;
        
        val place1 = PlaceGroup.WORLD(0);
        val place2 = PlaceGroup.WORLD(1);
       
        val matrixFragments = 
                    PlaceLocalHandle.make[Rail[HashMap[Long, NodeProb]]](PlaceGroup.WORLD,
                        () =>(place1.id == here.id) ? makeFragment(size1) : makeFragment(size2));
       
        

        /*
        at (place1) {
            matrixFragments() = new Rail[HashMap[Long, NodeProb]](size1, (i:long)=> new HashMap[Long, NodeProb]());
        }

        at (place2) {
            matrixFragments() = new Rail[HashMap[Long, NodeProb]](size2, (i:long)=> new HashMap[Long, NodeProb]());
        }
    
        */
        
        for (i in sparseMatrix.range()) {
            val row = sparseMatrix(i);
            if (i < size1) {
                at (place1) matrixFragments()(i) = row;
            } else {
                at (place2) matrixFragments()(i-size1) = row;
            }
        }

        for (val p in PlaceGroup.WORLD) {
            at (p) {
                Console.OUT.println("Place: " + p +" has "+ matrixFragments().size+" elements");
                prettyFragmentPrint(matrixFragments(), webGraph.size);
                Console.OUT.println();
            }
        }
          

        cutoff: long = webGraph.size / 2;

        //for (i in sparseMatrix.ranges()) {

          //if (i < cutoff)


        //}


        /*
        while(true) {

            var newSolution: Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>0.0);
            var norm: double = 0.0;

            for (val i in solutions.range()) { 
                for (val j in solutions.range()) {
              
                    var sum: double = (1-dampingFactor) / n;
                    if (sparseMatrix(i).containsKey(j)) {
                        sum += dampingFactor * sparseMatrix(i).get(j).value.prob;
                    }
                    //Console.OUT.print(sum+" "); 

                    newSolution(i) += (solutions(j) * sum);
                }
          
                norm += newSolution(i);
            //Console.OUT.println("| [ "+solutions(i) + " ]");
            }
            //Console.OUT.println("Update: "+solutions);
          
            for (i in newSolution.range()) {
              newSolution(i) = newSolution(i) / norm;
            }
         
          //Console.OUT.println("Eps: "+epsilon);
          //Console.OUT.println("Distance: "+distance(solutions, newSolution));
          
          if (distance(solutions, newSolution) <= epsilon) {
            break;

          }

          //Console.OUT.println("New solution: "+newSolution);
          //Console.OUT.println("Old solution: "+solutions);
          //Console.OUT.println();
          solutions = newSolution;
          
        }
*/


        return solutions;
    }
    
    

    public def graphToMatrix(webGraph: Rail[WebNode]) : Rail[HashMap[Long, NodeProb]] {
  
        var sparseMatrix: Rail[HashMap[Long, NodeProb]] = new  Rail[HashMap[Long, NodeProb]](webGraph.size, (i:long)=> new HashMap[Long, NodeProb]());
        val totalLinks:double = webGraph.size;

    
        Console.OUT.println("Graph size");
        Console.OUT.println(webGraph.size);
        for (wn in webGraph) {
            numLinks: double = wn.links.size();
            //sparseMatrix(wn.id-1n) = new HashMap[Long, NodeProb]();
      
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

        var sum: double = 0;

        for (i in v1.range()) {
            sum += Math.pow(v1(i) - v2(i), 2);  

        }

        return Math.sqrt(sum);

    }

}
