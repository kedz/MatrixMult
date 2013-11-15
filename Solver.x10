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
        Console.OUT.println("u = "+solutions);
        
        prettyPrint(sparseMatrix);
        Console.OUT.println();


        for (var itr:Long = 0; itr < 1000; itr++) {

            var newSolution: Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>0.0);
            
            for (val i in solutions.range()) { 
                for (val j in solutions.range()) {
              
                    var sum: double = (1-dampingFactor) / n;
                    if (sparseMatrix(j).containsKey(i)) {
                        sum += dampingFactor * sparseMatrix(j).get(i).value.prob;
                    }
                    Console.OUT.print(sum+" "); 

                    newSolution(i) += (solutions(j) * sum);
                }
          
            Console.OUT.println("| [ "+solutions(i) + " ]");
            }
            //Console.OUT.println("Update: "+solutions);
          
            var norm: double = 0;
            for (i in newSolution.range()) {
              norm += newSolution(i);
            }
            for (i in newSolution.range()) {
              newSolution(i) = newSolution(i) / norm;
            }
         
          Console.OUT.println("Eps: "+epsilon);
          Console.OUT.println("Distance: "+distance(solutions, newSolution));
          
          if (distance(solutions, newSolution) <= epsilon) {
            break;

          }

          Console.OUT.println("New solution: "+newSolution);
          Console.OUT.println("Old solution: "+solutions);
          Console.OUT.println();
          solutions = newSolution;
          
          

          
          
        }



        return solutions;
    	}


    public def graphToMatrix(webGraph: Rail[WebNode]) : Rail[HashMap[Long, NodeProb]] {
  
        var sparseMatrix: Rail[HashMap[Long, NodeProb]] = new  Rail[HashMap[Long, NodeProb]](webGraph.size);
        val totalLinks:double = webGraph.size;


    
        Console.OUT.println("Graph size");
        Console.OUT.println(webGraph.size);
        for (wn in webGraph) {
            numLinks: double = wn.links.size();
            sparseMatrix(wn.id-1n) = new HashMap[Long, NodeProb]();
      
            if (numLinks >0) {
                for (lwn in wn.links) {
                    val prob: double = 1.0 / numLinks;        
                    sparseMatrix(wn.id-1n).put(lwn.id-1n, new NodeProb(lwn.id-1n, prob));        
        
                }
                
            } else {
                for (i in webGraph.range()) {
                    val prob: double = 1.0 / totalLinks;        
                    sparseMatrix(wn.id-1n).put(i, new NodeProb(i, prob));        
                    

                }

            }

            Console.OUT.println("Node: "  + wn.id);
        }
    
    
    return sparseMatrix;

  }




  public def prettyPrint(sparseMatrix: Rail[HashMap[Long, NodeProb]]) {

    for (val i in sparseMatrix.range()) {
    
      for (val j in sparseMatrix.range()) {
        if (sparseMatrix(j).containsKey(i)) {
          val prob: double = sparseMatrix(j).get(i).value.prob;
          Console.OUT.print(String.format("%0.3f ", new Rail[Any](1, (i:Long)=>prob )));
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
