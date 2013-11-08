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
    	  //val sparseMatrix = graphToMatrix(webGraph, dampingFactor);
    	  val sparseMatrix = graphToMatrix(webGraph, 1.0);
        prettyPrint(sparseMatrix);

          var solutions:Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>1.0/webGraph.size);

        
        for (var itr:Long = 0; itr < 100; itr++) {

          var newSolution: Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>0.0);

          for (val i in solutions.range()) { 
            for (val j in solutions.range()) {
              
              var sum: double = (1-dampingFactor) / n;
              if (sparseMatrix(i).containsKey(j)) {
              
                sum += dampingFactor * sparseMatrix(i).get(j).value.prob;
              }

              newSolution(i) += solutions(j) * sum;
            }

          }

          
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

          solutions = newSolution;
          
          

          Console.OUT.println(solutions);
          
          
        }



        return solutions;
    	}


  public def graphToMatrix(webGraph: Rail[WebNode], dampingFactor: double) : Rail[HashMap[Long, NodeProb]] {
  
    var sparseMatrix: Rail[HashMap[Long, NodeProb]] = new  Rail[HashMap[Long, NodeProb]](webGraph.size);
    val totalLinks:double = webGraph.size;

    val priorProb:double = (1d - dampingFactor) / totalLinks; 

    
    Console.OUT.println("Graph size");
    Console.OUT.println(webGraph.size);
    for (wn in webGraph) {
      numLinks: double = wn.links.size();
      sparseMatrix(wn.id-1n) = new HashMap[Long, NodeProb]();

      for (lwn in wn.links) {
        val prob: double = dampingFactor/numLinks + priorProb;
        
        sparseMatrix(wn.id-1n).put(lwn.id-1n, new NodeProb(lwn.id-1n, prob));        
        
      }
    
      Console.OUT.println("Node: "  + wn.id);
    }
    
    
    return sparseMatrix;

  }




  public def prettyPrint(sparseMatrix: Rail[HashMap[Long, NodeProb]]) {

    for (val i in sparseMatrix.range()) {
    
      for (val j in sparseMatrix.range()) {
        if (sparseMatrix(i).containsKey(j)) {

          Console.OUT.print(sparseMatrix(i).get(j).value.prob+" ");
        } else 
          Console.OUT.print("0.000 ");
    
      }
      Console.OUT.println();

    }


  }


  public class NodeProb {
    val id: int;
    val prob: double;
    def this(id: int, prob: double) {
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
