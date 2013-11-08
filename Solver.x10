import x10.util.Timer;
import x10.util.ArrayList;

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
    		val n = webGraph.size;
    		var solutions:Rail[Double] = new Rail[Double](webGraph.size, (i:Long)=>1.0/webGraph.size);
        	return solutions;
    	}
}
