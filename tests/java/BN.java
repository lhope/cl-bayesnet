/*
  This is free and unencumbered software released into the public
  domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a
  compiled binary, for any purpose, commercial or non-commercial, and
  by any means.

  In jurisdictions that recognize copyright laws, the author or
  authors of this software dedicate any and all copyright interest in
  the software to the public domain. We make this dedication for the
  benefit of the public at large and to the detriment of our heirs and
  successors. We intend this dedication to be an overt act of
  relinquishment in perpetuity of all present and future rights to
  this software under copyright law.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  For more information, please refer to <http://unlicense.org/>
*/

package bn.java;

/**
   This is a minimal interface for Bayesian network queries.  The
   interface allows probability queries by node index or name.  No
   facility is provided for inquiring into the underlying Bayesian
   network structure.

   It allows the most common queries: conditional and unconditional
   probabilities of nodes, as well as unconditional queries across
   multiple nodes.

   Access is also provided to the underlying query engine so further
   query types can be constructed.

   Copyright (c) 2007, Lucas Hope.
 */
public interface BN {

    /** Return the number of nodes in the Bayesian network. */
    public int getNumNodes();

    /** Return an array of the names of each node in order.  This
     * array should not be modified. */
    public java.lang.String[] getNodeNames();

    /** Return the number of states in the numbered node. */
    public int getNumStates(int node);

    /** Return the number of states in the named node. */
    public int getNumStates(java.lang.String node);

    /** Return an array of the state names in the numbered node. This
     * array should not be modified. */
    public java.lang.String[] getStateNames(int node);

    /** Return an array of the state names in the named node. This
     * array should not be modified. */
    public java.lang.String[] getStateNames(java.lang.String node);
	    
    /** Return the unconditional prior probability of the node.
     * @return A normalized array of probababilities.
     */
    public double[] query(int node);

    /** Return the unconditional prior probability of the node.
     * @return A normalized array of probababilities.
     */
    public double[] query(java.lang.String node);

    /** Return the probability that all the nodes are in the given
     * states. 
     * @return a probability.	
     */
    public double query(int[] nodes, int[] states);

    /** Return the probability that all the nodes are in the given
     * states. 
     * @return a probability.	
     */
    public double query(java.lang.String[] nodes, java.lang.String[] states);

    /** Return the conditional probability distribution of node given
     * the nodes are in the given states.
     * @return A normalized array of probababilities.
     */
    public double[] query(int node, int[] nodes, int[] states);

    /** Return the conditional probability distribution of node given
     * the nodes are in the given states.
     * @return A normalized array of probababilities.
     */
    public double[] query(java.lang.String node, 
			java.lang.String[] nodes, java.lang.String[] states);

    /** Return the probability that the node is in the given state.
	@param state An array of length <code>getNumNodes()</code>,
	whose elements are the node states (less than zero indicates
	no evidence).
	@return a probability.	
     */
    public double query(int[] state);

}
