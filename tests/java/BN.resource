// Start BN.resource
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
   benefit of the public at large and to the detriment of our heirs
   and successors. We intend this dedication to be an overt act of
   relinquishment in perpetuity of all present and future rights to
   this software under copyright law.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
   CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   For more information, please refer to <http://unlicense.org/>
*/
	String[] nodeNames = getNodeNames();

	//stateHashes = new Hashtable<java.lang.String,Integer>[getNumNodes()];
	stateHashes = new Hashtable[getNumNodes()];

	for(int i = 0; i < nodeNames.length; i ++) {
	    nodeHash.put(nodeNames[i],i);
	    String[] states = getStateNames(i);
	    stateHashes[i] =
		new Hashtable<java.lang.String,Integer>();
	    for(int j = 0; j < states.length; j ++)
		{ stateHashes[i].put(states[j],j); }
	}
    }

    /** A hash table mapping node name to its index. */
    protected Hashtable<java.lang.String,Integer> nodeHash = 
	new Hashtable<java.lang.String,Integer>();

    /** An array of hashes mapping state names to indices. */
    protected Hashtable<java.lang.String,Integer>[] stateHashes;

    /** Returns 1 if val is possible, given state and 0 otherwise.  If
     * state is less than 0, anything is possible. */
    protected static double match(int state, int val) {
	if(state < 0) { return 1.0; }
	return (state == val) ? 1.0 : 0.0;
    }

    /** Destructively normalize vec.  Assumes vec is non-zero. */
    protected static double[] normalize(double[] vec) {
	double tot = 0.0d;
	for(int i = 0; i < vec.length; i ++) 
	    { tot += vec[i]; }
	for(int i = 0; i < vec.length; i ++) 
	    { vec[i] /= tot; }

	return vec;
    }

    /** Returns a fresh array of length getNumNodes, initialized to
     * -1. */
    protected int[] netState() {
	int[] retval = new int[getNumNodes()];
	for(int i = 0; i < retval.length; i ++) 
	    { retval[i] = -1; }
	return retval;
    }


    /** Return the unconditional prior probability of node. */
    public double[] query(int node) {
	int[] netState = netState();
	double[] prob = new double[getNumStates(node)];
	for(int i = 0; i < prob.length; i ++) {
	    netState[node] = i;
	    prob[i] = query(netState);
	}
	
	return normalize(prob);
    }

    /** Return the probability that all the nodes are in the given
     * states. */
    public double query(int[] nodes, int[] states) {
	int[] netState = netState();

	for(int i = 0; i < nodes.length; i ++)
	    { netState[nodes[i]] = states[i]; }

    	return query(netState);
    }

    /** Returns the conditional probability distribution of node given
     * the nodes are in the given states. */
    public double[] query(int node, int[] nodes, int[] states)
    {
	int[] netState = netState();

	for(int i = 0; i < nodes.length; i ++)
	    { netState[nodes[i]] = states[i]; }

    	double[] prob = new double[getNumStates(node)];
	for(int i = 0; i < prob.length; i ++) {
	    netState[node] = i;
	    prob[i] = query(netState);
	}
	
	return normalize(prob);
    }

    /** Return the unconditional prior probability of node. */
    public double[] query(java.lang.String node) {
	return query(nodeHash.get(node));
    }

    /** Return the probability that all the nodes are in the given
     * states. */
    public double query(java.lang.String[] nodes, java.lang.String[] states) {
	int[] nodeNums = new int[nodes.length];
	int[] stateNums = new int[nodes.length];
	
	for(int i = 0; i < nodes.length; i ++) {
	    nodeNums[i] = nodeHash.get(nodes[i]);
	    stateNums[i] = stateHashes[nodeNums[i]].get(states[i]);
	}
	return query(nodeNums, stateNums);
    }

    /** Returns the conditional probability distribution of node given
     * the nodes are in the given states. */
    public double[] query(java.lang.String node, 
			java.lang.String[] nodes, java.lang.String[] states) {
	int[] nodeNums = new int[nodes.length];
	int[] stateNums = new int[nodes.length];
	
	for(int i = 0; i < nodes.length; i ++) {
	    nodeNums[i] = nodeHash.get(nodes[i]);
	    stateNums[i] = stateHashes[nodeNums[i]].get(states[i]);
	}
	return query(nodeHash.get(node), nodeNums, stateNums);
    }

    public java.lang.String[] getStateNames(java.lang.String node)
    {
	return getStateNames(nodeHash.get(node)); 
    }

    public int getNumStates(java.lang.String node)
    {
	return getNumStates(nodeHash.get(node)); 
    }
// End BN.resource
