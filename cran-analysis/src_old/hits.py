# https://networkx.github.io/documentation/networkx-1.10/reference/generated/networkx.algorithms.link_analysis.hits_alg.hits.html
# https://www.csie.ntu.edu.tw/~azarc/sna/networkx/networkx/algorithms/hits.py

import numpy
import networkx
from networkx.exception import NetworkXError

def hits_numpy(M,max_iter=100,tol=1.0e-6,nodelist=None):
    """Return a NumPy array of the hubs and authorities."""
    import numpy
    #import networkx
    #M=networkx.to_numpy_matrix(G,nodelist=nodelist)
    (n,m)=M.shape # should be square
    A=M.T*M # authority matrix
    x=numpy.ones((n,1))/n
    # power iteration on authority matrix
    for i in range(max_iter):
        xlast=x
        x=numpy.dot(A,x)
        x=x/x.sum()
        # check convergence, l1 norm            
        err=numpy.abs(x-xlast).sum()
        if err < n*tol:
            a=numpy.asarray(x).flatten()
            # h=M*a
            h=numpy.asarray(numpy.dot(M,a)).flatten()
            h/=h.sum()
            return h,a
    raise NetworkXError("page_rank: power iteration failed to converge in %d iterations."%(i+1))

nodelist = None
max_iter = 100
tol = 1.0e-6
G = networkx.path_graph(4)
h,a = hits_numpy(G)

M = numpy.matrix(
    [
        [0,0,0,0,0,0,1,1,1,1,1,0,0,0],
        [0,0,0,0,0,0,0,0,1,0,0,1,1,0],
        [0,0,0,0,0,0,1,1,0,0,0,0,0,0],
        [0,0,0,0,0,0,1,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,1,0,0,1,1,1],
        [0,0,0,0,0,0,1,0,0,0,0,0,1,1],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    ])



