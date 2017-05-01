# 1. Select initial set of web pages relevant to the user’s query
# 2. Initialize vectors
# 3. Iteratively update vectors
# 4. Normalize vector scores
# 5. Output vectors

## Authority
# A vertex is considered an authority if it has many pages that link to it (i.e., it has a high indegree)
## Hub
# A vertex is considered a hub if it points to many other vertices (i.e., it has a high outdegree)

# https://networkx.github.io/documentation/networkx-1.10/reference/generated/networkx.algorithms.link_analysis.hits_alg.hits.html
# https://www.csie.ntu.edu.tw/~azarc/sna/networkx/networkx/algorithms/hits.py

library(reshape2)
library(dplyr)
library(arules)
library(igraph)

txt <- '
A,B,C,D,E
C,F,G,,
A,B,,,
A,,,,
C,F,G,H,
A,G,H,,
'
df <- read.csv(text = txt, header = FALSE, stringsAsFactors = FALSE) %>%
    mutate(id = row_number()*100)
df_long <- melt(df, id = "id") %>% filter(value != '') %>% select(id, value)
trans <- as(split(df_long[,'value'], df_long['id']), "transactions")
#methods(class = class(trans))
#as(trans@data, "dgCMatrix")

G <- graph.data.frame(df_long)
V(G)$type <- V(G)$name %in% df_long[, 1]
#get.adjacency(G)

itemM <- as(trans@data, "dgCMatrix")
item_info <- trans@itemInfo[[1]]
item_no <- length(item_info)
itemset_info <- trans@itemsetInfo[[1]]
itemset_no <- length(itemset_info)
leftM <- Matrix(rep(0, itemset_no^2), nrow = itemset_no, sparse = TRUE)
bottomM <- Matrix(rep(0, (itemset_no + item_no)*item_no), nrow = item_no, sparse = TRUE)

A <- rBind(cBind(leftM, t(itemM)), bottomM)
row.names(A) <- c(itemset_info, item_info)
colnames(A) <- row.names(A)
G1 <- graph.adjacency(A, mode = c('directed'), weighted = NULL)

# par(mfrow = c(1,2))
# plot(G)
# plot(G1)
# par(mfrow = c(1,1))


get_hits(get.adjacency(G), 10)
t(get_hits(A, 2000)[[2]])
hits(trans)

get_hits1 <- function(A, k, tol = 1e8){ 
    #Get number of nodes(rows) in adjacency matrix
    nodes <- dim(A)[1] 
    
    #Initialize authority and hub vector to 1 for each node
    auth <- c(rep(1, nodes)) 
    hub <- c(rep(1, nodes)) 
    
    for (i in 1:k) {
        #Authority and Hub scores are calculated using HITS mathematical definition
        auth <- t(A) %*% hub
        hub <- A %*% auth
        
        #Normalize Hub and Authority scores
        auth <- auth/sqrt(sum(auth * auth)) 
        hub <- hub/sqrt(sum(hub * hub)) 
    } 
    
    c(auth, hub)
}

#def hits(G,max_iter=100,tol=1.0e-8,nstart=None):

get_hits <- function(A, k){ 
    #Get number of nodes(rows) in adjacency matrix
    nodes <- dim(A)[1] 
    
    #Initialize authority and hub vector to 1 for each node
    auth <- c(rep(1, nodes)) 
    hub <- c(rep(1, nodes)) 
    
    for (i in 1:k) {
        #Authority and Hub scores are calculated using HITS mathematical definition
        auth <- t(A) %*% hub
        hub <- A %*% auth
        
        #Normalize Hub and Authority scores
        auth <- auth/sqrt(sum(auth * auth)) 
        hub <- hub/sqrt(sum(hub * hub)) 
    } 
    
    c(auth, hub)
}




