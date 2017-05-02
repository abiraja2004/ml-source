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

run_hits <- function(A, k = 100, tol = 1e-6, verbose = FALSE){ 
    #Get number of nodes(rows) in adjacency matrix
    nodes <- dim(A)[1] 
    
    #Initialize authority and hub vector to 1 for each node
    auth <- c(rep(1, nodes)) 
    hub <- c(rep(1, nodes)) 
    
    for (i in 1:k) {
        auth_last <- auth
        
        #Authority and Hub scores are calculated using HITS mathematical definition
        auth <- t(A) %*% hub
        hub <- A %*% auth
        
        #Normalize Hub and Authority scores
        auth <- auth/sqrt(sum(auth * auth)) 
        hub <- hub/sqrt(sum(hub * hub))
        
        err <- sum(abs(auth - auth_last))
        if (verbose) {
            message('msg: iteration ', i, ' error - ', err)
        }
        
        if (err < nodes * tol) {
            break;
        }
    }
    
    if (err > nodes * tol) {
        warning('power iteration failed to converge in ', (i+1), ' iterations')
    }
    
    return (list(auth = auth, hub = hub))
}

get_hits <- function(A, itemsets, items, k = 100, tol = 1e-6, verbose = FALSE) {
    hits <- run_hits(A, k, tol, verbose)
    hub <- hits$hub[1:length(itemsets)]
    names(hub) <- itemsets
    auth <- hits$auth[(length(itemsets)+1):length(hits$auth)]
    names(auth) <- items
    list(auth = auth, hub = hub)
}

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
trans1 <- as(split(df_long[,'value'], df_long['id']), "transactions")
#methods(class = class(trans))
#as(trans@data, "dgCMatrix")

tmat <- dcast(df_long, id ~ value, fun.aggregate = length)
row.names(tmat) <- tmat[, 1]
tmat <- tmat[, -1]
tmat[] <- lapply(tmat[], function(x) x > 0)
as(tmat, 'transactions')

G <- graph.data.frame(df_long)
V(G)$type <- V(G)$name %in% df_long[, 1]
#get.adjacency(G)

itemM <- as(trans1@data, "dgCMatrix")
item_info <- if ('levels' %in% names(trans1@itemInfo)) {
    trans1@itemInfo[, 'levels']
} else {
    trans1@itemInfo[[1]]
}
item_no <- length(item_info)
itemset_info <- trans1@itemsetInfo[[1]]
itemset_no <- length(itemset_info)
# leftM <- Matrix(rep(0, itemset_no^2), nrow = itemset_no, sparse = TRUE)
# bottomM <- Matrix(rep(0, (itemset_no + item_no)*item_no), nrow = item_no, sparse = TRUE)
leftM <- sparseMatrix(i = itemset_no, j = itemset_no, x = 0)
bottomM <- sparseMatrix(i = item_no, j = (itemset_no + item_no), x = 0)
A <- rBind(cBind(leftM, t(itemM)), bottomM)

hub_arules <- hits(trans)
hits <- get_hits(A, itemset_info, item_info)
hub <- hits$hub
auth <- hits$auth

# igraph has hub_score/authority_score but not equivalent

