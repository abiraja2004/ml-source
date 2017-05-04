library(reshape2)
library(dplyr)
library(igraph)
library(arules)
library(arulesViz)

concat_items <- function(data, idcol = 'id') {
    df <- do.call(rbind, lapply(as.data.frame(t(data)), function(e) {
        splt_fac <- ifelse(names(e) == idcol, 'id', 'items')
        splt <- split(e, splt_fac)
        id <- unname(splt[['id']])
        items <- unname(splt[['items']][splt[['items']] != ''])
        data.frame(items = paste0('{', paste(items, collapse = ","), '}'),
                   id = id, stringsAsFactors = FALSE)
    }))
    rownames(df) <- NULL
    df
}

get_adj <- function(trans) {
    itemM <- trans@data
    item_info <- trans@itemInfo[[1]]
    item_no <- length(item_info)
    itemset_info <- trans@itemsetInfo[[1]]
    itemset_no <- length(itemset_info)
    leftM <- sparseMatrix(i = itemset_no, j = itemset_no, x = 0)
    bottomM <- sparseMatrix(i = item_no, j = (itemset_no + item_no), x = 0)
    rBind(cBind(leftM, t(itemM)), bottomM)
}

run_hits <- function(A, k = 100, tol = 1e-8, verbose = FALSE){ 
    #Get number of nodes(rows) in adjacency matrix
    nodes <- dim(A)[1] 
    #Initialize authority and hub vector to 1 for each node
    auth <- c(rep(1, nodes)) 
    hub <- c(rep(1, nodes))
    for (i in 1:k) {
        auth_last <- auth
        #Authority and Hub scores are calculated
        auth <- t(A) %*% hub
        hub <- A %*% auth
        #Normalize Hub and Authority scores
        auth <- auth/sqrt(sum(auth * auth)) 
        hub <- hub/sqrt(sum(hub * hub))
        err <- sum(abs(auth - auth_last))
        if (verbose) message('msg: iteration ', i, ' error - ', err)
        if (err < nodes * tol) {
            break
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

## create transaction data
#data("SunBai")
txt <- 'A,B,C,D,E\nC,F,G,,\nA,B,,,\nA,,,,\nC,F,G,H,\nA,G,H,,'
df <- read.csv(text = txt, header = FALSE, stringsAsFactors = FALSE) %>%
    mutate(id = row_number()*100)

concat_items(df)

df_long <- melt(df, id = "id") %>% filter(value != '') %>% select(id, value)
trans <- as(split(df_long[,'value'], df_long['id']), "transactions")

## plot graph
# manual
G <- graph.data.frame(df_long)
V(G)$type <- V(G)$name %in% df_long[, 1]
plot(G, layout = layout.bipartite)

## adjacency matrix
get.adjacency(G)

A <- get_adj(trans)
A

## run HITS
hub_a <- hits(trans)

hits <- get_hits(A, trans@itemsetInfo[[1]], trans@itemInfo[[1]])
hub <- hits$hub
auth <- hits$auth

hub_compare <- data.frame(trans = names(hub), items = concat_items(df)$items,
                          hub_a = unname(hub_a), hub = unname(hub))
hub_compare
auth

## add transaction weight
transactionInfo(trans)[['weight']] <- hub_a
info <- concat_items(df) %>% cbind(weight = transactionInfo(trans)$weight)
info

## support
supp_n <- itemFrequency(trans, weighted = FALSE)
supp_w <- itemFrequency(trans, weighted = TRUE)
supp_compare <- data.frame(supp_n = supp_n, supp_w = supp_w)
supp_compare

# support of item A
sum(info$weight[grepl('A', info$items)])/sum(info$weight)

par(mfrow = c(1,2))
itemFrequencyPlot(trans, main = "Unweighted frequency")
itemFrequencyPlot(trans, weighted = TRUE, main = "Weighted frequency")
par(mfrow = c(1,1))

## frequent itemsets
itemsets_n <- eclat(trans, parameter = list(support = 0.3))
itemsets_w <- weclat(trans, parameter = list(support = 0.3))

inspect(sort(itemsets_n))
inspect(sort(itemsets_w))

# apriori
# itemsets_a <- apriori(trans, parameter = list(target = 'frequent', support = 0.3))
# inspect(sort(itemsets_a))

## rule induction
rules_n <- ruleInduction(itemsets_n, confidence = 0.8)
rules_w <- ruleInduction(itemsets_w, confidence = 0.8)

inspect(sort(rules_n))
inspect(sort(rules_w))

# apriori
# rules_a <- ruleInduction(itemsets_a, trans, confidence = 0.8)
# inspect(sort(rules_a))


