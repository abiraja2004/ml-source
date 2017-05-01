library(reshape2)
library(dplyr)
library(igraph)
library(arules)
library(arulesViz)

# http://michael.hahsler.net/research/arules_RUG_2015/demo/#mine-association-rules

## helpers
# get item sets per transaction id
concat_items <- function(data) {
    unname(do.call(c, lapply(as.data.frame(t(data)), function(e) {
        e <- e[e != '']
        paste0('{', paste(e, collapse = ","), '}')
    })))
}

# HITS implementation
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

## create transaction data
#data("SunBai")
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

## calculate transaction weights
# manual
G <- graph.data.frame(df_long)
V(G)$type <- V(G)$name %in% df_long[, 1]
#G
plot(G, layout = layout.bipartite)

kmax <- 15
H <- get_hits(G, kmax)
wm <- H[[2]][rownames(H[[2]]) %in% df_long$id]

# arules
w <- hits(trans)

df_w <- data.frame(w_arules = w, w_manual = wm)

## add transaction weight
transactionInfo(trans)[['weight']] <- w
cbind(transactionInfo(trans), itemset = concat_items(df[, names(df) != 'id'])) %>%
    select(transactionID, itemset, weight)

## support
n_support <- itemFrequency(trans, weighted = FALSE)
w_support <- itemFrequency(trans, weighted = TRUE)
# A: (0.5176528+0.2321374+0.1476262+0.4123691)/sum(transactionInfo(trans)$weight)
df_support <- data.frame(n_support = n_support, w_support = w_support)

par(mfrow = c(1,2))
itemFrequencyPlot(trans, main = "Unweighted frequency")
itemFrequencyPlot(trans, weighted = TRUE, main = "Weighted frequency")
par(mfrow = c(1,1))

## frequent itemsets
# Closed Frequent Itemset
# http://www.hypertextbookshop.com/dataminingbook/public_version/contents/chapters/chapter002/section004/blue/page002.html
n_itemsets <- eclat(trans, parameter = list(support = 0.3), control = list(verbose = TRUE))
w_itemsets <- weclat(trans, parameter = list(support = 0.3), control = list(verbose = TRUE))
a_itemsets <- apriori(trans, parameter = list(target = 'frequent', support = 0.3), control = list(verbose = TRUE))

inspect(sort(n_itemsets))
inspect(sort(w_itemsets))
inspect(sort(a_itemsets))

## rule induction
n_rules <- ruleInduction(n_itemsets, confidence = 0.8)
w_rules <- ruleInduction(w_itemsets, confidence = 0.8)
a_rules <- ruleInduction(a_itemsets, trans, confidence = 0.8)

inspect(sort(n_rules))
inspect(sort(w_rules))
inspect(sort(a_rules))

# r_measure <- 'improvement'
# n_redundant <- is.redundant(n_rules, measure = r_measure)
# w_redundant <- is.redundant(w_rules, measure = r_measure)
# a_redundant <- is.redundant(a_rules, measure = r_measure)
# 
# inspect(sort(n_rules[!n_redundant]))
# inspect(sort(w_rules[!w_redundant]))
# inspect(sort(a_rules[!a_redundant]))



## remove duplicate rules 
# for better comparison we sort the rules by confidence and add Bayado's improvement 
rules <- sort(rules, by = "confidence") 
quality(rules)$improvement <- interestMeasure(rules, measure = "improvement") 
inspect(rules[1:5])

sum(is.redundant(rules, measure = "confidence")) 
sum(is.redundant(rules, measure = "improvement"))

rules[!is.redundant(rules, measure = "confidence")] rules[!is.redundant(rules,
measure = "improvement")]

rules_up <- rules[!is.redundant(rules)]





