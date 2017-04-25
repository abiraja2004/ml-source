# https://rdrr.io/
# https://www.rdocumentation.org/

# https://whoosh.readthedocs.io/en/latest/index.html
# https://www.slideshare.net/ssuserb92f8d/what-is-the-best-full-text-search-engine-for-python
# http://rachbelaid.com/postgres-full-text-search-is-good-enough/

###### metrics
file_path <- file.path(getwd(), 'book-code', 'Module 1', 'Code', 'Chapter 3 code files')
file_name <- 'shopping_transaction_log.csv'

df <- read.csv(file.path(file_path, file_name), header = FALSE, stringsAsFactors = FALSE)

check_has_items <- function(items, df, andOr = "and") {
    unlist(lapply(1:nrow(df), function(i) {
        num_items <- sum(unlist(lapply(items, function(itm) {
            grepl(itm, paste(df[i,], collapse = "-"))
        })))
        if(andOr == "and") {
            num_items >= length(items) & num_items <= length(items)
        } else {
            num_items > 0
        }
    }))
}

get_support <- function(items, df, andOr = "and") {
    sum(check_has_items(items, df, andOr))/nrow(df)
}

### frequency
has_items <- check_has_items(c('beer', 'diapers'), df, "and")
freq <- sum(has_items)

### support - how likely an item set is found
## support is more about measureing the quality of rules detecting what has already happened from the past transactions
# IS_n = IS{beer, diapers}, S(IS_n) = freq(IS_n)/total_transactions
supp_b_and_d <- sum(check_has_items(c('beer', 'diapers'), df, 'and'))/sum(check_has_items("*", df, "or"))
# IS_x = IS{beer}, IS_y = IS{diapers}, S(IS{beer} -> IS{beer}) = freq(IS_x ∩ IS_y)/total_transactions
supp_b_to_d <- sum(check_has_items(c('beer', 'diapers'), df, 'and'))/sum(check_has_items("*", df, "or"))

### confidence - S(IS_x ∩ IS_y)/S(IS_x) => Freq(IS_x ∩ IS_y)/Freq(IS_x)
## confidence is more about detecting the quality of rules predicting what can happen in the future based on the past transactional data
conf_b_to_d <- sum(check_has_items(c('beer', 'diapers'), df, 'and'))/sum(check_has_items(c('beer'), df, 'and'))

### lift - S(IS_x ∩ IS_y)/S(IS_x)*S(IS_y)
lift_b_to_d <- get_support(c('beer', 'diapers'), df)/(get_support('beer', df)*get_support('diapers', df))

### minimum threshold on each 
# (1) to find item sets tha occur relatively frequently in transactions (support)
# (2) that show storing conditional relationship (confidence)
# (3) and that are more common than chance (lift)

###### techniques
### product contingency matrix
file_path <- file.path(getwd(), 'book-code', 'Module 1', 'Code', 'Chapter 3 code files')
file_name <- 'top_supermarket_transactions.csv'

data <- read.csv(file.path(file_path, file_name), stringsAsFactors = FALSE)
rownames(data) <- data[, 1]
data <- data[, -1]

sort(data['milk',], decreasing = TRUE)
mosaicplot(as.matrix(data), color = TRUE, title(main = 'Products Contingency Mosaic Plot'), las = 2)

## global recommendations
cat('Recommendations based on global products contingency matrix')
lapply(names(data), function(itm) {
    items <- sort(data[itm, ], decreasing = TRUE)
    items <- items[names(items) != itm]
    items[1:2]
})

## advanced contingency matrices
library(arules)
data("Groceries")

inspect(Groceries[1:3])

# measure = c("count", "support", "probability", "lift", "chiSquared")
ct <- crossTable(Groceries, measure = "count", sort = TRUE)
ct[1:5, 1:5]

ct <- crossTable(Groceries, measure = "support", sort = TRUE)
ct[1:5, 1:5]

ct <- crossTable(Groceries, measure = "lift", sort = TRUE)
ct[1:5, 1:5]

### frequent itemset generation
library(reshape2)
library(dplyr)
library(arules)

file_path <- file.path(getwd(), 'book-code', 'Module 1', 'Code', 'Chapter 3 code files')
file_name <- 'shopping_transaction_log.csv'

data <- read.csv(file.path(file_path, file_name), header = FALSE, stringsAsFactors = TRUE) %>%
    mutate(id = row_number())
data_long <- melt(data, id = "id") %>% filter(value != '') %>% select(id, value)
trans <- as(split(data_long[,'value'], data_long['id']), "transactions")

itemsets <- eclat(trans, parameter = list(support = 0.2, minlen = 2, maxlen = 2, target = "frequent itemsets", tidLists = TRUE))
inspect(itemsets)

itemsets <- eclat(trans, parameter = list(support = 0.2, minlen = 3, maxlen = 3, target = "frequent itemsets", tidLists = TRUE))
inspect(itemsets)

### association rule mining
library(arules)
library(arulesViz)

data("Groceries")
#methods(class = class(Groceries))
#?arules::merge

inspect(Groceries[1:3])
sort(itemFrequency(Groceries, type = "absolute"), decreasing = TRUE)[1:10]
itemFrequencyPlot(Groceries, topN = 10, type = "absolute")

## detecting & predicting shopping trends
metric_params <- list(supp = 0.001, conf = 0.5)
rules <- apriori(Groceries, parameter = metric_params)
inspect(rules[1:5])

## remove duplicate rules
# for better comparison we sort the rules by confidence and add Bayado's improvement
rules <- sort(rules, by = "confidence")
quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
inspect(rules[1:5])

sum(is.redundant(rules, measure = "confidence"))
sum(is.redundant(rules, measure = "improvement"))

rules[!is.redundant(rules, measure = "confidence")]
rules[!is.redundant(rules, measure = "improvement")]

rules_up <- rules[!is.redundant(rules)]

## specific patterns
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5, minlen = 2),
                 appearance = list(default = 'lhs', rhs = 'soda'), control = list(verbose = FALSE))
rules <- sort(rules, by = "confidence")
rules_up <- rules[!is.redundant(rules)]
inspect(sort(rules_up, decreasing = TRUE, by = 'confidence'))

params <- list(supp = 0.001, conf = 0.3, minlen = 2)
rules <- apriori(data = Groceries, parameter = params,
                 appearance = list(default = 'rhs', lhs = c('yogurt', 'sugar')), control = list(verbose = FALSE))
rules <- sort(rules, decreasing = TRUE, by = 'confidence')
inspect(rules)

## visualization
plot(rules, method = "graph", interactive = TRUE, shading = TRUE)

plot(rules, interactive = TRUE)

plot(rules, method = "graph", shading = TRUE)

