library(magrittr)
library(recommenderlab)

# coercion
set.seed(2358)
x <- sample(c(0:5, NA), 50, replace = TRUE, prob = c(rep(.4/6, 6), .6))
m <- matrix(x, ncol = 10,
            dimnames = list(user = paste0('u', 1:5), item = paste0('i', 1:10)))
r <- as(m, 'realRatingMatrix')

getRatingMatrix(r)
identical(as(r, 'matrix'), m)
all.equal(as(r, 'matrix'), m)
as(r, 'list')
as(r, 'data.frame')

# normalization
r_m <- normalize(r)
r_m
getRatingMatrix(r_m)
denormalize(r_m)

image(r, main = 'Raw Ratings')
image(r_m, main = 'Normalized Ratings')

# binarization
r_b <- binarize(r, minRating = 4)
r_b
as(r_b, 'matrix')

# inspection
data("Jester5k")
Jester5k

set.seed(1234)
r <- sample(Jester5k, 1000)
r

rowCounts(r[1,])
rowMeans(r[1,])
rowSums(r[1,])
as(r[1,], 'list')

hist(getRatings(r), breaks = 100)
r %>% normalize() %>% getRatings() %>% hist(breaks = 100)
r %>% normalize(method = 'Z-score') %>% getRatings() %>% hist(breaks = 100)

# counts per user
r %>% rowCounts() %>% hist(breaks = 50)
# avg rating per joke
r %>% colMeans() %>% hist(breaks = 20)

# creating recommender
recommenderRegistry$get_entries(dataType = 'realRatingMatrix')

r <- Recommender(Jester5k[1:1000], method = 'POPULAR')
names(getModel(r))
getModel(r)$topN

recom <- predict(r, Jester5k[1001:1002], n = 5)
recom
as(recom, 'list')

recom3 <- bestN(recom, n = 3)
recom3
as(recom3, 'list')

recom <- predict(r, Jester5k[1001:1002], type = 'ratings') # NA if ratted
recom
as(recom, 'matrix')[,1:10]

recom <- predict(r, Jester5k[1001:1002], type = 'ratingMatrix')
recom
as(recom, 'matrix')[,1:10]

# evaluation - predicted ratings
e <- evaluationScheme(Jester5k[1:1000], method = 'split', train = .9, given = 15, goodRating = 5)
e

r1 <- Recommender(getData(e, 'train'), 'UBCF')
r1

r2 <- Recommender(getData(e, 'train'), 'IBCF')
r2

p1 <- predict(r1, getData(e, 'known'), type = 'ratings')
p1

p2 <- predict(r2, getData(e, 'known'), type = 'ratings')
p2

error <- rbind(
    UBCF = calcPredictionAccuracy(p1, getData(e, 'unknown')),
    IBCF = calcPredictionAccuracy(p2, getData(e, 'unknown'))
)
error

# evaluation - top-N
scheme <- evaluationScheme(Jester5k[1:1000], method = 'cross', k = 4, given = 3, goodRating = 5)
scheme

results <- evaluate(scheme, method = 'POPULAR', type = 'topNList', n = c(1,3,5,10,15,20))
results

getConfusionMatrix(results)[[1]]
avg(results)
plot(results, annotate = TRUE)
plot(results, 'prec/rec', annotate = TRUE)

# comparison
# top-N
set.seed(2016)
scheme <- evaluationScheme(Jester5k[1:1000], method = 'split', train = .9, k = 1, given = 5, goodRating = 5)
algorithms <- list(
    'random items' = list(name = 'RANDOM', param = NULL),
    'popular items' = list(name = 'POPULAR', param = NULL),
    'user-based CF' = list(name = 'UBCF', param = list(nn = 50)),
    'item-based CF' = list(name = 'IBCF', param = list(k = 50)),
    'SVD approximation' = list(name = 'SVD', param = list(k = 50))
)
results <- evaluate(scheme, algorithms, type = 'topNList', n = c(1,3,5,10,15,20))
results

names(results)
results[['user-based CF']]

plot(results, annotate = c(1, 3), legend = 'topleft')
plot(results, "prec/rec", annotate = 3, legend = "bottomright")

# 0-1 data set
Jester_binary <- binarize(Jester5k, minRating = 5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary) > 20]
Jester_binary

scheme_binary <- evaluationScheme(Jester_binary[1:1000], method = 'split', train = .9, k = 1, given = 3)
scheme_binary

results_binary <- evaluate(scheme_binary, algorithms, type = 'topNList', n = c(1,3,5,10,15,20))
plot(results_binary, annotate = c(1,3), legend = 'topright')
