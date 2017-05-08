library(recommenderlab)
library(ggplot2)

data("MovieLense")


image(MovieLense, main = 'Raw ratings')
summary(getRatings(MovieLense))
summary(getRatings(normalize(MovieLense, method = 'Z-score')))

qplot(getRatings(MovieLense), binwidth = 1, main = 'Histogram of ratings', xlab = 'Rating')
qplot(getRatings(normalize(MovieLense, method = 'Z-score')),
      main = 'Histogram of normalized ratings', xlab = 'Rating')

qplot(rowCounts(MovieLense), binwidth = 10,
      main = 'Moviews Rated on average',
      xlab = '# of users', ylab = '# of movies rated')

qplot(colMeans(MovieLense), binwidth = .1,
      main = 'Mean rating of Movies',
      xlab = 'Rating', ylab = '# of movies')

recommenderRegistry$get_entries(dataType = 'realRatingMatrix')


scheme <- evaluationScheme(MovieLense, method = 'split', train = .9, k = 1, given = 10, goodRating = 4)
scheme

algorithms <- list(
    'random items' = list(name = 'RANDOM', param = list(normalize = 'Z-score')),
    'popular items' = list(name = 'POPULAR', param = list(normalize = 'Z-score')),
    'user-based CF' = list(name = 'UBCF', param = list(normalize = 'Z-score', method = 'Cosine', nn=50)),
    'item-based CF' = list(name = 'IBCF', param = list(normalize = 'Z-score'))
)
results <- evaluate(scheme, algorithms, n = c(1, 3, 5, 10, 15, 20))
plot(results, annotate = 1:4, legend = 'topleft')
plot(results, 'prec/rec', annotate = 3)


set.seed(2358)
x <- sample(c(-2:2, NA), 50, replace = TRUE, prob = c(rep(.4/5, 5), .6))
mat <- matrix(x, ncol = 10,
              dimnames = list(user = paste0('u', 1:5), item = paste0('i', 1:10)))
realM <- as(mat, 'realRatingMatrix')

## binary
rm(list = ls())
data("MovieLense")
movie_bin <- binarize(MovieLense, minRating = 3)
scheme_bin <- evaluationScheme(movie_bin, method = 'split', train = .9, k = 1, given = 6)
scheme_bin

algorithms <- list(
    'random items' = list(name = 'RANDOM', param = NULL),
    'popular items' = list(name = 'POPULAR', param = NULL),
    'user-based CF' = list(name = 'UBCF', param = NULL),
    'item-based CF' = list(name = 'IBCF', param = NULL),
    'association rules CF' = list(name = 'AR', param = NULL)
)

results_bin <- evaluate(scheme_bin, algorithms, n = c(1, 3, 5, 10, 15, 20))

plot(results_bin, annotate = 1:4, legend = 'topleft')
plot(results_bin, 'prec/rec', annotate = 3)
