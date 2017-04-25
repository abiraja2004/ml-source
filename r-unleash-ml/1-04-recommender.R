mf_based_ucf <- function(ratings_matrix, X, Y, K, epoch=5000, alpha=0.0002, beta=0.02){
    
    #transpose Y
    Y <- t(Y)
    
    # Iterate epoch number of times
    for (step in seq(epoch)){
        for (i in seq(nrow(ratings_matrix))){
            for (j in seq(length(ratings_matrix[i, ]))){
                if (ratings_matrix[i, j] > 0){
                    # error 
                    eij = ratings_matrix[i, j] - as.numeric(X[i, ] %*% Y[, j])
                    
                    # gradient calculation 
                    for (k in seq(K)){
                        X[i, k] = X[i, k] + alpha * (2 * eij * Y[k, j] - beta * X[i, k])
                        Y[k, j] = Y[k, j] + alpha * (2 * eij * X[i, k] - beta * Y[k, j])
                    }
                }
            }
        }
        
        # Overall Squared Error Calculation
        e = 0
        
        for (i in seq(nrow(ratings_matrix))){
            for (j in seq(length(ratings_matrix[i, ]))){
                if (ratings_matrix[i, j] > 0){
                    e = e + (ratings_matrix[i, j] - as.numeric(X[i, ] %*% Y[, j]))^2
                    for (k in seq(K)){
                        e = e + (beta/2) * (X[i, k]^2 + Y[k, j]^2)
                    }
                }
            }
        }
        
        # stop if error falls below this threshold
        if (e < 0.001){
            break
        }
    }
    
    #inner product
    pR <- X %*% Y
    pR <- round(pR, 2)
    return (pR)
}

file_path <- file.path(getwd(), 'book-code', 'Module 1', 'Code', 'Chapter 4 code files')
file_name <- 'product_ratings.csv'

raw_ratings <- read.csv(file.path(file_path, file_name), header = TRUE, stringsAsFactors = FALSE)

ratings_matrix <- data.matrix(raw_ratings)
rows <- nrow(ratings_matrix)
columns <- ncol(ratings_matrix)

# latent featuers
K <- 2
# user/item feature matrix
set.seed(1237)
X <- matrix(runif(rows*K), nrow = rows, byrow = TRUE)
Y <- matrix(runif(columns*K), nrow = columns, byrow = TRUE)

# params
epoch <- 10000
alpha <- 0.0002
beta <- 0.02


pred_matrix <- mf_based_ucf(ratings_matrix, X, Y, K, epoch = epoch)
colnames(pred_matrix) <- c("iPhone.4","iPhone.5s","Nexus.5","Moto.X","Moto.G","Nexus.6","One.Plus.One")


data.frame(actual = ratings_matrix[1, ], pred = pred_matrix[1, ])
data.frame(actual = ratings_matrix[3, ], pred = pred_matrix[3, ])
data.frame(actual = ratings_matrix[6, ], pred = pred_matrix[6, ])


###### production ready recommender engines
### extract, transform and analyze
library(reshape2)
library(dplyr)
library(recommenderlab)
file_path <- file.path(getwd(), 'book-code', 'Module 1', 'Code', 'Chapter 4 code files')
file_name <- 'product_ratings_data.csv'

raw_data <- read.csv(file.path(file_path, file_name), stringsAsFactors = FALSE)
ratings_matrix <- as(raw_data, 'realRatingMatrix')

image(ratings_matrix[1:6, 1:10])

set.seed(1237)
sample_ratings <- sample(ratings_matrix, 1000)

rowMeans(sample_ratings[1,])
hist(getRatings(sample_ratings), breaks = "FD", xlab = 'Product Ratings', main = 'Histogram of Product Ratings')
hist(getRatings(normalize(sample_ratings)), breaks = "FD", xlab = 'Normalized Product Ratings', main = 'Histogram of Product Ratings')
hist(rowCounts(sample_ratings), breaks = "FD", xlab = 'Number of Products', main = 'Histogram of Product Count Distribution')

### model preparation and prediction
ubcf_recommender <- Recommender(ratings_matrix[1:1000], "UBCF")
recommendations <- predict(ubcf_recommender, ratings_matrix[1010:1011], n = 5)
as(recommendations, 'list')

### model evaluation
eval_scheme <- evaluationScheme(ratings_matrix[1:500], method = 'split', train = 0.9, given = 15)
eval_scheme

training_recommender <- Recommender(getData(eval_scheme, "train"), "UBCF")
test_rating <- predict(training_recommender, getData(eval_scheme, "known"), type = "ratings")
error <- calcPredictionAccuracy(test_rating, getData(eval_scheme, "unknown"))

error
