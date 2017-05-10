library(parallel)
library(readr)
library(rvest)

#### process items
process <- function(f, items, cores = detectCores() - 1, init_str = NULL, combine = rbind, ...) {
    stopifnot(cores > 0)
    
    set_env <- function(init_str) {
        e <- new.env()
        e$init_str <- init_str
        e
    }
    
    if (cores == 1) {
        results <- f(items = items, ...)
    } else {
        message('msg: create cluster of ', cores, ' nodes')
        splits <- split(items, 1:cores)
        cl <- makeCluster(cores)
        if (!is.null(init_str)) {
            clusterExport(cl, 'init_str', envir = set_env(init_str))
            init <- clusterEvalQ(cl, eval(parse(text = init_str)))
        }
        results <- parLapplyLB(cl, splits, f, ...)
        stopCluster(cl)
        message('msg: combine results')
        results <- do.call(combine, results)
    }
    results
}




