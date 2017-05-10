library(lubridate)
library(readr)
library(data.table)
library(dplyr)
library(arules)

############################################################
################# basic utility function ###################
############################################################
get_path <- function(..., base = getwd()) {
    stopifnot(dir.exists(base))
    file.path(base, paste(list(...), collapse = '/'))
}

get_log_names <- function(path, extension = '.csv.gz', min_date = NULL, max_date = NULL) {
    files <- data.frame(filename = list.files(path), stringsAsFactors = FALSE) %>%
        mutate(date = as.Date(sub(extension, '', filename))) %>%
        filter(!is.na(date))
    if(!is.null(min_date)) files <- files %>% filter(date >= min_date)
    if(!is.null(max_date)) files <- files %>% filter(date <= max_date)
    files %>% select(filename) %>% unlist() %>% unname()
}

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

#### download data
# url pattern - http://cran-logs.rstudio.com/2017/2017-04-26.csv.gz
# items <- seq(as.Date('2017-01-01'), as.Date('2017-01-02'), by = 'day')
# out1 <- process(f = download_log, items = items, cores = 1, download_folder = get_path('raw'))
# out2 <- process(f = download_log, items = items, cores = 2, download_folder = get_path('raw'))
download_log <- function(items, ...) {
    args <- list(...)
    download_folder <- if(!is.null(args$download_folder)) {
        args$download_folder
    } else {
        getwd()
    }
    download_log <- function(item, download_folder) {
        base_url <- 'http://cran-logs.rstudio.com'
        year <- lubridate::year(lubridate::ymd(item))
        file_name <- paste0(item, '.csv.gz')
        url <- paste(base_url, year, file_name, sep = '/')
        download.file(url, file.path(download_folder, file_name))
    }
    dir.create(download_folder, showWarnings = FALSE)
    do.call(rbind, lapply(items, function(itm) {
        tryCatch({
            message('msg: start to download data for ', itm)
            download_log(itm, download_folder = download_folder)
            data.frame(date = itm, is_downloaded = TRUE, stringsAsFactors = FALSE)
        }, error = function(e) {
            message('error: fails to download data for ', itm)
            data.frame(date = itm, is_downloaded = FALSE, stringsAsFactors = FALSE)
        })
    }))
}

#### read files
# items <- file.path(get_path('raw'), list.files(get_path('raw'))[1:2])
# out1 <- process(f = read_files, items = items, cores = 1)
# init_str <- '{ library(readr); NULL }'
# out2 <- process(f = read_files, items = items, cores = 2, init_str = init_str, combine = rbind)
read_files <- function(items, ...) {
    do.call(rbind, lapply(items, function(itm) {
        read_csv(itm, ...)
    }))
}

#### package information
## package name
get_pkg_names <- function(base_url = 'https://cran.r-project.org/web/packages/') {
    by_name_url <- paste0(base_url, 'available_packages_by_name.html')
    read_html(by_name_url) %>% html_nodes('td a') %>% html_text()    
}

## package information
# items <- get_pkg_names()
# out1 <- process(f = get_package_info, items = items[1:2], cores = 1, attribute = c('version', 'depends', 'imports', 'suggests'))
# init_str <- '{ library(rvest); NULL }'
# out2 <- process(f = get_package_info, items = items[1:4], cores = 2, init_str = init_str, combine = rbind)
get_package_info <- function(items, ...) {
    args <- list(...)
    attribute <- if (!is.null(args$attribute)) {
        args$attribute
    } else {
        c('version', 'depends', 'imports', 'suggests', 'published')
    }
    base_url <- if (!is.null(args$base_url)) {
        args$base_url
    } else {
        'https://cran.r-project.org/web/packages/'
    }
    info <- do.call(rbind, lapply(items, function(itm) {
        message('msg: get attributes of ', itm)
        itm_info <- tryCatch({
            read_html(paste0(base_url, itm)) %>% html_nodes('td') %>% html_text()
        }, error = function(e) {
            NULL
        })
        atts <- if (!is.null(itm_info)) {
            do.call(c, lapply(attribute, function(a) {
                # grep beginning of string
                ind <- grep(paste0('^', a), itm_info, ignore.case = TRUE)
                if (length(ind) > 0) ind <- ind[1]
                att_values <- if (length(ind) > 0 && length(itm_info) > ind) {
                    itm_info[ind + 1]
                } else {
                    NA
                }
            }))
        } else {
            rep(NA, length(attribute))
        }
        data.frame(itm, t(atts), stringsAsFactors = FALSE)
    }))
    names(info) <- c('package', attribute)
    rownames(info) <- NULL
    info
}

#### parallel processing
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

############################################################
## functions related to constructing/merging transactions ##
############################################################
# split_log <- function(log, cores = detectCores() - 1) {
#     splits <- split(log, log$date)
#     if (cores > length(splits)) cores <- length(splits)
#     indx <- lapply(split(1:length(splits), 1:cores), unlist)
#     lapply(indx, function(i) splits[i])    
# }

filter_log <- function(log, less_than = NULL) {
    log <- log %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os)) %>%
        select(date, ip_id, r_version, r_arch, r_os, package) %>%
        distinct(date, ip_id, r_version, r_arch, r_os, package) %>%
        group_by(date, ip_id, r_version, r_arch, r_os) %>% mutate(count = n())
    if (!is.null(less_than)) {
        log %>% filter(count < less_than)
    } else {
        log
    }
}

add_group_idx <- function(log) {
    group_idx <- log %>% group_indices()
    bind_cols(log, data.frame(id = group_idx)) %>% ungroup()
}

keep_trans_cols <- function(log) {
    bind_rows(log) %>% mutate(trans_id = paste(gsub('-', '', date), id, sep = '_')) %>%
        select(trans_id, package, count)
}

set_split_map <- function() {
    data.frame(count = 1:19, splt = c(1, rep(2, 2), rep(3, 3), rep(4:5, each = 4), rep(6, 5)))
}

split_log <- function(log, split_map = set_split_map()) {
    log <- log %>% filter_log() %>% add_group_idx() %>% keep_trans_cols() %>%
        inner_join(split_map, by = 'count') %>% setDT() %>% setkey(trans_id)
    split(log, by = 'splt')
}

construct_trans <- function(log) {
    log_cast <- log %>% select(trans_id, package) %>% 
        dcast(formula = trans_id ~ package, fun.aggregate = length)
    ids <- log_cast$trans_id
    log_cast <- log_cast[, -1] %>% as.matrix()
    rownames(log_cast) <- ids
    as(log_cast, 'transactions')
}

get_trans_info <- function(trans) {
    m <- trans@data
    items <- trans@itemInfo %>% unlist() %>% unname()
    itemsets <- trans@itemsetInfo %>% unlist() %>% unname()
    dimnames(m) <- list(items, itemsets)
    list(m = m, items = items, itemsets = itemsets)
}

get_sm <- function(info, items_all) {
    m <- info$m
    items <- info$items
    itemsets <- info$itemsets
    
    sm <- sparseMatrix(
        i = (length(items_all) - length(items)),
        j = ncol(m),
        x = 0,
        dimnames = list(items_all[!items_all %in% items], colnames(m))
    ) %>% as('ngCMatrix')
    sm[nrow(sm), ncol(sm)] <- FALSE
    sm
}

# do.call(merge_trans, list(trans1, trans2, trans3))
merge_trans <- function(...) {
    trans <- list(...)
    infos <- lapply(trans, get_trans_info)
    
    ms <- lapply(infos, function(info) info[['m']])
    items <- lapply(infos, function(info) info[['items']])
    itemsets <- lapply(infos, function(info) info[['itemsets']])
    
    items_all <- sort(unique(unlist(items)))
    sms <- lapply(infos, get_sm, items_all = items_all)
    
    m <- do.call(cBind, lapply(1:length(ms), function(i) {
        mb <- rBind(ms[[i]], sms[[i]])
        mb[sort(rownames(mb)), ]
    }))
    as(m, 'transactions')
}

# merge_trans_pair(trans1, trans2)
merge_trans_pair <- function(trans1, trans2) {
    info1 <- get_trans_info(trans1)
    m1 <- info1$m
    items1 <- info1$items
    itemsets1 <- info1$itemsets
    
    info2 <- get_trans_info(trans2)
    m2 <- info2$m
    items2 <- info2$items
    itemsets2 <- info2$itemsets
    
    # create 0 ngCMatrix for items not existing
    items_all <- sort(unique(c(items1, items2)))
    sm1 <- get_sm(info1, items_all)
    sm2 <- get_sm(info2, items_all)
    
    m1 <- rBind(m1, sm1)
    m2 <- rBind(m2, sm2)
    
    m <- cBind(m1[sort(rownames(m1)),], m2[sort(rownames(m2)), ])
    as(m, 'transactions')
}

# trans_files <- get_files(path = get_path('trans')(), rm_ptn = '.rds')
# trans_lst <- lapply(trans_files, function(x) {
#     message('msg: reading ', x)
#     read_rds(file.path(get_path('trans')(), x))
# })
# names(trans_lst) <- sub('.rds', '', trans_files)
# trans_all <- merge_trans_lst(trans_lst, excl = NULL)
# excl <- unique(set_split_map()$splt)[unique(set_split_map()$splt) != 1]
# trans_single <- merge_trans_lst(trans_lst, excl = excl)
# trans_multiple <- merge_trans_lst(trans_lst, excl = 1)
merge_trans_lst <- function(trans_lst, excl = unique(set_split_map()$splt)) {
    if (!is.null(excl) && all(sort(excl) == unique(set_split_map()$splt))) {
        stop('exclude all split groups')
    }
    
    trans_inner_merge <- lapply(names(trans_lst), function(nm) {
        message('msg: merge transactions, current - ', nm)
        lst <- if (!is.null(excl)) {
            trans_lst[[nm]][!names(trans_lst[[nm]]) %in% excl]
        } else {
            trans_lst[[nm]]
        }
        
        if(length(lst) < 2) {
            lst[[1]]
        } else {
            do.call(merge_trans, lst)
        }
    })
    
    if(length(trans_inner_merge) < 2) {
        trans_inner_merge
    } else {
        do.call(merge_trans, trans_inner_merge)
    }
}

save_trans <- function(files, from = get_path('raw')(), to = get_path('trans')()) {
    names(files) <- sub('.csv.gz', '', files)
    do.call(rbind, lapply(names(files), function(nm) {
        gc()
        message('msg: file name - ', nm)
        tryCatch({
            log <- read_csv(file.path(from, files[[nm]]))
            logs <- log %>% split_log()
            trans <- lapply(names(logs), function(nm) {
                message('msg: contructing logs of split group ', nm)
                construct_trans(logs[[nm]])
            })
            names(trans) <- names(logs)
            filename <- paste0(nm, '.rds')
            message('msg: saving to ', file.path(to, filename))
            write_rds(trans, file.path(to, filename), compress = 'gz')
            data.frame(date = nm, is_success = TRUE, stringsAsFactors = FALSE)
        }, error = function(e) {
            message('err: error caused')
            data.frame(date = nm, is_success = FALSE, stringsAsFactors = FALSE)
        })
    }))
}

psave_trans <- function(files, from = get_path('raw')(), to = get_path('trans')(), cores = detectCores() - 1) {
    splits <- split(files, 1:cores)
    cl <- makeCluster(cores)
    init <- clusterEvalQ(cl, { source(file.path(getwd(), 'utils.R')); NULL })
    lst <- parLapplyLB(cl, splits, save_trans, from = from, to = to)
    stopCluster(cl)
    bind_rows(lst)    
}

############################################################
###### functions related to executing HITS algorithm #######
############################################################
get_adj <- function(trans) {
    itemM <- trans@data
    item_info <- if ('levels' %in% names(trans@itemInfo)) {
        trans@itemInfo[, 'levels']
    } else {
        trans@itemInfo[[1]]
    }
    item_no <- length(item_info)
    itemset_info <- trans@itemsetInfo[[1]]
    itemset_no <- length(itemset_info)
    leftM <- sparseMatrix(i = itemset_no, j = itemset_no, x = 0)
    bottomM <- sparseMatrix(i = item_no, j = (itemset_no + item_no), x = 0)
    rBind(cBind(leftM, t(itemM)), bottomM)
}

run_hits <- function(A, k = 100, tol = 1e-8, verbose = FALSE){
    # mostly from Ch5 of Practical Graph Mining With R
    # https://www.csc2.ncsu.edu/faculty/nfsamato/practical-graph-mining-with-R/PracticalGraphMiningWithR.html
    
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

#
# knitPost aims to convert a R markdown (Rmd) file into a markdown file (md) for an article.
# It assumes the R markdown file should be named as '_YYYY-MM-DD-Article-Title.Rmd'
# when the article's title is YYYY-MM-DD-Article-Title.
# For further details, see http://jaehyeon-kim.github.io/intro/2014/11/19/R-Rroject-Setup/
#
# Usage
# source("src/knitSrc.R")
# knitPost("YYYY-MM-DD-Article-Title")
#
# last modified on Nov 20, 2014
#

knitPost <- function(title, base.url = "") {
    require(knitr)
    opts_knit$set(base.url = base.url)
    fig.path <- get_path('rmd')
    opts_chunk$set(fig.path = fig.path)
    #opts_chunk$set(fig.cap = "center")
    render_jekyll()
    knit(file.path(get_path('rmd'), paste0("_",title,".Rmd")), file.path(get_path('rmd'), paste0(title,".md")), envir = parent.frame())
    
    # move fig files
    #try(moveFigs(fig.path), TRUE)
}
