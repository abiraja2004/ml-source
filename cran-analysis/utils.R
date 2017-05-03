library(parallel)
library(lubridate)
library(scales)
library(ggplot2)
library(readr)
library(data.table)
library(dplyr)
library(igraph)
library(arules)
library(arulesViz)

############################################################
################# basic utility function ###################
############################################################
get_path <- function(base = getwd(), ...) {
    stopifnot(dir.exists(base))
    function() {
        file.path(base, paste(list(...), collapse = '/'))
    }
}

get_files <- function(path, rm_ptn = '.csv.gz', min_date = NULL, max_date = NULL) {
    files <- data.frame(filename = list.files(path), stringsAsFactors = FALSE) %>%
        mutate(date = as.Date(sub(rm_ptn, '', filename))) %>%
        filter(!is.na(date))
    if(!is.null(min_date)) files <- files %>% filter(date >= min_date)
    if(!is.null(max_date)) files <- files %>% filter(date <= max_date)
    files %>% select(filename) %>% unlist() %>% unname()
}

read_files <- function(files) {
    do.call(rbind, lapply(files, function(f) {
        read_csv(f)
    }))
}

pread_files <- function(files, path, cores = detectCores() - 1) {
    splits <- split(file.path(path, files), 1:cores)
    cl <- makeCluster(cores)
    init <- clusterEvalQ(cl, { library(readr); NULL })
    lst <- parLapplyLB(cl, splits, read_files)
    stopCluster(cl)
    bind_rows(lst)
}
#log <- pread_files(files[1:4], get_path('raw')(), 2)

############################################################
## functions related to constructing/merging transactions ##
############################################################
# split_log <- function(log, cores = detectCores() - 1) {
#     splits <- split(log, log$date)
#     if (cores > length(splits)) cores <- length(splits)
#     indx <- lapply(split(1:length(splits), 1:cores), unlist)
#     lapply(indx, function(i) splits[i])    
# }

filter_log <- function(log, less_than = 20) {
    bind_rows(log) %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os)) %>%
        select(date, ip_id, r_version, r_arch, r_os, package) %>%
        distinct(date, ip_id, r_version, r_arch, r_os, package) %>%
        group_by(date, ip_id, r_version, r_arch, r_os) %>% mutate(count = n()) %>%
        filter(count < less_than)
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
