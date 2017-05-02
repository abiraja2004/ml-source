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

data_path <- file.path(getwd(), 'data')
raw_path <- file.path(getwd(), 'raw')
files <- data.frame(filename = list.files(raw_path), stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(sub('.csv.gz', '', filename))) %>% 
    filter(date >= as.Date('2017-04-01')) %>%
    select(filename) %>% unlist() %>% unname()

read_files <- function(files) {
    do.call(rbind, lapply(files, function(f) {
        read_csv(f)
    }))
}

cores <- detectCores() - 1
splits <- split(file.path(raw_path, files), 1:cores)
cl <- makeCluster(cores)
init <- clusterEvalQ(cl, { library(readr); NULL })
log <- parLapplyLB(cl, splits, read_files)
stopCluster(cl)

log <- bind_rows(log)
# write_rds(log, file.path(data_path, 'log_201704.rds'), compress = 'gz')
# log <- read_rds(file.path(data_path, 'log_201704.rds'))

## quantiles of package download count per group
upper_qntl_of_downloads <- log %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os)) %>%
    group_by(date, ip_id, r_version, r_arch, r_os) %>%
    summarise(count = n()) %>% ungroup() %>% 
    select(count) %>% unlist() %>% quantile(probs = seq(0.95, 1, 0.00625))

# 95% 95.625%  96.25% 96.875%   97.5% 98.125%  98.75% 99.375%    100% 
# 11      12      14      15      17      20      26      40    5748 

rec_by_num_pkg <- log %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os)) %>%
    group_by(date, ip_id, r_version, r_arch, r_os) %>%
    summarise(count = n()) %>% ungroup() %>% 
    group_by(count) %>% summarise(rec_by_num_pkg = n())

ggplot(rec_by_num_pkg %>% filter(count < 20), aes(x=count, y=rec_by_num_pkg)) +
    geom_point() + scale_y_continuous('Number of records', labels = comma) + 
    scale_x_continuous('Number of packages downloaded') +
    ggtitle('Number of records per number of downloaded packages') +
    theme(plot.title = element_text(hjust = 0.5))

# summary_list <- list(upper_qntl_of_downloads = upper_qntl_of_downloads,
#                      rec_by_num_pkg = rec_by_num_pkg)
# write_rds(summary_list, file.path(data_path, 'summary_list_201704.rds'), compress = 'gz')
# summary_list <- read_rds(file.path(data_path, 'summary_list_201704.rds'))

## filter data and add group id - keep only less than or equal to max_cnt (default 20) downloads
split_log <- function(log, cores = detectCores() - 1) {
    splits <- split(log, log$date)
    if (cores > length(splits)) cores <- length(splits)
    indx <- lapply(split(1:length(splits), 1:cores), unlist)
    lapply(indx, function(i) splits[i])    
}

filter_log <- function(log, less_than = 20) {
    bind_rows(log) %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os)) %>%
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

## filter by size, r_version, r_arch and r_os
## also keep groups that downloads less than 20 packages
cores <- detectCores() - 1
splits <- split_log(log, cores)
cl <- makeCluster(cores)
init <- clusterEvalQ(cl, { library(dplyr); NULL })
log <- parLapplyLB(cl, splits, filter_log)
stopCluster(cl)

## add group id - date + group id will be transaction id
log <- bind_rows(log)
# nrow(log)
# [1] 16713287
log <- add_group_idx(log)

## keep only transaction records - transaction id and package
splits <- split_log(log, cores)
cl <- makeCluster(cores)
init <- clusterEvalQ(cl, { library(dplyr); NULL })
log <- parLapplyLB(cl, splits, keep_trans_cols)
stopCluster(cl)

log <- bind_rows(log)
# nrow(log)
# [1] 16713287
log_single <- log %>% filter(count == 1) %>% select(-count)
log_multiple <- log %>% filter(count > 1) %>% select(-count)

# log_split <- list(single = log_single, multiple = log_multiple)
# write_rds(log_split, file.path(data_path, 'log_split_201704.rds'), compress = 'gz')
# log_split <- read_rds(file.path(data_path, 'log_split_201704.rds'))
# log_single <- log_split$single
# log_multiple <- log_split$multiple

add_split_group <- function(log, cores) {
    get_split_group <- function(num_rows, max_splits) {
        do.call(c, lapply(1:num_rows, function(i) {
            which.min(ifelse(i < max_splits, 1, 2))
        }))
    }
    
    uqe_id <- log %>% select(trans_id) %>% distinct()
    max_splits <- as.integer((1:cores)*(nrow(uqe_id)/cores))
    split_group <- get_split_group(nrow(uqe_id), max_splits)
    uqe_id <- bind_cols(uqe_id, data.frame(group = split_group))
    log %>% inner_join(uqe_id, by = 'trans_id')
}


log_single <- add_split_group(log_single, cores)
log_multiple <- add_split_group(log_multiple, cores)
# log_single %>% group_by(group) %>% summarise(cnt = n()) %>% ungroup() %>% summarise(sum(cnt))

## log_multiple to construction association rules

split(df_long[,'value'], df_long['id'])
get_value <- function(df) {
    df[, 'value']
}
tmp <- df_long %>% group_by(id) %>% do(value = get_value(.))
tmp %>% select(vals) %>% lapply(function(x) {
    dim(x)
})

## log_single + log_multiple to generate authority values


trans <- as(log_filtered, 'transactions')
# write_rds(trans, file.path(data_path, 'trans_201704.rds'), compress = 'gz')
# trans <- read_rds(file.path(data_path, 'trans_201704.rds'))

itemM <- as(trans@data, "dgCMatrix")
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
A <- rBind(cBind(leftM, t(itemM)), bottomM)

hub_arules <- hits(trans)
hits <- get_hits(A, itemset_info, item_info, verbose = TRUE)
hub <- hits$hub
auth <- hits$auth

# > tail(sort(hub_arules))
# 7376445     7376446     7376447     7376448     7376449     7376450 
# 0.001781054 0.001781054 0.001781054 0.001781054 0.001781054 0.001781054 
# > tail(sort(hub))
# 7376445      7376446      7376447      7376448      7376449      7376450 
# 0.0009308594 0.0009308594 0.0009308594 0.0009308594 0.0009308594 0.0009308594 
# hub_df <- data.frame(id = names(hub_arules), hub_arules = hub_arules, hub = hub)

transactionInfo(trans)[['weight']] <- hub_arules

n_support <- itemFrequency(trans, weighted = FALSE)
w_support <- itemFrequency(trans, weighted = TRUE)

df_support <- data.frame(n_support = n_support, w_support = w_support)

n_itemsets <- eclat(trans, parameter = list(support = 0.0001), control = list(verbose = TRUE))
w_itemsets <- weclat(trans, parameter = list(support = 0.0001), control = list(verbose = TRUE))

inspect(sort(n_itemsets))
inspect(sort(w_itemsets))

