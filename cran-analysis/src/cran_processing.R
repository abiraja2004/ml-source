library(parallel)
library(scales)
library(ggplot2)
library(igraph)
library(arulesViz)

source(file.path(getwd(), 'src', 'utils.R'))

#### read log
files <- get_log_names(path = get_path('raw'), extension = '.csv.gz', min_date = as.Date('2017-04-01'))
items <- file.path(get_path('raw'), files)
init_str <- "{ source(file.path(getwd(), 'src', 'utils.R')); NULL}"
# log <- process(f = read_files, items = items, cores = detectCores() - 1, init_str = init_str, combine = rbind)
# write_rds(log, file.path(get_path('data'), 'log_201704.rds'), compress = 'gz')
# log <- read_rds(file.path(get_path('data'), 'log_201704.rds'))

#### filter by size, r_version, r_arch, r_os
log_filtered <- log %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os)) %>%
    select(date, ip_id, r_version, r_arch, r_os, package) %>%
    distinct(date, ip_id, r_version, r_arch, r_os, package) %>%
    group_by(date, ip_id, r_version, r_arch, r_os) %>% mutate(count = n()) %>%
    arrange(date, ip_id, r_version, r_arch, r_os)
# write_rds(log_filtered, file.path(get_path('data'), 'log_filtered_201704.rds'), compress = 'gz')
# log_filtered <- read_rds(file.path(get_path('data'), 'log_filtered_201704.rds'))

log_trans <- log_filtered %>% group_by(count) %>%
    summarise(num_rec = n()) %>% ungroup() %>%
    mutate(num_trans = num_rec/count, 
           prop_trans = round(num_trans/sum(num_trans)*100, 3)) %>%
    mutate(count = as.factor(count))
# write_rds(log_trans, file.path(get_path('data'), 'log_trans_201704.rds'), compress = 'gz')
# log_trans <- read_rds(file.path(get_path('data'), 'log_trans_201704.rds'))


ggplot(log_trans[1:20,], aes(x = count, y = prop_trans)) + 
    geom_bar(stat="identity") + scale_y_continuous(labels = comma) +
    ggtitle('Proportion of Transactions by Downloaded Packages') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'Proportion of Transactions', y = 'Transactions')

############################
############################
construct_trans <- function(items, ...) {
    args <- list(...)
    less_than <- if (!is.null(args$less_than)) {
        args$less_than
    } else {
        20
    }
    
    lapply(items, function(itm) {
        log <- read_csv(itm) %>% filter_log(less_than = less_than) %>%
            add_group_idx() %>% keep_trans_cols()
    })
}

read_files <- function(items, ...) {
    do.call(rbind, lapply(items, function(itm) {
        read_csv(itm, ...)
    }))
}








#save_trans(files)

trans_files <- get_files(path = get_path('trans'), rm_ptn = '.rds')
trans_lst <- lapply(trans_files, function(x) {
    message('msg: reading ', x)
    read_rds(file.path(get_path('trans'), x))
})
names(trans_lst) <- sub('.rds', '', trans_files)

trans_all <- merge_trans_lst(trans_lst, excl = NULL)
excl <- unique(set_split_map()$splt)[unique(set_split_map()$splt) != 1]
trans_single <- merge_trans_lst(trans_lst, excl = excl)
trans_multiple <- merge_trans_lst(trans_lst, excl = 1)
# nrow(trans_all) == lapply(list(trans_single, trans_multiple), nrow) %>% unlist() %>% sum()
# write_rds(list(all = trans_all, single = trans_single, multiple = trans_multiple), 
#           file.path(get_path('trans')(), 'trans.rds'), compress = 'gz')
# trans <- read_rds(file.path(get_path('trans'), 'trans.rds'))
# trans_all <- trans$all
# trans_single <- trans$single
# trans_multiple <- trans$multiple

hub_arules <- hits(trans_multiple)
names(hub_arules) <- trans_multiple@itemsetInfo[[1]]

A <- get_adj(trans_multiple)
hits <- get_hits(A, trans_multiple@itemsetInfo[[1]], trans_multiple@itemInfo[[1]], verbose = TRUE)
hub <- hits$hub
auth <- hits$auth

tail_a <- tail(sort(hub_arules), 20)
tail_m <- tail(sort(hub), 20)
hub_df <- data.frame(trans_a = names(tail_a), hub_a = as.vector(tail_a),
                     trans_m = names(tail_m), hub_m = as.vector(tail_m),
                     stringsAsFactors = FALSE) %>%
    mutate(is_same = trans_a == trans_m)

trans <- trans_multiple
transactionInfo(trans)[['weight']] <- hub_arules

n_support <- itemFrequency(trans, weighted = FALSE)
w_support <- itemFrequency(trans, weighted = TRUE)

tail_n <- tail(sort(n_support), 20)
tail_w <- tail(sort(w_support), 20)
#df_support <- data.frame(n_support = n_support, w_support = w_support)

n_itemsets <- eclat(trans, parameter = list(support = 0.001), control = list(verbose = TRUE))
w_itemsets <- weclat(trans, parameter = list(support = 0.001), control = list(verbose = TRUE))

inspect(head(n_itemsets))
inspect(head(w_itemsets))

n_rules <- ruleInduction(n_itemsets, confidence = 0.8)
w_rules <- ruleInduction(w_itemsets, confidence = 0.8)









