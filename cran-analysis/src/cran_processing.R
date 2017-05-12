library(parallel)
library(scales)
library(ggplot2)
library(rvest)
library(igraph)
library(arulesViz)

source(file.path(getwd(), 'src', 'utils.R'))

#### read log
files <- get_files(path = get_path('raw'), extension = '.csv.gz', min_date = as.Date('2017-04-01'))
items <- file.path(get_path('raw'), files)
# init_str <- "{ source(file.path(getwd(), 'src', 'utils.R')); NULL}"
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
    labs(x = 'Number of Downloaded Packages', y = 'Proportion of Transactions')

#### save transactions
files <- get_files(path = get_path('raw'), extension = '.csv.gz', min_date = as.Date('2017-04-01'))
items <- file.path(get_path('raw'), files)
init_str <- "{ source(file.path(getwd(), 'src', 'utils.R')); NULL}"
trans_save <- process(f = save_trans, items = items, cores = 4, init_str = init_str, combine = rbind)

#### merging transactions
txt <- 'A,B,C,D,E\nC,F,G,,\nA,B,,,\nA,,,,\nC,F,G,H,\nA,G,H,,'
df <- read.csv(text = txt, header = FALSE, stringsAsFactors = FALSE) %>%
    mutate(id = row_number()*100)
df_all <- df %>% melt(id = "id") %>% filter(value != '') %>% select(id, value)
df_1 <- df[1:3,] %>% reshape2::melt(id = "id") %>% 
    filter(value != '') %>% select(id, value)
df_2 <- df[4:6,] %>% reshape2::melt(id = "id") %>% 
    filter(value != '') %>% select(id, value)
trans_all <- as(split(df_all[, 'value'], df_all[, 'id']), 'transactions')
trans_1 <- as(split(df_1[, 'value'], df_1[, 'id']), 'transactions')
trans_2 <- as(split(df_2[, 'value'], df_2[, 'id']), 'transactions')
trans_merge <- do.call(bind_trans, list(trans_1, trans_2))

as(trans_all, 'data.frame') %>% 
    inner_join(as(trans_merge, 'data.frame'), by = c('transactionID' = 'itemsetID'))


files <- get_files(path = get_path('trans'), extension = '.rds')
items <- file.path(get_path('trans'), files)
init_str <- "{ source(file.path(getwd(), 'src', 'utils.R')); NULL}"
trans_all <- process(f = read_trans, items = items, cores = detectCores() - 1,
                     init_str = init_str, combine = bind_trans, excl_group = NULL)
trans_multiple <- process(f = read_trans, items = items, cores = detectCores() - 1,
                          init_str = init_str, combine = bind_trans, excl_group = 1)

trans_size <- data.frame(from_trans_all = nrow(trans_all),
                         from_trans_mult = nrow(trans_multiple))

log_trans %>% filter(as.integer(count) <= 20) %>% 
    summarise(from_log_all = sum(num_trans)) %>% bind_cols(trans_size)
























do.call(sum, tmp)

t <- read_rds(items[1])

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









