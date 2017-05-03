source(file.path(getwd(), 'utils.R'))

files <- get_files(path = get_path('raw')(), rm_ptn = '.csv.gz', min_date = as.Date('2017-04-01'))
# log <- pread_files(files, path = get_path('raw')(), cores = detectCores() - 1)
# write_rds(log, file.path(data_path, 'log_201704.rds'), compress = 'gz')
# log <- read_rds(file.path(data_path, 'log_201704.rds'))

#save_trans(files)

trans_files <- get_files(path = get_path('trans')(), rm_ptn = '.rds')
trans_lst <- lapply(trans_files, function(x) {
    message('msg: reading ', x)
    read_rds(file.path(get_path('trans')(), x))
})
names(trans_lst) <- sub('.rds', '', trans_files)

trans_all <- merge_trans_lst(trans_lst, excl = NULL)
excl <- unique(set_split_map()$splt)[unique(set_split_map()$splt) != 1]
trans_single <- merge_trans_lst(trans_lst, excl = excl)
trans_multiple <- merge_trans_lst(trans_lst, excl = 1)
# nrow(trans_all) == lapply(list(trans_single, trans_multiple), nrow) %>% unlist() %>% sum()
# write_rds(list(all = trans_all, single = trans_single, multiple = trans_multiple), 
#           file.path(get_path('trans')(), 'trans.rds'), compress = 'gz')
# trans <- read_rds(file.path(get_path('trans')(), 'trans.rds'))
# trans_all <- trans$all
# trans_single <- trans$single
# trans_multiple <- trans$multiple

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







