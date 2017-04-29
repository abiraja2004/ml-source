library(parallel)
library(lubridate)
library(readr)
library(dplyr)

# Market basket analysis
# https://rpubs.com/sbushmanov/180410

raw_path <- file.path(getwd(), 'raw')
filename <- '2017-04-03.csv.gz'

log <- read_csv(file.path(raw_path, filename))
# ## quantiles of package download count per group
# log %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os)) %>%
#     group_by(ip_id, r_version, r_arch, r_os) %>%
#     summarise(count = n()) %>% arrange(-count) %>% 
#     ungroup() %>% select(count) %>% unlist() %>%
#     quantile(probs = seq(0.95, 1, 0.0125))

## filter data and add group id - filter only less than or equal to max_cnt (default 20) downloads
filter_log <- function(log, max_cnt = 20, keep_trans_cols = FALSE, verbose = TRUE) {
    nrec <- nrow(log)
    log <- log %>% filter(size > 1024, !is.na(r_version), !is.na(r_arch), !is.na(r_os))
    if (verbose) {
        date_str <- log %>% select(date) %>% slice(1) %>% mutate(date = as.character(date)) %>% unlist()
        message('msg: ', nrec, ' download records for ', date_str)
        message('msg: ', nrec - nrow(log), ' records filtered out by size, r_version, r_arch and r_os values')
    }
    nrec <- nrow(log)
    log <- log %>% group_by(ip_id, r_version, r_arch, r_os) %>% mutate(count = n()) %>%
        filter(count <= max_cnt) %>% arrange(ip_id, r_version, r_arch, r_os)
    if (verbose) {
        message('msg: ', nrec - nrow(log), ' records filtered out due to ', max_cnt, ' of maximum downloads per group')
    }
    nrec <- nrow(log)
    group_ids <- log %>% group_indices()
    if (verbose) {
        message('msg: ', length(unique(group_ids)), ' group found out of ', nrec, ' records')
    }
    log <- bind_cols(log, data.frame(id = group_ids)) %>% ungroup()
    if (keep_trans_cols) {
        log <- log %>% mutate(trans_id = paste(gsub('-', '', date), id, sep = '_')) %>%
            select(trans_id, package)
    }
    log
}

log_all <- filter_log(log, keep_trans_cols = FALSE)
log_trans <- filter_log(log, keep_trans_cols = TRUE)


