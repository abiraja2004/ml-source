library(parallel)
library(lubridate)
library(readr)
library(dplyr)

# Loading huge graphs with igraph and R
# http://smallstats.blogspot.com.au/2012/12/loading-huge-graphs-with-igraph-and-r.html

#### download data
# url pattern
# http://cran-logs.rstudio.com/2017/2017-04-26.csv.gz
download_log_apply <- function(dates, download_folder = getwd(), only_missing = TRUE) {
    download_log <- function(date, download_folder = getwd()) {
        base_url <- 'http://cran-logs.rstudio.com'
        year <- lubridate::year(lubridate::ymd(date))
        file_name <- paste0(date, '.csv.gz')
        url <- paste(base_url, year, file_name, sep = '/')
        download.file(url, file.path(download_folder, file_name))
    }
    
    dir.create(download_folder, showWarnings = FALSE)
    
    files <- list.files(download_folder)
    is_found <- if (length(files) > 0 & only_missing) {
        as.Date(dates) %in% as.Date(sub('.csv.gz', '', files))
    } else {
        rep(FALSE, length(dates))
    }
    message('msg: ', sum(is_found), ' date(s) data found')
    message('\t', paste(dates[is_found], collapse = ', '))
    dates <- dates[!is_found]
    do.call(rbind, lapply(dates, function(d) {
        tryCatch({
            message('msg: start to download data for ', d)
            download_log(d, download_folder)
            data.frame(date = d, is_downloaded = TRUE, stringsAsFactors = FALSE)
        }, error = function(e) {
            message('\terror: fails to download data for ', d)
            data.frame(date = d, is_downloaded = FALSE, stringsAsFactors = FALSE)
        })
    }))
}

raw_path <- file.path(getwd(), 'raw')
start <- as.Date('2017-01-01')
end <- as.Date('2017-04-26')
days <- seq(start, end, by = 'day')

cores <- detectCores() - 1
days_split <- split(days, 1:cores)
cl <- makeCluster(cores)
init <- clusterEvalQ(cl, { library(lubridate); NULL })
result <- parLapplyLB(cl, days_split, download_log_apply, download_folder = raw_path)
stopCluster(cl)

result_df <- do.call(rbind, result)
#sum(as.Date(days) %in% as.Date(sub('.csv.gz', '', list.files(raw_path)))) == length(days)

