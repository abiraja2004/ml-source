library(parallel)
library(readr)
library(rvest)

## package information - name and version
get_package_names <- function(base_url = 'https://cran.r-project.org/web/packages/') {
    by_name_url <- paste0(base_url, 'available_packages_by_name.html')
    read_html(by_name_url) %>% html_nodes('td a') %>% html_text()    
}

get_package_info <- function(pkgs, attribute = c('version', 'depends', 'imports', 'suggests', 'published'), 
                             base_url = 'https://cran.r-project.org/web/packages/', verbose = TRUE) {
    set_pkg_url <- function(pkg) {
        paste0(base_url, pkg)
    }
    do.call(rbind, lapply(pkgs, function(p) {
        if (verbose) message('msg: get attributes of ', p)
        pkg_info <- tryCatch({
            read_html(set_pkg_url(p)) %>% html_nodes('td') %>% html_text()
        }, error = function(e) {
            NULL
        })
        atts <- if(!is.null(pkg_info)) {
            do.call(c, lapply(attribute, function(a) {
                # grep beginning of string
                ind <- grep(paste0('^', a), pkg_info, ignore.case = TRUE)
                if (length(ind) > 0) ind <- ind[1]
                att <- if (length(ind) > 0 && length(pkg_info) > ind) {
                    pkg_info[ind + 1]                
                } else {
                    NA
                }
                names(att) <- a
                att
            }))            
        } else {
            att <- rep(NA, length(attribute))
            names(att) <- attribute
            att
        }
        data.frame(package = p, t(atts), stringsAsFactors = FALSE)
    }))
}

package_names <- get_package_names()
write_rds(package_names, file.path(getwd(), 'package_names.rds'), compress = 'gz')

cores <- 4
item_split <- split(package_names, 1:cores)
cl <- makeCluster(cores)
init <- clusterEvalQ(cl, { library(rvest); NULL })
result <- parLapplyLB(cl, item_split, get_package_info)
stopCluster(cl)

result <- do.call(rbind, result)
data_path <- file.path(getwd(), 'data')
write_rds(result, file.path(data_path, 'package_info.rds'), compress = 'gz')

