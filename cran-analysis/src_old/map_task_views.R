library(parallel)
library(readr)
library(rvest)
library(dplyr)

###
get_task_views <- function(base_url = 'https://cran.r-project.org/web/views/') {
    read_html(base_url) %>% html_nodes('td a') %>% html_text()    
}

get_packages_by_view <- function(view, all_pkgs, base_url = 'https://cran.r-project.org/web/views/') {
    view_url <- paste0(base_url, view, '.html')
    a <- read_html(view_url) %>% html_nodes('a') %>% html_text()
    pkgs <- unique(a[a %in% all_pkgs])
    data.frame(package = pkgs, task_view = view)
}

get_packages_by_view_apply <- function(views, all_pkgs, base_url = 'https://cran.r-project.org/web/views/') {
    do.call(rbind, lapply(views, function(v) {
        message('msg: current view - ', v)
        get_packages_by_view(v, all_pkgs, base_url)
    }))
}

data_path <- file.path(getwd(), 'data')
all_pkgs <- read_rds(file.path(data_path, 'package_names.rds'))
views <- get_task_views()
packages_by_view <- get_packages_by_view_apply(views, all_pkgs)
write_rds(packages_by_view, file.path(data_path, 'packages_by_view.rds'), compress = 'gz')

packages_by_view %>% group_by(package) %>% 
    summarise(packages = paste(task_view, collapse = ",")) %>%
    write_rds(file.path(data_path, 'packages_by_view1.rds'), 'gz')


