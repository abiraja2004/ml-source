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
    #fig.path <- get_path('rmd')
    fig.path <- getwd()
    opts_chunk$set(fig.path = fig.path)
    #opts_chunk$set(fig.cap = "center")
    render_jekyll()
    #knit(file.path(get_path('rmd'), paste0("_",title,".Rmd")), file.path(get_path('rmd'), paste0(title,".md")), envir = parent.frame())
    knit(file.path(getwd(), paste0("_",title,".Rmd")), file.path(get_path('rmd'), paste0(title,".md")), envir = parent.frame())
    
    # move fig files
    #try(moveFigs(fig.path), TRUE)
}