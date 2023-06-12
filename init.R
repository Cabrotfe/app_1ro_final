# init.R
#
# Example R code to install packages if not already installed
# # aaa



my_packages = c("pacman","data.table","shiny",
                "readxl","ggplot2","dplyr",
                "writexl","DT","shinyauthr",
                "ggpubr","Hmisc","tidyr",
                "shinythemes","stringr",
                "plotly","shinyauthr",
                "shinyWidgets","rebus")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
