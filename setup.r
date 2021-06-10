pkgs <- c('gtrendsR','reshape2','ggplot2','directlabels','readr','yaml')
install.packages(pkgs)
lapply(pkgs, require, character.only = TRUE)
source('time-intervals.r')