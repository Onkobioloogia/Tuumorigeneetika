# summary(europe)
# dim(europe)
library(magrittr)
library(plyr);library(dplyr)
# europe <- read.csv("data/concord-2/europe.csv",header = FALSE)

munge <- function(x) {
  sites <- c("Stomach","Colon","Rectum","Liver","Lung",
             "Breast","Cervix","Ovary","Prostate",
             "Leukaemia.adult","ALL.children")
  colnames(x) <- c("Period",sites)
  newx <- x[!x$Period%in%"",]
  newx$Period %>% 
    as.character %>%
    .[grepl("[A-z]",.)] %>%
    rep(.,each = 4) %>%
    data.frame(Country=.,newx) %>% 
    .[!apply(., 1, function(x) x[1]==x[2]),] %>% {
      values <- "["(.,,3:11) %>% 
        apply(.,2, .%>%as.character%>%as.numeric)
      "["(.,,1:2) %>% cbind(values)}
}

concord <- list.files(path = "data/concord-2/", pattern = ".csv", full.names = TRUE) %>%
  sapply(read.csv,header = FALSE) %>%
  lapply(munge) %>%
  ldply %>%
  mutate(Region=gsub("data/concord-2//([A-z-]+).csv","\\1",.id),
         Period=sub("([0-9-]+) ","\\1",Period)) %>%
  droplevels

concord %>% summary

library(reshape2)
library(ggplot2)
concord %>% 
  melt %>% 
  ggplot(aes(x=factor(Period),y=value)) +
  geom_boxplot() +
  facet_grid(variable~Region,scales = "free_x")
