# summary(europe)
# dim(europe)
library(magrittr)
library(plyr);library(dplyr)

munge <- function(x) {
  sites <- c("Stomach","Colon","Rectum","Liver","Lung",
             "Breast","Cervix","Ovary","Prostate","Leukaemia","ALL")
  x %<>% "["(apply(.,2, function(y) !all(is.na(y))))
  colnames(x) <- c("Period",sites)
  newx <- x[!x$Period%in%"",]
  newx$Period %>% 
    as.character %>%
    .[grepl("[A-z]",.)] %>%
    rep(.,each = 4) %>%
    data.frame(Country=.,newx) %>% 
    .[!apply(., 1, function(x) x[1]==x[2]),] %>% {
      values <- "["(.,,3:ncol(.)) %>% 
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

library(reshape2)
library(ggplot2)
# library(ggthemes)

concord %>% 
  melt %>% 
  ggplot(aes(x=Period,y=value)) +
  geom_line(aes(group=Country)) +
  geom_point(aes(group=Country)) +
  facet_grid(variable~Region,scales = "free_x") +
  scale_x_discrete(breaks=c("1995-99", "2000-04", "2005-09"),
                     labels=c("'95-'99", "'00-'04", "'05-'09")) +
  ylab(bquote(5-year~age-standardised~net~survival)) +
  xlab(bquote(Period~group("(",years,")"))) +
  ggtitle(bquote(atop(Global~surveillance~of~cancer~survival~1995-2009,
                      list(paste(25.7,M)~patients,67~countries~group("(", CONCORD-2,")"))))) +
  theme(axis.text.x = element_text(angle=45,hjust=1))

# ggsave("concord-2.pdf")
# ggsave("concord-2.png")
