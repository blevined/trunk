library(dplyr)
library(lubridate)
library(igraph)


relato = read.csv("~/Downloads/relato-business-graph-database-QueryResult_all_maybe.csv",colClasses = rep("character",8))

relato$update_time_date = as.Date(relato$update_time_date)
relato$username = NULL


relato = relato %>% select(-home_name,-link_name)
relato = relato %>% select(-id_oid,-update_time_date)


keep_complete <- function(df){
  df = df[complete.cases(df),]
}
relato = keep_complete(relato)

relato = distinct(relato)

relato = relato %>% mutate(type = ifelse(relato$type == "","partnership",relato$type))

relato = relato %>% select(home_domain,link_domain,type) %>% mutate(type = as.factor(type))

relato = relato %>% mutate(weight = as.numeric(type)) %>% select(-type)

try = graph_from_data_frame(relato,directed = T)

adj = as.data.frame(get.adjacency(try,edges = T)




# 
# 
# relato = relato %>%  group_by(home_domain,update_time_date) %>%  mutate(numlinks = n())
# relato = relato %>%  group_by(home_domain,update_time_date,type) %>%  mutate(numlinks_by_type = n())
# 
# for (i in unique(relato$home_name)){
#   ifelse(relato)
# }
# 
# relato = relato %>% ungroup() %>%
#   mutate(matches = ifelse(relato$home_name %in% relato$link_name, T, F)) %>%
#   filter(matches = T)
# 
# 
# relato %>% filter(home_domain == "2015-03-07")
# 
# 
# 
# 
# 
# 
