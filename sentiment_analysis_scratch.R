library(tidytext)
library(stringr)
library(stats)

listings = read.csv("~/Downloads/listings.csv")

listings = listings[listings$host_identity_verified == "t",]


sent_master = get_sentiments("bing")

#positive and negative sentiment lists
positive_sentiment = sent_master[sent_master$sentiment == "positive",]
negative_sentiment = sent_master[sent_master$sentiment != "positive",]
negative_sentiment$word=gsub("[[:punct:]]", " ", negative_sentiment$word)

#count number of times a positive sentiment word shows up in description
for(i in c(1:nrow(listings))){ 
a[i] = c(sum(str_detect(listings[i,]$description,positive_sentiment$word),na.rm = T) -1)}
listings = cbind(listings,a) 
listings = listings %>% dplyr::rename(count_pos_description = a)


#count number of times a negative sentiment word shows up in description
for(i in c(1:length(listings$id))){ 
  b[i] = c(sum(str_detect(listings[i,]$description,negative_sentiment$word),na.rm = T) -1)}

listings = cbind(listings,b) 
listings = listings %>% dplyr::rename(count_neg_description = b)



listings$price as.character(listings$price)
listings$price = gsub("\\$","",listings$price)
listings$price = as.numeric(listings$price)



# j = read.csv("~/Downloads/listings.csv")
# j = j[j$host_identity_verified == "t",]
# listings$price = j$price



fit = lm(price ~ count_pos_description + count_neg_description, listings)
