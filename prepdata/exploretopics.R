setwd("~/Desktop/ERMining/")
topicwordsweightresult <- read.table("mallet_output_files/topic_words_weight.txt", header=F, sep="\t")
topickeys <- read.table("mallet_output_files/mb_keys.txt", header=F, sep="\t")
exploretopicwords <- function(topicnum, numwords) {
    require(magrittr)
    require(wordcloud)
    require(dplyr)
    topicwords <- topicwordsweightresult %>% filter(V1 == topicnum) %>% arrange(desc(V3))
    topicwords$V1 <- NULL
    topicwords <- topicwords[1:numwords,]
    wc <- wordcloud(topicwords$V2, topicwords$V3, random.order=FALSE)
    return(wc)
}
#enter topic number, number of words in function below to generate a wordcloud.
exploretopicwords(6,50)

exploretopics_byyear <- function(topic) {
  require(magrittr)
  require(ggplot2)
  require(plyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  byyear <- ddply(tm_values2, .(topicnum, year), summarise, mean_value = mean(value))
  filterbytopicnum <- byyear %>% filter(topicnum == topic)
  tby_plot <- ggplot(data=filterbytopicnum, aes(x=year.x, y=mean_value)) + geom_line() + geom_point()  + scale_x_continuous(name="Year", breaks=seq(1936, 1965, 2)) + ggtitle("Average Distribution By Year") #+ scale_y_continuous(name="Average Topic Weight", labels = "percent")
  tby_plot
}
exploretopics_byyear(6)
byyear <- ddply(tm_values, .(topicnum, year.x), summarise, sum_value = mean(value))
filterbytopicnum <- byyear %>% filter(topicnum == 0)
tby_plot <- ggplot(filterbytopicnum, aes(year.x, sum_value)) + geom_bar(stat="identity") + scale_x_continuous(name="Year", breaks=seq(1936, 1965, 2)) #+ scale_y_continuous(name="Topic Percentage")
tby_plot + scale_y_continuous(labels=percent)

#find the docs heavily associated with a topic
doc_topic_assoc <- function(topic, numberofrows) {
  require(magrittr)
  require(ggplot2)
  require(dplyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  bytopic <- tm_values %>% filter(topicnum == topic) %>% arrange(desc(value)) 
  head(bytopic, n=numberofrows)
}
doc_topic_assoc(6, 5)

##Create a column in tm_values that includes the url for each document
final_tm_values <- ddply(tm_values, .(file, topicnum, value, Columndate, year, PlaceName, Geolocation), summarise, docurl = paste('<a href=\"https://www2.gwu.edu/~erpapers/myday/displaydoc.cfm?_y=',year,'&_f=',file,'\">View the column</a>', sep=''))
write.csv(final_tm_values, "mallet_output_files/tmodeling_values.csv", row.names=FALSE )

##create RDS files to avoid having to load really big csv docs.
saveRDS(tm_values, "data/tm_values.rds")
saveRDS(topickeys, "data/topickeys.rds")
saveRDS(topicwordsweightresult, "data/topic_words_weight.rds")

##get unique values from placename column
unique(tm_values$PlaceName)
library("dplyr")
test <- tm_values %>% group_by(Longitude,Latitude) %>% summarize(count=n())


