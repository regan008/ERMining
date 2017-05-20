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
  tm_values <- read.csv(file = "~/desktop/ERMining/mallet_output_files/tmodeling_values.csv", header=TRUE)
  byyear <- ddply(tm_values, .(topicnum, year.x), summarise, mean_value = mean(value))
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

bytopic <- tm_values %>% filter(topicnum == 0) %>% arrange(desc(value)) 



for(i in 0:59) {
  exploretopics_byyear(i)
  path = paste("files/topics_by_year/topic_",i,".png", sep="")
  ggsave(path, width=10, height=4)
}

for(i in 1:60) {
  
  path = paste("files/wordclouds/topic_",i,".png", sep="")
  png(path,width=500,height=300)
  exploretopicwords(i, 40)
  dev.off()
}


explorealltopicsbyyear <- function() {
    require(magrittr)
    require(ggplot2)
    require(plyr)
    tm_values <- read.csv(file = "~/Desktop/ERMining/mallet_output_files/tmodeling_values.csv", header=TRUE)
    colnames(tm_values)[6] <- "Year"
    byyear <- ddply(tm_values, .(topic, Year), summarise, mean_value = mean(value))
    sp <- ggplot(data=byyear, aes(x=Year, y=mean_value)) + geom_line() + geom_point() + ggtitle("Average Distribution of Topic By Year") + facet_wrap(~ topic, ncol=5) 
    sp + facet_wrap(~ topic, ncol=5) 
    ggsave("figure1.png", plot = sp, path="~/Downloads", device = NULL, width = 30, height = 30, dpi = 300)
    
}
explorealltopicsbyyear()

explorealltopic_edchanges <- function() {
  require(magrittr)
  require(ggplot2)
  require(plyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  #byyear <- tm_values %>% group_by(topic, Year) %>% arrange() %>% summarize(total = sum(value) / 100)
  byyear <- ddply(tm_values, .(topic, Year.x), summarise, mean_value = mean(value))
  sp <- ggplot(data=byyear, aes(x=Year.x, y=mean_value)) + geom_line() + geom_point()
  years_with_ed_change <- c(1896,1899,1900,1901,1903,1904,1907,1909,1911,1912,1927,1932,1934)
  sp <- sp %>% + geom_vline(xintercept = years_with_ed_change, color="blue") + facet_wrap(~ topic, ncol=6) +  ggtitle("All Topics Over Time With Editorial Shifts Highlighted")
  ggsave("figure5.png", plot = sp, path="~/Downloads", device = NULL, width = 30, height = 30, dpi = 300)
}
explorealltopic_edchanges()






exploretopicbyyear_editorialcomms <- function(topicnum) {
  require(magrittr)
  require(ggplot2)
  require(plyr)
  tm_values <- read.csv(file = "mallet_output_files/tmodeling_values.csv", header=TRUE)
  byyear <- ddply(tm_values, .(topic, Year.x), summarise, mean_value = mean(value))
  filterbytopicnum <- byyear %>% filter(topic == topicnum)
  plot <- ggplot(data=filterbytopicnum, aes(x=Year.x, y=mean_value)) + geom_line() + geom_point() + scale_x_continuous(name="Year", breaks=seq(1895, 1936, 2)) 
  years_with_ed_change <- c(1896,1899,1900,1901,1903,1904,1907,1909,1911,1912,1927,1932,1934)
  tby_ec_plot <- plot %>% + geom_vline(xintercept = years_with_ed_change, color="blue") + ggtitle("Average Distribution By Year With Editorial Shifts Highlighted in Blue, Topic 23")
  ggsave("figure6.png", plot = tby_ec_plot , path="~/Downloads", device = NULL, width = 30, height = 30, dpi = 300)
}
exploretopicbyyear_editorialcomms(23)


word_count<-function(txt_doc){
  con<-file(txt_doc, "r", blocking=FALSE)
  x<-readLines(con)
  #Remove YAML front matter on Rmd
  if(length(grep("---",x))>0){x<-x[-seq(1,max(grep("---",x)))]}
  wrds<-0
  for(line in x){
    #Removes non character and splits
    split_line<-strsplit(gsub("[^[:alnum:] ]", "", line), " +")[[1]]
    #Removes empty string
    split_line<-split_line[split_line!=""]
    wrds<-wrds+length(split_line)
  }
  return(wrds)
}
files <- list.files(path="txt/", pattern="*.txt")
wc <-  function(countwords) {
  for(file in files){
    word_count(file)
  }
}

word_count("txt/1930_April.txt")
#read.table("mallet_output_files/word_topic_counts.txt")
