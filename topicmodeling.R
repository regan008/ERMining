##This script runs mallet and saves the results for further analysis. 

##
#Load Libraries
##
library(reshape2)
library(dplyr)
library(tidyr)
library(tools)
library(lubridate)
library(plyr)
##
## Prep and Variable Creation ##
##

# Set working directory
dir <- "~/Desktop/ERMining" # adjust to suit
setwd(dir)

# folder containing txt files for MALLET to work on
importdir <- "~/Desktop/ERMining/txt"

# name of file for MALLET to train model on
output <- "mb.mallet"

# set number of topics for MALLET to use
ntopics <- 30

# set optimisation interval for MALLET to use
optint <-  20

# set file names for output of model, extensions must be as shown
outputstate <-  "topic-state.gz"
outputtopickeys <- "~/Desktop/ERMining/mallet_output_files/mb_keys.txt"
outputdoctopics <- "~/Desktop/ERMining/mallet_output_files/mb_composition.txt"
topicwordsweight <- "~/Desktop/ERMining/mallet_output_files/topic_words_weight.txt"
stoplistfile <- "~/Desktop/ERMining/stoplists/en.txt"

# combine variables into strings ready for command line
##note to self: count also use `--num-top-words 15` to change number of top words in topic. I think the default is 10 
cd <- "cd ~/mallet" # location of the bin directory
import <- paste("bin/mallet import-dir --input", importdir, "--output", output, "--keep-sequence --stoplist-file ", stoplistfile,  sep = " ")
train  <- paste("bin/mallet train-topics  --input", output, "--num-topics", ntopics, "--optimize-interval",  optint, "--random-seed 45 --output-state", outputstate,  "--output-topic-keys", outputtopickeys, "--output-doc-topics", outputdoctopics, "--topic-word-weights-file", topicwordsweight, sep = " ")


##
# setup system enviroment for R
##
MALLET_HOME <- "~/Mallet/" # location of the bin directory

##
#Run Mallet
##
system(paste(cd, import, train, sep = " ; "), invisible = FALSE)



##
# Inspect results and mainpulate to be useful for analysis.
##
setwd(MALLET_HOME)

outputtopickeysresult <- read.table(outputtopickeys, header=F, sep="\t")
##rather than try to transform this file in R, a python script (reshapeMallet.py) was used to transform the file.
#outputdoctopicsresult <-read.table("~/Desktop/ERMining/mallet_output_files/mb_composition.txt", header=F, sep="\t")
topicwordsweightresult <- read.table(topicwordsweight, header=F, sep="\t")
## look at top ten files.
head(outputtopickeysresult, n=10)

# manipulate outputdoctopicsresult to be more useful 
setwd(dir = "~/Desktop/ERMining/")
results <- read.csv("mallet_output_files/weights.csv")

#Format the filename so there is no extension 
#results[1,1] %>% file_path_sans_ext()
results[,"file"] <- basename(file_path_sans_ext(results$file)) 

#read in metadata 
meta <- read.csv(file = "~/Desktop/ERMining/ER_MyDay_Metadata.csv", header = TRUE, sep = ",") 

#rewrite the filename so that they do not include an extension and match the results file
meta[,"file"] <- basename(file_path_sans_ext(meta$file))

#extract year and create a new column with it
meta$year <- year(as.Date(meta$Columndate,"%b %d, %Y"))

#join the files by matching the file name.
results <- left_join(results, meta, by = "file")

#get rid of the X file that came in with metadata.
results <- results %>% select(-X)
tmresults <- ddply(results, .(file, topicnum, value, Columndate, year, PlaceName, Geolocation), summarise, docurl = paste('<a href=\"https://www2.gwu.edu/~erpapers/myday/displaydoc.cfm?_y=',year,'&_f=',file,'\">View the column</a>', sep=''))
tmresults <- tmresults %>%
  mutate(location = Geolocation)

tmresults <- tmresults %>% separate(Geolocation, c('Latitude', 'Longitude'), sep= " ")

tmresults$Latitude = as.numeric(as.character(tmresults$Latitude))

tmresults$Longitude = as.numeric(as.character(tmresults$Longitude))
tmresults <- tmresults %>% select(-location)
#write csv with the results dataframe
write.csv(results, "~/Desktop/ERMining/mallet_output_files/tmodeling_values.csv", row.names=FALSE)
saveRDS(tmresults, "data/tm_values.rds")
#see the first 10 rows in console.
head(tmresults, n=10)




