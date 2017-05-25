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
library(forcats)
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
outputtopickeys <- "mallet_output_files/mb_keys.txt"
outputdoctopics <- "mallet_output_files/mb_composition.txt"
topicwordsweight <- "mallet_output_files/topic_words_weight.txt"
stoplistfile <- "/stoplists/en.txt"

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
meta <- read.csv(file = "~/Desktop/ERMining/prepdata/ER_MyDay_Metadata.csv", header = TRUE, sep = ",") 

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

tmresults <- tmresults %>%
  mutate(Place_Name = fct_collapse(PlaceName, 
                                   "New York, New York" = c("New York (N.Y., United States)40.73093 -73.99764","New York, New York, United States", "New York (N.Y., United States)40.73093 ", "New York (N.Y., United States)40.73093 -73.997640", "New York, New York, United States"), 
                                   "Hyde Park, New York" = c("Hyde Park (Dutchess County, N.Y., United States)41.78330 -73.93330", "Hyde Park (Dutchess County, N.Y., United States)41.78330 ", "Hyde Park (Dutchess County, N.Y., United States)41.7833 -73.9333", "Hyde Park, New York, United States", "Hyde Park (Dutchess County, New York, United States)"), 
                                   "Asheville, North Carolina" = c("Asheville (N.C., United States)35.6 -82.55", "Asheville (N.C., United States)35.60000 -82.55000", "Asheville (N.C., United States)35.6 -82.55", "Asheville (N.C., United States)35.60000 -82.55000"),
                                   "Atlanta, Georgia" = c("Atlanta (Ga., United States)33.7333 -84.3833", "Atlanta (Ga., United States)33.73330 ", "Atlanta (Ga., United States)33.73330 -84.38330", "Atlanta (Ga., United States)33.7333 -84.3833"),
                                   "New Haven, Connecticut" = c("New Haven (Conn., United States)41.307210 -72.934110", "New Haven (Conn., United States)41.30721 -72.93411", "New Haven (Conn., United States)41.30721 "),
                                   "Boston, Massachusetts" = c("Boston (Mass., United States)42.35000 -71.05000", "Boston (Mass., United States)42.35 -71.05", "Boston (Mass., United States)42.35000 ", "Boston (Mass., United States)42.35000-71.05000"),
                                   "Chicago, Illinois" = c("Chicago (Ill., United States)41.850000 -87.650000", "Chicago (Ill., United States)41.85000 -87.65000", "Chicago (Ill., United States)41.85 -87.65", "Chicago (Ill., United States)41.85000 "),
                                   "Detroit, Michigan" = c("Detroit (Mich., United States)42.33143 -83.04575", "Detroit (Mich., United States)42.33143 "),
                                   "London, England" = c("London (England, United Kingdom)51.5 -0.1667", "London (England, United Kingdom)51.50000 -0.16670", "Buckingham Palace, London, United Kingdom", "London (England, United Kingdom)51.50000 " ),
                                   "Frankfurt, Germany" = c("Frankfurt am Main (Germany)50.10000 8.68330"), 
                                   "Berlin, Germany" = c("Berlin (Germany)52.53330 13.41670", "Berlin (Germany)52.5333 13.4167"), 
                                   "Dublin, Ireland" = c("Dublin (Ireland)53.33330 -6.25000"),
                                   "Phoenix, Arizonia" = c("Phoenix (Ariz., United States)33.43330 -112.06670", "Phoenix (Ariz., United States)33.43330 ", "Phoenix (Ariz., United States)33.4333 -112.0667", "Phoenix (Ariz., United States)33.4333 "),
                                   "Los Angeles, California" = c("Los Angeles (Calif., United States)34.05 -118.2333", "Los Angeles (Calif., United States)34.05000 -118.23330", "Los Angeles (Calif., United States)34.050000 -118.233300"),
                                   "San Francisco, California" = c("San Francisco (Calif., United States)37.76670 -122.41670", "San Francisco (Calif., United States)37.7667 -122.4167", "San Francisco (Calif., United States)37.766700 -122.416700"),
                                   "Omaha, Nebraska" = c("Omaha (Neb., United States)41.25000 -95.93330", "Omaha (Neb., United States)41.25 -95.9333"), 
                                   "Hartford, Connecticut" = c("Hartford (Conn., United States)41.75000 -72.68330", "Hartford (Conn., United States)41.75 -72.6833"),
                                   "Louisville, Kentucky" = c("Louisville (Ky., United States)38.25000 -85.75000"),
                                   "Campobello Island, Canada" = c("Campobello Island (N.B., Canada)44.88330 -66.95000", "Campobello Island (N.B., Canada)44.8833 -66.95", "Campobello Island (N.B., Canada)44.88330 "),
                                   "Albany, New York" = c("Albany (N.Y., United States)42.65000 -73.75000", "Albany (N.Y., United States)42.65000 "),
                                   "Montreal, Canada" = c("Montreal (Quebec, Canada)45.50000 -73.60000"),
                                   "Portland, Oregon" = c("Portland (Or., United States)45.51670 -122.66670", "Portland (Or., United States)45.5167 -122.6667"),
                                   "Westbrook, Connecticut" = c("Westbrook (Conn., United States)41.28330 -72.43330", "Westbrook (Conn., United States)41.2833 -72.4333"),
                                   "Springfield, Illinois" = c("Springfield (Ill., United States)39.80000 -89.63330"),
                                   "Duluth, Minnesota" = c("Duluth (Minn., United States)46.78330 -92.10000"),
                                   "Geneva, Switzerland" = c("Geneva (Switzerland)46.21670 6.15000", "Geneva (Switzerland)46.2167 6.15"),
                                   "Philadelphia, Pennsylvania" = c("Philadelphia (Pa., United States)39.95000 -75.15000", "Philadelphia (Penn., United States)39.95000 -75.15000", "Philadelphia (Penn., United States)39.95000 "),
                                   "Zurich, Switzerland" = c("Zurich (Switzerland)47.38330 8.55000"),
                                   "Brussels, Belgium" = c("Brussels (Belgium)50.83330 4.33330", "Brussels (Belgium)50.8333 4.3333"),
                                   "Soestdijk, Utrecht" = c("Soestdijk (Utrecht, Netherlands)52.18330 5.31670"),
                                   "Paris, France" = c("Paris (France)48.85850 2.29456", "Paris (France)48.8585 2.294555"),
                                   "St.Louis, Missouri" = c("Saint Louis (Mo., United States)38.61670 -90.19500", "Saint Louis (Mo., United States)38.6167 -90.195"),
                                   "Winnipeg, Canada" = c("Winnipeg (Man., Canada)38.88943 -77.03519"),
                                   "Regina, Canada" = c("Regina (Sask., Canada)50.47237 -104.63330"),
                                   "Edmonton, Canada" = c("Edmonton (Alta., Canada)53.56670 -113.41670"),
                                   "Calgary, Canada" = c("Calgary (Alta., Canada)51.08330 -114.08330"),
                                   "Columbia, Mississippi" = c("Columbia (Miss., United States)38.95000 -92.33330"),
                                   "Webster Groves, Missouri" = c("Webster Groves (Mo., United States)38.58330 -90.35000"),
                                   "South Hadley, Massachusetts" = c("South Hadley (Mass., United States)42.25000 -72.56670"),
                                   "Bloomington, Indiana" = c("Bloomington (Ind., United States)39.15000 -86.51670"),
                                   "Ames, Iowa" = c("Ames (Iowa, United States)42.03330 -93.61670"),
                                   "Chapel Hill, North Carolina" = c("Chapel Hill (N.C., United States)35.90000 -79.05000", "Chapel Hill (N.C., United States)35.9 -79.05"),
                                   "Oslo, Norway" = c("Oslo (Norway)59.93330 10.75000"), 
                                   "Stockholm, Sweeden" = c("Stockholm (Sweden)59.33330 18.08330"),
                                   "Helsinki, Finland" = c("Helsinki (Finland)60.13330 25.00000", "Helsink (Finland)60.13330 25.00000"),
                                   "Copenhagan, Denmark" = c("Copenhagen (Denmark)55.71670 12.56670", "Copenhagen (Denmark)55.7167 12.5667"),
                                   "Hague, Netherlands" = c("Hague (Netherlands)52.0833 4.2667"),
                                   "Luxembourg" = c("Luxembourg49.75000 6.16670"), 
                                   "Nantucket, Massachusetts" = c("Nantucket (Mass., United States)41.28330 -70.10000"),
                                   "Connecticut" = c("Connecticut (United States)41.83330 -72.83330"), 
                                   "Lenox, Massachusetts" = c("Lenox (Mass., United States)42.35000 -73.28330"),
                                   "Eastport, Maine" = c("Eastport (Me., United States)44.90000 -66.98330"), 
                                   "Portland, Maine" = c("Portland (Me., United States)43.65000 -70.25000", "Portland (Me., United States)43.650000 -70.250000"),
                                   "Beirut, Lebanon" = c("Beirut (Lebanon)33.86670 35.50000"),
                                   "Amman, Jordan" = c("Amman (Jordan)31.95000 35.93330"),
                                   "Tel Aviv, Israel" = c("Tel Aviv (Israel)32.08330 34.80000", "Tel Aviv (Israel)32.0833 34.8"),
                                   "Tiberiya, Israel" = c("Tiberiya, Israel32.80000 35.53330"),
                                   "Haifa, Israel" = c("Haifa (Isreal)32.81670 34.98330"),
                                   "Karachi, Pakistan" = c("Karachi (Pakistan)24.85000 67.03330"),
                                   "Peshawar, Pakistan" = c("Peshawar, North-West Frontier, Pakistan34.01670 71.66670"),
                                   "Lahore, Pakistan" = c("Lahore (Punjab, Pakistan)31.56670 74.36670"), 
                                   "New Delhi, India" = c("New Delhi (India)28.61670 77.21670"), 
                                   "Bombay, India" = c("Bombay (India)18.93330 72.85000"),
                                   "Trivandrum, India" = c("Trivandrum (India)8.50000 76.95000"),
                                   "Mysore, India" = c("Mysore (India)12.30000 76.61670"),
                                   "Hyderabad, India" = c("Hyderabad (India)17.36670 78.43330"),
                                   "Agra, India" = c("Agra (India)27.18300 78.01600"),
                                   "Jaipur, India" = c("Jaipur (India)26.88330 75.83330"),
                                   "Allahabad, India" = c("Allahabad (India)25.45000 81.83330"),
                                   "Kathmandu, Nepal" = c("Kathmandu (Nepal)27.70000 85.31670"),
                                   "Calcutta, India" = c("Calcutta (India)22.50000 88.33330"),
                                   "Honolulu, Hawaii" = c("Honolulu (Hawaii, United States)21.30000 -157.85000"),
                                   "Dallas, Texas" = c("Dallas (Tex., United States)32.78330 -96.80000", "Dallas (Tex., United States)32.7833 -96.8", "Dallas (Tex., United States)32.78330 "),
                                   "Milwaukee, Wisconsin" = c("Milwaukee (Wis., United States)43.03330 -87.90000", "Milwaukee (Wis., United States)43.0333 -87.9", "Milwaukee (Wis., United States)43.03330 "),
                                   "Chile" = c("Chile-30.00000 -71.00000"),
                                   "Santiago, Chile" = c("Santiago (Chile)-33.45000 -70.66670"),
                                   "Northfield, Minnesota" = c("Northfield (Minn., United States)44.45 -93.15"),
                                   "Saint Paul, Minnesota" = c("Saint Paul (Minn., United States)44.9333 -93.0833", "Saint Paul (Minn., United States)44.93330 ", "Saint Paul (Minn., United States)44.93330 -93.08330"),
                                   "Raleigh, North Carolina" = c("Raleigh (N.C., United States)35.7667 -78.6333", "Raleigh (N.C., United States)35.76670 -78.63330"),
                                   "Dayton, Ohio" = c("Dayton (Ohio, United States)39.75000 -84.18330"),
                                   "Youngstown, Ohio" = c("Youngstown (Ohio, United States)41.05 -80.65", "Youngstown (Ohio, United States)41.05000 -80.65000"),
                                   "Cleveland, Ohio" = c("Cleveland, (Ohio, United States)52.53330 13.41670", "Cleveland, (Ohio, United States)52.5333 13.4167", "Cleveland, (Ohio, United States)52.53330 "),
                                   "Miam Beach, Florida" = c("Miami Beach (Fla., United States)25.78330 -80.11670", "Miami Beach (Fla., United States)25.7833 -80.1167"),
                                   "Sarasota, Florida" = c("Sarasota (Fla., United States)27.33330 -82.51670", "Sarasota (Fla., United States)27.3333 -82.5167"),
                                   "Daytona Beach, Florida" =c("Daytona Beach (Fla., United States)29.2 -81.0167", "Daytona Beach (Fla., United States)29.20000 -81.01670"),
                                   "New Orleans, Louisiana" = c("New Orleans (La., United States)29.95000 -90.06670", "New Orleans (La., United States)29.95 -90.0667"),
                                   "Tokyo, Japan" = c("Tokyo (Japan)35.75000 139.50000", "Tokyo (Japan)35.75 139.5"),
                                   "Japan" = c("Japan36.00000 138.00000"),
                                   "Kyoto, Japan" = c("Kyoto (Japan)35.00000 135.75000"),
                                   "Osaka, Japan" = c("Osaka (Japan)34.66670 135.50000"),
                                   "Nara, Japan" = c("Nara, Kinki, Japan34.68330 135.83330"),
                                   "Hiroshima, Japan" = c("Hiroshima (Japan)34.38330 132.45000"),
                                   "Kyushu, Japan" = c("Kyushu, Japan33.00000 131.00000"),
                                   "Fukuoka, Japan" = c("Fukuoka, Japan33.65000 130.35000"),
                                   "Nikko, Japan" = c("Nikko (Japan)36.75000 139.61670"),
                                   "Hong Kong, China" = c("Hong Kong22.25000 114.16670"),
                                   "New Delhi, India" = c("New Delhi (India)28.6167 77.2167"),
                                   "Athens, Greece" = c("Athens (Greece)38.00000 23.73330"), 
                                   "Delphi, Greece" = c("Delphi (Greece)38.48330 22.50000"),
                                   "Patras, Greece" = c("Patras, Greece38.23330 21.73330"),
                                   "Nauplia, Greece" = c("Nauplia, Greece37.56670 22.80000"),
                                   "Corinth, Greece" = c("Corinth, Greece37.93330 22.91670"),
                                   "Belgrade, Montenegro" = c("Belgrade (Montenegro)44.83330 20.50000"),
                                   "Sarajevo, Yugoslavia" = c("Sarajevo (Yugoslavia)43.85000 18.38330"),
                                   "Kotor, Yugoslavia" = c("Kotor (Yugoslavia)42.45000 18.76670"),
                                   "Dubrovnic, Yugoslavia" = c("Dubrovnic (Yugoslavia)44.00000 21.00000"),
                                   "Zagreb, Yugoslavia" = c("Zagreb (Yugoslavia(1918-1991))45.80000 15.96670"),
                                   "Brinuni, Croatia" = c("Brijuni, Istarska, Croatia44.91670 13.76670"),
                                   "Ljubljana, Yugoslavia" = c("Ljubljana (Yugoslavia)46.06670 14.50000"),
                                   "Vienna, Austria" = c("Vienna (Austria)48.21670 16.36670", "Vienna (Austria)48.2167 16.3667"), 
                                   "Meeker, Colorado" = c("Meeker (Colo., United States)40.0333 -107.9", "Meeker (Colo., United States)40.03330 -107.90000"),
                                   "Richmond, Virginia" = c("Richmond (Va., United States)37.55000 -77.45000"),
                                   "Nashville, Tennessee" = c("Nashville (Tenn., United States)35.50000 -85.00000", "Nashville (Tenn, United States)35.50000 -85.00000"),
                                   "Minneapolis, Minnesota" = c("Minneapolis (Minn., United States)44.96670 -93.25000", "Minneapolis (Minn., United States)44.9667 -93.25", "Minneapolis (Minn., United States)44.96670 "),
                                   "Des Moines, Iowa" = c("Des Moines (Iowa, United States)41.60000 -93.60000", "Des Moines (Iowa, United States)41.60000 "),
                                   "Pittsburgh, Pennsylvania" = c("Pittsburgh (Pa., United States)40.43330 -79.98330", "Pittsburgh (Penn., United States)40.4333 -79.9833"),
                                   "Salt Lake City, Utah" = c("Salt Lake City (Utah, United States)40.75000 -111.88330", "Salt Lake City (Utah, United States)40.75 -111.8833"),
                                   "Seattle, Washington" = c("Seattle (Wash., United States)47.60000 -122.31670", "Seattle (Wash., United States)47.6 -122.3167", "Seattle (Wash., United States)47.6 ", "Seattle (Wash., United States)47.600000 -122.316700", "Seattle (Wash., United States)47.60000 ", "Seattle (Wash., United States)47.6 -122.3167"),
                                   "Ohio" = c("Ohio (United States)40.00000 -80.83330"),
                                   "Canada" = c("Canada60.00000 -96.00000"),
                                   "Glen Falls, New York" = c("Glen Falls (N.Y., United States)43.3 -73.6333"),
                                   "Granville, Ohio" = c("Granville (Ohio, United States)40.0667 -82.5167"),
                                   "Flint, Michigan" = c("Flint (Mich., United States)43.00917 -83.68561", "Flint (Mich., United States)43.00917 "),
                                   "Fayetteville, Arkansas" = c("Fayetteville (Ark., United States)36.05 -94.15"),
                                   "San Mateo, California" = c("San Mateo (Calif., United States37.55 -122.3167"),
                                   "Reno, Nevada" = c("Revo (Nev., United States)39.5167 -119.8"),
                                   "Sioux City, Iowa" = c("Sioux City (Iowa, United States)42.5 -96.4"),
                                   "Eau Claire, Wisconsin" = c("Eau Claire (Wis., United States)44.8 -91.4833"),
                                   "Wisconsin" = c("Wisconsin (United States)45.00000 -90.00000"),
                                   "Indianapolis, Indiana" = c("Indianapolis (Ind., United States)39.7667 -86.15", "Indianapolis (Ind., United States)39.76670 ", "Indianapolis (Ind., United States)39.76670 -86.15000"),
                                   "Athens, Ohio" = c("Athens (Ohio, United States)39.3167 -82.1"),
                                   "Champaign, Illinois" = c("Champaign (Ill., United States)40.1 -88.2333", "Champaign (Ill., United States)40.10000 "),
                                   "Michigan" = c("Michigan (United States)44.00000 -85.00000"),
                                   "Maine" = c("Maine (United States)45.00000 -69.00000"),
                                   "Ontario, Canada" = c("Ontario (Canada)50.00000 -86.00000"),
                                   "Idaho" = c("Idaho (United States)45.00000 -115.00000"),
                                   "Colorado" = c("Colorado (United States)39.00000 -105.00000"),
                                   "Wyoming" = c("Wyoming (United States)44.00000 -105.83330"),
                                   "Massachussets" = c("Massachussets (United Staes)42.25000 -71.83330"),
                                   "Woodstock, New York" = c("Woodstock (N.Y., United States)42.31670 -74.00000"),
                                   "Beverly Hills, California" = c("Beverly Hills (Calif., United States)34.06670 -118.38330", "Beverly Hills (Calif., United States)34.0667 -118.3833"),
                                   "Fresno, California" = c("Fresno, (Calif., United States)36.73330 -119.76670", "Fresno, (Calif., United States)36.73330 "), 
                                   "Little Rock, Arkansas" = c("Little Rock (Ark., United States)34.7333 -92.2833", "Little Rock (Ark., United States)34.73330 "),
                                   "Houston, Texas" = c("Houston (Tex., United States)29.75 -95.35", "Houston (Tex., United States)29.75000 -95.35000"),
                                   "Corpus Christi, Texas" = c("Corpus Christi (Tex., United States)27.8 -97.3833"),
                                   "Denver, Colorado" = c("Denver (Col., United States)39.7333 -104.9833", "Denver (Col., United States)39.73330 -104.98330"),
                                   "Madison, Wisconsin" = c("Madison (Wis., United States)43.0667 -89.4"),
                                   "Cedar Falls" = c("Cedar Falls (Iowa, United States)42.5167 -92.4333"),
                                   "Waterloo, Iowa" = c("Waterloo (Iowa, United States)42.4833 -92.3333"),
                                   "Knoxville, Tennessee" = c("Knoxville (Tenn., United States)35.95 -83.9167", "Knoxville (Tenn., United States)35.95000 ", "Knoxville (Tenn., United States)35.95000 -83.91670"),
                                   "Lancaster, Pennsylvania" = c("Lancaster (Pa., United States)40.0333 -76.3"),
                                   "Marseille, France" = c("Marseille (France)43.3 5.3667"),
                                   "Rome, Italy" = c("Rome (Italy)41.8833 12.5", "Rome (Italy)41.88330 12.50000"),
                                   "Deganya Alef, Israel" = c("Deganya Alef (Israel)32.7 35.5833"),
                                   "Boulder, Colorado" = c("Boulder (Colo., United States)40 -105.2667"),
                                   "Fall River, Massachussets" = c("Fall River (Mass., United States)41.7 -71.15", "Fall River (Bristol County, Mass., United States)41.70000 -71.15000"),
                                   "Manila, Philippines" = c("Manila (Philippines)14.61670 120.96670"),
                                   "Jakarta, Indonesia" = c("Jakarta (Indonesia)-6.13330 106.75000"),
                                   "Bali, Indonesia" = c("Bali, Indonesia-8.50000 115.00000"),
                                   "Ubud, Indonesia" = c("Ubud (Bali, Indonesia)-8.50100 115.27100"),
                                   "Sanur, Indonesia" = c("Sanur (Bali, Indonesia)-8.70996 115.25600"),
                                   "Bangkok, Thailand" = c("Bangkok (Thailand)13.73330 100.50000", "Bangkok (Thailand)13.73330 100.50000"), 
                                   "United States" = c("United States38.00000 -98.00000"),
                                   "Coral Gables, Florida" = c("Coral Gables (Fla., United States)25.7167 -80.2667"),
                                   "Charlotte, North Carolina" = c("Charlotte (N.C., United States)35.21670 -80.83330", "Charlotte (N.C., United States)35.2167 -80.8333"),
                                   "Bemidji, Minnesota" = c("Bemidji (Minn., United States)47.4667 -94.8667"),
                                   "Burlington, Minnesota" = c("Burlington (Iowa, United States)40.8 -91.1"),
                                   "Cedar Rapids, Iowa" = c("Cedar Rapids (Iowa, United States)42 -91.6333"),
                                   "Rochester, New York" = c("Rochester (N.Y., United States)43.15 -77.6", "Rochester (N.Y., United States)43.15000 -77.60000"),
                                   "Buffalo, New York" = c("Buffalo (N.Y., United States)42.8833 -78.8667", "Buffalo (N.Y., United States)42.88330 -78.86670"), 
                                   "Syracuse, New York" = c("Syracuse (N.Y., United States)43.0333 -76.1333", "Syracuse (N.Y., United States)43.03330 -76.13330", "Syracuse (N.Y., United States)43.03330 "),
                                   "Baltimore, Maryland" = c("Baltimore (Mary., United States)39.2833 -76.6", "Baltimore (Md., United States)39.2833 -76.6", "Baltimore (Md., United States)39.28330 -76.60000"),
                                   "Bellingham, Washington" = c("Bellingham (Wash., United States)48.75 -122.4833"),
                                   "Longview, Washington" = c("Longview (Wash., United States)46.13330 -122.93330"),
                                   "San Antonio, Texas" = c("San Antonio (Tex., United States)29.4167 -98.4833", "San Antonio (Tex., United States)29.41670 -98.48330"),
                                   "Lubbock, Texas" = c("Lubbock (Tex., United States)33.5667 -101.85", "Lubbock (Tex., United States)33.56670 -101.85000"),
                                   "Albuquerque, New Mexico" = c("Albuquerque (N.M., United States)35.0833 -106.65"),
                                   "Winter Park, Florida" = c("Winter Park (Fla., United States)28.5833 -81.3333"),
                                   "Nahunta, Georgia" = c("Nahunta (Ga., United States)40.73093 -73.99764"),
                                   "Urbana, Illinois" = c("Urbana (Ill., United States)40.1 -88.2"),
                                   "Red Wing, Minnesota" = c("Red Wing ( Minn., United States)44.55 -92.5333"),
                                   "Fort Worth, Texas" = c("Fort Worth (Tex., United States)32.7167 -97.3167", "Fort Worth (Tex., United States)32.71670 ", "Fort Worth (Tex., United States)32.71670 -97.31670"),
                                   "Charleston, West Virginia" = c("Charleston (W. VA., United States)38.3333 -81.6167", "Charleston (W.Va., United States)38.33330 -81.61670"),
                                   "Toronto, Canada" = c("Toronto, (Ont., Canada)43.65434 -79.38626"),
                                   "Amsterdam, Netherlands" = c("Amsterdam (Netherlands)52.35 4.9"),
                                   "St. Nazaire, France" = c("St. Nazaire (France)47.2833 -2.2"),
                                   "Saint-LÃ, France´" = c("Saint-LÃ´ (France)49.1167 -1.0833"),
                                   "La Baule, France" = c("La Baule (France)l47.3 -2.3833"),
                                   "Parkersburg, West Virginia" = c("Parkersburg (W. Va, United States)39.2667 -81.55"),
                                   "Marquette, Michigan" = c("Marquette (Mich, United States)46.5333 -87.3833"),
                                   "Columbus, Ohio" = c("Columbus (Ohio, United States)39.95 -82.9833", "Columbus (Ohio, United States)39.95000 -82.98330"),
                                   "Bradford, Vermont" = c("Bradford (Vt, United States)43.9833 -72.1167"),
                                   "Wilmington, Delaware" = c("Wilmington (Del., United States)39.7333 -75.5333"),
                                   "Wichita, Kansas" = c("Wichita (Kan., United States)37.6833 -97.3333"),
                                   "Schenectady, New York" = c("Schenectady (N.Y., United States)42.8 -73.9333"),
                                   "St. Catherines, Canada" = c("St. Catharines (Canada)43.1667 43.1667"),
                                   "Durham, North Carolina" = c("Durham (N.C., United States)35.98330 -78.88330"),
                                   "Miam, Florida" = c("Miami (Fla., United States)25.7667 -80.1833", "Miami (Fla., United States)25.76670 -80.18330"),
                                   "North Manchester, Indiana" = c("North Manchester41.00000 -85.76670"),
                                   "Spokane, Washington" = c("Spokane (Wash., United States)47.65 -117.4167"),
                                   "Tacoma, Washington" = c("Tacoma (Wash., United States)47.25 -122.4333"),
                                   "Pasco, Washington" = c("Pasco (Wash., United States)46.23330 "),
                                   "Chico, California" = c("Chico (Calif., United States)39.7167 -121.8333"),
                                   "Modesto, California" = c("Modesto (Calif., United States)37.63330 "),
                                   "Santa Rosa, California" = c("Santa Rosa (Calif., United States)38.4333 "),
                                   "San Marcos, Texas" = c("San Marcos (TX, United States)29.8667 -97.9333", "San Marcos (TX, United States)29.8667 "),
                                   "Iowa City, Iowa" = c("Iowa City (Iowa, United States)41.65 -91.5167"),
                                   "Morocco" = c("Morocco32 -5"),
                                   "Madrid, Spain" = c("Madrid (Spain)"),
                                   "Rabat, Morocco" = c("Rabat (Morocco)34.03330 -6.85000"),
                                   "Fez, Morocco" = c("Fez (Morocco)34.08330 -5.00000"),
                                   "Marrakech, Morocco" = c("Marrakech (Morocco)31.81670 -8.00000"),
                                   "Lawton, Oklahoma" = c("Lawton (Okla., United States)34.6 -98.3833"),
                                   "Glasgow, Scotland" = c("Glasgow (Scotland)55.9167 -4.25", "Glasgow (Scotland, United Kingdom)55.91670 "),
                                   "Nottingham, England" = c("Nottingham (England)52.9667 -1.1667", "Nottingham (England)52.96670 -1.16670"),
                                   "Boyce, Virginia" = c("Boyce (Va., United States)39.0833 -78.05"),
                                   "Winchester, Virginia" = c("Winchester (Va. United States)39.1833 -78.15"),
                                   "Moscow, Russia" = c("Moscow (Russia)55.75000 37.70000", "Moscow (Russia)55.750000 37.700000"),
                                   "Tashkent, Uzbekistan" = c("Tashkent, Tashkent, Uzbekistan41.26670 69.21670"),
                                   "Samarkand, Uzbekistan" = c("Samarkand (Uzbekistan)39.66670 66.95000"),
                                   "Cincinnati, Ohio" = c("Cincinnati (Ohio, United States)39.15 -84.45", "Cincinnati (Ohio, United States)39.15000 -84.45000"),
                                   "Memphis, Tennessee" = c("Memphis (TN, United States)35.1333 -90.0333", "Memphis (TN, United States)35.13330 ", "Memphis (Tenn., United States)35.13330 -90.03330"),
                                   "Wilkes-Barre, Pennsylvania" = c("Wilkes-Barre (Pa., United States)41.23330 -75.86670"),
                                   "Ann Arbor, Michigan" = c("Ann Arbor (Mich., United States)42.28330 -83.73330"),
                                   "Toledo, Ohio" = c("Toldeo (OH, United States)41.65 -83.55"),
                                   "Beloit, Wisconsin" = c("Beloit (Wis., United States)32.35 -87.1333"),
                                   "Lincoln, Nebraska" = c("Lincoln (Neb., United States)40.8 -96.6667"),
                                   "Warm Springs, Georgia" = c("Warm Springs (Ga., United States)32.8833 -84.6667", "Warm Springs (Meriwether County, Ga., United States)32.88330 -84.66670", "Warm Springs (Ga., United States)32.88330 -84.66670"),
                                   "Grand Rapids, Michigan" = c("Grand Rapids (Mich., United States)42.95 -85.6667"),
                                   "Elyria, Ohio" = c("Elyria (Ohio, United States)41.3667 -82.1"),
                                   "Florida" = c("Florida (United States)28 -82"),
                                   "Tucson, Arizonia" = c("Tucson (Ariz., United States)32.2167 -110.9167"),
                                   "Redding, California" = c("Redding (Calif., United States)40.5833 -122.3833"),
                                   "Berkeley, California" = c("Berkeley (Calif., United States)37.8667 -122.2667"),
                                   "Willimington, Ohio" = c("Willimington (Ohio, United States)39.4333 -83.8167"),
                                   "Austin, Texas" = c("Austin (Texas, United States)30.2667 -97.7333"),
                                   "Norman, Oklahoma" = c("Norman (Okla., United States)35.2167 -97.4333"),
                                   "Norfolk, Virginia" = c("Norfolk (Va., United States)36.8333 -76.2833"),
                                   "Las Vegas, Nevada" = c("Las Vegas (Nev., United States)36.1667 -115.1333", "Las Vegas (N.M., United States)35.58330 -105.21670"),
                                   "Bozeman, Montana" = c("Bozeman (Mont., United Staes)45.6667 -111.0333"),
                                   "Danbury, Connecticut" = c("Danbury (Conn., United States)41.3833 -73.45"),
                                   "Jeffersonville, Indiana" = c("Jeffersonville (Ind., United States)38.2667 -85.7333"),
                                   "Monteagle, Tennessee" = c("Monteagle (Tenn., United States)35.2333 -85.8333"),
                                   "New Canaan, Connecticut" = c("New Canaan (Conn., United States)41.1333 -73.4833", "New Canaan (Conn., United States)41.13330 -73.48330"),
                                   "Fort Collins, Colorado" = c("Fort Collins, Colo.40.58330 -105.08330"),
                                   "Delaware, Ohio" = c("Delaware, Ohio40.28330 -83.06670"),
                                   "Saint Petersburg, Russia" = c("Saint Petersburg (Russia)59.8833 30.25"),
                                   "Kansas" = c("Kansas (United States)38.66670 -98.00000"),
                                   "Lima, Ohio" = c("Lima (Ohio, United States)40.7333 -84.1"),
                                   "Birmingham, Michigan" = c("Birmingham (Mich., United States)42.5333 -83.2"),
                                   "Mexico City, Mexico" = c("Mexico City (Mexico)19.40000 -99.15000"),
                                   "Allentown, Pennsylvania" = c("Allentown (Penn., United States)40.6 -75.4667"),
                                   "North Carolina" = c("North Carolina (United States)35.50000 -80.00000"),
                                   "Indiana" = c("Indiana (United States)40.00000 -86.00000"),
                                   "South Dakota" = c("South Dakota (United States)45.00000 -100.00000"),
                                   "Illinois" = c("Illinois (United States)40.00000 -89.00000"),
                                   "Missouri" = c("Missouri (United States)38.00000 -98.00000"),
                                   "Williamsburg, Virginia" = c("Williamsburg (Va., United States)37.26670 -76.70000"),
                                   "Vancouver, Washington" = c("Vancouver, Washington, Unites States45.63330 -122.65000"),
                                   "Eugene, Oregon" = c("Eugene (Or., United States)44.05000 -123.08330"),
                                   "Iran" = c("Iran32.00000 53.00000"),
                                   "Niles, Michigan" = c("Niles (Mich., United States)41.81670 -86.25000"),
                                   "Muncie, Indiana" = c("Muncie (Ind., United States)40.18330 -85.38330"),
                                   "Princeton, New Jersey" = c("Princeton (NJ, United States)40.36670 -74.66670"),
                                   "Kansas City, Kansas" = c("Kansas City (Kan., United States)39.10000 -94.61670"),
                                   "Milford, Connecticut" = c("Milford (Conn., United States)41.21670 -73.05000"),
                                   "Niantic, Connecticut" = c("Niantic, Connecticut Niantic, Connecticut41.31670 -72.18330"),
                                   "Kittery, Maine" = c("Kittery, Maine, United States43.08330 -70.73330"),
                                   "Castine, Maine" = c("Castine (ME, United States)44.38330 -68.80000", "Castine (ME, United States)44.3833 -68.8"),
                                   "Center Harbor, Maine" = c("Center Harbor, Maine, United States44.25000 -68.56670"),
                                   "Concord, New Hampshire" = c("Concord, New Hampshire, US43.20000 -71.53330", "Concord, New Hampshire, US43.20000 "),
                                   "Mansfield, Pennsylvania" = c("Mansfield, Pa., United States41.80000 -77.06670"),
                                   "Emporia, Kansas" = ("Emporia, Kan., United States38.40000 -96.16670"), 
                                   "Tulsa, Oklahoma" = c("Tulsa (Okla., United States)36.15000 -95.98330", "Tulsa (Okla., United States)36.15000 "),
                                   "De Kalb, Illinois" = c("De Kalb (Ill., United States)41.91670 -88.75000"),
                                   "Jackson, Michigan" = c("Jackson (Mich., United States)42.23330 -84.40000"),
                                   "Needles, California" = c("Needles (Calif., United States)34.83330 -114.60000"),
                                   "Monterey, California" = c("Monterey (Calif., United States)36.60000 -121.88330"),
                                   "San Diego, California" = c("San Diego (Calif., United States)32.7 -117.15", "San Diego (Calif., United States)32.70000 -117.15000"),
                                   "San Pedro, California" = c("San Pedro (Calif., United States)33.73330 -118.28330"),
                                   "Providence, Rhode Island" = c("Providence (RI, United States)41.8167 -71.4", "Providence (RI, United States)41.81670 "),
                                   "Winfield, Kansas" = c("Winfield (Kan., United States)37.23330 -96.98330"),
                                   "River Falls, Wisconsin" = c("River Falls (Wis., United States)44.85000 -92.61670"),
                                   "Macomb, Illinois" = c("Macomb (Ill., United States)40.45000 -90.66670"),
                                   "Pittsfield, Massachusetts" = c("Pittsfield (Mass., United States)42.45000 -73.23330"),
                                   "Bayside, New York" = c("BAYSIDE (N.Y., United States)40.76670 -73.76670"),
                                   "Cambridge, Massachusetts" = c("Cambridge (Mass., United States)42.36670 -71.10000"),
                                   "Kitchener, Canada" = c("Kitchener (Ont., Canada)43.45000 -80.50000"),
                                   "Joliet, Illinois" = c("Joliet (Ill., United States)41.5167 -88.0667"),
                                   "Port Huron, Michigan" = c("Port Huron (Mich., United States)42.9667 -82.4167"),
                                   "Logan, Utah" = c("Logan (UT, United States)41.7333 -111.8333", "Logan (UT, United States)41.73330 -111.83330"),
                                   "Simsbury, Connecticut" = c("Simsbury (Conn., United States)41.8667 -72.8000"),
                                   "Waltham, Massachusetts" = c("Waltham (Mass., United States)42.3667 -71.2333", "Waltham (Mass., United States)42.36670 -71.23330"),
                                   "Racine, Wisconsin" = c("Racine, Racine (county), Wisconsin, United States42.7167 -87.7667"),
                                   "Stroudsburg, Pennsylvania" = c("Stroudsburg (Pa, United States)40.9833 -75.1833"),
                                   "Bangor, Maine" = c("Bangor (Me., United States)44.8 -68.7667"),
                                   "Saint John's, Canada" = c("Saint John's, Newfoundland, Canada47.55 -52.6667"),
                                   "Mankato, Minnesota" = c("Mankato (Minn, United States)44.15 -93.9833"),
                                   "St. Petersburg, Florida" = c("St. Petersburg (Fla, United States)27.7667 -82.6667"),
                                   "Norton, Massachusetts" = c("NORTON (Mass., United States)41.96670 -71.18330"),
                                   "Jerusalem, Israel" = c("Jerusalem (Israel)31.76670 35.23330"),
                                   "St. Petersburg, Florida" = c("St. Petersburg (Fla, United States)27.7667 -82.6667"),
                                   "Norton, Massachusetts" = c("NORTON (Mass., United States)41.96670 -71.18330"),
                                   "Jerusalem, Israel" = c("Jerusalem (Israel)31.76670 35.23330"),
                                   "Slippery Rock, Pennsylvania" = c("Slippery Rock (Pa., United States)41.05000 -80.05000"),
                                   "St. Moritz, Switzerland" = c("St. Moritz (Switzerland)46.50000 9.83330"),
                                   "Reedsville, West Virginia" = c("Reedsville (W. Va., United States)39.50000 ", "Reedsville (W. Va., United States)39.50000 -79.78330", "Reedsville (Preston County, W.Va., United States)39.50000 -79.78330"),
                                   "Ithaca, New York" = c("Ithaca (N.Y., United States)42.43330 ", "Ithaca (N.Y., United States)42.43330 -76.48330"),
                                   "Washington, D.C." = c("Washington (D.C., United States)38.88943 ", "White House (Washington, D.C.)38.89761 -77.03637", "Washington, DC, United States38.88943 -77.035190", "Washington (D.C., United States)38.88943 -77.03519"),
                                   "Jacksonville, Florida" = c("Jacksonville (Fla., United States)30.31670 -81.65000"),
                                   "Poughkeepsie, New York" = c("Poughkeepsie (N.Y., United States)41.70000 ", "Poughkeepsie (N.Y., United States)41.700000 ", "Poughkeepsie (N.Y., United States)41.70000 -73.91670"),
                                   "Milton, Pennsylvania" = c("Milton, Pennsylvania, USA41.00000 "),
                                   "Grayville, Illinois" = c("Grayville, Illinois, USA38.25000 "),
                                   "Portsmouth, New Hampshire" = c("Portsmouth (Rockingham County, N.H., United States)43.06670 "),
                                   "Ellsworth, Maine" = c("Ellsworth (Me., United States)44.53330 ", "Ellsworth (Me., United States)44.53330 -68.41670"),
                                   "Biddeford, New York" = c("Biddeford (York County, Maine, United States)43.48330 "),
                                   "Saratoga Springs, New York" = c("Saratoga Springs (N.Y., United States)43.06670 -73.78330"),
                                   "Connellsville, Pennsylvania" = c("Connellsville (Fayette County, Pa., United States)40.01670 "),
                                   "Cheyenne, Wyoming" = c("Cheyenne (Laramie County, Wyo., United States)41.13330 "),
                                   "Pueblo, Colorado" = c("Pueblo (Pueblo County, Colo., United States)38.25000 "),
                                   "Kansas City, Missouri" = c("Kansas City (Mo., United States)39.08330 ", "Kansas City (Mo., United States)39.08330 -94.56670"),
                                   "Pontiac, Illinois" = c("Pontiac (Ill., United States)40.86670 "),
                                   "Galion, Ohio" = c("Galion (Crawford County, Ohio, United States)40.73330 "),
                                   "York, Pennsylvania" = c("York, Pennsylvania, USA39.95000 "),
                                   "Saginaw, Michigan" = c("Saginaw (Mich., United States)43.41670 "),
                                   "Arthurdale, West Virginia" = c("Arthurdale (W.Va., United States)39.48330 -79.80000", "Arthurdale (W.Va., United States)39.48330 "),
                                   "Huntsville, Alabama" = c("Huntsville (Madison County, Ala., United States)34.71670 "),
                                   "Oklahoma City, Oklahoma" = c("Oklahoma City (Okla., United States)35.46670 ", "Oklahoma City (Okla., United States)35.46670 -97.50000"),
                                   "Alva, Oklahoma" = c("Alva (Woods County, Okla., United States)36.80000 "),
                                   "Jackson, Mississippi" = c("Jackson (Miss., United States)32.28330 "),
                                   "Gatlinburg, Tennessee" = c("Gatlinburg (Tenn., United States)35.70000 "),
                                   "Charleston, South Carolina" = c("Charleston (S.C., United States)32.76670 -79.91670"),
                                   "Morgantown, West Virginia" = c("Morgantown (W.Va., United States)39.61670 "),
                                   "Louisa, Kentucky" = c("Louisa (Lawrence County, Ky., United States)38.10000 "),
                                   "Saybrook, Connecticut" = c("Saybrook (Middlesex County, Conn., United States)41.28330 ", "Saybrook (Middlesex County, Conn., United States)41.28330 -72.36670"),
                                   "Ilion, New York" = c("Ilion (Herkimer County, N.Y., United States)43.00000 "),
                                   "Wallace, North Carolina" = c("Wallace (Duplin County, N.C., United States)34.73330 "),
                                   "Rhinebeck, New York" = c("Rhinebeck (NY, United States)41.91670 "),
                                   "Montchanin, Delaware" = c("Montchanin (New Castle County, Del., United States)39.78330 "),
                                   "Elmira, New York" = c("Elmira (N.Y., United States)42.08330 ", "Elmira (N.Y., United States)42.08330 -76.80000"),
                                   "Clinton, Iowa" = c("Clinton (Clinton County, Iowa, United States)41.83330 -90.183300"),
                                   "Caspar, Wyoming" = c("Casper (Natrona County, Wyo., United States)42.86670 "),
                                   "Mammoth Hot Springs, Wyoming" = c("Mammoth Hot Springs (Yellowstone Park, Wyo., United States)44.96670 "),
                                   "Boise, Idaho" = c("Boise (Ada County, Idaho, United States)43.60000 "),
                                   "Patchogue, New York" = c("Patchogue (Suffolk County, N.Y., United States)40.75000 ", "Patchogue (N.Y., United States)40.75000 -73.00000"),
                                   "Rock Island, Illinois" = c("Rock Island (Ill., United States)41.50000 "),
                                   "Rockford, Illinois" = c("Rockford (Ill., United States)42.26670 "),
                                   "Danville, Illinois" = c("Danville (Ill., United States)40.11670 "),
                                   "Fort Wayne, Indiana" = c("Fort Wayne (Ind., United States)41.11670 ", "Fort Wayne (Ind., United States)41.11670 -85.11670"),
                                   "Greensburg, Pennsylvania" = c("Greensburg (Westmoreland County, Pa., United States)40.30000 "),
                                   "Missoula, Montana" = c("Missoula (Missoula County, Mont., United States)46.86670 -113.98330"),
                                   "Fargo, North Dakota" = c("Fargo, (N.D., United States)46.86670 -96.78330", "Fargo (N.D., United States)46.86670 -96.78330"),
                                   "Lexington, Kentucky" = c("Lexington (Fayette County, Ky., United States)38.03330 -84.50000"),
                                   "Winchester, Kentucky" = c("Winchester (Clark County, Ky., United States)37.98330 -84.16670"),
                                   "Amarillo, Texas" = c("Amarillo (Potter County, Texas, United States)35.21670 -101.81670"),
                                   "Santa Fe, New Mexico" = c("Santa Fe (Santa Fe County, N.M., United States)35.68330 -105.93330"),
                                   "El Paso, Texas" = c("El Paso (El Paso County, Texas, United States)31.75000 -106.48330"),
                                   "Hampton, Virginia" = c("Hampton (Hampton Indep. City, Va., United States)37.01670 -76.33330", "Hampton (Va., United States)37.01670 -76.33330"),
                                   "Utica, New York" = c("Utica (N.Y., United States)43.10000 -75.21670"),
                                   "Rochester, Minnesota" = c("Rochester (Minn., United States)44.01670 -92.46670"),
                                   "East Orange, New Jersey" = c("East Orange (N.J., United States)40.76670 -74.20000"),
                                   "Bowling Green, Kentucky" = c("Bowling Green (Ky., United States)36.98330 -86.43330"),
                                   "Johnson City, Tennessee" = c("Johnson City (Tenn., United States)36.30000 -82.35000"),
                                   "Roanoke, Virgina" = c("Roanoke (Va., United States)37.26670 -79.93330"),
                                   "Joplin, Missouri" = c("Joplin (Mo., United States)37.08330 -94.46670"),
                                   "Charleston, Illinois" = c("Charleston (Ill., United States)39.48330 -88.16670"),
                                   "Quincy, Illinois" = c("Quincy (Ill., United States)39.93330 -91.40000"),
                                   "Kalamazoo, Michigan" = c("Kalamazoo (Mich., United States)42.28330 -85.58330"),
                                   "Birmingham, Alabama" = c("Birmingham (Ala., United States)33.51670 -86.80000"),
                                   "Columbia, South Carolina" = c("Columbia (S.C., United States)35.91670 -76.25000"),
                                   "Natchez, Mississippi" = c("Natchez (Miss., United States)31.55000 -91.40000"),
                                   "Beaumont, Texas" = c("Beaumont (Texas, United States)30.08330 -94.10000"),
                                   "Edingurg, Texas" = c("Edinburg (Texas, United States)26.30000 -98.15000"),
                                   "Harlingen, Texas" = c("Harlingen (Texas, United States)26.18330 -97.68330"),
                                   "Yuma, Arizonia" = c("Yuma (Yuma County, Ariz., United States)32.71670 -114.61670"),
                                   "Worcester, Massachusetts" = c("Worcester (Mass., United States)42.25000 -71.80000"),
                                   "Sweetwater, Tennessee" = c("Sweetwater (Tenn., United States)35.60000 -84.45000"),
                                   "Gadsden, Alabama" = c("Gadsden (Ala., United States)34.00000 -86.00000"),
                                   "Montgomery, Alabama" = c("Montgomery (Ala., United States)32.36670 -86.30000"),
                                   "Greenville, South Carolina" = c("Greenville (S.C., United States)34.88330 -82.36670", "Greenville (N.C., United States)35.60000 -77.36670"),
                                   "Clarksburg, West Virginia" = c("Clarksburg (W.Va., United States)39.26670 -80.33330"),
                                   "Carbondale, Illinois" = c("Carbondale (Ill., United States)37.71670 -89.21670"),
                                   "Delavan, Illinois" = c("Delavan (Ill., United States)40.36670 -89.53330"),
                                   "Robinson, Illinois" = c("Robinson (Ill., United States)39.00000 -87.73330"),
                                   "Huntington, West Virginia" = c("Huntington (W.Va., United States)38.41670 -82.43330"),
                                   "Reading, Pennsylvania" = c("Reading (Pa., United States)40.33330 -75.91670"),
                                   "Hutchinson, Kansas" = c("Hutchinson (Kan., United States)38.05000 -97.91670"),
                                   "St. Joseph, Missouri" = c("St. Joseph (Mo., United States)39.76670 -94.83330"),
                                   "Logan, West Virginia" = c("Logan (W.Va., United States)37.83330 -81.98330"),
                                   "Golden Beach, Florida" = c("Golden Beach (Fla., United States)25.95000 -80.11670"),
                                   "Lansing, Michigan" = c("Lansing (Mich., United States)42.71670 -84.55000"),
                                   "Kokomo, Indiana" = c("Kokomo (Ind., United States)40.48330 -86.13330"),
                                   "Hamilton, Ohio" = c("Hamilton (Ohio, United States)39.38330 -84.55000"),
                                   "Yosemite, California" = c("Yosemite National Park (Mariposa County, Calif., United States)37.85000 -119.56670"),
                                   "Carlin, Nevada" = c("Carlin (Nev., United States)40.70000 -116.10000"),
                                   "Fort Smith, Arkansas" = c("Fort Smith (Ark., United States)35.38330 -94.38330"),
                                   "Battle Creek, Michigan" = c("Battle Creek (Mich., United States)42.31670 -85.16670"),
                                   "Chattanooga, Tennessee" = c("Chattanooga (Tenn., United States)35.04563 -85.30968"),
                                   "Elkins, West Virgina" = c("Elkins (Randolph County, W.Va., United States)38.91670 -79.83330"),
                                   "Wheeling, West Virginia" = c("Wheeling (Ohio County, W.Va., United States)40.05000 -80.71670"),
                                   "New London, Connecticut" = c("New London (Conn., United States)41.35000 -72.10000"),
                                   "Orange, New Jersey" = c("Orange (Essex County, N.J., United States)40.76670 -74.21670"),
                                   "Lakeside, Ohio" = c("Lakeside (Ottawa County, Ohio, United States)41.53330 -82.73330"),
                                   "Chautauqua, New York" = c("Chautauqua (Chautauqua County, N.Y., United States)42.20000 -79.46670"),
                                   "Buzzards Bay, Massachusetts" = c("Buzzards Bay (Barnstable County, Mass., United States)41.73330 -70.61670"),
                                   "Nahant, Massachusetts" = c("Nahant (Essex County, Mass., United States)42.41670 -70.91670"),
                                   "Waterville, Maine" = c("Waterville (Kennebec County, Maine, United States)44.55000 -69.61670"),
                                   "St. Johnsbury, Vermont" = c("St. Johnsbury (Caledonia County, Vt., United States)44.41670 -72.00000"),
                                   "Canton, Ohio" = c("Canton (Ohio, United States)40.78330 -81.36670"),
                                   "Johnstown, Pennsylvania" = c("Johnstown (Cambria County, Pa., United States)40.31670 -78.91670"),
                                   "Henderson, North Carolina" = c("Henderson (Vance County, N.C., United States)36.31670 -78.38330"),
                                   "Abilene, Texas" = c("Abilene (Taylor County, Texas, United States)32.43330 -99.71670"),
                                   "Laredo, Texas" = c("Laredo (Tex., United States)27.50000 -99.50000"),
                                   "Amherst, Massachusetts" = c("Amherst (Mass., United States)42.36670 -72.51670"),
                                   "Georgetown, South Carolina" = c("Georgetown (S.C., United States)33.36670 -79.28330"),
                                   "Valdosta, Georgia" = c("Valdosta (Ga., United States)30.81670 -83.26670"),
                                   "Tuskegee, Alabama" = c("Tuskegee (Ala., United States)32.41670 -85.68330"),
                                   "Mobile, Alabama" = c("Mobile (Ala., United States)30.68330 -88.03330"),
                                   "Greensboro, North Carolina" = c("Greensboro (N.C., United States)36.06670 -79.78330"),
                                   "Peoria, Illinois" = c("Peoria (Ill., United States)40.68330 -89.58330"),
                                   "Hollywood, California" = c("Hollywood (Los Angeles, Calif., United States)34.08330 -118.31670"),
                                   "Oakland, California" = c("Oakland (Calif., United States)37.80000 -122.26670"),
                                   "Burlington, Vermont" = c("Burlington (Vt., United States)44.46670 -73.20000"),
                                   "Fitchburg, Massachusetts" = c("Fitchburg (Mass., United States)42.58330 -71.80000"),
                                   "Pensacola, Florida" = c("Pensacola (Fla., United States)30.41670 -87.21670"),
                                   "St. Cloud, Minnesota" = c("St. Cloud (Minn., United States)45.55000 -94.15000"),
                                   "Coronado, California" = c("Coronado (Calif., United States)32.68330 -117.16670"),
                                   "Salisbury, North Carolina" = c("Salisbury (N.C., United States)35.66670 -80.46670"),
                                   "Long Beach, California" = c("Long Beach (Calif., United States)33.76670 -118.18330"),
                                   "England" = c("England (United Kingdom)53.00000 ", "England (United Kingdom)53.00000 -2.00000"),
                                   "Londonderry, Northern Ireland" = c("Londonderry (Northern Ireland, United Kingdom)55.00000 "),
                                   "Northhampton, Massachusetts" = c("Northampton (Mass., United States)42.31670 -72.63330"),
                                   "Newton, Kansas" = c("Newton (Kan., United States)38.03330 -97.33330"),
                                   "New Zealand" = c("New Zealand-42.00000 174.00000"),
                                   "Auckland, New Zealand" = c("Auckland (New Zealand) Wellington (New Zealand)-36.91670 -41.28330 174.78330 174.78330", "Auckland (New Zealand)-36.91670 174.78330"),
                                   "Canberra, Australia" = c("Canberra (Australia)-35.30000 149.13330"),
                                   "Melbourne, Australia" = c("Melbourne (Vic., Australia)-37.78048 144.96050"),
                                   "Sydney, Australia" = c("Sydney (Australia)-33.91670 151.16670"),
                                   "Carin, Australia" = c("Cairn (Qld., Australia)-16.85000 145.71670"),
                                   "Australia" = c("Australia-25.00000 135.00000"),
                                   "South Pacific" = c("South Pacific-0.50000 -91.00000"),
                                   "Guadalcanal, Solomon Islands" = c("Guadalcanal (Solomon Islands)-9.53330 160.20000"),
                                   "Kingston, Jamaica" = c("Kingston (Jamaica)17.96670 -76.80000"),
                                   "San Juan, Puerto Rico" = c("San Juan (P.R., United States)18.48330 -66.13330"),
                                   "West Indies" = c("West Indies12.16670 -69.00000"),
                                   "Natal, Brazil" = c("Natal (Brazil)-5.76670 -35.25000"),
                                   "South America" = c("South America-10.00000 -55.00000"),
                                   "Caracas, Venezuela" = c("Caracas (Venezuela)10.50000 -66.91670"),
                                   "Balboa, Panama" = c("Balboa, Canal Zone, Panama8.95000 -79.55000"),
                                   "Guatemala City, Guatemala" = c("Guatemala City, Guatemala14.63330 -90.36670"),
                                   "Lake Junaluska, North Carolina" = c("Lake Junaluska (N.C., United States)35.51670 -82.95000"),
                                   "Quebec City, Canada" = c("Quebec City (Quebec, Canada)46.83330 -71.25000"),
                                   "Montreat, North Carolina" = c("Montreat (Buncombe County, N.C., United States)35.63330 -82.30000"),
                                   "Keene, New Hampshire" = c("Keene (N.H., United States)42.93330 -72.26670")))
#write csv with the results dataframe
tmresults <- tmresults %>% select(-PlaceName)
tmresults <- rename(tmresults, c("file"="filename", "topicnum"="topic_number", "value"="document_weight", "Columndate"="column_date", "Latitude"="latitude", "Longitude"="longitude", "docurl"="original_document_link")) 
tmresults <- subset(tmresults, select=c(1,8,2,3,4,5,9,6,7)) 
write.csv(results, "mallet_output_files/tmodeling_values.csv", row.names=FALSE)
saveRDS(tmresults, "app/data/tm_values.rds")
#see the first 10 rows in console.
head(tmresults, n=10)




