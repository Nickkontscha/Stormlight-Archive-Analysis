

library(XML)
library(xml2)
library(openxlsx)
library(dplyr)

# Load the data of each book
source("loadData/loadData_characters.R")
source("loadData/loadData_wok.R")
source("loadData/loadData_wor.R")
source("loadData/loadData_oath.R")

# Fix minor encoding issues
Encoding(Oathbringer[["text"]]) <- "UTF-8"
Encoding(Oathbringer[["chapter"]]) <- "UTF-8"

# Get characters from the alphabet (and spaces) only and remove possible leading white spaces
WayOfKings$text <- gsub("[^[:alnum:][:space:]]", "", WayOfKings$text)
WordsOfRadiance$text <- gsub("[^[:alnum:][:space:]]", "", WordsOfRadiance$text)
Oathbringer$text <- gsub("[^[:alnum:][:space:]]", "", Oathbringer$text)

WayOfKings$chapter <- gsub("[^[:alnum:][:space:]]", "", WayOfKings$chapter)
WordsOfRadiance$chapter <- gsub("[^[:alnum:][:space:]]", "", WordsOfRadiance$chapter)
Oathbringer$chapter <- gsub("[^[:alnum:][:space:]]", "", Oathbringer$chapter)

# Join each book with their chapter names to get the information about who that chapter is about
WayOfKings <- left_join(WayOfKings, char_wok, by = "chapter")
WordsOfRadiance <- left_join(WordsOfRadiance, char_wor, by = "chapter")
Oathbringer <- left_join(Oathbringer, char_oath, by = "chapter")

# Join all books into a data frame and give the chapter nr a prefix to not confuse e.g. chapter 1 from one book with another
AllBooks <- rbind(WayOfKings, WordsOfRadiance, Oathbringer)
AllBooks$book <- c(rep("Way Of Kings", times = nrow(WayOfKings)),
                   rep("Words Of Radiance", times = nrow(WordsOfRadiance)),
                   rep("Oathbringer", times = nrow(Oathbringer)))
AllBooks$book <- factor(AllBooks$book, levels = unique(AllBooks$book))
AllBooks$character <- factor(AllBooks$character, levels = unique(AllBooks$character)[order(unique(AllBooks$character))])
AllBooks$nr[AllBooks$book == "Way Of Kings" & !is.na(AllBooks$nr)] <-  paste("WoK_", AllBooks$nr[AllBooks$book == "Way Of Kings" & !is.na(AllBooks$nr)], sep = "")
AllBooks$nr[AllBooks$book == "Words Of Radiance" & !is.na(AllBooks$nr)] <-  paste("WoR_", AllBooks$nr[AllBooks$book == "Words Of Radiance" & !is.na(AllBooks$nr)], sep = "")
AllBooks$nr[AllBooks$book == "Oathbringer" & !is.na(AllBooks$nr)] <-  paste("Oath_", AllBooks$nr[AllBooks$book == "Oathbringer" & !is.na(AllBooks$nr)], sep = "")
