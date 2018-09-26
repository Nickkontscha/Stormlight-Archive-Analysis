# https://github.com/mkearney/resist_oped/blob/master/R/01-features.R

#library(devtools)
#assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
#devtools::install_github("mkearney/textfeatures")

library(textfeatures)
library(tidyverse)
library(stringr)

library(corrplot)
library(maptree) # dendogram
library(dendextend)

source("loadData/loadData.R")

AllBooks <- AllBooks[, c("chapter", "nr", "text", "character", "book", "flashback")]
AllBooks <- AllBooks[!is.na(AllBooks$nr),]

rm(list=setdiff(ls(), "AllBooks"))

stopwords <- unique(stop_words$word)
stopwords <- c(stopwords, gsub("[^[:alnum:][:space:]]", "", stopwords))
stopwords <- unique(stopwords)

stopwords_regex <- paste(stopwords, collapse = "\\b|\\b")
stopwords_regex <- paste0("\\b", stopwords_regex, "\\b")
AllBooks$text <- str_replace_all(AllBooks$text, stopwords_regex, "")


# Create data set with just an id and text as required by textfeatures
data <- data_frame(
  id = AllBooks$character,
  text = AllBooks$text
)

# Feature extraction
# Computation intensive: I set it to numberOfLogicalCores - 1
tf <- textfeatures::textfeatures(data, word_dims = 300, threads = 3)

# Summarise by id
tfsum <- tf %>%
  group_by(id) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  ungroup()

# Vector of unique characters
characters <- unique(tfsum$id)

# Create numeric vectors of equal length for each id
cols <- map(characters,
            ~ filter(tfsum, id == .x) %>% select(-id) %>% as.list() %>% unlist()
)

# Create matrix
mat <- cols %>%
  unlist() %>%
  as.numeric() %>%
  matrix(nrow = length(characters), byrow = TRUE)

# Set row and column names
row.names(mat) <- characters

# Plot correlations
cor <- cor(t(mat))
#cor <- abs(cor)
corrplot(cor, order = "hclust", addrect = 6)

dissimilarity <- 1 - cor
#dissimilarity <- 1 - abs(cor)
distance <- as.dist(dissimilarity)
#plot(hclust(distance), main="Dissimilarity = 1 - |Correlation|", xlab="")

# Find best clusters for dendogramm
dend <- hclust(distance)
op_k <- kgs(dend, distance, maxclus = 20)
# plot (names (op_k), op_k, xlab="# clusters", ylab="penalty")
op_k[which(op_k == min(op_k))]

clusterNr <- as.integer(names(op_k[which(op_k == min(op_k))]))
dend <- distance %>% hclust() %>% as.dendrogram
par(mar = c(0,0,0,10))
dend %>% set("branches_k_color", k = clusterNr) %>% set("labels_col", k= clusterNr) %>% plot(horiz = TRUE) 
dend %>% rect.dendrogram(k = clusterNr, horiz = TRUE, border = 8, lty = 5, lwd = 1)

