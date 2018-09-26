
# I find the results a bit underwhelming

library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(ggthemr)
library(widyr)

ggthemr('fresh', layout = "minimal")

source("loadData/loadData.R")

AllBooks <- AllBooks[, c("chapter", "nr", "text", "character", "book", "flashback")]
AllBooks <- AllBooks[!is.na(AllBooks$nr),]

AllBooks$text <- tolower(AllBooks$text)
AllBooks$text <- paste(" ", AllBooks$text)
AllBooks$text <- gsub("(\\skaladins\\s|\\skal\\s|\\skals\\s)", " kaladin ", AllBooks$text)
AllBooks$text <- gsub("(\\sshallans\\s)", " shallan ", AllBooks$text)
AllBooks$text <- gsub("(\\sdalinars\\s)", " dalinar ", AllBooks$text)
AllBooks$index <- seq(1, nrow(AllBooks), 1)

rm(list=setdiff(ls(), "AllBooks"))

stopwords <- unique(stop_words$word)
stopwords <- c(stopwords, gsub("[^[:alnum:][:space:]]", "", stopwords))
stopwords <- unique(stopwords)


AllBooks_tokens <- AllBooks %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  filter(!word %in% stopwords)

#  the correlation network asks a question about which keywords occur more often together than with other keywords
keyword_cors <- AllBooks_tokens %>% 
  group_by(word) %>%
  filter(n() >= 50) %>%
  # use different items
  pairwise_cor(word, index, sort = TRUE, upper = FALSE)

rel_chars <- c("kaladin", "dalinar", "shallan", "kal", "kals", "kaladins", "dalinars", "shallans")
rel_keywodr_cors <- keyword_cors %>% 
  filter(item1 %in% rel_chars | item2 %in% rel_chars )


library(ggplot2)
library(igraph)
library(ggraph)

set.seed(10)
keyword_cors %>%
  filter(correlation > .80) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()






