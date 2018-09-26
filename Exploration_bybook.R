
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(ggthemr)
library(gridExtra)
library(stringr)
ggthemr('fresh', layout = "minimal")

source("loadData/loadData.R")

AllBooks <- AllBooks[, c("chapter", "nr", "text", "character", "book", "flashback")]
AllBooks <- AllBooks[!is.na(AllBooks$nr),]

rm(list=setdiff(ls(), "AllBooks"))

relChars <- c("Kaladin", "Shallan Davar", "Dalinar Kholin")
stopwords <- c(unique(stop_words$word), "kaladin", "dalinar", "shallan", "kal", "kals", "kaladins", "dalinars", "shallans")
stopwords <- c(stopwords, gsub("[^[:alnum:][:space:]]", "", stopwords))
stopwords <- unique(stopwords)

idf_smooth <- function(idf){
  idf <- log(1 + 2^(idf), base = 2)
  return(idf)
}


char_words_tokens <- AllBooks %>% 
  select(character, text, book) %>% 
  filter(character %in% relChars) %>% 
  unnest_tokens(word, text, drop = F) %>% 
  select(-text) %>% 
  #mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  group_by(book) %>% 
  count(character, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(!word %in% stopwords)%>% 
  filter(n > 10)

char_words_ngrams <- AllBooks %>% 
  select(character, text, book) %>%  
  filter(character %in% relChars) %>% 
  unnest_tokens(word, text, token = "skip_ngrams", n = 2, k = 3, drop = F) %>% 
  select(-text) %>% 
  filter(str_count(word, "\\w+")  == 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  #mutate(word = SnowballC::wordStem(word1, language = "english")) %>% 
  #mutate(word = SnowballC::wordStem(word2, language = "english")) %>% 
  filter(!word1 %in% stopwords) %>%
  filter(!word2 %in% stopwords) %>% 
  unite(word, word1, word2, sep = " ") %>% 
  group_by(book) %>% 
  count(character, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(n > 5)

char_words_tokens$character <- factor(char_words_tokens$character, levels = relChars)
char_words_ngrams$character <- factor(char_words_ngrams$character, levels = relChars)


plot_char_words_tokens <- char_words_tokens %>% 
  group_by(character, book) %>%
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering))

plot_char_words_ngrams <- char_words_ngrams %>% 
  group_by(character, book) %>%
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering))

c1 <- ggplot(plot_char_words_tokens, aes(reorder(ordering, n), n, fill = character)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~character+book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_char_words_tokens$ordering,
                   labels = plot_char_words_tokens$word) +
  scale_y_continuous(limits = c(0, 1250), 
                     labels = c(0, rep("",1), 300, rep("",1), 600, rep("",1), 900, rep("",1), 1200),
                     breaks= seq(0, 1200, by = 150)) + 
  labs( y = "Frequency",
        x = NULL) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)]) +
  ggtitle("Most Frequent Terms in the Stormlight Archieve Ordered by Characters")

c2 <- ggplot(plot_char_words_ngrams, aes(reorder(ordering, n), n, fill = character)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~character+book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_char_words_ngrams$ordering,
                   labels = plot_char_words_ngrams$word) +
  scale_y_continuous(limits = c(0, 135), 
                     labels = c(0, rep("",1), 30, rep("",1), 60, rep("",1), 90, rep("",1), 120),
                     breaks= seq(0, 120, by = 15)) + 
  labs( y = "Frequency",
        x = NULL) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)])

# Same with TFIDF
plot_char_words_tokens_tfidf <- char_words_tokens %>% 
  group_by(character, book) %>%
  bind_tf_idf(word, character, n) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering)) %>% 
  mutate(idf = idf_smooth(idf)) %>%  
  mutate(tf_idf = tf*idf)

plot_char_words_ngrams_tfidf <- char_words_ngrams %>% 
  group_by(character, book) %>%
  bind_tf_idf(word, character, n) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering)) %>% 
  mutate(idf = idf_smooth(idf)) %>%  
  mutate(tf_idf = tf*idf)

c1_tfidf <- ggplot(plot_char_words_tokens_tfidf, aes(reorder(ordering, tf_idf), tf_idf, fill = character)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~character+book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_char_words_tokens_tfidf$ordering,
                   labels = plot_char_words_tokens_tfidf$word) +
  scale_y_continuous(labels = function(x) x*1000) +
  labs( y = "scaled tf-idf value",
        x = NULL) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)]) +
  ggtitle("Most Important Terms in the Stormlight Archieve Ordered by Characters")

c2_tfidf <- ggplot(plot_char_words_ngrams_tfidf, aes(reorder(ordering, tf_idf), tf_idf, fill = character)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~character+book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_char_words_ngrams_tfidf$ordering,
                   labels = plot_char_words_ngrams_tfidf$word) +
  scale_y_continuous(labels = function(x) x*100) +
  labs( y = "scaled tf-idf value",
        x = NULL) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)])

c1

c2

c1_tfidf

c2_tfidf