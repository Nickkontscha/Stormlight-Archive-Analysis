
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
idf_prob <- function(idf){
  idf <- log(2^(idf) - 1, base = 2)
  return(idf)
}
tf_aug <- function(n){
  tf <- 0.5+0.5*n/max(n)
  return(tf)
}
tf_bool <- function(n){
  tf <- ifelse(n > 0, 1, 0)
  return(tf)
}
##

chars <- AllBooks %>% 
  select(character, flashback, book) %>% 
  group_by(character, book) %>% 
  count(flashback) %>% 
  ungroup() %>% 
  arrange(flashback, desc(n)) %>% 
  filter(character %in% relChars)

chars$character <- factor(chars$character, levels = relChars)

ggplot(chars, aes( x = character, y = n, fill = flashback)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~book) +
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(7,2)]) +
  ggtitle("Number of Chapters of Each Character Ordered by Books") +
  labs(x = NULL,
       y = "Number of chapters",
       fill = "Flashback")
# Man sieht gut, über wen es in diesem Buch geht

book_words_tokens <- AllBooks %>% 
  select(book, text) %>% 
  unnest_tokens(word, text) %>% 
  #mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  count(book, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(!word %in% stopwords) %>% 
  filter(n > 10)

book_words_ngrams <- AllBooks %>% 
  select(book, text) %>% 
  unnest_tokens(word, text, token = "skip_ngrams", n = 2, k = 2) %>% 
  filter(str_count(word, "\\w+")  == 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  #mutate(word = SnowballC::wordStem(word1, language = "english")) %>% 
  #mutate(word = SnowballC::wordStem(word2, language = "english")) %>% 
  filter(!word1 %in% stopwords) %>%
  filter(!word2 %in% stopwords) %>% 
  unite(word, word1, word2, sep = " ") %>% 
  count(book, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(n > 10)


plot_book_words_tokens <- book_words_tokens %>% 
  group_by(book) %>%
  top_n(25, n) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", book, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering))

plot_book_words_ngrams <- book_words_ngrams %>% 
  group_by(book) %>%
  top_n(25, n) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", book, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering))

b1 <- ggplot(plot_book_words_tokens, aes(reorder(ordering, n), n, fill = book)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_book_words_tokens$ordering,
                   labels = plot_book_words_tokens$word) +
  scale_y_continuous(limits = c(0, 2500), 
                     labels = c(0, rep("",1), 500, rep("",1), 1000, rep("",1), 1500, rep("",1), 2000, rep("",1), 2500),
                     breaks= seq(0, 2500, by = 250)) + 
  labs( y = "Frequency",
        x = NULL) +
  coord_flip() +
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)]) +
  ggtitle("Most Frequent Terms in the Stormlight Archieve Ordered by Book")

b2 <- ggplot(plot_book_words_ngrams, aes(reorder(ordering, n), n, fill = book)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_book_words_ngrams$ordering,
                   labels = plot_book_words_ngrams$word) +
  scale_y_continuous(limits = c(0, 260), 
                     labels = c(0, rep("",1), 50, rep("",1), 100, rep("",1), 150, rep("",1), 200, rep("",1), 250),
                     breaks= seq(0, 250, by = 25)) + 
  labs( y = "Frequency",
        x = NULL) +
  coord_flip() +
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)])
# Ich habe die Namen nicht entfernt, weil man hier wieder sieht, um wem es in dem Buch mehr geht
# Buch 1 wenig Shallan und Oathbringer viel Dalinar
# Man sieht die Themen:
# WoK: Bridge, Sadeas, Soldiers, Army
# Oathbringer: Fused, eyes, red, Odium

# Same with TFIDF
plot_book_words_tokens_tfidf <- book_words_tokens %>% 
  group_by(book) %>%
  bind_tf_idf(word, book, n) %>% 
  top_n(20, tf_idf) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", book, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering)) %>% 
  mutate(idf = idf_smooth(idf)) %>% 
  mutate(tf_idf = tf*idf)

plot_book_words_ngrams_tfidf <- book_words_ngrams %>% 
  group_by(book) %>%
  bind_tf_idf(word, book, n) %>% 
  top_n(20, tf_idf) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", book, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering)) %>% 
  mutate(idf = idf_smooth(idf)) %>%  
  mutate(tf_idf = tf*idf)

b1_tfidf <- ggplot(plot_book_words_tokens_tfidf, aes(reorder(ordering, tf_idf), tf_idf, fill = book)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_book_words_tokens_tfidf$ordering,
                   labels = plot_book_words_tokens_tfidf$word) +
  scale_y_continuous(labels = function(x) x*1000) +
  labs( y = "scaled tf-idf value",
        x = NULL) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)]) +
  ggtitle("Most Important Terms in the Stormlight Archieve Ordered by Books") 

b2_tfidf <- ggplot(plot_book_words_ngrams_tfidf, aes(reorder(ordering, tf_idf), tf_idf, fill = book)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~book, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_book_words_ngrams_tfidf$ordering,
                   labels = plot_book_words_ngrams_tfidf$word) +
  scale_y_continuous(labels = function(x) x*100) +
  labs( y = "scaled tf-idf value",
        x = NULL) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)]) 
# WoK: Apothecary as WoK is about Kaladin and his Past
# Many Brightlords as they are at their camps

#
## Characters
#

char_words_tokens <- AllBooks %>% 
  select(character, text) %>% 
  filter(character %in% relChars) %>% 
  unnest_tokens(word, text) %>% 
  #mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  count(character, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(!word %in% stopwords)%>% 
  filter(n > 10)

char_words_ngrams <- AllBooks %>% 
  select(character, text) %>%  
  filter(character %in% relChars) %>% 
  unnest_tokens(word, text, token = "skip_ngrams", n = 2, k = 2) %>% 
  filter(str_count(word, "\\w+")  == 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  #mutate(word = SnowballC::wordStem(word1, language = "english")) %>% 
  #mutate(word = SnowballC::wordStem(word2, language = "english")) %>% 
  filter(!word1 %in% stopwords) %>%
  filter(!word2 %in% stopwords) %>% 
  unite(word, word1, word2, sep = " ") %>% 
  count(character, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(n > 10)

char_words_tokens$character <- factor(char_words_tokens$character, levels = relChars)
char_words_ngrams$character <- factor(char_words_ngrams$character, levels = relChars)


plot_char_words_tokens <- char_words_tokens %>% 
  group_by(character) %>%
  top_n(25, n) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering))

plot_char_words_ngrams <- char_words_ngrams %>% 
  group_by(character) %>%
  top_n(25, n) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering))

c1 <- ggplot(plot_char_words_tokens, aes(reorder(ordering, n), n, fill = character)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~character, scales = "free", drop = TRUE) +
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
  facet_wrap(~character, scales = "free", drop = TRUE) +
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
  group_by(character) %>%
  bind_tf_idf(word, character, n) %>% 
  top_n(20, tf_idf) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering)) %>% 
  mutate(idf = idf_smooth(idf)) %>%  
  mutate(tf_idf = tf*idf)

plot_char_words_ngrams_tfidf <- char_words_ngrams %>% 
  group_by(character) %>%
  bind_tf_idf(word, character, n) %>% 
  top_n(20, tf_idf) %>% 
  mutate(index = seq(1, length(n), 1)) %>% 
  unite("ordering", character, index, sep = "_", remove = FALSE) %>% 
  data.frame() %>% 
  mutate(ordering = factor(ordering, levels = ordering)) %>% 
  mutate(idf = idf_smooth(idf)) %>%  
  mutate(tf_idf = tf*idf)

c1_tfidf <- ggplot(plot_char_words_tokens_tfidf, aes(reorder(ordering, tf_idf), tf_idf, fill = character)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  facet_wrap(~character, scales = "free", drop = TRUE) +
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
  facet_wrap(~character, scales = "free", drop = TRUE) +
  scale_x_discrete(breaks = plot_char_words_ngrams_tfidf$ordering,
                   labels = plot_char_words_ngrams_tfidf$word) +
  scale_y_continuous(labels = function(x) x*100) +
  labs( y = "scaled tf-idf value",
        x = NULL) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "#ecf0f1")) + 
  scale_fill_manual(values = swatch()[c(3, 7, 2)])


gA <- ggplotGrob(b1)
gB <- ggplotGrob(b2)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
# By looking at the Word frequency, one can already guess what the book is about. 
# Looking at the terms with two words, it becomes even more clearer.
# One can definetly see that WoK and WoR are taking place in the shatted plains, while Oathbringer is being less centered.
# Other things are evolving, too. WoK is mainly about the bridge crews with Kaladin being the "main" character.
# And an important take away is that people surely nod and shake their heads a lot while taking deep breaths with wide eyes.

gA <- ggplotGrob(b1_tfidf)
gB <- ggplotGrob(b2_tfidf)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
# One can see the influence of TFIDF: Common terms like "shook head" are removed.
# Another symptom of the influence is that it isolates topics which emerge in one book only
# and about new characters appearing in the different books. Noteable examples are "Kabsal and Jam", a lot of bridges and
# some minor lighteyes/highprinces which take a role in the politics of the shattered plains.
# For WoR, one can observe new characters appearing like Eshonai, Zahel and Sebarial.
# I find Oathbringer to be the most interesting. One can see how the setting changes a lot. 
# Words about the shattered plains like bridge crews/runs or plateaus disappear completely. 
# The threat of the book is very dominant with the Fused, Odium and the color red which gets associated with them.

gA <- ggplotGrob(c1)
gB <- ggplotGrob(c2)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
# By looking at the frequencies, one can see which character is associateing with which group of people and what their main activities are.

gA <- ggplotGrob(c1_tfidf)
gB <- ggplotGrob(c2_tfidf)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
# Characters like Adolin, Jasnah, Navani were very prominent when sorting by Frequency
# When looking at the tfidf values, one can see more closely related people to the given characters in particular about characters from their past
# Family members and their spren are very prominant
# mmmm pattern is my favourite!


# Schwierigkeiten:
# Es war schwierig die y-Achsen aller Graphen ordentlich übereinander anzuordnen.
