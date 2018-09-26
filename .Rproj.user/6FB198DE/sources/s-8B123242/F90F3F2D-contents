
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(ggthemr)
library(gridExtra)
library(topicmodels)

source("loadData/loadData.R")

AllBooks <- AllBooks[, c("chapter", "nr", "text", "character", "book", "flashback")]
AllBooks <- AllBooks[!is.na(AllBooks$nr),]
AllBooks <- AllBooks[!duplicated(AllBooks$chapter), ]
AllBooks$index <- seq(1, nrow(AllBooks), 1)

rm(list=setdiff(ls(), "AllBooks"))

ggthemr('fresh', layout = "minimal")

stopwords <- unique(stop_words$word)
stopwords <- c(stopwords, gsub("[^[:alnum:][:space:]]", "", stopwords))
stopwords <- unique(stopwords)

AllBooks <- AllBooks[AllBooks$book == "Oathbringer",]
AllBooks$index <- seq(1, nrow(AllBooks), 1)

AllBooks_tokens <- AllBooks %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stopwords) %>% 
  mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  filter(!word %in% c("hand"))

word_counts <- AllBooks_tokens %>%
  count(index, word, sort = TRUE) %>%
  ungroup() %>% 
  filter(n > 3)

dtm <- word_counts %>%
  cast_dtm(index, word, n)

sel_idx <- slam::row_sums(dtm) > 0
sel_idx <- sort(as.integer(rownames(as.data.frame(sel_idx))))
dtm <- dtm[sel_idx, ]
AllBooks <- AllBooks[sel_idx,]

K <- 6 # number of topics
lda <- LDA(dtm, k = K, control = list(seed = 10))

tidy_lda <- tidy(lda)
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free") +
  theme_minimal()


# Visualisation
library("reshape2")
require(pals)

lda_result <- posterior(lda)
theta <- lda_result$topics
beta <- lda_result$terms
topicNames <- apply(lda::top.topic.words(beta, 7, by.score = T), 2, paste, collapse = " ")

topic_proportion <- aggregate(theta, by = list(chapter = AllBooks$index), mean)
# set topic names to aggregated columns
colnames(topic_proportion)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion, id.vars = "chapter")

# plot topic proportions per deacde as bar plot
ggplot(vizDataFrame, aes(x=chapter, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "chapter") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


