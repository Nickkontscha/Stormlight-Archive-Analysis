# https://www.tidytextmining.com/twitter.html

library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(ggthemr)
library(gridExtra)

source("loadData/loadData.R")

AllBooks <- AllBooks[, c("chapter", "nr", "text", "character", "book", "flashback")]
AllBooks <- AllBooks[!is.na(AllBooks$nr),]

rm(list=setdiff(ls(), "AllBooks"))

ggthemr('fresh', layout = "minimal")

relChars <- c("kaladin", "shallan", "dalinar")

stopwords <- unique(stop_words$word)
stopwords <- c(stopwords, gsub("[^[:alnum:][:space:]]", "", stopwords))
stopwords <- unique(stopwords)

AllBooks$text <- tolower(AllBooks$text)
AllBooks$text <- paste(" ", AllBooks$text)
AllBooks$text <- gsub("(\\skaladins\\s|\\skal\\s|\\skals\\s)", " kaladin ", AllBooks$text)
AllBooks$text <- gsub("(\\sshallans\\s)", " shallan ", AllBooks$text)
AllBooks$text <- gsub("(\\sdalinars\\s)", " dalinar ", AllBooks$text)


WoK <- AllBooks[AllBooks$book == "Way Of Kings", c("nr", "text")]
WoR <- AllBooks[AllBooks$book == "Words Of Radiance", c("nr", "text")]
Oath <- AllBooks[AllBooks$book == "Oathbringer", c("nr", "text")]

WoK$index <- seq(1, nrow(WoK), 1)
WoR$index <- seq(1, nrow(WoR), 1)
Oath$index <- seq(1, nrow(Oath), 1)

WoK_tokens <- WoK %>% 
  select(text, index) %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  count(index, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(word %in% relChars) %>% 
  arrange(index)

WoR_tokens <- WoR %>% 
  select(text, index) %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  count(index, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(word %in% relChars) %>% 
  arrange(index)

Oath_tokens <- Oath %>% 
  select(text, index) %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = SnowballC::wordStem(word, language = "english")) %>% 
  count(index, word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(word %in% relChars) %>% 
  arrange(index)

WoK_tokens$word <- gsub("kaladin", "Kaladin", WoK_tokens$word)
WoK_tokens$word <- gsub("shallan", "Shallan", WoK_tokens$word)
WoK_tokens$word <- gsub("dalinar", "Dalinar", WoK_tokens$word)

WoR_tokens$word <- gsub("kaladin", "Kaladin", WoR_tokens$word)
WoR_tokens$word <- gsub("shallan", "Shallan", WoR_tokens$word)
WoR_tokens$word <- gsub("dalinar", "Dalinar", WoR_tokens$word)

Oath_tokens$word <- gsub("kaladin", "Kaladin", Oath_tokens$word)
Oath_tokens$word <- gsub("shallan", "Shallan", Oath_tokens$word)
Oath_tokens$word <- gsub("dalinar", "Dalinar", Oath_tokens$word)

WoK_tokens$word <- factor(WoK_tokens$word, levels = c("Kaladin", "Shallan", "Dalinar"))
WoR_tokens$word <- factor(WoR_tokens$word, levels = c("Kaladin", "Shallan", "Dalinar"))
Oath_tokens$word <- factor(Oath_tokens$word, levels = c("Kaladin", "Shallan", "Dalinar"))


adjust <- 0.3
height <- 0.05
d1 <- ggplot(WoK_tokens, aes(x = n, fill = word)) +
  geom_density(alpha = 0.33,
               adjust = adjust) +
  scale_y_continuous(limit = c(0, height)) + 
  theme(legend.position="none") +
  labs( x = " ",
        y = "Density")

d2 <- ggplot(WoR_tokens, aes(x = n, fill = word)) +
  geom_density(alpha = 0.33,
               adjust = adjust) +
  scale_y_continuous(limit = c(0, height)) + 
  theme(legend.position="none") +
  labs( x = "Chapter Number",
        y = NULL)

d3 <- ggplot(Oath_tokens, aes(x = n, fill = word)) +
  geom_density(alpha = 0.33,
               adjust = adjust) +
  scale_y_continuous(limit = c(0, height)) +
  labs( x = " ",
        y = NULL)

grid.arrange(d1, d2, d3, ncol = 3)

######


WoK_tokens <- spread(WoK_tokens, key = word, value = n) %>% 
  replace_na(list(Kaladin = 0, Shallan = 0, Dalinar = 0)) %>% 
  gather(word, n, -index)

WoR_tokens <- spread(WoR_tokens, key = word, value = n) %>% 
  replace_na(list(Kaladin = 0, Shallan = 0, Dalinar = 0)) %>% 
  gather(word, n, -index)

Oath_tokens <- spread(Oath_tokens, key = word, value = n) %>% 
  replace_na(list(Kaladin = 0, Shallan = 0, Dalinar = 0)) %>% 
  gather(word, n, -index)

WoK_fun = function(vec){ as.numeric(vec[3]) / sum(WoK_tokens$n[ WoK_tokens$index == as.numeric(vec[1]) ]) *100 }
WoR_fun = function(vec){ as.numeric(vec[3]) / sum(WoR_tokens$n[ WoR_tokens$index == as.numeric(vec[1]) ]) *100 }
Oath_fun = function(vec){ as.numeric(vec[3]) / sum(Oath_tokens$n[ Oath_tokens$index == as.numeric(vec[1]) ]) *100 }

WoK_tokens$prop = apply(WoK_tokens , 1 , WoK_fun)
WoR_tokens$prop = apply(WoR_tokens , 1 , WoR_fun)
Oath_tokens$prop = apply(Oath_tokens , 1 , Oath_fun)

WoK_tokens$word <- factor(WoK_tokens$word, levels = c("Kaladin", "Shallan", "Dalinar"))
WoR_tokens$word <- factor(WoR_tokens$word, levels = c("Kaladin", "Shallan", "Dalinar"))
Oath_tokens$word <- factor(Oath_tokens$word, levels = c("Kaladin", "Shallan", "Dalinar"))

dd1 <- ggplot(WoK_tokens, aes(x = index, y = prop, fill = word)) + 
  geom_area(alpha=0.6 , size=.5, colour="black")+
  labs( x = " ",
        y = "Percentage") + 
  theme(legend.position = "top",
      legend.margin=margin(t = 0, unit='cm'),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.key = element_rect(fill = "white"))  + 
  scale_fill_manual(values = swatch()[c(2, 8, 9)],
                    guide = guide_legend(override.aes = list(fill = "white",
                                         color = "white"))
                    )

dd2 <- ggplot(WoR_tokens, aes(x = index, y = prop, fill = word)) + 
  geom_area(alpha=0.6 , size=.5, colour="black") +
  labs( x = "Chapter",
        y = "",
        fill = "Character: ") + 
  theme(legend.position = "top",
        legend.margin=margin(t = 0, unit='cm')) +
  scale_fill_manual(values = swatch()[c(2, 8, 9)]
                    )

dd3 <- ggplot(Oath_tokens, aes(x = index, y = prop, fill = word)) + 
  geom_area(alpha=0.6 , size=.5, colour="black") +
  labs( x = " ",
        y = " ")  +
  theme(legend.position = "top",
        legend.margin=margin(t = 0, unit='cm'),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "white")) + 
  scale_fill_manual(values = swatch()[c(2, 8, 9)],
                    guide = guide_legend(override.aes = list(fill = "white",
                                                             color = "white"))
                    )

library(grid)
grid.arrange(dd1, dd2, dd3, ncol = 3,
             top = textGrob("Percentage of how often a character is mentioned in each chapter",gp=gpar(fontsize=20,font=3)))


