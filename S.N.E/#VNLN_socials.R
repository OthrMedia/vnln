# data wrangling
library(dplyr)
library(here)
library(reshape2)
library(magrittr)
library(tidyverse)

# data visualization
library(ggplot2)
library(plotly)
library(igraph)
library(ggraph)
library(ggpubr)
library(cowplot)
library(wordcloud)
library(networkD3)
library(DT)

# text cleaning
library(tm)
library(widyr)
library(tidyr)
library(textdata)
library(tidytext)
library(stringr)
library(textstem)
library(sentimentr)

sne_tiktok <- read_csv("~/Othr.creatives/SNE/sne_tiktok.csv")
sne_insta <- read_csv("~/Othr.creatives/SNE/sne_insta.csv")
comments <- read_csv("~/Othr.creatives/SNE/insta_tiktok_comments.csv")
comments <- comments %>%
  select('user', 'comment')
emoji_counts <- read_csv("~/Othr.creatives/SNE/emoji_counts.csv")


theme <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background=element_rect(fill="white"),
               axis.line=element_line("black"),
               panel.border=element_rect(fill=NA,colour="black"),
               strip.background=element_rect(fill="grey 80",colour="black"),
               strip.text.x = element_text(size = 7))

insta_tiktok <- sne_tiktok %>%
  select(X1, description, createTime, playCount, commentCount, diggCount, platform) %>%
  rbind(sne_insta)

# change the description of each video
insta_tiktok$description[1] <- 'It is with great honor to announce the #VNLN Album!'
View(insta_tiktok)
insta_tiktok$description[4] <- 'It is with great honor to announce the #VNLN Album!'
insta_tiktok$description[2] <- 'Voice note lead note with Dr Khanyile'
insta_tiktok$description[5] <- 'Voice note lead note with Dr Khanyile'
insta_tiktok$description[3] <- 'Who have you partied with like this?'
insta_tiktok$description[6] <- 'Who have you partied with like this?'

durations <- insta_tiktok %>%
  # wrap text in the description column
  mutate(new_desc = str_wrap(insta_tiktok$description, width = 10))%>%
  ggplot(aes(x = new_desc, y = duration)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = 'Duration(s)') +
  theme

views <- insta_tiktok %>%
  # wrap text in the description column
  mutate(new_desc = str_wrap(insta_tiktok$description, width = 10))%>%
  ggplot(aes(x = new_desc, y = playCount, colour = platform, fill = platform)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  #facet_wrap(~platform) +
  labs(x = NULL, y = 'Views', title = 'How many views did the posts get?') +
  theme +
  scale_fill_manual(values = c('#171820', '#fdc029')) +
  scale_color_manual(values = c('#171820', '#fdc029')) +
  theme(legend.position='none')

likes <- insta_tiktok %>%
  # wrap text in the description column
  mutate(new_desc = str_wrap(insta_tiktok$description, width = 10))%>%
  ggplot(aes(x = new_desc, y = diggCount, colour = platform, fill = platform)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  #facet_wrap(~platform) +
  labs(x = NULL, y = 'Likes', title = 'How many likes did the posts get?') +
  theme +
  scale_fill_manual(values = c('#171820', '#fdc029')) +
  scale_color_manual(values = c('#171820', '#fdc029')) +
  theme(legend.title = element_blank(), legend.position = 'right')

# find the total views and likes
sum_likes <- insta_tiktok %>%
  group_by(description) %>%
  summarise(total_views = sum(playCount),
            total_likes = sum(diggCount))


# EMOJIS
top_emojis <- top_n(emoji_counts, 10)
counts <- top_emojis %>%
  ggplot(aes(y = reorder(emoji, count), x = count)) +
  geom_col(fill = '#171820') +
  labs(y = NULL, x = 'Count', title = 'Emojis') +
  theme(axis.text.y = element_text(size = 20)) +
  theme

ggplotly(counts, tooltip = 'count')

## COMMENTS
# clean the comments and remove emojis
responses <- comments %>%
  mutate(text = str_to_lower(comment)) %>%
  mutate(text = removeNumbers(text)) %>%
  mutate(text = removePunctuation(text)) %>%
  mutate(text = gsub("[^\x01-\x7F]", "", text)) #remove emojis

# convert everything to lowercase & tokenise
comments_clean <- responses %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  count(word)

data('stop_words')

comments_clean_words <- comments_clean %>%
  anti_join(stop_words)

comments_clean_words$lemma <- lemmatize_words(comments_clean_words$word)

# plot a wordcloud
word_cloud = wordcloud(words = comments_clean_words$word, freq = comments_clean_words$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#comment_cloud <- readPNG(wordcloud.png, native = TRUE, info = TRUE)


# look at the network
text_bigrams <- function(df) {
  bigrams <- df %>%
    unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
    count(bigram, sort = TRUE) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    mutate(word1 = lemmatize_words(word1)) %>%
    mutate(word2 = lemmatize_words(word2)) %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ")
  return(bigrams)
} 
# get bigrams
comments_bigrams <- text_bigrams(responses) %>%
  filter(bigram != 'NA NA')

comments_bigram_count <- comments_bigrams %>%
  filter(bigram != 'NA NA') %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n)

ScaleWeight <- function(x, lambda) {
  x / lambda
}

comments_network <-  comments_bigram_count %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = TRUE)

# Store the degree.
V(comments_network)$degree <- strength(graph = comments_network)
# Compute the weight shares.
E(comments_network)$width <- E(comments_network)$weight/max(E(comments_network)$weight)

# Create networkD3 object.
comments_network.D3 <- igraph_to_networkD3(g = comments_network)
# Define node size.
comments_network.D3$nodes %<>% mutate(Degree = (1E-2)*V(comments_network)$degree)
# Define color group (I will explore this feature later).
comments_network.D3$nodes %<>% mutate(Group = 1)

# Define edges width. 
comments_network.D3$links$Width <- 10*E(comments_network)$width

# plot network
plot_network <- forceNetwork(
  Links = comments_network.D3$links, 
  Nodes = comments_network.D3$nodes, 
  Source = 'source',
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = T, 
  linkDistance = 40, 
  arrows = TRUE,
  legend = FALSE,
  opacityNoHover = 1
)

plot_network
