###############################################################
### Sentiment analysis for Alexa ######
###############################################################

library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)

colnames(amazon_alexa)[4] <- "text"

#creatng the new variable for the product because we only have the color variation

amazon_alexa$product <- c()
for (i in 1:nrow(amazon_alexa)) {
  if (amazon_alexa$variation[i] == "Black" |  amazon_alexa$variation[i] == "White") {  
    amazon_alexa$product[i] <- "Echo Dot"
  } else {
    amazon_alexa$product[i] <- "Echo"
  } #closing if statement
}

alexa_token <- amazon_alexa%>%
  unnest_tokens(word, text)


echo <- alexa_token %>%
  filter(product == "Echo")

echo_dot <- alexa_token %>%
  filter(product == "Echo Dot")

afinn <- alexa_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  alexa_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  alexa_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################
library(ggplot2)

echo <- alexa_token %>%
  filter(product == "Echo")

echo_dot <- alexa_token %>%
  filter(product == "Echo Dot")
#Most common positive and negative words for Echo
bing_counts_echo <- echo %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_echo %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#Most common positive and negative words for Echo Dot
bing_counts_echo_dot <- echo_dot %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_echo_dot %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



#############################################
###### N-grams and tokenizing ###############
#############################################

library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)



alexa_bigrams <- amazon_alexa %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

library(tidyr)
bigrams_separated <- alexa_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)


######################################################
####### VISUALISING THE BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

###########################################################
###### #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram <- amazon_alexa %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>% 
  na.omit(quadrogram)

###########################################################
###### the tf_idf framework  ############
######on our quadro-gram #################
###########################################################



#####  quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(product, quadrogram) %>%
  bind_tf_idf(quadrogram, variation, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf