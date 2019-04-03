library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)


depression_data <- read.csv("~/all_all.csv", sep=",", stringsAsFactors = FALSE)

low_years_full <- filter(depression_data, depression_data$Year <1800)
glimpse(which(depression_data$Year==193))
#11592
depression_data$Year[11592]<- 2008

glimpse(which(depression_data$Year==219))
#13602
depression_data$Year[13602]<-2013

glimpse(which(depression_data$Year==509))
#11084
depression_data$Year[11084]<- 1974

glimpse(which(depression_data$Year==917
))
#5599
depression_data$Year[5599]<- 2008


glimpse(which(depression_data$Year==975))
#5909
depression_data$Year[5909]<-1989


glimpse(which(depression_data$Year==990))
#5450
depression_data$Year[5450]<-2015

glimpse(which(depression_data$Year==1517))
#18409
depression_data$Year[18409]<-2006

## export cleaned data
write.csv(depression_data, "all_all_18022019.csv")



depression_select <- depression_data %>% 
  select(Title, Abstract, Year, YTID, depID)


# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}


# fix (expand) contractions
depression_select$Title <- sapply(depression_select$Title, fix.contractions)
depression_select$Abstract <- sapply(depression_select$Abstract, fix.contractions)


removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)



depression_select$Title <- sapply(depression_select$Title, removeSpecialChars)
depression_select$Abstract <- sapply(depression_select$Abstract, removeSpecialChars)

depression_select$Title <- sapply(depression_select$Title, tolower)
depression_select$Abstract <- sapply(depression_select$Abstract, tolower)


summary(depression_select)


## setting custom colours 
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}


#data cleaning 

glimpse(depression_select)

ggplot(data=depression_select)+
  geom_histogram(mapping=aes(x=Year))

hist(depression_select$Year)


## load tm packages
library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)

theme_set(theme_minimal())


# turn into tokenized - one word per line
depression_token <- depression_select %>%
  unnest_tokens(word, Abstract) 

## From https://cfss.uchicago.edu/text_classification.html#create_document-term_matrix 
## create tidy: 
depression_token_tidy <- depression_select %>%
  unnest_tokens(word, Abstract) %>%
  # remove numbers
  filter(!str_detect(word, "^[0-9]*$")) %>%
  # remove stop words
  anti_join(stop_words) 
#%>%
  # stem the words
  # mutate(word = SnowballC::wordStem(word))

depression_dtm <- depression_token_tidy %>%
  # get count of each token in each document
  count(depID, word) %>%
  # create a document-term matrix with all features and tf weighting
  cast_dtm(document = depID, term = word, value = n)


depression_dtm


depression_dtm_weight <- depression_token_tidy %>%
  # get count of each token in each document
  count(depID, word) %>%
  # create a document-term matrix with all features and tf weighting
  cast_dtm(document = depID, term = word, value = n, 
           weighting = tm::weightTfIdf)


depression_dtm_weight



# remove sparse words
sparse_depression <- removeSparseTerms(depression_dtm, sparse = .99)
sparse_depression

#tf-idf
depression_tfidf <- depression_token_tidy %>%
  count(depID, word) %>%
  bind_tf_idf(term = word, document = depID, n = n)

# sort the data frame and convert word to a factor column
plot_depression <- depression_tfidf %>%
  arrange(desc(tf_idf))%>%
  mutate(word = factor(word, levels = rev(unique(word))))

# graph the top 10 tokens for 4 categories
graph <- plot_depression %>%
  count(word, sort=TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Document") +
  ggtitle("Most Frequently Used Words in Depression Corpus") +
  coord_flip()

graph

## from https://cfss.uchicago.edu/text_classification.html#estimate_model

depression_rf <- train(x = as.matrix(depression_dtm), 
                       y = factor(depression_select$depID), 
                       method = "rf", 
                       ntree = 200, 
                       trControl - trainControl(method = "oob"))


# remove stopwords
data("stop_words")
depression_token <- depression_token %>%
  anti_join(stop_words) 

depression_token %>%
  count(word, sort = TRUE)

depression_filtered <- depression_token %>%
  distinct()


##


## positive and negative words
get_sentiments("bing")

library(tidyr)

depression_sentiment <- depression_token %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(YTID, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)



## top words across the dataset
depression_top <- depression_filtered %>%
  count(word, sort=TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Document") +
  ggtitle("Most Frequently Used Words in Depression Corpus") +
  coord_flip()

#plot of top 20 words
depression_top


## create word cloud 
depression_counts <- depression_filtered %>%
  count(word, sort=TRUE)


library(wordcloud2)
wordcloud2(depression_counts[1:300,], size = .5)


## adjectives etc

library(udpipe)
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = model$file_model)

english_dep <- udpipe_annotate(udmodel_english, depression_select$Abstract)

english_dep_x <- data.frame(english_dep)

# install.packages("lattice")
library(lattice)
stats <- txt_freq(english_dep_x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))

barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

#nouns
stats_nouns <- subset(english_dep_x, upos %in% c("NOUN"))
stats_nouns <- txt_freq(stats_nouns$token)
stats_nouns$key <- factor(stats_nouns$key, levels = rev(stats_nouns$key))

barchart(key ~ freq, data = head(stats_nouns, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


#adjectives
stats_adj <- subset(english_dep_x, upos %in% c("ADJ")) 
stats_adj <- txt_freq(stats_adj$token)
stats_adj$key <- factor(stats_adj$key, levels = rev(stats_adj$key))
barchart(key ~ freq, data = head(stats_adj, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")


#verbs

stats_verb <- subset(english_dep_x, upos %in% c("VERB")) 
stats_verb <- txt_freq(stats_verb$token)
stats_verb$key <- factor(stats_verb$key, levels = rev(stats_verb$key))
barchart(key ~ freq, data = head(stats_verb, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")



## using RAKE
stats_rake <- keywords_rake(x = english_dep_x, term = "lemma", group = "doc_id", 
                       relevant = english_dep_x$upos %in% c("NOUN", "ADJ"))
stats_rake$key <- factor(stats_rake$keyword, levels = rev(stats_rake$keyword))
barchart(key ~ rake, data = head(subset(stats_rake, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


# RAKE noun-verb pairs
english_dep_x$phrase_tag <- as_phrasemachine(english_dep_x$upos, type = "upos")
stats_nounverb <- keywords_phrases(x = english_dep_x$phrase_tag, term = tolower(english_dep_x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats_nounverb <- subset(stats_nounverb, ngram > 1 & freq > 3)
stats_nounverb$key <- factor(stats_nounverb$keyword, levels = rev(stats_nounverb$keyword))
barchart(key ~ freq, data = head(stats_nounverb, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")





### Topic Modelling: 
## https://www.tidytextmining.com/topicmodeling.html#latent-dirichlet-allocation 
# install.packages("topicmodels") 
library(topicmodels) 

## data must be in Document Term Matric format with term freq weighting
## use output from depression_dtm above

dep_lda <- LDA(sparse_depression, k = 3, control = list(seed = 1234))
dep_lda



## word-topic probabilities
dep_topics <- tidy(dep_lda, matrix = "beta")
dep_topics

dep_top_terms <- dep_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# most common terms within each topic
dep_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



## greatest difference in beta
library(tidyr)

beta_spread <- dep_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread


## document - topic probabilities
dep_docs <- tidy(dep_lda, matrix = "gamma")

dep_docs %>% 
    arrange(topic, gamma)

ordered_dep_docs <- dep_docs[order(gamma),]

# how many docus from mix of 3 topics: 
tidy(sparse_depression) %>%
  filter(document == 6) %>%
  arrange(desc(count))
