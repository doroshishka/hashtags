# hashtags
Plotting hashtags with Tidy Tweet

install.packages("data.table")
library(data.table)
getwd()
setwd("directory")
data <- fread("processed_2700_2016_sockpuppet.csv")


# :::::::#blacklivesmatter:::::::

# subset a dataset that appears above hashtags in tweet
data2 <- data[grepl("#blacklivesmatter\\b|#BlackLivesMatter\\b|#Blacklivesmatter\\b", data$tweet), ] 


# plot
data2$`created at` <- format(as.POSIXct(data2$`created at`, format='%m/%d/%Y %H:%M'), format='%m/%d/%Y')

#1
plot(table(data2$`created at`))

#2
library(ggplot2)
table <- table(data2$`created at`)
table <- as.data.frame(table)
table$Var1 <- as.Date(table$Var1, "%m/%d/%Y")
str(table$Var1) # check if Var1 is date
ggplot(table, aes(x=Var1,y=Freq)) +
        geom_line() +
        xlab("Date") +
        labs(title="#blacklivesmatter")

# associated words

which( colnames(data2)=="tweet" ) #14
tweet <- data2[,14]

library(dplyr)
library(stringr)
library(tidytext)
library(tibble)
library(wordcloud)
library(tidyr)
library(igraph)
library(ggraph)
library(reshape2)

# calling a function line

generate_df (id, df_RT, df_n_r)

tidy_tweet <- data_frame(text = as.character(tweet[[1]])) %>%
        unnest_tokens(word, text)
tidy_tweet <- anti_join(tidy_tweet, stop_words)

tidy_tweet %>%
        count(word, sort = TRUE)

tidy_tweet %>% #graphical representation of word frequencies
        count(word, sort = TRUE) %>%
        filter(n > 100) %>%    #words used 100 times or more
        mutate(word = reorder(word, n)) %>% #orders the words by n, not alphabetically
        ggplot(aes(word, n)) +
        geom_col() + 
        coord_flip()

tidy_tweet %>% #graphical representation of word frequencies
        count(word, sort = TRUE) %>%
        filter(n > 70 & n < 2000) %>%    #words used more than 70 but less than 2000
        mutate(word = reorder(word, n)) %>% #orders the words by n, not alphabetically
        ggplot(aes(word, n)) +
        geom_col() + 
        coord_flip()

tidy_tweet %>% #wordcloud
        anti_join(stop_words) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words =80))


tweet[1]

bing <-get_sentiments("bing")
bing_tweet <- tidy_tweet %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
bing_tweet
bing_tweet %>%
        filter(n > 15) %>%
        mutate(n = ifelse(sentiment =="negative", -n, n)) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(data2$is.puppet)
