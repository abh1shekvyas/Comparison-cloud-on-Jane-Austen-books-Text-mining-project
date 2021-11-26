###Installing required Libraries

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext) 
library(reshape2)
library(ggplot2)
library(wordcloud)
 
###Grouping the sentences by books and filtering the Pride and Prejudice(pnp) book using 'Filter() function'
###Using Mutate function we have added 2 colums for line number and chapter number
pnp <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>% ## line and chapter number added just to understand data better
  ungroup()%>%
  filter(book=="Pride & Prejudice" )

#View(pnp)

###Using unnest token to make them 1 word/line.The book has 122,204 words
tidypnp <- pnp %>%
  unnest_tokens(word, text)

###after removing stopwords the book Pride and Prejudice now contains 37246 words
tidypnp <- tidypnp %>%
  anti_join(stop_words)
#View(tidypnp)


###after using inner_join we are left with 1404 'unique words'
bing_word_counts <- tidypnp %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)  ##count function will give frequency of words
 
#View(bing_word_counts)

###By plotting the bar chart we saw that the frequency of word 'miss' is unusually high and can have multiple meaning so we can remove the word miss
bing_word_counts %>%
  group_by(sentiment) %>%  
  top_n(25) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% ## Mutate and reorder is used so that graph bars are sorted
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + # using facet wrap we get free y axis for different sentiments
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
###Plotting bar chart for "bottom n" words using top_n() function
bing_word_counts %>%
  group_by(sentiment) %>%  
  top_n(-25,word) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
class(bing_word_counts)


##removing word miss from the data bing_word_counts

finaldata <- bing_word_counts[-c(1),] # we have removed 1st row which contains 'miss'

#Comparison cloud 
finaldata%>%
   acast(word ~ sentiment, value.var = "n", fill = -0.5) %>%
   comparison.cloud(title.colors=c("dark red","green"),colors = c(" black", "blue"),
                    max.words = 1500)
 