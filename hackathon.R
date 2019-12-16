
 #setwd("C:/Users/gramener/Desktop/Hackathon")

require(tidyverse)
require(janitor)
require(reshape2)
require(tidytext)
require(tokenizers)


data <- read.csv("abcnews-date-text.csv", stringsAsFactors = F) %>% clean_names()

#extractions on date:
data$year <- substring(data$publish_date,1,4)
data$month <- substring(data$publish_date,5,6)
data$day <- substring(data$publish_date, 7,8)

sum(is.na(data))
#0

data$headline_len <- str_length(data$headline_text)
max(data$headline_len)
min(data$headline_len)

##### Text Analysis on Word Frequency ########################################################################################
text_df <- tibble(data$headline_text)

names(text_df)[names(text_df) == "data$headline_text"] <- "text"

 word <- text_df %>%
  unnest_tokens(word, text)


word_count <- word %>% group_by(word) %>% summarise(count = n())

#counting most frequent words:
word_count <- word_count %>% anti_join(stop_words)
word_count <- word_count[-(1:4845),] 

#plot_1:
library(ggplot2)


##concidering words only above 10000 count:
word_count %>%
  filter(count > 10000) %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(word, count)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#subseting data set by year:

yr_2003 <- subset(data, data$year==2003)
yr_2004 <- subset(data, data$year==2004)
yr_2005 <- subset(data, data$year==2005)
yr_2006 <- subset(data, data$year==2006)
yr_2007 <- subset(data, data$year==2007)
yr_2008 <- subset(data, data$year==2008)
yr_2009 <- subset(data, data$year==2009)
yr_2010 <- subset(data, data$year==2010)
yr_2011 <- subset(data, data$year==2011)
yr_2012 <- subset(data, data$year==2012)
yr_2013 <- subset(data, data$year==2013)
yr_2014 <- subset(data, data$year==2014)
yr_2015 <- subset(data, data$year==2015)
yr_2016 <- subset(data, data$year==2016)
yr_2016 <- subset(data, data$year==2017)


######## Comapring 2003, 2004 across 2005 and comparing proportion spread of words across these two years in comparison with 2005:
####2003#####
word_2003 <- yr_2003 %>%
  unnest_tokens(word, headline_text) %>% anti_join(stop_words)

word_2003 %>%
  count(word, sort = TRUE)

####2004#####
word_2004 <- yr_2004 %>%
  unnest_tokens(word, headline_text) %>% anti_join(stop_words)

word_2004 %>%
  count(word, sort = TRUE)

####2005#####
word_2005 <- yr_2005 %>%
  unnest_tokens(word, headline_text) %>% anti_join(stop_words)

word_2005 %>%
  count(word, sort = TRUE)


library(tidyr)

frequency <- bind_rows(mutate(word_2003, year = "2003"),
                       mutate(word_2004, year = "2004"), 
                       mutate(word_2005, year = "2005")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(year, word) %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(year, proportion) %>% 
  gather(year, proportion, `2003`:`2004`)

frequency <- na.omit(frequency)

# expect a warning about rows with missing values being removed

library(scales)
ggplot(frequency, aes(x = proportion, y = `2005`, color = abs(`2005` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~year, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "2005", x = NULL)


####Sentiment- Analysis ## (using tidy-text sentiments inbuilt lexicon ) ######################################################################################################

library(tidytext)
sentiments


word_df <- data %>%
  unnest_tokens(word, headline_text) %>% anti_join(stop_words)

word_df %>%
  count(word, sort = TRUE)


########## using inner joing with sentiments lexicon for headlines containing positive words:

#positive sentiments:
sentiments_pos <- sentiments %>% 
  filter(sentiment == "positive")

positives_in_headline <- word_df  %>%
  inner_join(sentiments_pos) %>%
  count(word, sort = TRUE)

#negative sentiments:
sentiments_neg <- sentiments %>% 
  filter(sentiment == "negative")

negatives_in_headline <- word_df  %>%
  inner_join(sentiments_neg) %>%
  count(word, sort = TRUE)

#calculating sentiment score across years which match words in bing lexicon:
library(tidyr)
sentiment_score <- word_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(year, month,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#plotting matching sentiment score across all months in each year:

library(ggplot2)

# positives headlines:
ggplot(sentiment_score, aes(month,positive, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 2, scales = "free_x")

# negative headlines:
ggplot(sentiment_score, aes(month,negative, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 2, scales = "free_x")

# overall score summary:
ggplot(sentiment_score, aes(month,sentiment, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 2, scales = "free_x")

###################### Majority (nearly all) of headlines tells a negative sentiment news.############################################


#### Most common positive and negative words:

bing_word_counts <- word_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# plot to gauge contribution of top 10 words(+ve and -ve) in bing lexicon score when iterated over words_df:
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Boxplot showing growth of +ve and -ve by year:
f<- ggplot(data = sentiment_score, aes(x= positive, y= negative, color=year))
f + geom_boxplot()
# Extra:
f + geom_jitter() + geom_boxplot(size= 1, alpha= 0.5, outlier.color = NA)

# # A scatter plot that includes year trends for the positive-negative relationship. 
d <- ggplot(data=sentiment_score ,aes(x=positive, y=negative, color=year))
d + geom_point() + geom_smooth(fill=NA, size=1.2)

p <- ggplot(data = sentiment_score)
# A scatter plot classified by year showing positive, negative and overall sentiment as size
p + geom_point(aes(x=positive,y= negative,
                   color = year, size=sentiment))

