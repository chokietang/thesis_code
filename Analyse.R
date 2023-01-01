library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(fixest)
library(estimatr)
library(broom)
library(modelsummary)

setwd("~/Master Marketing Analytics/Thesis/Giveaways")


# CHANGE DATAFRAME INTO DATATABLE
author_df <- fread("author_df.csv")
book_df <- fread("book_df_cleanPublisher.csv")
giveaways_thesis <- fread("giveaways_thesis.csv")
reviews_thesis_notext <- fread("reviews_thesis_notext.csv")
reviews_thesis_text <- fread("reviews_thesis_text.csv")

# MERGE DATASETS
data <- book_df %>%
  inner_join(giveaways_thesis, by = c("id" = "book_id","title" = "book_title", "url" = "book_url"), suffix = c("_book_df", "_giveaway_df")) %>%
  inner_join(author_df, by = c("id" = "book_id")) %>%
  distinct(giveaway_id, .keep_all = TRUE) %>%
  select(c("id","book_publication_date","average_rating","ratings_count","text_reviews_count","format_giveaway_df","copy_n","giveaway_end_date"))


# STAR RATING ANALYSE
# MERGE DATA WITH REVIEW NOTEXT
rating_merge <- data %>%
  inner_join(reviews_thesis_notext, by = c("id"="book_id")) %>%
  select(-c(order_filter, rating_filter, review_id, new_review_id, reviewer_name, text_reviews_count))

# CHANGE DATES INTO DATE FORMAT
rating_merge$book_publication_date <- ymd(rating_merge$book_publication_date)
rating_merge$giveaway_end_date <- mdy(rating_merge$giveaway_end_date)
rating_merge$time <- mdy(rating_merge$time)

# CLEAN DATA
rating_clean <- rating_merge %>%
  filter(copy_n >= 5, average_rating > 0, time >= giveaway_end_date - years(1), time <= giveaway_end_date + years(1))

# DATA AGGREGATION FROM DAILY TO MONTHLY-YEAR LEVEL
rating_aggregation <- rating_clean
rating_aggregation$after <- ifelse(rating_aggregation$time <= rating_aggregation$giveaway_end_date, 0, 1)
rating_aggregation$time <- floor_date(rating_aggregation$time, "month")
rating_aggregation$format_giveaway_df <- ifelse(rating_aggregation$format_giveaway_df == "Print book", 0, 1)

rating_aggregation$format_giveaway_df <- as.factor(rating_aggregation$format_giveaway_df)

rating_aggregation <- rating_aggregation %>% 
  group_by(id, time, after) %>% # also group by after, because some of floor_date$time -> some are before and some are after the giveaway
  mutate(ratings = mean(ratings),
         n_reviews_month = n()) %>%
  distinct(time, .keep_all = TRUE)

# FIXED EFFECT MODEL
rating_fe <- feols(ratings ~ after + after:format_giveaway_df
                      |
                      id + time,
                      cluster = ~id, # need to cluster standard errors at the book-level
                      data = rating_aggregation)
summary(rating_fe)
rating_coefs <- tidy(rating_fe, conf.int = TRUE)
msummary(list(rating_fe))


# DESCRIPTIVE STATISTICS OF THE STAR RATING DATASET
n_distinct(rating_aggregation$id) # number of books that participated in the giveaways
summary(rating_aggregation)
sd(rating_aggregation$ratings)

digitalvskindle <- rating_aggregation %>%
  group_by(id) %>%
  count(format_giveaway_df)
table(digitalvskindle$format_giveaway_df) # proportion of printed books and kindle books giveaways


# PLOT OF THE AVERAGE STAR RATING OF ALL BOOKS AT YEAR-MONTH LEVEL
print_only_rating <- rating_aggregation %>%
  filter(format_giveaway_df == "0")
y_rating <- aggregate(ratings ~ time, data = print_only_rating, FUN = mean)

kindle_only_rating <- rating_aggregation %>%
  filter(format_giveaway_df == "1")
y2_rating <- aggregate(ratings ~ time, data = kindle_only_rating, FUN = mean)

ggplot()+
  geom_line(data = y_rating, mapping = aes(x = time, y = ratings, colour = "Print")) +
  geom_line(data = y2_rating, mapping = aes(x = time, y = ratings, colour = "Kindle")) +
  scale_colour_manual("",
                     breaks = c("Print","Kindle"),
                     values = c("blue", "red")) +
  xlab("Time") +
  scale_y_continuous("Average Star Rating")



# TEXT REVIEWS - SENTIMENT ANALYSIS
# MERGE DATASET WITH REVIEW_THESIS_TEXT DATASET
text_merge <- data %>%
              inner_join(reviews_thesis_text, by = c("id" = "book_id")) %>%
              select(-c(ratings, ratings_count)) %>%
              filter(!(text==""))

# CHANGE DATES INTO DATE FORMAT
text_merge$book_publication_date <- ymd(text_merge$book_publication_date)
text_merge$giveaway_end_date <- mdy(text_merge$giveaway_end_date)
text_merge$time <- as.Date(text_merge$time) # time was stored as IDate -> change format to Date, this way all dates have the same format

text_merge$format_giveaway_df <- ifelse(text_merge$format_giveaway_df == "Print book", 0, 1)


# TEXT ANALYSIS ONE YEAR BEFORE AND AFTER THE GIVEAWAY
# CLEAN DATA
data_text <- text_merge %>%
              filter(copy_n >= 5, average_rating > 0, time >= giveaway_end_date - years(1), time <= giveaway_end_date + years(1))

# BING LEXICON
sentiment_bing <- data_text %>%
  unnest_tokens(word, text) %>% # separate each word from the review text into a new row
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing"), by = "word")

sentiment_review <- sentiment_bing %>%
  count(new_review_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(valence = case_when(
    sentiment > 0 ~ "positive",
    sentiment < 0 ~ "negative",
    sentiment == 0 ~ "neutral"))

sentiment_analysis <- data_text %>%
  inner_join(sentiment_review, by = "new_review_id") %>%
  select(-c("negative","positive")) %>%
  filter(valence != "neutral") # exclude neutral reviews from the dataset; want impact of giveaway on positive/negative reviews

sentiment_analysis$valence <- ifelse(sentiment_analysis$valence == "negative", 0, 1)
sentiment_analysis$time <- floor_date(sentiment_analysis$time, "month")
sentiment_analysis$after <- ifelse(sentiment_analysis$time <= sentiment_analysis$giveaway_end_date, 0, 1)

sentiment_analysis$format_giveaway_df <- as.factor(sentiment_analysis$format_giveaway_df)

sentiment_analysis <- sentiment_analysis %>% 
  group_by(id, time, after) %>%
  mutate(mean_valence = mean(valence),
         total_reviews_per_month = n()) %>%
  distinct(time, .keep_all = TRUE)

# SENTIMENT ANALYSIS
sentiment_fe <- feols(mean_valence ~ after + after:format_giveaway_df 
                      |
                        id + time,
                      cluster = ~id, # need to cluster standard errors at the book level
                      data = sentiment_analysis)
summary(sentiment_fe)
sentiment_coefs <- tidy(sentiment_fe, conf.int = TRUE)
msummary(list(sentiment_fe))


# DESCRIPTIVE DATA OF TEXT REVIEWS
n_distinct(sentiment_analysis$id) # number of books that participated in the giveaways 
summary(sentiment_analysis)
sd(sentiment_analysis$mean_valence)

digitalvskindle2 <- sentiment_analysis %>%
  group_by(id) %>%
  count(format_giveaway_df)
table(digitalvskindle2$format_giveaway_df) # proportion of printed books and kindle books giveaways


# PLOT OF THE AVERAGE TEXT VALENCE OF ALL BOOKS AT YEAR-MONTH LEVEL
print_only_text <- sentiment_analysis %>%
  filter(format_giveaway_df == "0")
y_text <- aggregate(mean_valence ~ time, data = print_only_text, FUN = mean)

kindle_only_text <- sentiment_analysis %>%
  filter(format_giveaway_df == "1")
y2_text <- aggregate(mean_valence ~ time, data = kindle_only_text, FUN = mean)

ggplot()+
  geom_line(data = y_text, mapping = aes(x = time, y = mean_valence, colour = "Print")) +
  geom_line(data = y2_text, mapping = aes(x = time, y = mean_valence, colour = "Kindle")) +
  scale_colour_manual("",
                      breaks = c("Print","Kindle"),
                      values = c("blue", "red")) +
  xlab("Time") +
  scale_y_continuous("Average Text Valence")



# TEXT ANALYSIS TWO YEARS BEFORE AND AFTER THE GIVEAWAY
data_text2 <- text_merge %>%
  filter(copy_n >= 5, average_rating > 0, time >= giveaway_end_date - years(2), time <= giveaway_end_date + years(2))

# BING LEXICON
sentiment_bing2 <- data_text2 %>%
  unnest_tokens(word, text) %>% # separate each word from the review text into a new row
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing"), by = "word")

sentiment_review2 <- sentiment_bing2 %>%
  count(new_review_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(valence = case_when(
    sentiment > 0 ~ "positive",
    sentiment < 0 ~ "negative",
    sentiment == 0 ~ "neutral"))

sentiment_analysis2 <- data_text2 %>%
  inner_join(sentiment_review2, by = "new_review_id") %>%
  select(-c("negative","positive")) %>%
  filter(valence != "neutral") # exclude neutral reviews; want impact of giveaway on positive/negative reviews

sentiment_analysis2$valence <- ifelse(sentiment_analysis2$valence == "negative", 0, 1)
sentiment_analysis2$time <- floor_date(sentiment_analysis2$time, "month")
sentiment_analysis2$after <- ifelse(sentiment_analysis2$time <= sentiment_analysis2$giveaway_end_date, 0, 1)

sentiment_analysis2$format_giveaway_df <- as.factor(sentiment_analysis2$format_giveaway_df)

sentiment_analysis2 <- sentiment_analysis2 %>% 
  group_by(id, time, after) %>%
  mutate(mean_valence = mean(valence),
         total_reviews_per_month = n()) %>%
  distinct(time, .keep_all = TRUE)

# SENTIMENT ANALYSIS 2
sentiment_fe2 <- feols(mean_valence ~ after + after:format_giveaway_df 
                      |
                        id + time,
                      cluster = ~id, # need to cluster standard errors at the book level
                      data = sentiment_analysis2)
summary(sentiment_fe2)
sentiment_coefs2 <- tidy(sentiment_fe2, conf.int = TRUE)
msummary(sentiment_fe2)


# ALL OUTPUT INTO ONE FIGURE
output <- msummary(list("Star Rating" = rating_fe, 
              "Text Reviews - 1 year" = sentiment_fe,
              "Text Reviews - 2 years" = sentiment_fe2),
         coef_rename = c("after" = "After",
                         "after:format_giveaway_df" = "AfterxKindle"),
         stars = TRUE,
         gof_omit = "R2 Pseudo|AIC|BIC|Log.Lik.")
output