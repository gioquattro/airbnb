# library(dplyr)
# library(janeaustenr)
# library(tidytext)
library(data.table)
library(ggplot2) 
library(textcat) # to infer the language

load_from_db = function(mydb, table_name)
{
  rs = dbSendQuery(mydb, paste("select * from", table_name))
  df = fetch(rs, n=-1)
  dbClearResult(rs)
  dt = as.data.table(df)
  return(dt)
}

proces_review = function(review)
{
  Encoding(review$comments) <- "latin1"
  Encoding(review$created_at) <- "latin1"
  review$nwords = sapply(gregexpr("\\W+", review$comments), length) + 1
  review$nchars = nchar(review$comments)
  review$year = as.numeric( substr(review$created_at, (nchar(review$created_at)-5), (nchar(review$created_at)-1) ) )
  return(review)
}

quantText = function(mytext, ngrams)
{
  library(quanteda)
  doc_term_matrix <- dfm(mytext, stem = T, remove = c("will", stopwords("english")), remove_punct = TRUE, ngrams = ngrams)
  doc_term = data.table( data.frame(doc_term_matrix) )
  detach(package:quanteda)
  return(doc_term)
}

plotText = function(mytext, ngrams)
{
  library(quanteda)
  doc_term_matrix <- dfm(mytext, stem = T, remove = c("will", stopwords("english")), remove_punct = TRUE, ngrams = ngrams)
  set.seed(100)
  textplot_wordcloud(doc_term_matrix, min.freq = 10, random.order = FALSE,
                     rot.per = .25,
                     colors = RColorBrewer::brewer.pal(8,"Dark2"))
  detach(package:quanteda)
}


# df should be something like c("year", "word", "n")
# tfidf = function(df)
# {
#   df = data.frame(df)
#   # computing the totl word count per year
#   
#   total_words <- df %>% 
#     group_by(year) %>% 
#     summarize(total = sum(n))
# 
#   df <- left_join(df, total_words)
# 
#     
#   # plotting the word distribution per year
#   
#   myplot = ggplot(df, aes(n/total, fill = year)) +
#     geom_histogram(show.legend = FALSE) +
#     scale_x_sqrt() +
#     facet_wrap(~year, ncol = 2, scales = "free_y")
#   print(myplot)
#   
#   # computing the term frequency per year
#   
#   freq_by_rank <- df %>% 
#     group_by(year) %>% 
#     mutate(rank = row_number(), 
#            `term frequency` = n/total)
#   
#   # plotting the frequency vs rank (we should get a power law dist. - straight line in the plot)
#   
#   myplot = 
#     ggplot(freq_by_rank, aes(rank, `term frequency`, color = year)) + 
#     geom_line(size = 1.2, alpha = 0.8) + 
#     scale_x_log10() +
#     scale_y_log10()
#   
#   print(myplot)
#   
#   # removing the too popular and impopular words
#   # 
#   # rank_subset <- freq_by_rank %>% 
#   #   filter(rank < 500,
#   #          rank > 10)
#   
#   # computing df-idf
#   
#   df <- df %>%
#     bind_tf_idf(word, year, n)
#   
#   df %>%
#     select(-total) %>%
#     arrange(desc(tf_idf))
#   
#   # plotting the most popular words per year
#   
#   plot_df <- df %>%
#     arrange(desc(tf_idf)) %>%
#     mutate(word = factor(word, levels = rev(unique(word))))
#   
#   myplot <-
#     plot_df %>% 
#     group_by(year) %>% 
#     top_n(15) %>% 
#     ungroup %>%
#     ggplot(aes(word, tf_idf, fill = year)) +
#     geom_col(show.legend = FALSE) +
#     labs(x = NULL, y = "tf-idf") +
#     facet_wrap(~year, ncol = 2, scales = "free") +
#     coord_flip()
#   print(myplot)
#   
#   return(df)
# }


# book_words <- austen_books() %>%
#   unnest_tokens(word, text) %>%
#   count(book, word, sort = TRUE) %>%
#   ungroup()
# 
# colnames(book_words) = c("year", "word", "n")
# 
# tfidf(book_words)
