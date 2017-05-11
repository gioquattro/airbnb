source("textAnalysisFunc.R")


# 
# loading datast
# 

library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
# mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')
dbListTables(mydb)
review = load_from_db(mydb, "review")
detach(package:RMySQL)

review = proces_review(review)

table(review$year)

library(quanteda)

words = NULL
t0 = Sys.time()
factor = 2000
for ( i in 1:round(nrow(review)/factor,0) )
# for ( i in 1:10 )
{
  ti = Sys.time()
  rev_i = review[ as.numeric(row.names(review))>=factor*(i-1) & as.numeric(row.names(review))<factor*(i) ]
  rev_i$inferredLanguage = textcat(rev_i$comments)
  rev_i = rev_i[inferredLanguage=='english']

  doc_term_mi <- dfm(rev_i$comments, stem = T, remove = c("will", stopwords("english")), remove_punct = TRUE, ngrams = 1)
  doc_term_i = data.table( data.frame(doc_term_mi) )
  
  words_i = data.table(iteration=i, word=colnames(doc_term_i) )
  words = rbind(words, words_i)
  writeLines( paste("Step", i, "(", round(100*i/(nrow(review)/factor),1), "%)", 
                    "--", round(Sys.time() - ti, 1), "sec",
                    "--", round(Sys.time() - t0, 1), "sec (tot time)") )
}
detach(package:quanteda)

names(words)
words_gr = words[, .N, by=word]

table(words_gr$N)
words_gr[N==5]$word
length(words_gr[N>10]$word)

write.table(words_gr, file = "words_count.csv", sep = ",", na = "", 
            row.names = F, col.names = T)
