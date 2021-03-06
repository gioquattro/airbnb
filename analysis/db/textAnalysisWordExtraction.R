library(data.table)
library(ggplot2)

load_from_db = function(mydb, table_name)
{
  rs = dbSendQuery(mydb, paste("select * from", table_name))
  df = fetch(rs, n=-1)
  dbClearResult(rs)
  dt = as.data.table(df)
  
  if (colnames(dt)[1] == "row_names")
    dt = dt[, -c("row_names"), with=F]
  
  return(dt)
}


library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
# mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')
dbListTables(mydb)
listing = load_from_db(mydb, "listing")
review = load_from_db(mydb, "review")
review_language = load_from_db(mydb, "review_language")
detach(package:RMySQL)


# 
# join review with review_language
# 

setkey(review, idReview)
setkey(review_language, idReview)
review = review[review_language, nomatch=0]
remove(review_language)


# 
# reviews only for guests which have a year
# 

review = review[!(author_id %in% listing$id_host)]



library(quanteda)
words = NULL
t0 = Sys.time()
factor = 100
max_i = round(nrow(review)/factor,0)
set.seed(1977)
iterations = sample(1:max_i, 1000)
xi = 0
for ( i in iterations )
# for ( i in 1:3 )
{
  xi = xi+1
  ti = Sys.time()
  rev_i = review[ as.numeric(row.names(review))>=factor*(i-1) & as.numeric(row.names(review))<factor*(i) ]
  rev_i = rev_i[inferredLanguage=='english']
  
  # doc_term_mi <- dfm(rev_i$comments, stem = T, remove = c("will", stopwords("english")), remove_punct = TRUE, ngrams = 2)
  doc_term_mi <- dfm(rev_i$comments, stem = F, remove_punct = TRUE, ngrams = 1)
  doc_term_i = data.table( data.frame(doc_term_mi) )
  
  words_i = data.table(iteration=i, word=colnames(doc_term_i) )
  words = rbind(words, words_i)
  print(Sys.time() - ti)
  writeLines( paste("Step", xi, "(", round(100*xi/length(iterations),1), "%)"))
}
print(Sys.time() - t0)
detach(package:quanteda)

names(words)
words_gr = words[, .N, by=word]

table(words_gr$N)
ggplot(words_gr, aes(N)) + 
  geom_histogram(col="black", fill="green", bins=104) + 
  scale_x_sqrt(breaks=c(1,2,3,4,5,7,10,20,30,40,50,70,100,500,1000)) +
  scale_y_sqrt(breaks=c(10,100,1000,3000,5000,10000,20000,30000))

words_gr[N==5]$word
length(words_gr[N>10]$word)


write.table(words_gr, file = "word_count.csv", sep = ",", na = "", 
            row.names = F, col.names = T)


library(RMySQL)
dbWriteTable(mydb, value=words_gr, name="word_count", append=F, row.names=F, col.names=T) 
detach(package:RMySQL)
