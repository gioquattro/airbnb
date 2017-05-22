source("myFunctions.R")


# 
# loading datast
# 

library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
# mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')
dbListTables(mydb)
guest = load_from_db(mydb, "guest")
host = load_from_db(mydb, "host")
listing = load_from_db(mydb, "listing")
review = load_from_db(mydb, "review")
review_language = load_from_db(mydb, "review_language")
detach(package:RMySQL)


# 
# join review and language
# 

setkey(review, idReview)
setkey(review_language, idReview)
review = review[review_language, nomatch=0]
remove(review_language)

# 
# pre-processing
# 

review = pre_proces_review(review)
print_reviewSummary(review)


# 
# removing invalid entries and duplicates
# 

review = review[author_id!='#']
print_reviewSummary(review)

library(sqldf)
review = as.data.table( sqldf("select * from review group by id") )
print_reviewSummary(review)


# 
# reviews only for guests which have a year
# 

review = review[!(author_id %in% listing$id_host) & !is.na(year)]
print_reviewSummary(review)


# 
# reviews only in English written by active authors
# 

review = review[nwords>50]
print_reviewSummary(review)
review = english_review(review)
print_reviewSummary(review)



library(quanteda)
mfdict <- dictionary(file = "LIWC2007_English080730.dic", format = "LIWC")
doc_term_matrix <- dfm(review$comments, stem = FALSE, dictionary = mfdict)
doc_term_matrixN = doc_term_matrix/review$nwords
doc_term = data.table( data.frame(doc_term_matrixN) )
