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
word_count = load_from_db(mydb, "word_count")
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


# 
# extract doc_term
# 

library(quanteda)
mfdict <- dictionary(file = "LIWC2007_English080730.dic", format = "LIWC")
doc_term_matrix <- dfm(review$comments, stem = FALSE, dictionary = mfdict)
# doc_term_matrixN = doc_term_matrix/review$nwords
doc_term = data.table( data.frame(doc_term_matrix) )
detach(package:quanteda)


# 
# join year and relevant fields
# 

relevant_fields = c("i", "we", "you", "shehe", "they", "past", "present", "future",
                    "swear", "social", "family", "friend", "humans", 
                    "posemo", "negemo", "anx", "anger", "sad",
                    "see", "hear", "feel", "sexual", "space", "time",   
                    "work", "achieve", "leisure", "home", "money" ,"relig")  
doc_term = cbind(review[, 'year', with=F], doc_term[, relevant_fields, with=F] )


doc_term_processed = data.table( 
  year = doc_term$year,
  social = doc_term$social, 
  home = doc_term$home,
  posemo = doc_term$posemo, 
  negemo = doc_term$negemo,
  social_vs_home = (doc_term$social+3)/(doc_term$home+3),
  pos_vs_neg = (doc_term$posemo+3)/(doc_term$negemo+3)
)

summary(doc_term_processed)


printSocialHomePosNeg = function(doc_term_processed, alpha, selected_year=NA)
{
  if (is.na(year)) {
    year="All years"
  } else {
    doc_term_processed = doc_term_processed[year==selected_year]
  }
  
  mycor = cor.test(doc_term_processed$social_vs_home, doc_term_processed$pos_vs_neg)
  nstars = ifelse(mycor$p.value<0.001, "***",
                  ifelse(mycor$p.value<0.01, "**",
                         ifelse(mycor$p.value<0.05, "*",
                                ifelse(mycor$p.value<0.1, ".", ""))))
  stat = paste(round(mycor$estimate,2), nstars)

    print(
      ggplot(doc_term_processed, aes(x=pos_vs_neg, y=social_vs_home)) + 
        geom_point(alpha=alpha) +
        geom_smooth(method = "lm") +
        scale_x_sqrt(breaks=c(0,1,2,3,5,10), limits=c(0,10)) + 
        scale_y_sqrt(breaks=c(0,1,2,3,5,10,15,20), limits=c(0,20)) +
        annotate("text", label=paste("cor =", stat), x = 0.1, y = 20, size = 4, colour = "red") +
        ggtitle(selected_year)
    )
  
}

printSocialHomePosNeg(doc_term_processed, alpha=0.01)
printSocialHomePosNeg(doc_term_processed, alpha=0.2, selected_year=2010)
printSocialHomePosNeg(doc_term_processed, alpha=0.2, selected_year=2011)
printSocialHomePosNeg(doc_term_processed, alpha=0.1, selected_year=2012)
printSocialHomePosNeg(doc_term_processed, alpha=0.1, selected_year=2013)
printSocialHomePosNeg(doc_term_processed, alpha=0.05, selected_year=2014)
printSocialHomePosNeg(doc_term_processed, alpha=0.05, selected_year=2015)
printSocialHomePosNeg(doc_term_processed, alpha=0.02, selected_year=2016)
printSocialHomePosNeg(doc_term_processed, alpha=0.01, selected_year=2017)




# 
# visualise
# 

doc_term_melt = melt(doc_term_processed, 
                     id.vars = c("year"),
                     measure.vars = c("social_vs_home", "pos_vs_neg"))


ggplot(doc_term_melt, aes(x=variable, y=value)) +
  geom_violin() +
  geom_boxplot(fill=7, width=.1, outlier.size=NA) +
  facet_grid(.~year) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

# doc_term_gr = doc_term_melt[, .(value=mean(value)), by=list(year, variable)]
# ggplot(doc_term_gr, aes(x=variable, y=value)) + 
#   geom_bar(stat="identity", col=1) +
#   facet_grid(year ~ .) +
#   theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
