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

doc_term = cbind(review[, c('year', 'comments', 'locale',
                            'inferredLanguage', 'nwords'), with=F], 
                 doc_term[, relevant_fields, with=F] )


# 
# defining metrics
# 

doc_term_processed = data.table( 
  social_vs_home = (doc_term$social+3)/(doc_term$home+3),
  pos_vs_neg = (doc_term$posemo+3)/(doc_term$negemo+3)
)

doc_term_processed = cbind(doc_term[, c('year', 'comments', 'locale',
                            'inferredLanguage', 'nwords',
                            'social', 'home', 
                            'posemo', 'negemo'), with=F], 
                 doc_term_processed )


summary(doc_term_processed[,c('social', 'home', 
                              'posemo', 'negemo', 
                              'social_vs_home', 'pos_vs_neg'), with=F])
#      social             home            posemo           negemo        social_vs_home      pos_vs_neg    
# Min.   :  0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.0000   Min.   : 0.2308   Min.   : 0.250  
# 1st Qu.:  5.000   1st Qu.: 1.000   1st Qu.: 5.000   1st Qu.: 0.0000   1st Qu.: 1.3750   1st Qu.: 2.333  
# Median :  8.000   Median : 3.000   Median : 7.000   Median : 0.0000   Median : 2.0000   Median : 3.000  
# Mean   :  9.825   Mean   : 3.306   Mean   : 7.211   Mean   : 0.4882   Mean   : 2.2570   Mean   : 3.056  
# 3rd Qu.: 12.000   3rd Qu.: 4.000   3rd Qu.: 9.000   3rd Qu.: 1.0000   3rd Qu.: 2.8182   3rd Qu.: 3.667  
# Max.   :176.000   Max.   :72.000   Max.   :45.000   Max.   :25.0000   Max.   :22.6667   Max.   :12.667  


# 
# sanity check
# 

table(doc_term_processed$inferredLanguage)
# english 
# 43259

testSHH = doc_term_processed[social_vs_home>10]
testSHL = doc_term_processed[social_vs_home<.5]
testPNH = doc_term_processed[pos_vs_neg>7]
testPNL = doc_term_processed[pos_vs_neg<.5]

testSHH$comments[1:20]
testSHL$comments[1:20]
testPNH$comments[1:20]
testPNL$comments[1:20]



# 
# shorter reviews for homes and negative emotions?
# 

printSocialHomeWords = function(doc_term_processed, alpha, selected_year=NA)
{
  if (is.na(selected_year)) {
    selected_year="All years"
  } else {
    doc_term_processed = doc_term_processed[year==selected_year]
  }
  
  mycor = cor.test(doc_term_processed$social_vs_home, doc_term_processed$nwords)
  nstars = ifelse(mycor$p.value<0.001, "***",
                  ifelse(mycor$p.value<0.01, "**",
                         ifelse(mycor$p.value<0.05, "*",
                                ifelse(mycor$p.value<0.1, ".", ""))))
  stat = paste(round(mycor$estimate,2), nstars)
  
  print(
    ggplot(doc_term_processed, aes(x=social_vs_home, y=nwords)) + 
      geom_point(alpha=alpha) +
      geom_smooth(method = "lm") +
      scale_x_log10() + 
      scale_y_log10() +
      annotate("text", label=paste("cor =", stat), x = 1, y = 1000, size = 4, colour = "red") +
      ggtitle(selected_year)
  )
}


printSocialHomeWords(doc_term_processed, alpha=0.01)
printSocialHomeWords(doc_term_processed, alpha=0.2, selected_year=2010)
printSocialHomeWords(doc_term_processed, alpha=0.2, selected_year=2011)
printSocialHomeWords(doc_term_processed, alpha=0.1, selected_year=2012)
printSocialHomeWords(doc_term_processed, alpha=0.1, selected_year=2013)
printSocialHomeWords(doc_term_processed, alpha=0.05, selected_year=2014)
printSocialHomeWords(doc_term_processed, alpha=0.05, selected_year=2015)
printSocialHomeWords(doc_term_processed, alpha=0.02, selected_year=2016)
printSocialHomeWords(doc_term_processed, alpha=0.01, selected_year=2017)



printPosNegWords = function(doc_term_processed, alpha, selected_year=NA)
{
  if (is.na(selected_year)) {
    selected_year="All years"
  } else {
    doc_term_processed = doc_term_processed[year==selected_year]
  }
  
  mycor = cor.test(doc_term_processed$pos_vs_neg, doc_term_processed$nwords)
  nstars = ifelse(mycor$p.value<0.001, "***",
                  ifelse(mycor$p.value<0.01, "**",
                         ifelse(mycor$p.value<0.05, "*",
                                ifelse(mycor$p.value<0.1, ".", ""))))
  stat = paste(round(mycor$estimate,2), nstars)
  
  print(
    ggplot(doc_term_processed, aes(x=pos_vs_neg, y=nwords)) + 
      geom_point(alpha=alpha) +
      geom_smooth(method = "lm") +
      scale_x_log10() + 
      scale_y_log10() +
      annotate("text", label=paste("cor =", stat), x = 1, y = 1000, size = 4, colour = "red") +
      ggtitle(selected_year)
  )
}


printPosNegWords(doc_term_processed, alpha=0.01)
printPosNegWords(doc_term_processed, alpha=0.2, selected_year=2010)
printPosNegWords(doc_term_processed, alpha=0.2, selected_year=2011)
printPosNegWords(doc_term_processed, alpha=0.1, selected_year=2012)
printPosNegWords(doc_term_processed, alpha=0.1, selected_year=2013)
printPosNegWords(doc_term_processed, alpha=0.05, selected_year=2014)
printPosNegWords(doc_term_processed, alpha=0.05, selected_year=2015)
printPosNegWords(doc_term_processed, alpha=0.02, selected_year=2016)
printPosNegWords(doc_term_processed, alpha=0.01, selected_year=2017)



# 
# distributions
# 

ggplot(doc_term_processed, aes(social_vs_home)) + geom_histogram() + scale_x_log10()
ggplot(doc_term_processed, aes(pos_vs_neg)) + geom_histogram() + scale_x_log10() + scale_y_sqrt()


# 
# visualise scatterplot
# 

printSocialHomePosNeg = function(doc_term_processed, alpha, selected_year=NA)
{
  if (is.na(selected_year)) {
    selected_year="All years"
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
# visualise evolution over time
# 

doc_term_melt = melt(doc_term_processed, 
                     id.vars = c("year"),
                     measure.vars = c("social_vs_home", "pos_vs_neg"))


ggplot(doc_term_melt, aes(x=variable, y=value)) +
  geom_violin() +
  geom_boxplot(fill=7, width=.1, outlier.size=NA) +
  facet_grid(.~year) +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

# doc_term_gr = doc_term_melt[, .(value=mean(value)), by=list(year, variable)]
# ggplot(doc_term_gr, aes(x=variable, y=value)) + 
#   geom_bar(stat="identity", col=1) +
#   facet_grid(year ~ .) +
#   theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
