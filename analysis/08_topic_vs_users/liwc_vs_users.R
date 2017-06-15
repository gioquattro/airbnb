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
review = as.data.table( sqldf("select * from review group by author_id, listing_id, comments") )
print_reviewSummary(review)


# 
# reviews only for guests which have a year
# 

# review = review[!(author_id %in% listing$id_host) & !is.na(year)]
# print_reviewSummary(review)


# 
# reviews only in English
# 

review = english_review(review)
print_reviewSummary(review)


# 
# length of reviews (too short reviews may not give statistical significant results)
# 

# ggplot(review, aes(x=factor(year), y=nwords)) + 
#   geom_violin(alpha=0.5, fill=3) +
#   geom_boxplot(width=.1, fill=7, outlier.size=NA) +
#   scale_y_log10(breaks=c(1,5,10,50,100,500,1000,5000)) 
# 
# ggplot(review, aes(x=factor(year), y=nwords)) + 
#   geom_jitter(alpha=0.01, size=.1, width=.3) +
#   geom_violin(alpha=0.5, fill=3) +
#   geom_boxplot(width=.1, fill=7, outlier.size=NA) +
#   ylim(0,200)
# 
# review = review[nwords>10]
# print_reviewSummary(review)


# 
# not an host
# 

review = review[!author_id %in% host$Id]
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
# defining metrics
# 

doc_term2 = cbind(review[, c('author_id', 'listing_id', 'year', 'comments', 'locale',
                             'inferredLanguage', 'nwords'), with=F], 
                  doc_term[, c('social', 'home', 'posemo', 'negemo'), with=F] )

doc_term2$social_vs_home = (doc_term2$social+3)/(doc_term2$home+3)
doc_term2$pos_vs_neg = (doc_term2$posemo+3)/(doc_term2$negemo+3)

summary(doc_term2[,c('social', 'home', 
                     'posemo', 'negemo', 
                     'social_vs_home', 'pos_vs_neg'), with=F])
#   social            home            posemo           negemo        social_vs_home     pos_vs_neg    
# Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.0000   Min.   :0.2727   Min.   :0.4444  
# 1st Qu.: 2.000   1st Qu.: 1.000   1st Qu.: 3.000   1st Qu.: 0.0000   1st Qu.:1.1667   1st Qu.:2.0000  
# Median : 5.000   Median : 2.000   Median : 5.000   Median : 0.0000   Median :1.6250   Median :2.3333  
# Mean   : 6.505   Mean   : 2.345   Mean   : 5.685   Mean   : 0.2537   Mean   :1.8521   Mean   :2.7145  
# 3rd Qu.: 9.000   3rd Qu.: 3.000   3rd Qu.: 7.000   3rd Qu.: 0.0000   3rd Qu.:2.3333   3rd Qu.:3.3333  
# Max.   :56.000   Max.   :20.000   Max.   :36.000   Max.   :15.0000   Max.   :8.4286   Max.   :9.7500  



# 
# adding locale for authors
# 


# most common locales
locales = doc_term2[, .(cnt=length(comments)), by=locale]
setorder(locales, -cnt)
locales$tcnt = cumsum(locales$cnt)
nrow( locales[cnt>2] )/nrow( locales )
max( locales[cnt>2]$tcnt )/max( locales$tcnt )
write.table(locales, row.names = F, col.names = T, file = "locales.csv", sep = ";")



countries = fread("Country.csv")
library(stringr)
countries$city = str_replace_all(countries$city, ' ', '')
doc_term2$locale = str_replace_all(doc_term2$locale, ' ', '')

doc_term3 = as.data.table( sqldf("select t1.*, 
                                         t2.country as country_guest,
                                         t3.city as city_listing,
                                         t3.country as country_listing
                                  from doc_term2 t1
                                  left join countries t2 on t1.locale=t2.city
                                  left join listing t3 on t1.listing_id=t3.id")
)

doc_term3$country_guest = str_replace_all(doc_term3$country_guest, '"', '')
doc_term3$country_listing = str_replace_all(doc_term3$country_listing, '"', '')

cultural = fread("world_cultural_factors.csv")


# 
# do the countries have the same names as in cultural?
# 

countries_guest = doc_term3[, .(cnt=length(comments)), by=country_guest]
setorder(countries_guest, -cnt)

countries_listing = doc_term3[, .(cnt=length(comments)), by=country_listing]
setorder(countries_listing, -cnt)

cultural$country
# [1] "Argentina"            "Australia"            "Austria"              "Belgium"              "Brazil"              
# [6] "Canada"               "China"                "Colombia"             "Costa Rica"           "Denmark"             
# [11] "Ecuador"              "Egypt"                "El Salvador"          "Finland"              "France"              
# [16] "Germany"              "Greece"               "Guatemala"            "Hong Kong"            "India"               
# [21] "Indonesia"            "Iran"                 "Iraq"                 "Ireland"              "Israel"              
# [26] "Italy"                "Jamaica"              "Japan"                "Libya"                "Malaysia"            
# [31] "Mexico"               "Netherlands"          "New Zealand"          "Nigeria"              "Norway"              
# [36] "Pakistan"             "Panama"               "Peru"                 "Philippines"          "Poland"              
# [41] "Portugal"             "Russia"               "Saudi Arabia"         "Sierra Leon"          "Singapore"           
# [46] "South Korea"          "Spain"                "Sweden"               "Switzerland"          "Switzerland"         
# [51] "Taiwan"               "Thailand"             "Turkey"               "United Arab Emirates" "United Kingdom"      
# [56] "United States"        "Uruguay"              "Venezuela"  

countries_guest$country_guest[1:20]
# [1] NA            "USA"         "UK"          "Canada"      "Germany"     "France"      "Australia"   "Italy"       "Netherlands"
# [10] "Spain"       "Singapore"   "Switzerland" "Denmark"     "Russia"      "Ireland"     "New Zealand" "Brazil"      "Hong Kong"  
# [19] "Portugal"    "Israel"               

countries_listing$country_listing[1:20]
# [1] NA               "United States"  "null"           "United Kingdom" "Italy"          "Canada"         "Spain"         
# [8] "France"         "Australia"      "Japan"          "Mexico"         "Portugal"       "Germany"        "Greece"        
# [15] "Netherlands"    "Ireland"        "Croatia"        "Indonesia"      "New Zealand"    "Czech Republic"

doc_term3[country_guest=="USA"] <- "United States"
doc_term3[country_guest=="UK"] <- "United Kingdom"


# 
# joining GDP
# 

doc_term4 = as.data.table(
  sqldf("select t1.*, t2.GDP_per_capita_in_dollars GDP_guest, t3.GDP_per_capita_in_dollars GDP_listing
         from doc_term3 t1
         left join cultural t2 on t1.country_guest=t2.country
         left join cultural t3 on t1.country_listing=t3.country")
)


doc_term4$GDP_delta = doc_term4$GDP_listing - doc_term4$GDP_guest
# 
doc_term4$GDP_guest_br = cut(doc_term4$GDP_guest, breaks=10)
doc_term4$GDP_listing_br = cut(doc_term4$GDP_listing, breaks=10)
doc_term4$GDP_delta_br = cut(doc_term4$GDP_delta, breaks=10)

names(doc_term4)
# [1] "author_id"        "listing_id"       "year"             "comments"         "locale"           "inferredLanguage"
# [7] "nwords"           "social"           "home"             "posemo"           "negemo"           "social_vs_home"  
# [13] "pos_vs_neg"       "country_guest"    "city_listing"     "country_listing"  "GDP_guest"        "GDP_listing"     
# [19] "GDP_delta"        "GDP_guest_br"     "GDP_listing_br"   "GDP_delta_br"   

ggplot( doc_term4, aes(x=GDP_guest_br, y=social_vs_home) ) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(size=.5, alpha=.01) 
  
ggplot( doc_term4, aes(x=GDP_guest_br, y=pos_vs_neg) ) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(size=.5, alpha=.01) 


ggplot( doc_term4, aes(x=GDP_listing_br, y=social_vs_home) ) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(size=.5, alpha=.01) 

ggplot( doc_term4, aes(x=GDP_listing_br, y=pos_vs_neg) ) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(size=.5, alpha=.01) 


ggplot( doc_term4, aes(x=GDP_delta_br, y=social_vs_home) ) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(size=.5, alpha=.01) 

ggplot( doc_term4, aes(x=GDP_delta_br, y=pos_vs_neg) ) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(size=.5, alpha=.01) 




# 
# grouping by quartiles
# 

doc_termLS = doc_term4[social_vs_home<1.1]
doc_termHS = doc_term4[social_vs_home>2.33]
doc_termLP = doc_term4[pos_vs_neg<2]
doc_termHP = doc_term4[pos_vs_neg>3.33]

summary( doc_termLS[, c('GDP_guest', 'GDP_listing', 'GDP_delta'), with=F] )
#  GDP_guest      GDP_listing      GDP_delta     
# Min.   : 3500   Min.   : 3500   Min.   :-58600  
# 1st Qu.:30500   1st Qu.:30500   1st Qu.: -7225  
# Median :37300   Median :39400   Median :     0  
# Mean   :35225   Mean   :37357   Mean   : -1119  
# 3rd Qu.:41000   3rd Qu.:47200   3rd Qu.:  6800  
# Max.   :62100   Max.   :62100   Max.   : 58600  
# NA's   :13139   NA's   :11261   NA's   :17612   

summary( doc_termHS[, c('GDP_guest', 'GDP_listing', 'GDP_delta'), with=F] )
#  GDP_guest      GDP_listing      GDP_delta       
# Min.   : 3500   Min.   : 3500   Min.   :-58600.0  
# 1st Qu.:30000   1st Qu.:30500   1st Qu.: -6300.0  
# Median :35700   Median :39400   Median :     0.0  
# Mean   :33801   Mean   :36664   Mean   :   599.8  
# 3rd Qu.:39400   3rd Qu.:47200   3rd Qu.:  8900.0  
# Max.   :62100   Max.   :62100   Max.   : 58600.0  
# NA's   :14022   NA's   :12447   NA's   :18892     

summary( doc_termLP[, c('GDP_guest', 'GDP_listing', 'GDP_delta'), with=F] )
#  GDP_guest      GDP_listing      GDP_delta       
# Min.   : 3500   Min.   : 3500   Min.   :-58600.0  
# 1st Qu.:30500   1st Qu.:30500   1st Qu.: -6100.0  
# Median :35700   Median :39400   Median :     0.0  
# Mean   :34412   Mean   :37493   Mean   :   271.8  
# 3rd Qu.:40300   3rd Qu.:47200   3rd Qu.:  7800.0  
# Max.   :62100   Max.   :62100   Max.   : 58600.0  
# NA's   :16920   NA's   :14940   NA's   :22938     

summary( doc_termHP[, c('GDP_guest', 'GDP_listing', 'GDP_delta'), with=F] )
#  GDP_guest      GDP_listing      GDP_delta     
# Min.   : 3500   Min.   : 3500   Min.   :-58600  
# 1st Qu.:30500   1st Qu.:30500   1st Qu.: -7900  
# Median :37300   Median :39100   Median :     0  
# Mean   :34747   Mean   :35861   Mean   : -1133  
# 3rd Qu.:40300   3rd Qu.:47200   3rd Qu.:  7800  
# Max.   :62100   Max.   :62100   Max.   : 58600  
# NA's   :8296    NA's   :7831    NA's   :11545   

































guest_LS = guest[id %in% review_g_LS$author_id]
guest_HS = guest[id %in% review_g_HS$author_id]
guest_LP = guest[id %in% review_g_LP$author_id]
guest_HP = guest[id %in% review_g_HP$author_id]

guest_HS$location[1:50]

listing$city[1:50]


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
      geom_smooth(col=2) +
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
      geom_smooth(col=2) +
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

ggplot(doc_term_processed, aes(social_vs_home)) + geom_histogram(col=1, fill=3, bins=100) + scale_x_log10()
ggplot(doc_term_processed, aes(pos_vs_neg)) + geom_histogram(col=1, fill=3, bins=100) + scale_x_log10() + scale_y_sqrt()


# 
# visualise social vs emotion
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
        geom_smooth(col=2) +
        scale_x_log10() + 
        scale_y_log10() +
        annotate("text", label=paste("cor =", stat), x = 1, y = 1000, size = 4, colour = "red") +
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

ggplot(doc_term_processed, aes(x=factor(year), y=social_vs_home)) + 
  geom_jitter(alpha=0.01, size=.05, width=.3) +
  geom_violin(alpha=0.7, fill=3) +
  geom_boxplot(width=.1, fill=7, outlier.size=NA) +
  scale_y_log10()

ggplot(doc_term_processed, aes(x=factor(year), y=pos_vs_neg)) + 
  geom_jitter(alpha=0.01, size=.05, width=.3) +
  geom_violin(alpha=0.7, fill=3) +
  geom_boxplot(width=.1, fill=7, outlier.size=NA) +
  scale_y_log10()


# doc_term_melt = melt(doc_term_processed, 
#                      id.vars = c("year"),
#                      measure.vars = c("social_vs_home", "pos_vs_neg"))
# 
# 
# ggplot(doc_term_melt, aes(x=variable, y=value)) +
#   geom_violin() +
#   geom_boxplot(fill=7, width=.1, outlier.size=NA) +
#   facet_grid(.~year) +
#   theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
