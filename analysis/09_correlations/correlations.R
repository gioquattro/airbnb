names(world_cultural_factors)
names(review_iso)

review_iso$isEnglish = ifelse(review_iso$inferredLanguage=='english', 1, 0)
summary(review_iso$isEnglish)
review_iso = review_iso[!is.na(isEnglish)]

byLanguage = review_iso[, .(n=length(id)), by=list(Code3,inferredLanguage)]

review_iso_g = review_iso[, .(n=length(id), english_ratio=mean(isEnglish)), by=Code3]
review_iso_g = review_iso_g[n>100]

setkey(review_iso_g, Code3)
setkey(world_cultural_factors, Code3)
review_iso_g = review_iso_g[world_cultural_factors, nomatch=0]

library(ggplot2)
names(review_iso_g)

ggplot(review_iso_g, aes(x=english_ratio, y=individualism, label=Code3)) + geom_label()
ggplot(review_iso_g, aes(x=english_ratio, y=power_distance, label=Code3)) + geom_label()
ggplot(review_iso_g, aes(x=english_ratio, y=GDP_per_capita_in_dollars, label=Code3)) + geom_label()


countries = review_iso_g
rm(list=setdiff(ls(), c("countries", "review_iso")))
countries_en = countries[english_ratio>0.6]

review_iso_english = review_iso[Code3 %in% countries_en$Code3 & isEnglish==1]

library(quanteda)
mfdict <- dictionary(file = "LIWC2007_English080730.dic", format = "LIWC")
doc_term_matrix <- dfm(review_iso_english$comments, stem = FALSE, dictionary = mfdict)
# doc_term_matrixN = doc_term_matrix/review$nwords
doc_term = data.table( data.frame(doc_term_matrix) )
detach(package:quanteda)

review_iso_english = cbind(review_iso_english, doc_term)
remove(doc_term)

library(stringr)
review_iso_english$nwords = str_count(review_iso_english$comments, '\\w+')
detach(package:stringr)


# Metrics
#   length = #words (length of a review)
#   neg = #neg/#words (negative sentiment)
#   pos = #pos/#words (positive sentiment)
#   house = #house/#words (house focused)
#   social = #social/#words (social focused)
#   self = #(i,me,my,myself)/#words (self focused)
#   altruistic = #(we,our,us,ourselves,she,he,his,her,they,their)/#words (focused on others)
#   location = #(location,access,close by,to,distance,convenient,functional)/#words (focus on location and accessibility)
#   money = #money/#words (focus on money and value)

review_iso_english$m_length = review_iso_english$nwords
review_iso_english$m_neg = review_iso_english$negemo / review_iso_english$nwords
review_iso_english$m_pos = review_iso_english$posemo / review_iso_english$nwords
review_iso_english$m_house = review_iso_english$home / review_iso_english$nwords
review_iso_english$m_social = review_iso_english$social / review_iso_english$nwords
review_iso_english$m_self = (review_iso_english$i+review_iso_english$ipron) / review_iso_english$nwords
review_iso_english$m_altruistic = (review_iso_english$we+review_iso_english$you+review_iso_english$shehe+review_iso_english$they) / review_iso_english$nwords
review_iso_english$m_location = review_iso_english$space / review_iso_english$nwords
review_iso_english$m_money = review_iso_english$money / review_iso_english$nwords


by_comments = as.data.table(
  sqldf("select comments, count(*) freq_comment
        from review_iso
        group by comments")
  )

review_iso_english = as.data.table(
  sqldf("select *
        from review_iso_english 
        where comments not like 'The host canceled this reservation%' ")
  )

review_iso_english = as.data.table(
  sqldf("select *
        from review_iso_english 
        where comments not like 'The guest canceled this reservation%' ")
  )

review_iso_english = as.data.table(
  sqldf("select *
        from review_iso_english 
        where comments not like 'The reservation was canceled%' ")
  )


summary(review_iso_english$m_length)

hist( (review_iso_english$m_length ))
hist( (review_iso_english$m_neg ))
hist( (review_iso_english$m_pos ))
hist( (review_iso_english$m_house ))
hist( (review_iso_english$m_social ))
hist( (review_iso_english$m_self ))
hist( (review_iso_english$m_altruistic ))
hist( (review_iso_english$m_location ))
hist( (review_iso_english$m_money ))


log_plus_c = function(x)
{
  y = setdiff(x, 0)
  miny = min(y)
  x_tranf = log10(x+miny)
  return(x_tranf)
}

review_iso_english$ml_length = log_plus_c(review_iso_english$m_length)
review_iso_english$ml_neg = log_plus_c(review_iso_english$m_neg)
review_iso_english$ml_pos = log_plus_c(review_iso_english$m_pos)
review_iso_english$ml_house = log_plus_c(review_iso_english$m_house)
review_iso_english$ml_social = log_plus_c(review_iso_english$m_social)
review_iso_english$ml_self = log_plus_c(review_iso_english$m_self)
review_iso_english$ml_altruistic = log_plus_c(review_iso_english$m_altruistic)
review_iso_english$ml_location = log_plus_c(review_iso_english$m_location)
review_iso_english$ml_money = log_plus_c(review_iso_english$m_money)

review_iso_english_long = review_iso_english[m_length>5]

# method 1

metrics_by_contry = review_iso_english_long[, .(n=length(id),
                                           m_length = mean(ml_length),
                                           m_neg=mean(ml_neg),
                                           m_pos=mean(ml_pos),
                                           m_house=mean(ml_house),
                                           m_social=mean(ml_social),
                                           m_self=mean(ml_self),
                                           m_altruistic=mean(ml_altruistic),
                                           m_location=mean(ml_location),
                                           m_money=mean(ml_money)), by=Code3]

setkey(countries, Code3)
setkey(metrics_by_contry, Code3)
metrics_by_contry = metrics_by_contry[countries, nomatch=0]

names(metrics_by_contry)

varx = c("m_length", "m_neg", "m_pos", "m_house", "m_social", "m_self", "m_altruistic", 
         "m_location", "m_money")
vary = c("power_distance", "individualism", "overall_pace_means", "GDP_per_capita_in_dollars")


metrics_by_contry$overall_pace_means = as.numeric(metrics_by_contry$overall_pace_means)

library(psych)
corr.test(metrics_by_contry[, varx, with=F], metrics_by_contry[, vary, with=F], method="pearson", adjust="fdr")


#              power_distance individualism overall_pace_means GDP_per_capita_in_dollars
# m_length              -0.04          0.45              -0.49                      0.12
# m_neg                  0.19          0.23              -0.55                     -0.19
# m_pos                 -0.43          0.07               0.28                      0.16
# m_house               -0.40          0.34              -0.77                      0.68
# m_social               0.09         -0.34               0.39                     -0.31
# m_self                -0.03          0.05              -0.80                      0.17
# m_altruistic           0.16         -0.34               0.46                     -0.41
# m_location            -0.13          0.51              -0.46                      0.29
# m_money                0.29          0.13              -0.39                     -0.04
# Sample Size 
#              power_distance individualism overall_pace_means GDP_per_capita_in_dollars
# m_length                 26            26                 13                        26
# m_neg                    26            26                 13                        26
# m_pos                    26            26                 13                        26
# m_house                  26            26                 13                        26
# m_social                 26            26                 13                        26
# m_self                   26            26                 13                        26
# m_altruistic             26            26                 13                        26
# m_location               26            26                 13                        26
# m_money                  26            26                 13                        26
# Probability values  adjusted for multiple tests. 
#              power_distance individualism overall_pace_means GDP_per_capita_in_dollars
# m_length               0.88          0.15               0.25                      0.67
# m_neg                  0.54          0.44               0.20                      0.54
# m_pos                  0.17          0.81               0.54                      0.59
# m_house                0.18          0.25               0.02                      0.01
# m_social               0.77          0.25               0.35                      0.27
# m_self                 0.90          0.87               0.02                      0.59
# m_altruistic           0.59          0.25               0.27                      0.18
# m_location             0.67          0.08               0.27                      0.30
# m_money                0.30          0.66               0.35                      0.88
# 


ggplot(metrics_by_contry, aes(x=GDP_per_capita_in_dollars, y=m_house, label=Code3)) + geom_label()
ggplot(metrics_by_contry, aes(x=individualism, y=m_neg, label=Code3)) + geom_smooth(method = "lm") +geom_label()
ggplot(metrics_by_contry, aes(x=individualism, y=m_location, label=Code3)) + geom_smooth(method = "lm") +geom_label()

ggplot(review_iso_english[Code3=='USA'], aes(x=m_house)) + geom_histogram() + xlim(0,0.5)
ggplot(review_iso_english[Code3=='IND'], aes(x=m_house)) + geom_histogram() + xlim(0,0.5)


# method 2

setkey(review_iso_english, Code3)
setkey(countries, Code3)
review_iso_english_m = review_iso_english[countries, nomatch=0]

varx = c("ml_length", "ml_neg", "ml_pos", "ml_house", "ml_social", "ml_self", "ml_altruistic", 
         "ml_location", "ml_money")
vary = c("power_distance", "individualism", "overall_pace_means", "GDP_per_capita_in_dollars")
review_iso_english_m$overall_pace_means =  as.numeric(review_iso_english_m$overall_pace_means)
corr.test(review_iso_english_m[, varx, with=F], review_iso_english_m[, vary, with=F], method="pearson", adjust="fdr")
# no corr at all!

ggplot(review_iso_english_m, aes(m_pos, m_neg)) + geom_smooth(method = "lm") + geom_point(size=.1, alpha=.1)
       