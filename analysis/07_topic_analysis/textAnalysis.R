source("textAnalysisFunc.R")


# 
# loading datast
# 

library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
# mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')
dbListTables(mydb)
guest = load_from_db(mydb, "guest")
listing = load_from_db(mydb, "listing")
review = load_from_db(mydb, "review")
detach(package:RMySQL)

review = proces_review(review)



# 
# reviews only for guests, from 2011 (too few reviews in 2010!) and in English
# 

review_g = review[author_id %in% guest$id]
# review_test = NULL
# review_test = rbind(review_test, review_g[1])
# review_test = rbind(review_test, review_g[2])
# review_test = rbind(review_test, review_g[3])
# review_test = rbind(review_test, review_g[4])


nrow(review)
# 157131
nrow(review_g)
# 2203
# 
# Why this huge discrepancy??

table(review_g$year)
# 2010 2011 2012 2013 2014 2015 2016 2017 
#    9   45  159  131  178  263  517  395 
review_g = review_g[year!=2010 & !is.na(year)]

review_g$inferredLanguage = textcat(review_g$comments)
ggplot(review_g, aes(inferredLanguage)) + geom_bar() + facet_grid(year ~ .)
review_g = review_g[inferredLanguage=='english']



# 
# Length of reviews
# 

summary(review_g$nwords)
ggplot(review_g, aes(x=factor(year), y=nwords)) + geom_boxplot() + geom_point(alpha=0.1, col=3) + ylim(0,250)




# 
# text analysis
# 

term_freq_long = quantText(review_g$comments, ngrams=1)
# plotText( review_g[review_g$year==2011]$comments, ngrams=1 )
# plotText( review_g[review_g$year==2012]$comments, ngrams=1 )
# plotText( review_g[review_g$year==2013]$comments, ngrams=1 )
# plotText( review_g[review_g$year==2014]$comments, ngrams=1 )
# plotText( review_g[review_g$year==2015]$comments, ngrams=1 )
# plotText( review_g[review_g$year==2016]$comments, ngrams=1 )
# plotText( review_g[review_g$year==2017]$comments, ngrams=1 )

term_freq_year = cbind(review_g[, c('year'), with=F], term_freq_long)
term_freq_year = term_freq_year[!is.na(year)]

term_freq_ungrouped = melt(term_freq_year, id.vars = c("year"))
colnames(term_freq_ungrouped) = c("year", "word", "value")
term_freq_ungrouped$value = ifelse(term_freq_ungrouped$value==0, 0, 1)
head(term_freq_ungrouped)

setkey(term_freq_ungrouped, year, word)
term_freq = term_freq_ungrouped[, sum(value), by=list(year, word)]
colnames(term_freq) = c("year", "word", "n")
summary(term_freq)

setkey(review_g, year)
review_g_year = review_g[, .N, by=year]
colnames(review_g_year) = c("year", "reviews")

setkey(term_freq, year)
setkey(review_g_year, year)
term_freq = term_freq[review_g_year, nomatch=0]
term_freq$tf = term_freq$n/term_freq$reviews
summary(term_freq)

setkey(term_freq, word)
term_freq_avgtf = term_freq[, mean(tf), by=word]
colnames(term_freq_avgtf) = c("word", "avg_tf")
summary(term_freq_avgtf)

setkey(term_freq, word)
setkey(term_freq_avgtf, word)
term_freq = term_freq[term_freq_avgtf, nomatch=0]
term_freq$tf_ratio = term_freq$tf/term_freq$avg_tf
summary(term_freq)

head(term_freq)

term_freq_highFreq = term_freq[avg_tf>0.03 & tf_ratio>1.5]
ggplot(term_freq_highFreq, aes(x=word, y=tf_ratio)) +
  geom_bar(stat="identity", aes(fill=avg_tf), col=1) +
  facet_wrap(~year, ncol=2, scales="free") + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  scale_fill_gradient(name = "popularity", trans = "log" )

term_freq_highFreq = term_freq[avg_tf>0.03 & tf_ratio<1/1.5]
ggplot(term_freq_highFreq, aes(x=word, y=tf_ratio)) +
  geom_bar(stat="identity", aes(fill=avg_tf), col=1) +
  facet_wrap(~year, ncol=2, scales="free") + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  scale_fill_gradient(name = "popularity", trans = "log" )


term_freq[word=='san_francisco']

term_freq[word=='happi']
#    year  word  n reviews         tf    avg_tf  tf_ratio
# 1: 2011 happi  0      38 0.00000000 0.0303572 0.0000000
# 2: 2012 happi  6     134 0.04477612 0.0303572 1.4749754
# 3: 2013 happi  2     107 0.01869159 0.0303572 0.6157218
# 4: 2014 happi  4     153 0.02614379 0.0303572 0.8612057
# 5: 2015 happi  5     209 0.02392344 0.0303572 0.7880650
# 6: 2016 happi 12     385 0.03116883 0.0303572 1.0267361
# 7: 2017 happi 20     295 0.06779661 0.0303572 2.2332960



ggplot(term_freq, aes(n)) + 
  geom_histogram() + 
  scale_x_sqrt( breaks=c(0,1,2,3,5,10,50,100) ) +
  scale_y_sqrt( breaks=c(0,1,2,3,5,10,50,100,500,1000,2000,5000) ) +
  facet_wrap(~year, ncol=2, scales="free") 

table(review_g$year)
# 2010 2011 2012 2013 2014 2015 2016 2017 
#    9   45  159  131  178  263  517  395 

library(ineq)
ineq( term_freq[year==2011]$n ,type="Gini")
ineq( term_freq[year==2012]$n ,type="Gini")
ineq( term_freq[year==2013]$n ,type="Gini")
ineq( term_freq[year==2014]$n ,type="Gini")
ineq( term_freq[year==2015]$n ,type="Gini")
ineq( term_freq[year==2016]$n ,type="Gini")
ineq( term_freq[year==2017]$n ,type="Gini")

length( unique(term_freq[year==2011 & n>0]$word) )
length( unique(term_freq[year==2012 & n>0]$word) )
length( unique(term_freq[year==2013 & n>0]$word) )
length( unique(term_freq[year==2014 & n>0]$word) )
length( unique(term_freq[year==2015 & n>0]$word) )
length( unique(term_freq[year==2016 & n>0]$word) )
length( unique(term_freq[year==2017 & n>0]$word) )
