library(RMySQL)
library(data.table)

# 
# loading datast
# 

mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
# mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')

dbListTables(mydb)

rs = dbSendQuery(mydb, "select * from guest")
guest = fetch(rs, n=-1)

rs = dbSendQuery(mydb, "select * from listing")
listing = fetch(rs, n=-1)

rs = dbSendQuery(mydb, "select * from review")
review = fetch(rs, n=-1)

Encoding(review$comments) <- "latin1"
Encoding(review$created_at) <- "latin1"

guest = data.table(guest)
listing = data.table(listing)
review = data.table(review)



# 
# reviews only for guests
# 

review_g = review[author_id %in% guest$id]
review_g$year = as.numeric( substr(review_g$created_at, (nchar(review_g$created_at)-5), (nchar(review_g$created_at)-1) ) )
review_g$year = factor(review_g$year)

table(review_g$year)
# 2010 2011 2012 2013 2014 2015 2016 2017 
#    4   16   72   65   83  131  275  209 

review_g$inferredLanguage = textcat(review_g$comments)
review_g$nwords = sapply(gregexpr("\\W+", review_g$comments), length) + 1
review_g$nchars = nchar(review_g$comments)

ggplot(review_g, aes(inferredLanguage)) + geom_bar() + facet_grid(year ~ .)

review_g = review_g[!is.na(year) & inferredLanguage=='english']

summary(review_g$nwords)
ggplot(review_g, aes(x=factor(year), y=nwords)) + geom_boxplot() + geom_point(alpha=0.1, col=3) + ylim(0,250)




# 
# text analysis
# 

library("quanteda")
doc_term_matrix <- dfm(review_g$comments, stem = FALSE)
# doc_term_matrix_norm = doc_term_matrix / review_g$nwords

doc_term_df = data.table( data.frame(doc_term_matrix) ) 

doc_term_df_join = cbind(review_g[, c('year'), with=F], doc_term_df)
doc_term_df_join$year = factor(doc_term_df_join$year)
table(doc_term_df_join$year)
typeof(doc_term_df_join$year)


# 
# analysis by year
# 

attach(doc_term_df_join)
aggdata <-aggregate( doc_term_df_join, by=list(year), FUN=mean, na.rm=TRUE)
detach(doc_term_df_join)
aggdata$Group.1 = NULL




# 
# visualise
# 

library(reshape)
meltdata <- melt(aggdata, id=c("year"))
names(meltdata)

library(ggplot2)
ggplot(meltdata, aes(x=variable, y=value)) + geom_bar(stat="identity", col=1, fill=3) +
  facet_grid(year ~ .) + theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
