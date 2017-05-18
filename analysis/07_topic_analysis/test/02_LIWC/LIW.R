library(RMySQL)
library(data.table)

# 
# loading datast
# 

mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')

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

table(review_g$year)
# 2010 2011 2012 2013 2014 2015 2016 2017 
#    4   16   72   65   83  131  275  209 

review_g$textid = paste0( "text", row.names(review_g) )



# 
# text analysis
# 

library("quanteda")
mfdict <- dictionary(file = "LIWC2007_English080730.dic", format = "LIWC")
doc_term_matrix <- dfm(review_g$comments, stem = FALSE, dictionary = mfdict)
doc_term_df = data.frame(doc_term_matrix) 
doc_term_df$textid = row.names(doc_term_df)



# 
# analysis by year
# 

names(doc_term_df)
names(review_g)

detach("package:RMySQL", unload=TRUE)
library(sqldf)

doc_term_df2 =
  sqldf("select t1.*, t2.year
         from doc_term_df t1
         join review_g t2 on t1.textid=t2.textid")

doc_term_df2$textid = NULL

attach(doc_term_df2)
aggdata <-aggregate(doc_term_df2, by=list(year), FUN=mean, na.rm=TRUE)
detach(doc_term_df2)
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
