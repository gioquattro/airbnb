source("myFunctions.R")
library("quanteda")


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

review = pre_proces_review(review)


# 
# reviews only for guests which have a year
# 

review_g = review[!(author_id %in% listing$id_host) & !is.na(year)]

table(review_g$year)
# 2008  2009  2010  2011  2012  2013  2014  2015  2016  2017 
#    1    20   235  1106  3657  5997  9915 17255 39189 58071 

summary(review_g$nwords)
ggplot(review_g, aes(x=factor(year), y=nwords)) + 
  geom_violin(fill=4) +  
  geom_boxplot(fill=7, width=0.1, outlier.size=NA) +  
  # geom_point(alpha=0.1, col=3) + 
  scale_y_sqrt(limits=c(0,200), breaks=c(0,1,2,3,5,10,20,30,50,100,150,200))

ggplot(review_g, aes(x=factor(year), y=nwords)) + 
  geom_violin(fill=4) +  
  geom_boxplot(fill=7, width=0.1, outlier.size=NA) +  
  # geom_point(alpha=0.1, col=3) + 
  scale_y_continuous(limits=c(0,200), breaks=c(0,1,2,3,5,10,20,30,50,100,150,200))



t0 = Sys.time()
doc_term_full = NULL
factor = 1000
max_i = round(nrow(review_g)/factor,0)
iterations = sample(1:max_i, 100)
xi = 0
for ( i in iterations)
{  
  xi = xi+1
  ti = Sys.time()

  # 
  # sample
  # 

  review_sample = review_g[ as.numeric(row.names(review_g))>=factor*(i-1) & as.numeric(row.names(review_g))<factor*(i) ]
  review_sample = post_proces_review(review_sample)
  
  
  # 
  # text analysis
  # 
  
  mfdict <- dictionary(file = "LIWC2007_English080730.dic", format = "LIWC")
  doc_term_matrix <- dfm(review_sample$comments, stem = FALSE, dictionary = mfdict)
  doc_term_matrixN = doc_term_matrix/review_sample$nwords
  doc_term = data.table( data.frame(doc_term_matrixN) )
  
  doc_term_year = cbind( review_sample[,'year',with=F], doc_term )
  doc_term_full = rbind(doc_term_full, doc_term_year)
  
  print(Sys.time() - ti)
  writeLines( paste("Step", xi, "(", round(100*xi/length(iterations),1), "%)"))
}
print(Sys.time() - t0)
detach(package:quanteda)

doc_term_melt = melt(doc_term_full, id.vars = c("year"))
write.table(doc_term_melt,
            sep = ",", eol = "\n", na = "", dec = ".", row.names = F, col.names = T,
            gzfile("doc_term_melt.csv.gz"))


# 
# load doc_term_melt.csv.gz if we want to re-run the script
# 

doc_term_melt2010 = doc_term_melt[year>=2010]

setkey(doc_term_melt2010, year, variable)
doc_term_gr = doc_term_melt2010[, mean(value), by=list(year, variable)]
colnames(doc_term_gr) = c("year", "variable", "value")
setkey(doc_term_gr, variable)
doc_term_gr[, abs_mean:=mean(value), by=variable]
doc_term_gr$gain = doc_term_gr$value/doc_term_gr$abs_mean

doc_term_gr$gain_break = cut(doc_term_gr$gain, breaks=c(-1,0.2,0.5,2,5,1000))




# 
# visualise
# 

ggplot(doc_term_gr, aes(x=variable, y=gain, fill=gain_break)) + 
  geom_bar(stat="identity", col=1) +
  facet_grid(year ~ .) +
  scale_y_sqrt(breaks=c(0,0.2,0.5,1,2,5)) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

write.table(doc_term_melt, file = "doc_term_melt.csv", sep = ",", na = "", 
            row.names = F, col.names = T)


