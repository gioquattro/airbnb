library(RMySQL)
library(data.table)

mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')

dbListTables(mydb)

rs = dbSendQuery(mydb, "select * from guest")
guest = fetch(rs, n=-1)

rs = dbSendQuery(mydb, "select * from listing")
listing = fetch(rs, n=-1)

rs = dbSendQuery(mydb, "select * from review")
review = fetch(rs, n=-1)

Encoding(review$comments) <- "latin1"
Encoding(review$locale) <- "latin1"
Encoding(review$created_at) <- "latin1"

# cols = colnames(guest)
# guest = data.table(guest)
# for (i in cols)
# {
#   guest[,i, with=F] = ifelse(guest[,i, with=F]=='null', NA, guest[,i, with=F])
# }

# 

guest = data.table(guest)
head(guest$id)
typeof(guest$first_name)

levels(droplevels(guest$id))

x = corpus(guest$id)
typeof(guest$location)

x = data.frame(a=rnorm(1000), b=rnorm(1000))
summary(guest)
# 

library(quanteda)

mfdict <- dictionary(file = "LIWC2007_English080730.dic", format = "LIWC")
