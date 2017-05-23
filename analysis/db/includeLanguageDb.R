source("myFunctions.R")


# 
# loading datast
# 
t0 = Sys.time()

library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
# mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')
dbListTables(mydb)
review = load_from_db(mydb, "review")

review$inferredLanguage = textcat(review$comments)
review = review[, c("idReview", "inferredLanguage"), with=F]

dbWriteTable(mydb, value=review, name="review_language", append=F, row.names=F, col.names=T) 
detach(package:RMySQL)

print(Sys.time() - t0)

