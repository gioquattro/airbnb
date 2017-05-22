source("myFunctions.R")


# 
# loading datast
# 

library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
dbListTables(mydb)

host = read.csv("Dump 180517/hostInfo2.txt", sep = "/")
dbWriteTable(mydb, value=host, name="host", append=F) 
detach(package:RMySQL)

