source("myFunctions.R")


# 
# loading datast
# 

library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='airbnb', host='127.0.0.1')
# mydb = dbConnect(MySQL(), user='root', password='1234567890', dbname='airbnb', host='127.0.0.1')
dbListTables(mydb)
guest = load_from_db(mydb, "guest")
guestSeen = load_from_db(mydb, "guestSeen")
host = load_from_db(mydb, "host")
isocode = load_from_db(mydb, "isocode")
listing = load_from_db(mydb, "listing")
locale = load_from_db(mydb, "locale")
review = load_from_db(mydb, "review")
review_language = load_from_db(mydb, "review_language")
word_count = load_from_db(mydb, "word_count")
world_cultural_factors = load_from_db(mydb, "world_cultural_factors")
detach(package:RMySQL)


# 
# join review and language
# 

setkey(review, idReview)
setkey(review_language, idReview)
review = review[review_language, nomatch=0]
remove(review_language)


# 
# join guest locale with isocode
# 

names(locale)
names(isocode)
setnames(isocode, "Name", "nation")

isocode = rbind(isocode, data.table(nation='USA', Code2='US', Code3='USA', CodeNum=840))
isocode = rbind(isocode, data.table(nation='USVI', Code2="VI", Code3='VIR', CodeNum=850))
isocode = isocode[nation!='Taiwan']
isocode = rbind(isocode, data.table(nation='Taiwan', Code2=NA, Code3='TWN', CodeNum=NA))

length(isocode$nation)
length(unique(isocode$nation))

setkey(locale, nation)
setkey(isocode, nation)
locale_iso = locale[isocode, nomatch=0]




# 
# join listing country with isocode
# 

listing$nation = gsub('"', '', listing$country)
setkey(listing, nation)
setkey(isocode, nation)
listing_iso = listing[isocode, nomatch=0]
setdiff(unique(listing$nation), unique(listing_iso$nation))



# 
# uniform guest - guestSeen
# 

names(guest)
names(guestSeen)
setnames(guestSeen, "firstName", "first_name")

guest = guest[, names(guestSeen), with=F]
guest_all = rbind(guest, guestSeen)

remove(guest)
remove(guestSeen)
guest = guest_all
remove(guest_all)



# 
# join review locale and iso
# 

review = review[!is.na(locale)]
review = review[locale!='']
review = review[locale!=' ']
review = review[locale!='  ']

names(review)
names(locale)
names(isocode)

cityNames = locale
remove(locale)
setnames(cityNames, "cityName", "locale")

names(review)
names(cityNames)
names(isocode)

review$locale = gsub(' ', '', review$locale)
cityNames$locale = gsub(' ', '', cityNames$locale)

library(sqldf)
review_iso = as.data.table(
  sqldf("select t1.*, t3.Code3
         from review t1
         join cityNames t2 on t1.locale=t2.locale
         join isocode t3 on t2.nation=t3.nation")
)


