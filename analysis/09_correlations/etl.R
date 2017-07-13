source("myFunctions.R")

set_to_null = function(mydt)
{
  mydt[mydt==''] = NA
  mydt[mydt==' '] = NA
  mydt[mydt=='-'] = NA
  mydt[mydt=='NA-NA'] = NA
  mydt[mydt=='null'] = NA
  return(mydt)
}

convert_date = function(mydate)
{
  mydt = data.table(created_at = mydate)
  
  library(stringr)
  table( gsub(" ", "", str_sub(mydt$created_at, 1, -6)) )
  table( gsub(" ", "", str_sub(mydt$created_at, -5)) )
  mydt$created_at_month = gsub(" ", "", str_sub(mydt$created_at, 1, -6))
  mydt[created_at_month=="Gennaio"]$created_at_month = '01'
  mydt[created_at_month=="Febbraio"]$created_at_month = '02'
  mydt[created_at_month=="Marzo"]$created_at_month = '03'
  mydt[created_at_month=="Aprile"]$created_at_month = '04'
  mydt[created_at_month=="Maggio"]$created_at_month = '05'
  mydt[created_at_month=="Giugno"]$created_at_month = '06'
  mydt[created_at_month=="Luglio"]$created_at_month = '07'
  mydt[created_at_month=="Agosto"]$created_at_month = '08'
  mydt[created_at_month=="Settembre"]$created_at_month = '09'
  mydt[created_at_month=="Ottobre"]$created_at_month = '10'
  mydt[created_at_month=="Novembre"]$created_at_month = '11'
  mydt[created_at_month=="Dicembre"]$created_at_month = '12'
  mydt$created_at_year = gsub(" ", "", str_sub(mydt$created_at, -5))
  detach(package:stringr)
  
  mydt$created_at = paste(mydt$created_at_year, mydt$created_at_month, sep="-")
  mydt = set_to_null(mydt)

  return(mydt$created_at)
}


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
# uniform guest - guestSeen
# 

names(guest)
names(guestSeen)
setnames(guestSeen, "firstName", "first_name")

guest$is_seed = T
guestSeen$is_seed = F 
guest = guest[, -c("idGuest"), with=F]

guest$first_name = gsub('"', '', guest$first_name)
guest$location = gsub('"', '', guest$location)
guest$about = gsub('"', '', guest$about)
guest = set_to_null(guest)

guestSeen$has_profile_pic = NA
guestSeen$identity_verified = NA
guestSeen$school = NA
guestSeen$work = NA
guestSeen$friends_count = NA
guestSeen$neighborhood = NA
guestSeen$show_travel_for_work = NA
guestSeen$verification_labels = NA
guestSeen$recommendation_count = NA
guestSeen$response_rate = NA
guestSeen$response_time = NA

guestSeen = guestSeen[, names(guest), with=F]
guest = rbind(guest, guestSeen)

remove(guestSeen)




# 
# uniform review
# join review locale and iso
# 

review = review[, -c("idReview"), with=F]

table(review$created_at)
review$created_at = gsub("\xa0", "", review$created_at)
table(review$created_at)
review$created_at = convert_date(review$created_at)
table(review$created_at)

review = set_to_null(review)


review = review[, -c("created_at_month", "created_at_year"), with=F]

names(review)
names(locale)
names(isocode)

cityNames = locale
remove(locale)
setnames(cityNames, "cityName", "locale")

names(review)
names(cityNames)
names(isocode)

review$locale[1:10]
cityNames$locale[1:10]
library(stringi)
review$locale = stri_sub(review$locale, 2)

library(sqldf)
review_iso = as.data.table(
  sqldf("select t1.*, t3.Code3
         from review t1
         left join cityNames t2 on t1.locale=t2.locale
         left join isocode t3 on t2.nation=t3.nation")
)




# 
# fixing host
# 

host$createdAt = convert_date(host$createdAt)
host$createdAt = gsub("\r", "", host$createdAt)



# 
# fixig listing fields
# join listing country with isocode
# 

listing$nation = gsub('"', '', listing$country)
setkey(listing, nation)
setkey(isocode, nation)
listing_iso = listing[isocode, nomatch=0]
setdiff(unique(listing$nation), unique(listing_iso$nation))
