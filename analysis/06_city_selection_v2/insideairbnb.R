library(ggplot2)
library(sqldf)
library(lubridate)

df1 = read.csv("insideairbnb.csv")
df2 = read.csv("cities_temporal.csv")
df3 = read.csv("cities_spread.csv")

names(df1)
names(df2)
names(df3)

df = sqldf("select df1.*, 
                   df2.timeMin, df2.timeR01, df2.timeL01, df2.timeMax,
                   df3.neighbourhoods, df3.diversity2015
            from df1
            join df2 on df1.city=df2.city
            join df3 on df1.city=df3.city
            where df3.neighbourhoods>20")

df$num_listings = df$apartments + df$private_rooms + df$shared_rooms
df$bnb_penetration = df$num_listings / df$poulation
df$ratio_apartments = df$apartments / df$num_listings
df$ratio_private_rooms = df$private_rooms / df$num_listings
df$ratio_shared_rooms = df$shared_rooms / df$num_listings

names(df)
# [1] "city"                "country"             "price"               "apartments"         
# [5] "private_rooms"       "shared_rooms"        "poulation"           "timeMin"            
# [9] "timeR01"             "timeL01"             "timeMax"             "neighbourhoods"     
# [13] "diversity2015"       "num_listings"        "bnb_penetration"     "ratio_apartments"   
# [17] "ratio_private_rooms" "ratio_shared_rooms" 

df$timeMin = ymd(df$timeMin)
df$timeL01 = ymd(df$timeL01)
df$timeR01 = ymd(df$timeR01)
df$timeMax = ymd(df$timeMax)


table(df$country)
#   Australia         Austria         Belgium          Canada 
#           2               1               1               5 
#       China         Denmark          France         Germany 
#           0               0               0               1 
#      Greece         Ireland           Italy           Spain 
#           1               0               1               3 
# Switzerland The Netherlands  United Kingdom   United States 
#           1               1               3              12 


ggplot(df, aes(x=num_listings, y=ratio_apartments, label=city)) + 
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  scale_x_log10(breaks=c(1000,2000,5000,10000,20000,50000)) +
  theme(legend.position="none")

ggplot(df, aes(x=bnb_penetration, y=ratio_apartments, label=city)) + 
  geom_smooth() +
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  theme(legend.position="none")

ggplot(df, aes(x=timeL01, y=timeR01, label=city)) + 
  geom_smooth() +
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  theme(legend.position="none")

ggplot(df, aes(timeR01)) + 
  geom_histogram(col=1, fill=3, binwidth=100)

ggplot(df, aes(x=timeMin, y=timeR01, label=city)) + 
  geom_smooth() +
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  theme(legend.position="none")

ggplot(df, aes(x=timeMax, y=timeR01, label=city)) + 
  geom_smooth() +
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  theme(legend.position="none")

ggplot(df, aes(x=neighbourhoods, y=diversity2015, label=city)) + 
  geom_smooth() +
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  scale_x_log10() +
  theme(legend.position="none")
