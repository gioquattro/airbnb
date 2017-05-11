library(ggplot2)

df = read.csv("insideairbnb.csv")
df$num_listings = df$apartments + df$private_rooms + df$shared_rooms
df$bnb_penetration = df$num_listings / df$poulation
df$ratio_apartments = df$apartments / df$num_listings
df$ratio_private_rooms = df$private_rooms / df$num_listings
df$ratio_shared_rooms = df$shared_rooms / df$num_listings

names(df)
#  [1] "city"                "country"             "price"              
#  [4] "apartments"          "private_rooms"       "shared_rooms"       
#  [7] "poulation"           "num_listings"        "bnb_penetration"    
# [10] "ratio_apartments"    "ratio_private_rooms" "ratio_shared_rooms" 

table(df$country)
#   Australia        Austria        Belgium         Canada 
#           3              1              2              5 
# 
#       China        Denmark         France        Germany 
#           1              1              1              1 
# 
#      Greece        Ireland          Italy          Spain 
#           1              1              2              3 
# 
# Switzerland TheNetherlands  UnitedKingdom   UnitedStates 
#           1              1              3             16 

ggplot(df, aes(x=paste0(city,", ",country), y=num_listings)) + geom_bar(stat='identity', aes(fill=country)) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(df, aes(x=paste0(city,", ",country), y=bnb_penetration)) + geom_bar(stat='identity', aes(fill=country)) +
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(df, aes(x=num_listings, y=ratio_apartments, label=city)) + 
  geom_smooth() +
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  scale_x_log10(breaks=c(1000,2000,5000,10000,20000,50000)) +
  theme(legend.position="none")

ggplot(df, aes(x=bnb_penetration, y=ratio_apartments, label=city)) + 
  geom_smooth() +
  geom_label(size=2, alpha=0.5, aes(fill=country)) +
  theme(legend.position="none")

