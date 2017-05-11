library(lubridate)
library(ggplot2)
library(sqldf)

# loading data
# 
reviews = read.csv("reviews.csv")
names(reviews)
# [1] "listing_id" "date"  

# transforming data types
reviews$date = ymd(reviews$date)
reviews$listing_id = factor(reviews$listing_id)

# all listings having at least one review
by_listing = 
  sqldf("select listing_id, count(*) reviews
         from reviews
         group by listing_id")
summary(by_listing)
#   listing_id      reviews      
# 9356   :   1   Min.   :  1.00  
# 12625  :   1   1st Qu.:  6.00  
# 12899  :   1   Median : 19.00  
# 29931  :   1   Mean   : 36.94  
# 36836  :   1   3rd Qu.: 49.00  
# 37676  :   1   Max.   :438.00  
# (Other):2962      

# sampling data
by_listing_sample = by_listing[sample(nrow(by_listing), 300), ]
summary(by_listing_sample)
#  listing_id     reviews      
# 41601  :  1   Min.   :  1.00  
# 67532  :  1   1st Qu.:  6.00  
# 114086 :  1   Median : 19.00  
# 264248 :  1   Mean   : 35.95  
# 264961 :  1   3rd Qu.: 45.00  
# 335414 :  1   Max.   :234.00  
# (Other):294      

review_sample =
  sqldf("select t1.*
         from reviews t1
         join by_listing_sample t2 on t1.listing_id=t2.listing_id")

ggplot(review_sample, aes(x=listing_id, y=date)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# - Findings:
# 1. Listing_id are sequential numbers: the bigger the id, the later the corresponding listing has been created
# 2. We have only listings that are active now (last scraping). No track of listings that were active in the past, but are no longer active.
# 
# - Implications
# We can estimate when the listing has been created by the date of its reviews. 
# Also, the creation date has to be antecedent than the creation day of the listings having bigger id.
# We have to acknowledge some bias in the data, some old data is lost.