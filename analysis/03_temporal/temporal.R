# Warning:
# This script needs the data in the format of yyyy-mm-dd. E.g., '2014-02-26'.
# Check if this is the case for the considered cities.

library(lubridate)
library(ggplot2)
library(sqldf)


# 
# functions
# 

pre_processing = function(df)
{
  df$dateString = df$date
  df$date = ymd(df$date)
  df$listing_id = factor(df$listing_id)
  
  df$seq = row.names(df)
  df$cumsum = cumsum(df$seq)
  df$ratio = df$cumsum/max(df$cumsum)
  
  return(df)
}

plotting = function(df, city, sub_title)
{
  pdf( paste0("plots/", city, "_", sub_title, ".pdf") , width=5, height=5) 
  myplot = ggplot(df, aes(x=date, y=ratio)) + 
    geom_line() +
    scale_y_log10() +
    geom_hline(yintercept=0.1, linetype="dotted") +
    geom_hline(yintercept=0.01, linetype="dotted") +
    geom_hline(yintercept=0.001, linetype="dotted") +
    ggtitle( paste(city, "-", sub_title) )
  print(myplot)
  dev.off()
}

computing_time = function(df)
{
  timeMin = as.character( sqldf("select min(dateString) from df") )
  time01 = as.character( sqldf("select max(dateString) from df where ratio<0.1") )
  time001 = as.character( sqldf("select max(dateString) from df where ratio<0.01") )
  time0001 = as.character( sqldf("select max(dateString) from df where ratio<0.001") )
  timeMax = as.character( sqldf("select max(dateString) from df") )
  
  return(list(timeMin=timeMin, 
              time01=time01, 
              time001=time001, 
              time0001=time0001, 
              timeMax=timeMax))
}



# 
# main
# 

data_dir = "../../data/insideairbnb/"
allcities = list.files( data_dir)
cities = NULL
for( city in allcities)
{
  # city = 'London'
  writeLines( paste("\n\nAnalysing", city) )
  
  # loading data
  # 
  reviews = read.csv( paste0(data_dir, city, "/reviews.csv") )
  
  # structure of the data
  # 
  writeLines("... structure of the data")
  print( names(reviews) )
  writeLines("... structure of time")
  print( head(reviews$date) )
  
  # grouping data
  # 
  listings = sqldf("select listing_id, min(date) date, count(*) n_reviews from reviews group by listing_id")

  # ordering data
  # 
  reviews = sqldf("select * from reviews order by date")
  listings = sqldf("select * from listings order by date")
  
  # transforming data types
  # 
  reviews = pre_processing(reviews)
  listings = pre_processing(listings)

  # plotting
  # 
  plotting(reviews, city, "reviews")
  plotting(listings, city, "listings")
  
  # computing time
  # 
  time_reviews = computing_time(reviews)
  time_listings = computing_time(listings)
  
  cities = rbind(cities, data.frame(city=city, 
                                    timeMin=time_reviews$timeMin, 
                                    timeR01=time_reviews$time01, 
                                    timeR001=time_reviews$time001, 
                                    timeR0001=time_reviews$time0001, 
                                    timeL01=time_listings$time01, 
                                    timeL001=time_listings$time001, 
                                    timeL0001=time_listings$time0001, 
                                    timeMax=time_reviews$timeMax, 
                                    n_reviews=nrow(reviews),
                                    n_listings=nrow(listings)))
}

write.table(cities, file = "output/cities_temporal.csv", sep = ",", row.names = F)
