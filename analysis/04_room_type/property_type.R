library(lubridate)
library(ggplot2)
library(sqldf)

# 
# functions
# 

pre_processing = function(df)
{
  df$apt = ifelse(df$room_type=='Entire home/apt', 1, 0)
  df$room_p = ifelse(df$room_type=='Private room', 1, 0)
  df$room_s = ifelse(df$room_type=='Shared room', 1, 0)
  
  df$cum_apt = cumsum(df$apt)
  df$cum_room_p = cumsum(df$room_p)
  df$cum_room_s = cumsum(df$room_s)
  
  df$ratio_apt = df$cum_apt / (df$cum_apt+df$cum_room_p+df$cum_room_s)
  df$ratio_room_p = df$cum_room_p / (df$cum_apt+df$cum_room_p+df$cum_room_s)
  df$ratio_room_s = df$cum_room_s / (df$cum_apt+df$cum_room_p+df$cum_room_s)
  
  return(df)
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
  writeLines( paste(city, "...") )
  
  # loading data
  # 
  listings = read.csv( paste0(data_dir, city, "/listings.csv") )
  reviews = read.csv( paste0(data_dir, city, "/reviews.csv") )
  
  # joining data
  listings_j = 
    sqldf("select t1.*, min(t2.date) first_review
          from listings t1
          join reviews t2 on t1.id=t2.listing_id
          group by t1.id
          order by min(t2.date)")
  # 
  reviews_j = 
    sqldf("select t1.*, t2.date review_date
          from listings t1
          join reviews t2 on t1.id=t2.listing_id
          order by t2.date")
  
  # adding date
  # 
  listings_j$date = ymd(listings_j$first_review)
  reviews_j$date = ymd(reviews_j$review_date)
  
  # processing data
  # 
  listings_j = pre_processing(listings_j)
  reviews_j = pre_processing(reviews_j)
  # 
  listings_j$dataset = 'listings'
  reviews_j$dataset = 'reviews'
  # 
  listings_j = listings_j[c('date', 'ratio_apt', 'dataset')]
  reviews_j = reviews_j[c('date', 'ratio_apt', 'dataset')]
  # 
  all = rbind(listings_j, reviews_j)
  
  # plotting
  # 
  pdf( paste0("plots/", city, ".pdf") , width=6, height=5) 
  myplot = ggplot(all, aes(x=date, y=ratio_apt, col=dataset)) + 
    geom_line() +
    ggtitle(city ) +
    ylim(0,1)
  print(myplot)
  dev.off()
}
