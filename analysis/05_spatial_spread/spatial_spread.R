library(rgdal) #spatial library
library(ggplot2) #plotting
library(maptools) #spatial library
library(sqldf) #to query data using classic sql
library(lubridate) #to manage date
library(raster) #to campute the area of a polygon
library(vegan) #to compute diversity indexes

# 
# functions
# 

# joining and grouping all data
# 
join_and_group = function(map_df, reviews_join)
{
  by_neighbourhood =
    sqldf(paste("select t1.neighbourhood id, count(t2.listing_id) n_reviwes, t1.area_sqkm
                from map_df t1
                left join reviews_join t2 on t1.neighbourhood=t2.neighbourhood 
                group by t1.neighbourhood"))
  by_neighbourhood$reviwes_density = by_neighbourhood$n_reviwes/by_neighbourhood$area_sqkm
  by_neighbourhood$reviwes_density_norm = by_neighbourhood$reviwes_density^0.25
  summary(by_neighbourhood$reviwes_density)
  summary(by_neighbourhood$reviwes_density_norm)
  
  return(by_neighbourhood)
}

join_and_group_by_year = function(map_df, reviews_join, year)
{
  by_neighbourhood =
    sqldf(paste("select t1.neighbourhood id, count(t2.listing_id) n_reviwes, t1.area_sqkm
                from map_df t1
                left join reviews_join t2 on t1.neighbourhood=t2.neighbourhood and t2.year=", year, "
                group by t1.neighbourhood"))
  by_neighbourhood$reviwes_density = by_neighbourhood$n_reviwes/by_neighbourhood$area_sqkm
  by_neighbourhood$reviwes_density_norm = by_neighbourhood$reviwes_density^0.25
  summary(by_neighbourhood$reviwes_density)
  summary(by_neighbourhood$reviwes_density_norm)
  
  return(by_neighbourhood)
}



# 
# main
# 

data_dir = "../../data/insideairbnb/"
allcities = list.files( data_dir)
cities = NULL
for( city in allcities)
  # for(i in 1:1)
  tryCatch({
    {
      # city = 'London'
      writeLines( paste("\n\n***", city, "***") )
      
      # loading tables
      # 
      listings = read.csv( paste0(data_dir, city, "/listings.csv") )
      reviews = read.csv( paste0(data_dir, city, "/reviews.csv") )
      
      # structure of the data
      # 
      names(listings)
      names(reviews)
      
      # joining listing - reviews
      # 
      reviews_join =
        sqldf("select t1.*, t2.neighbourhood, t2.latitude, t2.longitude
              from reviews t1
              join listings t2 on t1.listing_id=t2.id")
      reviews_join$year = as.numeric( substr(reviews_join$date, 1, 4) )
      table( reviews_join$year )
      writeLines( paste("First review: year", min(reviews_join$year)) )
      writeLines( paste("Last review: year", max(reviews_join$year)) )
      
      # loading shape file
      # 
      map = readOGR(paste0(data_dir, city, "/neighbourhoods.geojson"), "OGRGeoJSON")
      
      # structure of the map
      # 
      names(map)
      
      # check that coordindate system is in longlat
      # 
      print( crs(map) )
      
      # computing the area of each polygon
      # 
      map$area_sqkm <- area(map) / 1000000
      
      # extracting data frame from map having information of neighbourhood name and area
      # 
      map_df = map@data
      
      #fortify shape file to get into dataframe 
      # 
      map.f <- fortify(map, region = "neighbourhood")
      class(map.f)
      head(map.f)
      
      # row to output for the current city
      # (higher diversity: higher number of different neighbourhoods involved)
      # 
      cities_newrow = data.frame(city=city, neighbourhoods=nrow(map_df), diversity2008=NA, diversity2009=NA, diversity2010=NA, diversity2011=NA, 
                                 diversity2012=NA, diversity2013=NA, diversity2014=NA, diversity2015=NA, diversity2016=NA)
      for (year in min(reviews_join$year):max(reviews_join$year))
      {
        # reviews year year
        reviews_by_year = reviews_join[reviews_join$year==year,]

        # joining and grouping all data
        # 
        neigh_by_year = join_and_group_by_year(map_df, reviews_join, year)
        div_by_year = diversity(neigh_by_year$reviwes_density, "inv") / nrow(neigh_by_year)
        writeLines( paste0("diversity ", year, ": ", round(div_by_year,3)) )
        
        # update value of the row to output
        # 
        if (year==2008) cities_newrow$diversity2008 = div_by_year
        if (year==2009) cities_newrow$diversity2009 = div_by_year
        if (year==2010) cities_newrow$diversity2010 = div_by_year
        if (year==2011) cities_newrow$diversity2011 = div_by_year
        if (year==2012) cities_newrow$diversity2012 = div_by_year
        if (year==2013) cities_newrow$diversity2013 = div_by_year
        if (year==2014) cities_newrow$diversity2014 = div_by_year
        if (year==2015) cities_newrow$diversity2015 = div_by_year
        if (year==2016) cities_newrow$diversity2016 = div_by_year
        
        # merge the two dataframes together by the id variable
        # then order the data by the “order” variable
        # 
        merge.shp.coef = merge(map.f, neigh_by_year, by="id", all.x=TRUE)
        final.plot = merge.shp.coef[order(merge.shp.coef$order), ] 
        
        # plot
        # 
        if (!file.exists( paste0("plots/", city) )) 
          dir.create( paste0("plots/", city) )
        # 
        pdf( paste0("plots/", city, "/", city, year, ".pdf") , width=14, height=10) 
        myplot = 
          ggplot() +
          geom_polygon(data = final.plot, 
                       aes(x = long, y = lat, group = group, fill = reviwes_density_norm), 
                       color = "white", size = 0.2) + 
          geom_point(data = reviews_by_year, aes(x=longitude, y=latitude), size=0.001, alpha=0.01) +
          scale_fill_distiller(palette = "YlOrRd")+
          coord_map() +
          ggtitle( paste(city, year, sep=" - ") ) +
          theme(legend.position="none") 
        print(myplot)
        dev.off()
      }
      
      # updating the file to output
      # 
      cities = rbind(cities, cities_newrow)
    }
  }, error=function(e){})

# saving the spatial diversity of reviews for each city (year by year)
# 
write.table(cities, file = "output/cities_spread.csv", sep = ",", row.names = F)