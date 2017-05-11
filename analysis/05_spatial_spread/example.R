# plotting maps
# file:///Users/giovanni/Downloads/Maps.html

library(rgdal)
library(ggplot2)
# library(rgeos)
library(maptools)

"GeoJSON" %in% ogrDrivers()$name

map = readOGR("../../data/insideairbnb/London/neighbourhoods.geojson", "OGRGeoJSON")
plot(map)

class(map)
# [1] "SpatialPolygonsDataFrame"
# attr(,"package")
# [1] "sp"

names(map)
# [1] "neighbourhood"       "neighbourhood_group"

print(map$neighbourhood)
# [1] Kingston upon Thames   Croydon                Bromley               
# [4] Hounslow               Ealing                 Havering              
# [7] Hillingdon             Harrow                 Brent                 
# [10] Barnet                 Enfield                Waltham Forest        
# [13] Redbridge              Sutton                 Lambeth               
# [16] Southwark              Lewisham               Greenwich             
# [19] Bexley                 Richmond upon Thames   Merton                
# [22] Wandsworth             Hammersmith and Fulham Kensington and Chelsea
# [25] City of London         Westminster            Camden                
# [28] Tower Hamlets          Islington              Hackney               
# [31] Haringey               Newham                 Barking and Dagenham  
# 33 Levels: Barking and Dagenham Barnet Bexley Brent Bromley ... Westminster

# they are the id

print(map$neighbourhood_group)
# [1] <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#   [14] <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#   [27] <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#   Levels: 

num.neighbourhood = length(map$neighbourhood)

mydata = data.frame(id=map$neighbourhood, prevalence=rnorm(num.neighbourhood, 55, 20))
head(mydata)
#          neighbourhood                   id prevalence
# 1 Kingston upon Thames Kingston upon Thames   49.13873
# 2              Croydon              Croydon   56.24465
# 3              Bromley              Bromley   70.97622
# 4             Hounslow             Hounslow   22.95994
# 5               Ealing               Ealing   42.57383
# 6             Havering             Havering   40.92105

# they should be the same as map$neighbourhood
# they should be unique!
print(mydata$neighbourhood)

#fortify shape file to get into dataframe 
map.f <- fortify(map, region = "neighbourhood")
class(map.f)

head(map.f)
#       long      lat order  hole piece                   id                  group
# 1 0.069715 51.54406     1 FALSE     1 Barking and Dagenham Barking and Dagenham.1
# 2 0.070351 51.54392     2 FALSE     1 Barking and Dagenham Barking and Dagenham.1
# 3 0.070854 51.54380     3 FALSE     1 Barking and Dagenham Barking and Dagenham.1
# 4 0.071031 51.54378     4 FALSE     1 Barking and Dagenham Barking and Dagenham.1
# 5 0.071114 51.54377     5 FALSE     1 Barking and Dagenham Barking and Dagenham.1
# 6 0.071635 51.54361     6 FALSE     1 Barking and Dagenham Barking and Dagenham.1


# We can see that the class is now a common dataframe and each row is a longitude and latitude.
# 
# Now we can merge the two dataframes together by the id variable, 
# making sure to keep all the observations in the spatial dataframe. 
# Importantly, we need to order the data by the “order” variable. 
# You can see what happens if you don’t do this and it’s not pretty.


#merge with coefficients and reorder
merge.shp.coef = merge(map.f, mydata, by="id", all.x=TRUE)
final.plot = merge.shp.coef[order(merge.shp.coef$order), ] 

#basic plot
ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = prevalence), 
               color = "white", size = 0.1) + 
  coord_map()


# point-in-polygon operations
# https://www.nceas.ucsb.edu/scicomp/usecases/point-in-polygon

listings = read.csv("listings.csv")
coordinates(listings) <- c("longitude", "latitude")

# tell R that bear coordinates are in the same lat/lon reference system
# as the parks data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
proj4string(listings) <- proj4string(map)

# combine is.na() with over() to do the containment test; note that we
# need to "demote" parks to a SpatialPolygons object first
inside.map <- !is.na(over(listings, as(map, "SpatialPolygons")))

# what fraction of listings were inside the map?
mean(inside.map)
# [1] 0.9996352

# use 'over' again, this time with map as a SpatialPolygonsDataFrame
# object, to determine which neighbourhood (if any) contains each listing, and
# store the neighbourhood name as an attribute of the listing data
listings$neighbourhood = over(listings, map)$Unit_Name

listings2 = data.frame(listings)

# draw a map big enough to encompass all points (but don't actually plot
# the points yet), then add in park boundaries superimposed upon a map
# of the United States
plot(coordinates(listings), type="n")

plot(map)
points(listings@coords, col = "red", cex = 1)

plot(map, border="green", add=TRUE)

points(listings[!inside.map, ], pch=1, col="gray")
points(listings[inside.map, ], pch=1, col="gray")


# spatial join
# http://gis.stackexchange.com/questions/137621/join-spatial-point-data-to-polygons-in-r

a.data <- over(listings, map[,"neighbourhood"])
listings$neighbourhood_code <- a.data$neighbourhood

head(listings@data)
