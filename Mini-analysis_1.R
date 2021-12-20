# Mini-analysis 1 

# Set the working directory (Set your own)
setwd("~/MVE190_Data_Management/git_folder_mve190_project/MVE190_Project")

# Load relevant libraries
library(vioplot)
library(geosphere)

#read the data file with airbnb data
abnb<-read.csv("AirBnB_NYCity_2019.csv")

#abnb<- data.frame(abnb)

# Get overview of the Abnb data set
head(abnb)
colnames(abnb) # The categories

# Violin plot of Price Distribution for Accommodation Types
vioplot(
  abnb$price ~ abnb$room_type,
  xlab = "Accommodation Type",
  ylab = "Price [$/night]",
  main = "Price Distribution for Accommodation Types",
  col = "turquoise3")

# Violin plot of Price Distribution per Neighbourhoood
vioplot(
  abnb$price ~ abnb$neighbourhood_group,
  xlab = "Group of Neighbourhoods",
  ylab = "Price [$/night]",
  main = "Price Distribution per Neighbourhoood",
  col = "turquoise3")

#### Coordinates of some points of interest
# Expensive area TriBeCa
intr_coord1<-c(-74.007819, 40.718266)

# Financial district on Manhattan
intr_coord2<-c(-74.011, 40.708)

# Central Park on Manhattan
intr_coord3<-c(-73.9661, 40.7799) 

# Statue of liberty
intr_coord4<-c(-74.0445, 40.6892)

# JFK airport
intr_coord5<-c(-73.7781, 40.6413) 

# Newark airport
intr_coord6<-c(-74.1745, 40.6895) 

# The coordinate of the listings
list_coord<-cbind(abnb$longitude, abnb$latitude)

# Calculate distance in meters between points
di <- distHaversine(list_coord, intr_coord1) 

# Add distance to coordinate of interest to the abnb data 
abnb2<-cbind(abnb, di/1000)

# Switch name of column header for distance from di to dist
colnames(abnb2) <-c("id",
                  "name",
                  "host_id",
                  "host_name",
                  "neighbourhood_group",
                  "neighbourhood", 
                  "latitude",
                  "longitude", 
                  "room_type",
                  "price", 
                  "minimum_nights",
                  "number_of_reviews", 
                  "last_review",
                  "reviews_per_month",
                  "calculated_host_listings_count",
                  "availability_365",
                  "dist")

# Remove the listings with 0 days availability
abnb_0free <- subset(abnb2, availability_365 > 0)
abnb_0f <- subset(abnb_0free, price > 0)

attach(abnb_0f)


# Subdivide Abnb into accommodation types
room <- subset(abnb_0f, room_type == "Private room")
home <- subset(abnb_0f, room_type == "Entire home/apt")
shared_room <- subset(abnb_0f, room_type == "Shared room")

# Mean price for the diffrent accommodation types
mean_room <- mean(room$price)
mean_home <- mean(home$price)
mean_shared_room <- mean(shared_room$price)

sprintf("As we can see there is a large overlap in pricing between accomodation 
        types, so no immediate conclusion can be drawn just from that")
sprintf("The mean price ($$/night) for home/apt: %f, private room: %f and 
        shared room: %f", mean_home, mean_room, mean_shared_room)

# Sample the subsets
set.seed(123)
samp <- room[sample(nrow(room), 750),]
#samp <- shared_room[sample(nrow(shared_room), 1000), ]
#samp <- home[sample(nrow(home), 1000), ]
attach(room)

# Fit linear model, distance to price (untransformed)
linmod <- lm((price) ~ (dist))

# Summarize model fit
summary(linmod)

# Plot the scatter points and the fit
plot(dist, price, 
     xlab = "Distance [km]", 
     ylab = "Price [$/night]",
     main = "Price vs Distance to TriBeCa (Private Room)")
abline(linmod, col="red3",  lwd = 2)
text(20, 1100, expression("Linear fit"))
text(20, 1000, expression(y == 145.6404 -5.2947*x))

# Error plot
zero <- numeric(length(price))
res <- price - (145.6404 -5.2947 * dist)
plot(dist,
     res,
     xlab = "Distance [km]",
     ylab = "Residual price [$/night]",
     main = "Residual Price vs Distance to TriBeCA (Private Room)")
lines(c(-3, 30), c(0, 0), col = "blue", lwd = 2)

#### Try different transformations
# log y
linmod <- lm(I(log(price)) ~ dist)
summary(linmod)
plot((dist), log(price),
     xlab = "Distance [km]", 
     ylab = "log(Price)",
     main = "log(Price) vs Distance to TriBeCa (Private Room)")
abline(linmod, col = "red3",  lwd = 2)
text(26, 3, expression("Linear fit"))
text(26, 2.5, expression(log(price) == 4.6348 -0.0348*(dist)))

# weights or influence
inf <- lm.influence(linmod)

plot(inf$coefficients[,1],
     ylab = expression("Change in" ~ beta ~ 0),
     xlab = "Observation index",
     main = "Cange in intercept when removing one observation at a time")
abline(h =0, col = "red3",  lwd = 2)

plot(inf$coefficients[,2],
     ylab = expression("Change in" ~ beta ~ 1),
     xlab = "Observation index",
     main = "Cange in slope when removing one observation at a time")
abline(h =0, col = "red3",  lwd = 2)

# log x
linmod <- lm((price) ~ log(dist))
summary(linmod)
plot(log(dist), (price))
abline(linmod, col = "red3",  lwd = 2)

# log x and log price
linmod <- lm(I(log(price)) ~ I(log(dist)))
summary(linmod)
plot(log(dist), 
     log(price),
     xlab = "Distance [km]",
     ylab = "Residual price [$/night]",
     main = "log(Price) vs log(Distance) to TriBeCA (Private Room)")
abline(linmod, col = "red3",  lwd = 2)
text(-1, 6.3, expression("Linear fit"))
text(-1, 6, expression(y == 5.06042 -0.34701*x))

# Residual plot
zero <- numeric(length(price))
res <- I(log(price)) - (5.06042 -0.34701 * I(log(dist)))
plot(dist,
     res,
     xlab = "Distance [km]",
     ylab = "Residual log(price)",
     main = "Residual log(Price) vs log(Distance) to TriBeCA (Private Room)")
lines(c(-3, 30), c(0, 0), col = "blue", lwd = 2)

# 1/x
linmod <- lm((price) ~ I(1 / dist))
summary(linmod)
plot(1 / (dist), (price))
abline(linmod, col = "red3",  lwd = 2)

# 1/y
linmod <- lm(I(1 / price) ~ (dist))
summary(linmod)
plot((dist), 1 / (price))
abline(linmod, col = "red3",  lwd = 2)

# 1/x and 1/y (probably not so good...)***
linmod <- lm(I(1 / price) ~ I(1 / dist))
summary(linmod)
plot(1 / (dist), 1 / (price))
abline(linmod, col = "red3",  lwd = 2)

# sqrt y
linmod <- lm(I(sqrt(price)) ~ (dist))
summary(linmod)
plot((dist), sqrt(price))
abline(linmod, col = "red3",  lwd = 2)

# sqrt x
linmod <- lm((price) ~ I(sqrt(dist)))
summary(linmod)
plot(sqrt(dist), (price))
abline(linmod, col = "red3",  lwd = 2)

# sqrt x and sqrt price
linmod <- lm(I(sqrt(price)) ~ I(sqrt(dist)))
summary(linmod)
plot(sqrt(dist), sqrt(price))
abline(linmod, col = "red3",  lwd = 2)
#------------------- Some analysis tools I added at last minute ---------------#

grDevices::windows()
pairs(~price+latitude+longitude+dist+availability_365)

grDevices::windows()
pairs(~price+minimum_nights+reviews_per_month+number_of_reviews)

