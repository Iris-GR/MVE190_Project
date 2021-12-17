# Mini-analysis 2

# Set the working directory (Set your own)
setwd("~/MVE190_Data_Management/git_folder_mve190_project/MVE190_Project")

# Load relevant libraries
library(vioplot)
library(geosphere)
library(arm)
library(dotwhisker)
library(effectsize) 

#read the data file with airbnb data
abnb<-read.csv("AirBnB_NYCity_2019.csv")

#abnb<- data.frame(abnb)

# Get overview of the Abnb data set
head(abnb)
colnames(abnb) # The categories

# The coordinate of point of interest (Expensive area)
intr_coord1 <- c(-74.007819, 40.718266) 

# The coordinate of the listings
list_coord <- cbind(abnb$longitude, abnb$latitude)

# Calculate distance in meters between points
di <- distHaversine(list_coord, intr_coord1) 

# Add distance to coordinate of interest to the abnb data frame
abnb2 <- cbind(abnb, di/1000)

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

# Remove the listings with 0 price
abnb_no_0 <- subset(abnb2, abnb2$price > 0)

#abnb_no_00 <-subset(abnb_no_0, abnb_no_0$price<2000)
attach(abnb_no_0)

hood_group <- factor(neighbourhood_group)
hood <- factor(neighbourhood)
ro_type <- factor(room_type, labels = c("Entire home/apt",
                                        "Private room",
                                        "Shared room"))

# Plot the neighborhood group vs listing price
plot(hood_group, price,
     xlab = "Neighbourhood group",
     ylab = "Price [$/night]",
     main = "Price for each Neighbourhood group",
     cex = 1.2,
     cex.axis = 1.2,
     cex.lab = 1.3)

#### Test different models, calculate eta^2 and Cohens f
mod_0 <- lm((price) ~ dist + ro_type, x = TRUE)
summary(mod_0)
eta_squared(mod_0)
cohens_f(mod_0)

mod_1 <- lm(price ~ hood_group + dist + ro_type, x = TRUE)
summary(mod_1)

mod_2 <- lm(price ~ hood_group + ro_type, x = TRUE)
summary(mod_2)

mod_3 <- lm(I(log(price)) ~ ro_type + dist, x = TRUE)
summary(mod_3)
eta_squared(mod_3)
cohens_f(mod_3)

mod_4 <- lm(I(log(price)) ~ ro_type + I(log(dist)), x = TRUE)
summary(mod_4)
eta_squared(mod_4)
cohens_f(mod_4)

mod_4_1 <-
  lm(I(log(price)) ~ ro_type + (dist) + reviews_per_month, x = TRUE)
summary(mod_4_1)
eta_squared(mod_4_1)
cohens_f(mod_4_1)

mod_5 <-
  lm(I(log(price)) ~ ro_type + (dist) + number_of_reviews, x = TRUE)
summary(mod_5)
eta_squared(mod_5)
cohens_f(mod_5)

mod_6 <- lm(I(log(price)) ~ hood_group + ro_type + I(log(dist)), x = TRUE)
summary(mod_6)
eta_squared(mod_6)
cohens_f(mod_6)

mod_7 <- lm(I(log(price)) ~ ro_type + hood, x = TRUE)
summary(mod_7)
eta_squared(mod_7)
cohens_f(mod_7)

mod_8 <- lm(I(log(price)) ~ ro_type + I(log(dist)) + hood, x = TRUE)
summary(mod_8)
eta_squared(mod_8)
cohens_f(mod_8)

mod_9 <- lm(I(log(price)) ~ dist + ro_type + hood, x = TRUE)
summary(mod_9)
eta_squared(mod_9)
cohens_f(mod_9)

mod_10 <- lm(I(log(price)) ~ dist + ro_type, x = TRUE)
summary(mod_10)
eta_squared(mod_10)
cohens_f(mod_10)

mod_11 <-
  lm(I(log(price)) ~ dist + ro_type + hood + number_of_reviews, x = TRUE)
summary(mod_11)
eta_squared(mod_11)
cohens_f(mod_11)

# F-test for nested models (full, reduced)
anova(mod_10, mod_9)
anova(mod_7, mod_9)
anova(mod_9, mod_11)

# Check that we get the same estimates as from the summary
summary(mod_1)$cov.unscaled %*% t(mod_1$x) %*% price

# Compare the coefficients magnitude
coefplot(mod_1)
dwplot(mod_1)
