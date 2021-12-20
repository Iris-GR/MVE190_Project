# The cars data set
setwd("C:/R workspace/MVE190/MVE190_Project_new/MVE190_Project")
#------------------------ Libraries -------------------------------------------#
# Here all libraries which will be used are specified.
install.packages("corrplot")
library(corrplot)
library(vioplot)
library(geosphere)
library(dplyr)
library(ISLR)
library(leaps)
library(olsrr)
library(ggplot2)
library(rgl)  
library(scatterplot3d)  
library(arm)
library(dotwhisker)
library(beepr)
library(car)

#------------------------ Loading data ----------------------------------------#
# In this part of the project it is the dataset CarPrice which will be analysed.

cars <- read.csv("CarPrice.csv", header = T )
cars <- data.frame(cars)

attach(cars)
#Notable: there are in total 205 cars in the dataset. 
# Minimum price is 5118 dollar.
# Maximum price is 45400 dollar. 

#----------------------- Sorting and removing variables -----------------------#
# Only three cars have enginelocation == rear. 
# Decided to remove these as they did not feel representative.
cars <- subset(cars, enginelocation == "front")

# Only small amount of cars with aspiration == turbo,
# Decided to focus on cars with standard aspiration:
cars <- subset(cars, aspiration == "std")

grDevices::windows()
vioplot(compressionratio, main="Violin plot for compression ratio", ylab = "Compression ratio")
# After lookin at the violin plot for compression ratio, I decided to remove
# all cars with ratio over 15.
cars <- subset(cars, compressionratio <= 15)

# Possibly also only look at gas cars? It seems that all diesel cars have
# already been excluded??
grDevices::windows()
plot(fueltype,price)

# It seems that the engine size might be grouped too?
grDevices::windows()
vioplot(enginesize, main="Violin plot for engine size", ylab = "Compression ratio")

cars <- subset(cars, enginesize <= 200)



cars <- subset(cars, fueltype=="gas")


# Suspect that id is not important for price:
cars <- subset(cars, select = -c(car_ID))

attach(cars)
#------------------------ Categorical data ------------------------------------#
# In this section, we factorise the categorical variables to be able to do some 
# more extensive analysis later.

CarName <- factor(CarName)
carbody <- factor(carbody)
fueltype <- factor(fueltype)
aspiration <- factor(aspiration)
fuelsystem <- factor(fuelsystem)
cylindernumber <- factor(cylindernumber)
enginetype <- factor(enginetype)
enginelocation <- factor(enginelocation)
drivewheel <- factor(drivewheel)
doornumber <- factor(doornumber)

#------------------------ Analysis of effect ----------------------------------#
# Here, some preliminary analysis of the variables and their effects are done.

grDevices::windows()
# plot(CarName,price)
pairs(~price+CarName+carbody+carheight)

grDevices::windows()
pairs(~price+carlength+enginetype+enginesize)

grDevices::windows()
pairs(~price+boreratio+peakrpm+highwaympg+citympg)

grDevices::windows()
pairs(~price+symboling+doornumber+drivewheel)

grDevices::windows()
pairs(~price+wheelbase+carwidth+curbweight+cylindernumber)

grDevices::windows()
pairs(~price+fuelsystem+stroke+horsepower)

grDevices::windows()
plot(fueltype,price)


# By themselves:
# Negative impact:
# citympg 
# highwaympg

# Positive impact:
# enginesize, horsepower, carheight, 
 
vif(lm(
  price ~ highwaympg + horsepower + enginesize + stroke + compressionratio + carlength + carheight + curbweight + carwidth + wheelbase))

?corrplot





