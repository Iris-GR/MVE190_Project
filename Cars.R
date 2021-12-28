# The cars data set
setwd("C:/R workspace/MVE190/MVE190_Project_new/MVE190_Project")
#------------------------ Libraries -------------------------------------------#
# Temporary file for looking at the cars dataset

# Load libraries
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

##### Dataset preparation ######################################################

# Load the cars dataset
cars.0 <- data.frame(read.csv("CarPrice.csv", header = T ))

# Add the numeric/continuous variables to dataset
cont.var.index <- c(1, 2, 10:14, 17, 19:26)
cars <- data.frame(cars.0[, cont.var.index])

attach(cars.0)
#------------------------ Categorical data ------------------------------------#
# In this section, we factorise the categorical variables to be able to do some 
# more extensive analysis later.

# Add the categorical variables as assigned categorical factors and change the 
# reference level to the desired one
# Car name
cars$CarName <- factor(CarName)

# Fuel type
cars$fueltype <- factor(fueltype)
relevel(cars$fueltype, ref = "gas")

# Aspiration
cars$aspiration <- factor(aspiration)

# Number of doors (excluding trunk dooor)
cars$doornumber <- factor(doornumber)

# Type of car body
cars$carbody <- factor(carbody)
relevel(cars$carbody, ref = "hatchback")

# Type of wheel drive
cars$drivewheel <- factor(drivewheel)
relevel(cars$drivewheel , ref ="rwd")

# Engine location
cars$enginelocation <- factor(enginelocation)

# Engine type
cars$enginetype <- factor(enginetype)
relevel(cars$enginetype, ref = "ohc")

# Number of cylinders
cars$cylindernumber <- factor(cylindernumber)
relevel(cars$cylindernumber , ref ="four")

# Fuel system
cars$fuelsystem <- factor(fuelsystem)
relevel(cars$fuelsystem , ref ="2bbl")

#### Data overview #############################################################
attach(cars)
#------------------------ Loading data ----------------------------------------#
# In this part of the project it is the dataset CarPrice which will be analysed.

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

#-------------------------- Variance Inflation Factor -------------------------#
# Here, the multicollineraty between the continuous variables is analysed 
# This is done through the usage of the variance inflation factor. 
# When variables which seem highly collinear are located, their explanatory 
# powers for the price are compared, and the least relevant of them removed. 

# Trial 1:
vif(lm(
  price ~ highwaympg + citympg + horsepower + enginesize + stroke + compressionratio + carlength + carheight + curbweight + carwidth + wheelbase))
# Highwaympg and citympg seem dependent on each other. 

mod1 <- lm(price~highwaympg) 
summary(mod1) # r2adj = 0.4841

mod2 <- lm(price~citympg)
summary(mod2) # r2adj = 0.4676

# Remove citympg

# Trial 2:
vif(lm(
  price ~ highwaympg + horsepower + enginesize + stroke + compressionratio + carlength + carheight + curbweight + carwidth + wheelbase))


grDevices::windows()
pairs(~price + carlength + curbweight + wheelbase)


# Curbweight, carlength, and wheelbase seem dependent?

mod3 <- lm(price~curbweight)
summary(mod3) # r2adj = 0.6962

mod4 <- lm(price~carlength)
summary(mod4) # r2adj = 0.4638

mod5 <- lm(price~wheelbase)
summary(mod5) # r2adj = 0.3306

# remove carlength and wheelbase
vif(lm(
  price ~ highwaympg + horsepower + compressionratio  + enginesize + carheight + carwidth))

# Removed curbweight.

#Final toll: highwaympg, horsepower, enginesize, carwidth, comressionratio, carheight

grDevices::windows()
pairs(~price + highwaympg + enginesize + compressionratio + curbweight + stroke)






