# Hopefully the final cars script

# Set working directory (set your own)
setwd("~/MVE190_Data_Management/git_folder_mve190_project/MVE190_Project")

#### Load libraries ############################################################
library(corrplot)
library(vioplot)
library(ISLR)
library(leaps)
library(olsrr)
library(arm)
library(car)
library(MASS)
library(beepr)

#### Dataset preparation #######################################################

# Load the raw cars dataset
cars.raw<- data.frame(read.csv("CarPrice.csv", header = T ))

# Create cars data set, start with numeric/continuous variables 
cont.var.index <- c(1, 2, 10:14, 17, 19:26)
cars <- data.frame(cars.raw[, cont.var.index])

# Add the categorical variables as assigned categorical factors and change the 
# reference level to the desired one
# Car name
cars$CarName <- factor(cars.raw$CarName)

# Fuel type
cars$fueltype <- factor(cars.raw$fueltype)
cars$fueltype <-relevel(cars$fueltype, ref = "gas")

# Aspiration
cars$aspiration <- factor(cars.raw$aspiration)

# Number of doors (excluding trunk door)
cars$doornumber <- factor(cars.raw$doornumber)

# Type of car body
cars$carbody <- factor(cars.raw$carbody)
cars$carbody <-relevel(cars$carbody, ref = "hatchback")

# Type of wheel drive
cars$drivewheel <- factor(cars.raw$drivewheel)
cars$drivewheel <-relevel(cars$drivewheel , ref ="rwd")

# Engine location
cars$enginelocation <- factor(cars.raw$enginelocation)

# Engine type
cars$enginetype <- factor(cars.raw$enginetype)
cars$enginetype <- relevel(cars$enginetype, ref = "ohc")

# Number of cylinders
cars$cylindernumber <- factor(cars.raw$cylindernumber)
cars$cylindernumber <-relevel(cars$cylindernumber , ref ="four")

# Fuel system
cars$fuelsystem <- factor(cars.raw$fuelsystem)
cars$fuelsystem <- relevel(cars$fuelsystem , ref ="2bbl")

#### Dataset overview ##########################################################

sprintf("Number of variables in Cars dataset: %i", length(cars[1, ]))
sprintf("Number of numeric variables: %i, whereof one is ID", 16)
sprintf("Number of categorical variables: %i", 10)
sprintf("Number of observations (cars): %i", length(cars[, 1]))

#### Find the 1-3 most relevant categorical variables ##########################

# In this section the variabel names are in reference to the cars dataset
attach(cars)

# The car names explain a lot (R^2 = 0.84), however, there are 147 car names so
# it is a highly specialized categorical variable and not many observations in 
# each subcategory.
plot(price ~ CarName)
summary(lm(price ~ CarName))

# The fuel type 
plot(price ~ fueltype)
vioplot(price ~ fueltype)
summary(lm(price ~ fueltype))

# The aspiration
plot(price ~ aspiration)
vioplot(price ~ aspiration)
summary(lm(price ~ aspiration))
length(which(cars$aspiration == "std"))
length(which(cars$aspiration == "turbo"))

# Door number, four or two doors have a very similar distribution
plot(price ~ doornumber)
vioplot(price ~ doornumber)
summary(lm(price ~ doornumber))

# Drive wheel seems to have an impact on price (R^2 = 0.40) and there are 
# Several observations in each of the three categories
plot(price ~ drivewheel)
vioplot(price ~ drivewheel)
summary(lm(price ~ drivewheel))

# Much higher price for the cars with engine back, but since there are only 
# three cars with engine back.
plot(price ~ enginelocation)
vioplot(price ~ enginelocation)
summary(lm(price ~ enginelocation))
length(which(cars$enginelocation == "rear"))

# The engine type is a possible categorical factor
plot(price ~ enginetype)
vioplot(price ~ enginetype)
summary(lm(price ~ enginetype))
length(which(cars$enginetype == "dohcv"))
length(which(cars$enginetype == "rotor"))

# The nymber of cylinders in motor
plot(price ~ cylindernumber)
vioplot(price ~ cylindernumber)
summary(lm(price ~ cylindernumber))
length(which(cars$cylindernumber == "three"))
length(which(cars$cylindernumber == "twelve"))

# The fuel system  
plot(price ~ fuelsystem)
vioplot(price ~ fuelsystem)
summary(lm(price ~ fuelsystem))

# Conclusion: The 3 most relevant categorical variables appear to be drivewheel, 
# cylindernumber and fuelsystem. This means that CarName, fueltype, aspiration, 
# doornumber, enginelocation and enginetype will be less studied moving on.

#### Check multicollinearity ###################################################

# Here, the multicollineraty between the continuous variables is analysed 
# This is done through the usage of the variance inflation factor. 
# When variables which seem highly collinear are located, their explanatory 
# powers for the price are compared, and the least relevant of them removed.

pairs(~price + carlength + curbweight + wheelbase)

plot(price ~ symboling)
summary(lm(price ~ symboling))

plot(price ~ wheelbase)
summary(lm(price ~ wheelbase))

plot(price ~ carlength)
summary(lm(price ~ carlength))

plot(carlength ~ carwidth)
summary(lm(carlength ~ carwidth))

# Should only one of a group of correlated variables be used? Which one?
# pairs,corrplot, VIF or other methods?
summary(lm(cars$price ~ cars$highwaympg))
summary(lm(cars$price ~ cars$citympg))

summary(lm(price ~ cars$carlength))
summary(lm(price ~ cars$carwidth))
summary(lm(price ~ cars$carheight))

# Can length, width and height, which are correlated with each other, be 
# combined into a "total" car size? No, probably not a good idea.
cars$size.tot <- cars$carlength + cars$carwidth + cars$carheight
summary(lm(price ~ cars$size.tot))

# Dependence among categorical variables
# A chi-squared test of independence between two categorical varaibles can be 
# done (not same as testing for multicolinearity) 
chisq.test(table(cars$fuelsystem, cars$enginetype))
chisq.test(table(cars$aspiration, cars$fueltype))
chisq.test(table(cars$drivewheel, cars$fueltype))

#### Variable transformation? ##################################################

# Transformation could be appropriate after studying residuals, however, a first
# check the range/scale and distribution of values are done for the continuous
# variables
vioplot(symboling)
vioplot(wheelbase)
vioplot(carlength)
vioplot(carwidth)
vioplot(carheight)
vioplot(curbweight)
vioplot(enginesize)
vioplot(boreratio)
vioplot(stroke)
vioplot(compressionratio)
vioplot(horsepower)
vioplot(peakrpm)
vioplot(citympg)
vioplot(highwaympg)
vioplot(price)

# Horsepower and price have observation values on a wide range, transform them
vioplot(log(horsepower))
vioplot(log(price))

#### hh #####

# !!!!! Inte klar,förlåt :(


