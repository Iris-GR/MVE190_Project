# The cars data set
setwd("C:/R workspace/MVE190/MVE190_Project_new/MVE190_Project")
#------------------------ Libraries -------------------------------------------#
# Here all libraries which will be used are specified.
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

#------------------------ Loading data ----------------------------------------#
# In this part of the project it is the dataset CarPrice which will be analysed.

cars <- read.csv("CarPrice.csv", header = T )
cars <- data.frame(cars)

attach(cars)

#Notable: there are in total 205 cars in the dataset. 
# Minimum price is 5118 dollar.
# Maximum price is 45400 dollar. 

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
#plot(CarName,price)
pairs(~price+CarName+horsepower+highwaympg+carheight)

grDevices::windows()
plot(enginesize,price)

#By themselves:
#Negative impact:
# citympg 
# highwaympg

#Positive impact:
#enginesize, horsepower, carheight, 




