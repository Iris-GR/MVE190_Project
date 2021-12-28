# Temporary file for looking at the cars dataset

# Set working directory (set your own)
setwd("~/MVE190_Data_Management/git_folder_mve190_project/MVE190_Project")

# Load libraries
library(corrplot)
library(vioplot)
library(dplyr)
library(ISLR)
library(leaps)
library(olsrr)
library(ggplot2)
library(rgl)  
library(scatterplot3d)  
library(arm)
library(dotwhisker)
library(car)
library(leaps)
library(MASS)

##### Dataset preparation ######################################################

# Load the cars dataset
cars.0 <- data.frame(read.csv("CarPrice.csv", header = T ))

# Add the numeric/continuous variables to dataset
cont.var.index <- c(1, 2, 10:14, 17, 19:26)
cars <- data.frame(cars.0[, cont.var.index])

# Add the categorical variables as assigned categorical factors and change the 
# reference level to the desired one
# Car name
cars$CarName <- factor(cars.0$CarName)

# Fuel type
cars$fueltype <- factor(cars.0$fueltype)
relevel(cars$fueltype, ref = "gas")

# Aspiration
cars$aspiration <- factor(cars.0$aspiration)

# Number of doors (excluding trunk dooor)
cars$doornumber <- factor(cars.0$doornumber)

# Type of car body
cars$carbody <- factor(cars.0$carbody)
relevel(cars$carbody, ref = "hatchback")

# Type of wheel drive
cars$drivewheel <- factor(cars.0$drivewheel)
relevel(cars$drivewheel , ref ="rwd")

# Engine location
cars$enginelocation <- factor(cars.0$enginelocation)

# Engine type
cars$enginetype <- factor(cars.0$enginetype)
relevel(cars$enginetype, ref = "ohc")

# Number of cylinders
cars$cylindernumber <- factor(cars.0$cylindernumber)
relevel(cars$cylindernumber , ref ="four")

# Fuel system
cars$fuelsystem <- factor(cars.0$fuelsystem)
relevel(cars$fuelsystem , ref ="2bbl")

#### Data overview #############################################################
attach(cars)

#### Find the 1-3 most relevant categorical variables ##########################
# The car names explain a lot (R^2 = 0.84), however, there are 147 car names so
# it is a highly specialized categorical variable and not many observations in 
# each subcategory.
plot(price ~ CarName)
vioplot(price ~ CarName)
summary(lm(price ~ CarName))

plot(price ~ fueltype)
vioplot(price ~ fueltype)
summary(lm(price ~ fueltype))

# The aspiration
plot(price ~ aspiration)
vioplot(price ~ aspiration)
summary(lm(price ~ aspiration))

# Door number, four or two have a very similar distribution
plot(price ~ doornumber)
vioplot(price ~ doornumber)
summary(lm(price ~ doornumber))

# Which drive wheel seems to have an impact on price (R^2 = 0.40) and there are 
# Several observations in each of the three categories
plot(price ~ drivewheel)
vioplot(price ~ drivewheel)
summary(lm(price ~ drivewheel))

# Much higher price for the cars with engine back, but since there are only 
# three cars with engine back.
plot(price ~ enginelocation)
vioplot(price ~ enginelocation)
summary(lm(price ~ enginelocation))

# The engine type is a possible categorical factor
plot(price ~ enginetype)
vioplot(price ~ enginetype)
summary(lm(price ~ enginetype))
which(cars$enginetype=="dohcv")
which(cars$enginetype=="rotor")

plot(price ~ cylindernumber)
vioplot(price ~ cylindernumber)
summary(lm(price ~ cylindernumber))

# The fuel system  is a possible categorical factor
plot(price ~ fuelsystem)
vioplot(price ~ fuelsystem)
summary(lm(price ~ fuelsystem))

# Conclusion disregard: CarName, fueltype, aspiration, doornumber and 
# enginelocation.
# Include : drivewheel, (enginetype), cylindernumber and fuelsystem

#### Check multicollinearity among numerical variables #########################
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
summary(lm(price ~ cars$highwaympg))
summary(lm(price ~ cars$citympg))

summary(lm(price ~ cars$carlength))
summary(lm(price ~ cars$carwidth))
summary(lm(price ~ cars$carheight))
# Is it possible to combine a few numeric variables?

# Can length, width and height, which are correlated with each other, be combined
# into a "total" car size? No, probably not a good idea
cars$size.tot <- cars$carlength + cars$carwidth + cars$carheight
summary(lm(price ~ cars$size.tot))

# A chi-squared test of independence between two categorical varaibles can be 
# done (not same as testing for multicolinearity) 
chisq.test(table(cars$fuelsystem, cars$enginetype))
chisq.test(table(cars$aspiration, cars$fueltype))
chisq.test(table(cars$drivewheel, cars$fueltype))

#### Creation of different models and comparison/testing #######################

# Which are the most influential variables/ very non-interesting variables to discard?

# Which variables are good to transform?
# Residual standardization and plotting.

# Try the more automated version of model selection (backward search).
# Make pMSE plots 

model <- lm(price ~ carwidth + horsepower+ highwaympg+compressionratio)
k <- 
plot(k)
ols_step_all_possible(model)
# Narrow it down to a few models. 
# Do nested/partial F-tests if possible.
# Maybe test the variance inflation factor (VIF)?
# Akaike’s AIC and BIC criterion
# Divide into test and training data and check predictability.
# K-fold and/or Leave-one-out (LOOCV) cross validation.

#### Final model, more in depth analysis #######################################

# Select a final model from previous testing.
# Interactions
# Fit the model on the full dataset.
# What are the coefficients and their interpretation.
# Confidence intervals of coefficients. Plot CI for model.
# Plot the observed Y vs Y_hat.
# Plot standardized residuals.
# Compute Cook's distance and plot it to find influential obseravations.
# Identify possible outliers. Should they be removed? If so re-fit model and 
# redo analysis
