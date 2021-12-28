# Something 
# Added new line

# Temporary file for looking at the cars dataset

# Set working directory (set your own)
setwd("~/MVE190_Data_Management/git_folder_mve190_project/MVE190_Project")

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

#### Find the 1-3 most relevant categorical variables ##########################

#### Check multicollinearity among numerical variables #########################

# Should only one of a group of correlated variables be used? Which one?
# corrplot, VIF or other methods?
# Is it possible to combine a few numeric variables?

#### Creation of different models and comparison/testing #######################

# Which are the most influential variables/ very non-interesting variables to discard?
# Which variables are good to transform?
# Residual standardization and plotting.

# Try the more automated version of model selection (backward search).
# Make pMSE plots 

# Narrow it down to a few models. 
# Do nested/partial F-tests if possible.
# Maybe test the variance inflation factor (VIF)?
# Akaike’s AIC and BIC criterion
# Divide into test and training data and check predictability.
# K-fold and/or Leave-one-out (LOOCV) cross validation

#### Final model, more in depth analysis #######################################

# Select a final model from previous testing.
# Fit the model on the full dataset.
# What are the coefficients and their interpretation.
# Confidence intervals of coefficients. Plot CI for model.
# Plot the observed Y vs Y_hat.
# Plot standardized residuals.
# Compute Cook's distance and plot it to find influential obseravations.
# Identify possible outliers. Should they be removed? If so re-fit model and 
# redo analysis
