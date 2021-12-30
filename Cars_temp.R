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
library(beepr)

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

# Conclusion disregard: CarName, fueltype, aspiration, doornumber, 
# enginelocation and enginetype
# Include : drivewheel, cylindernumber and fuelsystem

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
summary(lm(cars$price ~ cars$highwaympg))
summary(lm(cars$price ~ cars$citympg))

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

# First create different data subsets
# Full cars dataset excluding car id
cars.id.reduced <- data.frame(cars[, -1])

# Only numerical/continuous variables (variables)
cars.num <- data.frame(cars.id.reduced[, 1:15])
  
# Cars minus: CarName, fueltype, aspiration, doornumber, enginelocation and 
# enginetype ( variables)
cars.cat.reduced <- data.frame(cars.id.reduced[, -c(16:19, 22, 23)])
  
# cars.cat.reduced minus continuous highly correlated (carlength, wheelbase, 
# citympg, carheight) ( variables)
cars.cat.num.reduced <- data.frame(cars.cat.reduced[, -c(2, 3, 5, 13)])
  
# Cars.cat.num.reduced with horsepower log transformed
cars.reduced.hp.trans <- data.frame(cars.cat.num.reduced[, -8])
cars.reduced.hp.trans$log.horsepower <- log(cars$horsepower)

# With horsepower and price log transformed
cars.reduced.hp.price.trans <- data.frame(cars.reduced.hp.trans[, -10])
cars.reduced.hp.price.trans$log.price <- log(cars$price)

# Try the more automated version of model selection (backward search).
# The olsrr does not divide data into test and training

###############################
# Full linear regression model on whole cars data set (excluding id)
mod.1 <- lm(price ~ ., data = cars.id.reduced)

# Ols step all possible search on full cars dataset
mod.a1 <- ols_step_all_possible(mod.1)
mod.a1
plot(mod.a1)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.1)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on akike criterion on full cars dataset
mod.b1 <- ols_step_backward_aic(mod.1)
mod.b1
plot(mod.b1)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.1)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward step search based on akike criterion on full cars dataset
mod.c1 <- ols_step_forward_aic(mod.1)
mod.c1
plot(mod.c1)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.1)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on p-value criterion on full cars dataset
mod.d1 <- ols_step_backward_p(mod.1)
mod.d1
plot(mod.d1)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.1)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward search based on p-value criterion on full cars dataset
mod.e1 <- ols_step_forward_p(mod.1)
mod.e1
plot(mod.e1)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.1)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

###############################
# Full linear regression model on only all numerical variables in cars data set 
# (excluding id)
mod.2 <- lm(price ~ ., data = cars.num)

# Ols step all possible search on full cars dataset
mod.a2 <- ols_step_all_possible(mod.2)
mod.a2
plot(mod.a2)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.2)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on akike criterion on full cars dataset
mod.b2 <- ols_step_backward_aic(mod.2)
mod.b2
plot(mod.b2)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.2)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward step search based on akike criterion on full cars dataset
mod.c2 <- ols_step_forward_aic(mod.2)
mod.c2
plot(mod.c2)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.2)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on p-value criterion on full cars dataset
mod.d2 <- ols_step_backward_p(mod.2)
mod.d2
plot(mod.d2)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.2)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward search based on p-value criterion on full cars dataset
mod.e2 <- ols_step_forward_p(mod.2)
mod.e2
plot(mod.e2)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.2)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))


###################
# Full linear regression model on numerical variables and 3 selected 
# categorical variables ()
mod.3 <- lm(price ~ ., data = cars.cat.reduced)

# Ols step all possible search on full cars dataset
mod.a3 <- ols_step_all_possible(mod.3)
mod.a3
plot(mod.a3)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.3)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on akike criterion on full cars dataset
mod.b3 <- ols_step_backward_aic(mod.3)
mod.b3
plot(mod.b3)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.3)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward step search based on akike criterion on full cars dataset
mod.c3 <- ols_step_forward_aic(mod.3)
mod.c3
plot(mod.c3)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.3)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on p-value criterion on full cars dataset
mod.d3 <- ols_step_backward_p(mod.3)
mod.d3
plot(mod.d3)
mod.d3$model
mod.result <- mod.d3$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.3)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward search based on p-value criterion on full cars dataset
mod.e3 <- ols_step_forward_p(mod.3)
mod.e3
plot(mod.e3)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.3)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

#############################
# Full linear regression model on cars.cat.reduced minus continuous highly 
# correlated (carlength, wheelbase, citympg, carheight)
mod.4 <- lm(price ~ ., data = cars.cat.num.reduced)

# Ols step all possible search on full cars dataset
mod.a4 <- ols_step_all_possible(mod.4)
mod.a4
plot(mod.a4)
mod.a4$model
mod.result <- mod.a4$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.4)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on akike criterion on full cars dataset
mod.b4 <- ols_step_backward_aic(mod.4)
mod.b4
plot(mod.b4)
mod.b4$model
mod.result <- mod.b4$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.4)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward step search based on akike criterion on full cars dataset
mod.c4 <- ols_step_forward_aic(mod.4)
mod.c4
plot(mod.c4)
mod.c4$model
mod.result <- mod.c4$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.4)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on p-value criterion on full cars dataset
mod.d4 <- ols_step_backward_p(mod.4)
mod.d4
plot(mod.d4)
mod.d4$model
mod.result <- mod.d4$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.4)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward search based on p-value criterion on full cars dataset
mod.e4 <- ols_step_forward_p(mod.4)
mod.e4
plot(mod.e4)
mod.e4$model
mod.result <- mod.e4$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.4)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

#####################################
# Full linear regression model on Cars.cat.num.reduced with horsepower 
# log transformed
mod.5 <- lm(price ~ ., data = cars.reduced.hp.trans)

# Ols step all possible search on full cars dataset
mod.a5 <- ols_step_all_possible(mod.5)
beep(1)
mod.a5
plot(mod.a5)
mod.a5$model
mod.result <- mod.a5$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.5)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on akike criterion on full cars dataset
mod.b5 <- ols_step_backward_aic(mod.5)
mod.b5
plot(mod.b5)
mod.b5$model
mod.result <- mod.b5$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.5)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward step search based on akike criterion on full cars dataset
mod.c5 <- ols_step_forward_aic(mod.5)
mod.c5
plot(mod.c5)
mod.c5$model
mod.result <- mod.c5$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.5)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on p-value criterion on full cars dataset
mod.d5 <- ols_step_backward_p(mod.5)
mod.d5
plot(mod.d5)
mod.d5$model
mod.result <- mod.d5$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.5)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward search based on p-value criterion on full cars dataset
mod.e5 <- ols_step_forward_p(mod.5)
mod.e5
plot(mod.e5)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.5)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

######################
# Full linear regression model on Cars.cat.num.reduced with horsepower and
# price log transformed
mod.6 <- lm(price ~ ., data = cars.reduced.hp.price.trans)

# Ols step all possible search on full cars dataset
mod.a6 <- ols_step_all_possible(mod.6)
mod.a6
plot(mod.a6)
# mod.result <- lm()
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.6)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on akike criterion on full cars dataset
mod.b6 <- ols_step_backward_aic(mod.6)
mod.b6
plot(mod.b6)
mod.b6$model
mod.result <- mod.b6$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.6)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward step search based on akike criterion on full cars dataset
mod.c6 <- ols_step_forward_aic(mod.6)
mod.c6
plot(mod.c6)
mod.c6$model
mod.result <- mod.c6$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.6)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols backward search based on p-value criterion on full cars dataset
mod.d6 <- ols_step_backward_p(mod.6)
mod.d6
plot(mod.d6)
mod.d6$model
mod.result <- mod.d6$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.6)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

# Ols forward search based on p-value criterion on full cars dataset
mod.e6 <- ols_step_forward_p(mod.6)
mod.e6
plot(mod.e6)
mod.e6$model
mod.result <- mod.e6$model
ols_plot_resid_fit(mod.result, print_plot = TRUE)
ols_plot_resid_stand(mod.result, print_plot = TRUE)
ols_mallows_cp(mod.result, mod.6)
ols_prep_outlier_obs(ols_prep_cdplot_data(mod.result))

###################
# OLS Conclusions: 
## Variables to remove:
# (car_ID)
# CarName
# citympg
# aspiration
# curbweight
# symboling
# doornumber 
# enginelocation
# enginetype

## variables to include:
# horsepower/log(horsepower)
# cylindernumber
# fuelsystem
# highwaympg

## Maybe include variables:
# enginesize
# boreratio
# carwidth
# peakrpm
# carbody
# drivewheel

## Maybe litle less include variables:
# Stroke
# wheelbase
# compressionratio


####################

# Which variables are good to transform?
# Residual standardization and plotting.
# Make pMSE plots

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




















