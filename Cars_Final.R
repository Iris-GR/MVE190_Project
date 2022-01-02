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

#### Find the most relevant categorical variables ##############################

# In this section the variable names are in reference to the cars dataset
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

# The number of cylinders in motor
plot(price ~ cylindernumber)
vioplot(price ~ cylindernumber)
summary(lm(price ~ cylindernumber))
length(which(cars$cylindernumber == "three"))
length(which(cars$cylindernumber == "twelve"))

# The fuel system  
plot(price ~ fuelsystem)
vioplot(price ~ fuelsystem)
summary(lm(price ~ fuelsystem))

# The type of car body ()
plot(price ~ carbody)
vioplot(price ~ carbody)
summary(lm(price ~ carbody))

# Conclusion: The 4 most relevant categorical variables appear to be drivewheel, 
# cylindernumber, carbody and fuelsystem. This means that CarName, fueltype, 
# aspiration, doornumber, enginelocation and enginetype will be less studied
# moving on.

#### Check multicollinearity ###################################################

# Here, the multicollineraty between the continuous variables is analysed 
# This is done through the usage of the variance inflation factor. 
# When variables which seem highly collinear are located, their explanatory 
# powers for the price are compared, and the least relevant of them removed.

# Trial 1, test all numeric variables (except car id)
vif(lm(price ~ symboling + wheelbase + carlength + carwidth + carheight +
         curbweight + enginesize + boreratio + stroke + compressionratio+
         horsepower + peakrpm + citympg + highwaympg))

# Highwaympg and citympg have the strongest linear dependence. Exclude from 
# models the mpg with least explanatory power on price (in this case citympg)
summary(lm(price ~ highwaympg)) # R2adj = 0.4841
summary(lm(price ~ citympg)) # R2adj = 0.4676

# Trial 2, find more linearly dependent variables 
vif(lm(price ~ symboling + wheelbase + carlength + carwidth + carheight +
         curbweight + enginesize + boreratio + stroke + compressionratio+
         horsepower + peakrpm + highwaympg))

# The variables with highest vif are curbweight, carlength, and wheelbase, check
# visualy for linear dependence
grDevices::windows()
pairs(~price + carlength + curbweight + wheelbase)

# Exclude from future models the two variables with the least explanatory power 
# on price (carwidth and wheelbase)
summary(lm(price ~ curbweight)) # r2adj = 0.6962
summary(lm(price ~ carwidth))   # r2adj = 0.4638
summary(lm(price ~ wheelbase))  # r2adj = 0.3306

# Trial 3, find more linearly dependent variables 
vif(lm(price ~ symboling + carwidth + carheight + curbweight + enginesize + 
         boreratio + stroke + compressionratio + horsepower + peakrpm + 
         highwaympg))

# curbweight now has vif 14.00 (>10), so exclude from future models

# Trial 4, check vif for remaining numerical variables (ok vif!)
vif(lm(price ~ symboling + carwidth + carheight + enginesize + boreratio + 
         stroke + compressionratio + horsepower + peakrpm + highwaympg))

# Check linear dependence visually. (In general no strong linear relations)
grDevices::windows()
pairs(~symboling + carwidth + carheight + enginesize + boreratio + 
         stroke + compressionratio + horsepower + peakrpm + highwaympg)

# Conclusion: Proceed with variables symboling, carwidth, carheight, enginesize
# boreratio, stroke, compressionratio, horsepower, peakrpm and highwaympg

#### Variable transformation? ##################################################

# Transformation could be appropriate after studying residuals, however, a first
# check for the range/scale and distribution of values are done for the 
# continuous/numeric variables
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

# Horsepower and price have observation values on a wide range, log transform 
vioplot(log(horsepower))
vioplot(log(price))

#### Creation of models and comparison/testing (test/training data) ############

# pmse....


#### Creation of models and comparison/testing (olsrr package) #################

#### The ols_step_all_possible takes to long time if run with all variables in
# the cars dataset at once. First test the backward step search with respect to
# aic and the p-value on the full cars dataset (excluding the carID(not 
# relevant), enginelocation and carname)

## Ols backward search based on akaike criterion 
mod.1b <- ols_step_backward_aic(mod.1)
mod.1b
grDevices::windows()
plot(mod.1b)

## Ols forward step search based on akaike criterion 
mod.1c <- ols_step_forward_aic(mod.1)
mod.1c
grDevices::windows()
plot(mod.1c)

## Ols backward search based on p-value criterion 
mod.1d <- ols_step_backward_p(mod.1)
mod.1d
grDevices::windows()
plot(mod.1d)

## Ols forward search based on p-value criterion 
mod.1e <- ols_step_forward_p(mod.1)
mod.1e
grDevices::windows()
plot(mod.1e)

#### Linear regression model containing the 4 chosen categorical variables and 
# the non-linearly dependent numeric variables (price and horsepower log 
# transformed)

mod.1 <- lm(I(log(price)) ~ symboling + carwidth + carheight + enginesize + 
              boreratio + stroke + compressionratio + I(log(horsepower)) + 
              peakrpm + highwaympg + carbody+ drivewheel + cylindernumber +
              fuelsystem, data = cars)
summary(mod.1)

### Ols step all possible search to find reduced models
mod.1a <- ols_step_all_possible(mod.1)

# Display and plot model
mod.1a
grDevices::windows()
plot(mod.1a)

# We took best model with four covariates, mod #470 or # 473 appear
# to be best

# nr 470: carwidth, I(log(horsepower)), carbody and fuelsystem. Lower Mallows Cp
# (50.40848), higher aic (-98.43165)
mod.1a[470,] 

# nr 473: carwidth, I(log(horsepower)), carbody and drivewheel. Lower aic 
# (-100.3443), higher Mallows Cp (60.28348)
mod.1a[473,] 

# Choose model nr 473
mod.1a.result <- lm(I(log(price)) ~ carwidth + I(log(horsepower)) + carbody + 
                      drivewheel, data = cars)
summary(mod.1a.result)


#### Final model, more in depth analysis #######################################

# Best model selected from previous testing:
# log(price) ~ carwidth + log(horsepower) + carbody + drivewheel

# Fit the final model on the full dataset
mod.final <- lm(I(log(price)) ~ carwidth + I(log(horsepower)) + carbody + 
                  drivewheel, data = cars)

# What are the coefficients and their interpretation?
summary(mod.final)

# Confidence intervals of coefficients. Plot CI for model.


# Plot the observed Y vs Y_hat.
ols_plot_obs_fit(mod.final, print_plot = TRUE)
plot(mod.final$fitted.values ~ I(log(price)))

# Residuals
ols_plot_resid_fit(mod.final, print_plot = TRUE)

# Standardized and studentized residuals (!!! fix so that it is studentized to)
ols_plot_resid_stand(mod.final, print_plot = TRUE)
#ols_plot_resid_stud(mod.final, print_plot = TRUE)

# Compute Cook's distance and plot it to find influential observations
ols_plot_cooksd_chart(mod.final, print_plot = TRUE)
ols_plot_cooksd_bar(mod.final, print_plot = TRUE)


#### Check if interactions are significant at alpha = 0.05 level (add one 
# interaction at a time)
mod.full <- lm(I(log(price)) ~ carwidth+ I(log(horsepower)) * carbody +
                 drivewheel, data = cars)
mod.red <- lm(I(log(price)) ~ carwidth + I(log(horsepower)) + carbody + 
                drivewheel, data = cars) 

# Partial F-test on nested models
anova(mod.red , mod.full)

# The model with primary effects and the interaction between 
# log(horsepower) and carbody is worth having over the reduced model according 
# to F-test. 

# Standardized and studentized residuals (!!! fix so that it is studentized to)
ols_plot_resid_stand(mod.full, print_plot = TRUE)
#ols_plot_resid_stud(mod.final, print_plot = TRUE)

# Compute Cook's distance and plot it to find influential observations
ols_plot_cooksd_chart(mod.full, print_plot = TRUE)
log(price[99])
mod.final$fitted.values[99]

# However, the addition of interaction leads to observation 99 being an outlier
# and very influential. So the conclusion is to not include the interaction term



































