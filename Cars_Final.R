# Cars dataset script

# Set working directory (set your own)

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

# The type of car body
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

#### Creation of models and comparison/testing (olsrr package) #################

#### The ols_step_all_possible takes to long time if run with many variables from
# the cars dataset at once. First test the backward step search with respect to
# aic and the p-value on the dataset (excluding the carID,
# enginelocation, carname and the linearly dependent vnumeric variables)

mod.0 <- lm(I(log(price)) ~ symboling + carwidth + carheight + enginesize + 
              boreratio + stroke + compressionratio + I(log(horsepower)) + 
              peakrpm + highwaympg + carbody+ drivewheel + enginelocation +
              enginetype + cylindernumber + fuelsystem, data = cars)

## Ols backward search based on akaike criterion 
mod.0b <- ols_step_backward_aic(mod.0)
mod.0b
grDevices::windows()
plot(mod.0b)

## Ols forward step search based on akaike criterion 
mod.0c <- ols_step_forward_aic(mod.0)
mod.0c
grDevices::windows()
plot(mod.0c)

## Ols backward search based on p-value criterion 
mod.0d <- ols_step_backward_p(mod.0)
mod.0d
grDevices::windows()
plot(mod.0d)

## Ols forward search based on p-value criterion 
mod.0e <- ols_step_forward_p(mod.0)
mod.0e
grDevices::windows()
plot(mod.0e)

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

# Akaike model selection
plot(mod.1a$aic ~ mod.1a$n,
     xlab = "Complexity", 
     ylab = "AIC",
     main = "Akaike Model Selection",
     cex = 0.7,
     cex.lab = 1.3,
     cex.main = 1.5,)
text(x = c(4),
     y = c(-98.5),
     labels= c("Model 473"),
     pos = 1)
symbols(x = c(4),
       y = c(-101),
       circles = 0.13,
       add = T, 
       inches = F, 
       lwd = 2,
       fg = "red3")

# Mallow's model selection
plot(mod.1a$cp ~ mod.1a$n,
     xlab = "Complexity", 
     ylab = "Mallow's Cp",
     main = "Mallow's Cp Model Selection",
     cex = 0.7,
     cex.lab = 1.3,
     cex.main = 1.5,)
text(x = c(4),
     y = c(52),
     labels= c("Model 470"),
     pos = 1)
symbols(x = c(4),
        y = c(50.40848),
        circles = 0.13,
        add = T, 
        inches = F, 
        lwd = 2,
        fg = "red3")

# We took best model with four covariates, mod #470 or # 473 appear
# to be good

# nr 470: carwidth, I(log(horsepower)), carbody and fuelsystem. Lower Mallows Cp
# (50.40848), higher aic (-98.43165), similar R^2 (0.8676765)
mod.1a[470,] 

# nr 473: carwidth, I(log(horsepower)), carbody and drivewheel. Lower aic 
# (-100.3443), higher Mallows Cp (60.28348), similar R^2 (0.8658633)
mod.1a[473,] 

# Choose model nr 473
mod.1a.result <- lm(I(log(price)) ~ carwidth + I(log(horsepower)) + carbody + 
                      drivewheel, data = cars)
summary(mod.1a.result)

#### Predicting “unseen observations” (Test and training data) #################

# First do k-fold cross validation to find models. The function regsubset from 
# the leaps package can not handle several categorical covariates at once and 
# furthermore not either categorical covariates with with more than a few levels.
# For example "fuelsystem" can not be run. Test models with all numeric 
# variables that are not strongly correlated with each other and then carbody,
# and "drivewheel" included , as "cylindernumber" and "fuelsystem" were not 
# possible.

m <- lm(I(log(price)) ~ symboling + carwidth + carheight + enginesize + 
          boreratio + stroke + compressionratio + I(log(horsepower)) + 
          peakrpm + highwaympg + carbody + drivewheel, data = cars, x = T)

# Extract the design matrix from model "m"
X <- m$x  

# Remove the intercept column from design matrix
X <- X[, -c(1)]
X <- as.matrix(X)

# Set the y variable
y <- log(cars$price)

# Force in columns that contain the levels of the categorical covariates
m <- regsubsets(X, 
                y, 
                int = T, 
                nbest = 1000, # !!! try maybe nbest = 1?
                nvmax = dim(X)[2],
                method = c("ex"), 
                really.big = T, 
                force.in = c(11:16))

cleaps <- summary(m, matrix = T)

# Print models and display which covariates have been forced in 
cleaps$which

# Reorder the columns according to the order found in cleaps$which
col.order <- c(colnames(cleaps$which))

# Also reorder the columns in X according to the order found in cleaps$which
X <- X[, col.order[-1]]

# Save the models produced by cleaps$which in varaiable called "Models"
Models <- cleaps$which

# Set randomization seed for reproducibility
set.seed(123)  

# The number of partitions to the dataset 
K <- 5

# The number of observations
n <- length(y)

# Pseudo randomize the order of indexes i
ii <- sample(seq(1,n), n) 

# The fold size
foldsize <- floor(n / K)

# For this dataset the n/k is even so each fold has same size
sizefold <- rep(foldsize, K)

# Create empty matrix for the prediction error
Pred.errors <- matrix(0, dim(Models)[1], K)

# Initialize number of indeces used for testing
iused <- 0  

# For each fold
for (k in (1:K)) {
  
  # The current testing set
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  
  # The current training set
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  
  # Note the used fold indexes
  iused <- iused + length(itest) 
  
  # Only intercept
  Pred.errors[1,k] <- sum((y[itest] - mean(y[itrain]))^2)  
  
  # For all model subsets
  for (mm in (1:dim(Models)[1])) { 
    
    # The design matrix for training for model mm
    xtrain <- X[itrain,Models[mm,2:dim(Models)[2]]]
    
    # Insert the intercept column (ones) for estimation also of the intercept 
    # via least squares
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  
    
    # Least squares formula for parameter estimates
    beta.hat <- solve(t(xtrain) %*% xtrain) %*% t(xtrain) %*% y[itrain]  
    
    # Extract design matrix for testing for model mm 
    xtest <- X[itest, Models[mm, 2:dim(Models)[2]]]
    
    # Insert the intercept column (ones) to be albe to make predictions
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  
    
    # Calculate the predicted y
    ypred <- xtest %*% beta.hat
    
    # Calculate the predicted errors for the predicted y
    Pred.errors[mm, k] <- sum((y[itest] - ypred) ^ 2) 
  }
}

# Save the final prediction errors for the models 
PE <- apply(Pred.errors, 1, sum) / n  

# Rearrange the PE values according to increasing model complexity (does not 
# really work, to many levels in the categorical covariates)
# Models
# complexity <- c(rep(2,11),
#                 rep(3,55),
#                 rep(4,165),
#                 rep(5,330),
#                 rep(6,462),
#                 rep(7,462),
#                 rep(8,330),
#                 rep(9,165),
#                 rep(10,55), 
#                 rep(111,11),
#                 12)
# 
# # Plot the complexity vs the prediction error (Note, not right complexity!)
# grDevices::windows()
# plot(complexity,PE) 

# Find smallest prediction error for model with four covariates (says 2 here 
# bacuse it has problems counting categorical covariates)
PE[complexity == 2]

# Model 11, which is log(price) ~ carwidth + log(horsepower) + carbody + 
# drivewheel, has smallest rediction error among models with four covariates.
Models[11,] 

# The chosen model
chosen.mod <- lm(y[itrain] ~ X[itrain, Models[11, -1] == T])

# Parameter estimates from chosen enmodel on training data
beta.chosen <- chosen.mod$coefficients  

# Select the variables present in chosen model from the test data
design.chosen <- X[itest, Models[11, -1] == T]

# Insert a column (ones) for the intercept
design.chosen <- cbind(rep(1, dim(design.chosen)[1]), design.chosen)

# Obtain predictions using the covariates from the (last) test set 
ypred <- design.chosen %*% beta.chosen

# Compare predictions with test responses
grDevices::windows()
plot(y[itest], 
     ypred,
     xlab = "Y test", 
     ylab = "Y prediction",
     main = "Predicted Response VS Test Response",
     sub = "log(price) ~ carwidth + log(horsepower) + carbody + drivewheel",
     cex = 1.2,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
abline(0, 
       1,
       lwd = 1.5)   

#### Final model, more in depth analysis #######################################

# Best model selected from previous testing:
# log(price) ~ carwidth + log(horsepower) + carbody + drivewheel

# Check so that log(horsepower) and carwidth are not linearly correlated
vif(lm(I(log(price)) ~ carwidth + I(log(horsepower)) , data = cars))

## Check if interactions are significant at alpha = 0.05 level (add one 
# interaction at a time). 
mod.full.1 <- lm(I(log(price)) ~ carwidth+ I(log(horsepower)) * carbody +
                 drivewheel, data = cars)
mod.red.1 <- lm(I(log(price)) ~ carwidth + I(log(horsepower)) + carbody + 
                drivewheel, data = cars) 

# Partial F-test on nested models
anova(mod.red.1 , mod.full.1)

# The model with primary effects and the interaction between 
# log(horsepower) and carbody is worth having over the reduced model according 
# to F-test. 

# Plot the observed Y vs Y_hat.
ols_plot_obs_fit(mod.full.1, print_plot = TRUE)

# Compute Cook's distance and plot it to find influential observations
ols_plot_cooksd_chart(mod.full.1, print_plot = TRUE)
C.D.full.1 <- cooks.distance(mod.full.1)

# Plot the Cook's distance
plot(C.D.full.1,
     xaxt='n',
     xlab = "Observation i", 
     ylab = "D",
     main = "Cook's Distance",
     #sub = "log(price) ~ carwidth + log(horsepower) + carbody + drivewheel + log(horsepower) * carbody",
     cex = 0.7,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
axis(1, at = seq(0, 205, by = 20), las = 1)
text(x = c(99),
     y = c(1.86),
     labels= c("ID 99, Nissan Clipper"),
     pos = 4)
# symbols(x = c(99),
#         y = c(1.865),
#         circles = 2,
#         add = T, 
#         inches = F, 
#         lwd = 2,
#         fg = "red3")

# Investigate car 99
log(price[99])
mod.full.1$fitted.values[99]
cars[99,]

# Residuals
ols_plot_resid_fit(mod.full.1, print_plot = TRUE)

# Studentized residuals 
ols_plot_resid_stud(mod.full.1, print_plot = TRUE)

stud.res.full.1 <- studres(mod.full.1)

# Significance level
alpha <- 0.05

# Number of observations
n.full.1 <- length(cars[,1])

# Significance level corrected for number of observations
alpha.cor.full.1 <- alpha / n.full.1

# Find the cut-offs for the studentized residuals using the quantile function
z.full.1 <- qnorm(alpha.cor.full.1 / 2)

plot(stud.res.full.1,
     ylim = c(-4.5, 4.5),
     xaxt='n',
     xlab = "Observation i", 
     ylab = expression("r"[i]),
     main = "Studentized Residuals",
     #sub = "log(price) ~ carwidth + log(horsepower) + carbody + drivewheel + log(horsepower) * carbody",
     cex = 0.7,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
axis(1, at = seq(0, 205, by = 20), las = 1)
abline(h = 0,
       lwd = 2)
abline(h = -z.full.1,
       col = "red3",
       lwd = 2)
abline(h = z.full.1, 
       col = "red3",
       lwd = 2)
text(x = c(190),
     y = c(-4),
     expression("-z"[alpha]*""["/(2n)"]),
     cex = 1.3,
     col = "red3")
text(x = c(190),
     y = c(4),
     expression("z"[alpha]*""["/(2n)"]),
     cex = 1.3,
     col = "red3")
text(x = c(99),
     y = c(4.3),
     labels= c("ID 99, Nissan Clipper"),
     pos = 4)

## Car 99 appears is an outlier comparing residuals and car 99 is a very
# extreme observation according to Cooks distance. Try refitting without 
# cars 99 and 168
mod.full.2 <- lm(I(log(price)) ~ carwidth+ I(log(horsepower)) * carbody +
                 drivewheel, data = cars, subset = c(-99))
mod.red.2 <- lm(I(log(price)) ~ carwidth + I(log(horsepower)) + carbody + 
                drivewheel, data = cars, subset = c(-99)) 

# Partial F-test on nested models
anova(mod.red.2, mod.full.2)

# Plot the observed Y vs Y_hat.
ols_plot_obs_fit(mod.full.2, print_plot = TRUE)

# Compute Cook's distance and plot it to find influential observations
ols_plot_cooksd_chart(mod.full.2, print_plot = TRUE)
ols_plot_cooksd_bar(mod.full.2, print_plot = TRUE)

# Residuals
ols_plot_resid_fit(mod.full.2, print_plot = TRUE)

# Studentized residuals 
ols_plot_resid_stud(mod.full.2, print_plot = TRUE)

### Let the final model be the mod.full.2 (log(price) ~carwidth + 
# log(horsepower) * carbody + drivewheel, without car 99 )
mod.final <- lm(I(log(price)) ~ carwidth + drivewheel + I(log(horsepower)) * 
                  carbody, data = cars, subset = c(-99))

# What are the coefficients and their interpretation?
summary(mod.final)

# 95% confidence interval
confint(mod.final, level = 0.95)

# Cook's distance
C.D.final <- cooks.distance(mod.final)

# Plot the Cook's distance
plot(C.D.final,
     xaxt='n',
    xlab = "Observation i", 
    ylab = "D",
    main = "Cook's Distance for Final Model",
    #sub = "(Car ID 99 excluded)",
    cex = 0.7,
    cex.lab = 1.3,
    cex.main = 1.5,
    pch=21,
    bg = 1)
axis(1, at = seq(0, 205, by = 20), las = 1)
text(x = c(70,  129),
     y = c(0.167, 0.41),
     labels= c("ID 70, Buick Century", 
               "ID 129, Porsche Boxter"),
     pos = 1)

# Residuals
ols_plot_resid_fit(mod.final, print_plot = TRUE)

# Studentized residuals 
ols_plot_resid_stud(mod.final, print_plot = TRUE)
stud.res.final <- studres(mod.final)

# Significance level
alpha <- 0.05

# Subset the cars dataset
cars.subset <- cars[c(-99),] 

# Number of observations
n.final <- length(cars.subset[,1])

# Significance level corrected for number of observations
alpha.cor.final <- alpha / n.final

# Find the cut-offs for the studentized residuals using the quantile function
z.final <- qnorm(alpha.cor.final / 2)

plot(stud.res.final,
     ylim = c(-4.5, 4.5),
     xaxt='n',
     xlab = "Observation i", 
     ylab = expression("r"[i]),
     main = "Studentized Residuals for Final Model",
     #sub = "log(price) ~ carwidth + log(horsepower) + carbody + drivewheel + log(horsepower) * carbody",
     cex = 0.7,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
axis(1, at = seq(0, 205, by = 20), las = 1)
abline(h = 0,
       lwd = 2)
abline(h = -z.final,
       col = "red3",
       lwd = 2)
abline(h = z.final, 
       col = "red3",
       lwd = 2)
text(x = c(190),
     y = c(-4),
     expression("-z"[alpha]*""["/(2n)"]),
     cex = 1.3,
     col = "red3")
text(x = c(190),
     y = c(4),
     expression("z"[alpha]*""["/(2n)"]),
     cex = 1.3,
     col = "red3")

# Plot the observed Y vs Y_hat.
ols_plot_obs_fit(mod.final, print_plot = TRUE)

plot(mod.final$fitted.values ~ I(log(cars.subset$price)),
     xlab = "Observed log(price)",
     ylab = "Fitted log(price)",
     main = "Observed VS Fitted Response for Final Model",
     cex = 0.8,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
abline(0, 1, lwd = 1.5)



















