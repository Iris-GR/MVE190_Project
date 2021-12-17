setwd("~/Linear statistical models/mini-presentations/presentation_3")
library(vioplot)
library(geosphere)
library(dplyr)
library(ISLR)
library(leaps)
library(olsrr)
library(ggplot2)
library(rgl)  # load the package
library(scatterplot3d)  # load the package
# some nice graphics to compare the coefficients magnitude
library(arm)

# or similarly using dwplot, but I think this is a little buggy at times...
library(dotwhisker)
library(beepr)
#-----------------------------Loading data and modifying it-----------------------------#
AirBNB <- read.csv("AirBnB_NYCity_2019.csv")
AirBNB <- data.frame(AirBNB)
AirBNB <- subset(AirBNB, price > 0)

#Remove useless variables:
AirBNB <- subset(AirBNB,select=-c(id,host_id,name,host_name,
                                  last_review,reviews_per_month,calculated_host_listings_count,
                                  availability_365))

length(AirBNB$number_of_reviews)


#-----------------Distances----------------------------------------------------#
#Coorditas for rich neighbourhood:
pi <- c(-74.007819,40.718266)

#Make vector with latitude and longitude coordinates for each ABNB:
location <- cbind(longitude,latitude)


dist <- numeric(length(latitude))
#Make vector with distances to rich neighbourhood:
for(i in 1:length(latitude)){
  dist[i] <- distHaversine(location[i,],pi)
}
beep(3)
#Add column for dist:
AirBNB <- cbind(AirBNB,dist/1000)

attach(AirBNB)
#--------------------Room type and Neighbourhood--------------------------------------#

r_type<-factor(room_type)
hood <- factor(neighbourhood_group)

#------------------------------------------------------------------------------#

mod1<-lm(log(price)~dist+r_type+number_of_reviews,x=TRUE)
summary(mod1)

#:::::::::::: WE START WITH THE REGSUBSETS FUNCTION FROM THE LEAPS PACKAGE :::::::::::::::::::::::::::
#:::::::::::: (THE ATTEMPT WITH THE OLSRR PACKAGE IS FURTHER BELOW) ::::::::::::::::::::::::::::::::::

X <- mod1$x  # extract the design matrix
X <- X[,-c(1)] # remove the intercept

# form training and test data
set.seed(123)
frac <- 0.7 # training is 70% the whole dataset
trainindeces <- sample(seq(1,dim(X)[1]),round(dim(X)[1]*frac))


#Training data
xx <- as.matrix(X[trainindeces,]) 
yy <- log(price[trainindeces])


#Test data
xxt <- as.matrix(X[-trainindeces,])
yyt <- log(price[-trainindeces])



# columns 2 and 3 in xx are levels of categorical covariate. Let's force it in each model to fit
mod1 <-
  regsubsets(
    xx,
    yy,
    int = T,
    nbest = 1000,
    nvmax = dim(X)[2],
    method = c("ex"),
    really.big = T,
    force.in = c(2, 3)
  )
cleaps<-summary(mod1,matrix=T)
cleaps$which

# notice regsubsets is not really optimized to work with categorical covariates. 

# so just for your illustration, I create a vector realdimensions for the actual complexity of the several models
realdimension <- c(rep(2,2),3)  # we have 2 models with real dimension 2

# also, notice that when we use force.in then regsubsets rearranges the order of covariates so that the categorical
# dummy variables appear first. WE MUST TAKE THIS INTO ACCOUNT! SO here is another hack:
# we reorder the columns of xx and xxt according to the order found in cleaps$which
col.order <- c(colnames(cleaps$which))
xx <- xx[,col.order[-1]]   # now the order of columns in xx is consistent with the order in cleaps$which
xxt <- xxt[,col.order[-1]] # now the order of columns in xxt is consistent with the order in cleaps$which

pmses<-rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
  # here we fit training data
  mmr<-lm(yy~xx[,cleaps$which[jj,-1]==T])
  # (i) obtain the design matrix for the given model
  # by selecting "active variables" in TEST data
  design <- xxt[,cleaps$which[jj,-1]==T]
  # (ii) add a column of ones for the intercept
  design <- cbind(rep(1,dim(xxt)[1]),design)
  PEcp<-sum((yyt-design%*%mmr$coef)^2)/length(yyt)
  # the above is the pMSE for the current model.
  # let's store it in a vector
  pmses[jj]<-PEcp
}
pmses

windows()
plot(realdimension,pmses) # plot the pmses vs the real dimensions of the models

pmses
# so a quite good model seem to have realdimension == 2. Let's find the smallest pmse for a model with dimension 3 
pmses[realdimension==2]
# this has to be the one with "origin" (obviously), "horsepower" and "weight" which has 14.622 (this is model #6)

cleaps$which[1,]  # having pmse = 0.272191


# Now let's compare predictions using such model with the observed data from the test set
chosenmod <- lm(yy~xx[,cleaps$which[1,-1]==T])  # fit the training data with our favorite model  (model #6)
betachosen <- chosenmod$coefficients  # parameter estimates from our favorite model on training data
# select "active variables" in TEST data, corresponding to model #6 
designchosen <- xxt[,cleaps$which[1,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(xxt)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set

windows()
plot(yyt,ypred)  # compare predictions with test responses
abline(0,1)   # not bad!


#:::::::::::::::: WE NOW TRY THE OLSRR PACKAGE :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# notice, unfortunately it does not seem possible to set the size of the data used for training and testing.
# IN fact I DO NOT THINK THE DATASET IS EVER SPLIT INTO TRAINING AND TEST DATA
# this is however ok, as the measures considered by the package (AIC, Cp etc) can be defined without need training and test data.
# But this also mean that we are not going to select a model based on out-of-sample prediction


modols<-lm(log(price)~AirBNB$dist+r_type+number_of_reviews,data=AirBNB)
out<-ols_step_all_possible(modols) # all subsets regressions!

?ols_step_all_possible()


plot(out)  # from the Mallow's Cp it seems a model with size 3 is good, and the plot shows that model #22 is good.
# Let's go find such a model
out
# So we first look at the value of N, for N=3. There we see that Index=22 for N=3 corresponds to a model having Mallow's Cp = 2.16.
# This is the model with "origin", "horsepower" and "weight".
# We selected the same model as with regsubsets however I believe this is not necessarily to be expected.
# With regsubsets we had to force in the categorical covariate, but olsrr also allows for models without the origin covariate.
# This looks really nice and practical!

#------------------------------------------------------------------------------#

truemod<-lm(log(price)~r_type+dist+number_of_reviews+minimum_nights,x=TRUE)
summary(truemod)

X1 <- truemod$x  # extract the design matrix
X1 <- X1[,-c(1)] # remove the intercept

# form training and test data
set.seed(123)
frac <- 0.7 # training is 70% the whole dataset
trainindeces <- sample(seq(1,dim(X1)[1]),round(dim(X1)[1]*frac))


#Training data
xx <- as.matrix(X1[trainindeces,]) 
yy <- log(price[trainindeces])


#Test data
xxt <- as.matrix(X1[-trainindeces,])
yyt <- log(price[-trainindeces])

head(xx)
dim(xx)
xx
# columns 2 and 3 in xx are levels of categorical covariate. Let's force it in each model to fit
truemod <-
  regsubsets(
    xx,
    yy,
    int = T,
    nbest = 1000,
    nvmax = dim(X1)[2],
    method = c("ex"),
    really.big = T,
    force.in = c(1,2)
  )
cleaps<-summary(truemod,matrix=T)
cleaps$which

# notice regsubsets is not really optimized to work with categorical covariates. 

# so just for your illustration, I create a vector realdimensions for the actual complexity of the several models
realdimension <- c(rep(2,3),rep(3,3),4)  # we have 2 models with real dimension 2


# also, notice that when we use force.in then regsubsets rearranges the order of covariates so that the categorical
# dummy variables appear first. WE MUST TAKE THIS INTO ACCOUNT! SO here is another hack:
# we reorder the columns of xx and xxt according to the order found in cleaps$which
col.order <- c(colnames(cleaps$which))
xx <- xx[,col.order[-1]]   # now the order of columns in xx is consistent with the order in cleaps$which
xxt <- xxt[,col.order[-1]] # now the order of columns in xxt is consistent with the order in cleaps$which

pmses<-rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
  # here we fit training data
  mmr<-lm(yy~xx[,cleaps$which[jj,-1]==T])
  # (i) obtain the design matrix for the given model
  # by selecting "active variables" in TEST data
  design <- xxt[,cleaps$which[jj,-1]==T]
  # (ii) add a column of ones for the intercept
  design <- cbind(rep(1,dim(xxt)[1]),design)
  PEcp<-sum((yyt-design%*%mmr$coef)^2)/length(yyt)
  # the above is the pMSE for the current model.
  # let's store it in a vector
  pmses[jj]<-PEcp
}
pmses
realdimension

windows()
plot(realdimension,pmses, main="Prediction MSE", xlab="Number of covariates", ylab="pMSE") # plot the pmses vs the real dimensions of the models

pmses
# so a quite good model seem to have realdimension == 2. Let's find the smallest pmse for a model with dimension 3 
pmses[realdimension==2]
# this has to be the one with "origin" (obviously), "horsepower" and "weight" which has 14.622 (this is model #6)

cleaps$which

cleaps$which[1,]  # having pmse = 0.272191


# Now let's compare predictions using such model with the observed data from the test set
chosenmod <- lm(yy~xx[,cleaps$which[1,-1]==T])  # fit the training data with our favorite model  (model #6)
betachosen <- chosenmod$coefficients  # parameter estimates from our favorite model on training data
# select "active variables" in TEST data, corresponding to model #6 
designchosen <- xxt[,cleaps$which[1,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(xxt)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set

windows()
plot(yyt,ypred)  # compare predictions with test responses
abline(0,1)   # not bad!

#---------------------------------- Interactions ------------------------------#

full_interactions<-lm(log(price)~dist+r_type+dist:r_type,x=TRUE)
summary(full_interactions)

red_interactions <- lm(log(price)~dist+r_type)
summary(full_interactions)

length(price)

windows()
plot(room_type,log(price))


plot3d(dist,r_type,log(price))

anova(red_interactions,full_interactions)
qf(1-0.05,1,length(price)-4)

#----------------------------------Interactions pred-----------------------------------------#

truemod1<-lm(log(price)~r_type+dist+dist:r_type,x=TRUE)
summary(truemod1)


X1 <- truemod1$x  # extract the design matrix
X1 <- X1[,-c(1)] # remove the intercept

# form training and test data
set.seed(123)
frac <- 0.7 # training is 70% the whole dataset
trainindeces <- sample(seq(1,dim(X1)[1]),round(dim(X1)[1]*frac))


#Training data
xx <- as.matrix(X1[trainindeces,]) 
yy <- log(price[trainindeces])


#Test data
xxt <- as.matrix(X1[-trainindeces,])
yyt <- log(price[-trainindeces])

head(xx)
dim(xx)

# columns 2 and 3 in xx are levels of categorical covariate. Let's force it in each model to fit
truemod <-
  regsubsets(
    xx,
    yy,
    int = T,
    nbest = 1000,
    nvmax = dim(X1)[2],
    method = c("ex"),
    really.big = T,
    force.in = c(1,2)
  )

summary(truemod)
cleaps<-summary(truemod,matrix=T)
cleaps$which

# notice regsubsets is not really optimized to work with categorical covariates. 

# so just for your illustration, I create a vector realdimensions for the actual complexity of the several models
realdimension <- c(rep(2,2),3)  # we have 2 models with real dimension 2

# also, notice that when we use force.in then regsubsets rearranges the order of covariates so that the categorical
# dummy variables appear first. WE MUST TAKE THIS INTO ACCOUNT! SO here is another hack:
# we reorder the columns of xx and xxt according to the order found in cleaps$which
col.order <- c(colnames(cleaps$which))
xx <- xx[,col.order[-1]]   # now the order of columns in xx is consistent with the order in cleaps$which
xxt <- xxt[,col.order[-1]] # now the order of columns in xxt is consistent with the order in cleaps$which

head(xx)



pmses<-rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
  # here we fit training data
  mmr<-lm(yy~xx[,cleaps$which[jj,-1]==T])
  # (i) obtain the design matrix for the given model
  # by selecting "active variables" in TEST data
  design <- xxt[,cleaps$which[jj,-1]==T]
  # (ii) add a column of ones for the intercept
  design <- cbind(rep(1,dim(xxt)[1]),design)
  PEcp<-sum((yyt-design%*%mmr$coef)^2)/length(yyt)
  # the above is the pMSE for the current model.
  # let's store it in a vector
  pmses[jj]<-PEcp
}
pmses
realdimension

windows()
plot(realdimension,pmses) # plot the pmses vs the real dimensions of the models

pmses
# so a quite good model seem to have realdimension == 2. Let's find the smallest pmse for a model with dimension 3 
pmses[realdimension==2]
# this has to be the one with "origin" (obviously), "horsepower" and "weight" which has 14.622 (this is model #6)

cleaps$which

cleaps$which[3,]  # having pmse = 0.272191


# Now let's compare predictions using such model with the observed data from the test set
chosenmod <- lm(yy~xx[,cleaps$which[3,-1]==T])  # fit the training data with our favorite model  (model #6)
betachosen <- chosenmod$coefficients  # parameter estimates from our favorite model on training data
# select "active variables" in TEST data, corresponding to model #6 
designchosen <- xxt[,cleaps$which[3,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(xxt)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set

windows()
plot(yyt,ypred,main="Test data analysis: y_pred vs y_test",xlab="y_test", ylab="y_pred")  # compare predictions with test responses
abline(0,1,col="red")   # not bad!
