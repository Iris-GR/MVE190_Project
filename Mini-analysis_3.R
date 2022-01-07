# Mini-analysis 3

# Set working directory

#------------------ Libraries -------------------------------------------------#
#Here are all the libraries used (or potentially not used) presented.
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
#--------------------Loading data and modifying it-----------------------------#
#Load the data used in the project:
AirBNB <- read.csv("AirBnB_NYCity_2019.csv")
AirBNB <- data.frame(AirBNB)

#We remove price = 0 to avoid error with the logarithm (and because they are 
# weird):
AirBNB <- subset(AirBNB, price > 0)

#We also remove useless variables:
AirBNB <- subset(AirBNB,select=-c(
  id,
  host_id,
  name,
  host_name,
  last_review,
  reviews_per_month,
  calculated_host_listings_count,
  availability_365
))

#-----------------Distances----------------------------------------------------#
#In this section, the distance covariate is calculated. It is still the distance
# to TriBeCa we are looking at.


#Coordinates for TriBeCa (a rich neighbourhood located in Manhattan):
pi <- c(-74.007819,40.718266)

#Make vector with latitude and longitude coordinates for each ABNB:
location <- cbind(longitude,latitude)

#Initialise dist as a vector with length latitute.
dist <- numeric(length(latitude))

#Make vector with distances to rich neighbourhood:
for(i in 1:length(latitude)){
  dist[i] <- distHaversine(location[i,],pi)
}
#beep(3)

#We add a column for dist (in km) in the AirBNB dataframe:
AirBNB <- cbind(AirBNB,dist/1000) #NOTE: Unit of dist is km.

attach(AirBNB) 
#--------------------Room type and Neighbourhood-------------------------------#
#Here we handle the categorial covariates roomtype and neighbourhood group.
#That is, we factorise them inte different levels with the function factor:

r_type<-factor(room_type) #"Entire home/apt" is baseline.
hood <- factor(neighbourhood_group) 

#---------------------Fit model------------------------------------------------#
#Here we fit the first multivariate model. It contains two continuous covariates
#(dist and number of reviews), and one categorical (roomtype, baseline 
#"Entire home/apt"). The logarithmic transformation of the response variabel
#is also used.

mod1<-lm(log(price)~dist+r_type+number_of_reviews,x=TRUE)
summary(mod1)

#----------------- test and training data to see predictability ---------------#
#Here, the regsubset function is used to test the predictability/robustness
#of the model. The dataset is divided into 70% training data and 30% test data.
#Parameters are fitted to the training data, and the test data observations
#are then plotted against the model with parameters fitted to the training
#data, with test data covariates, to see if there are large discrepancies.

X <- mod1$x  #The design matrix is extracted
X <- X[,-c(1)] #The intercept is removed

#Training and test data is formed
set.seed(123)
frac <- 0.7 #Training = 70% whole dataset
trainindeces <- sample(seq(1,dim(X)[1]),round(dim(X)[1]*frac))


#Training data
xx <- as.matrix(X[trainindeces,]) 
yy <- log(price[trainindeces])


#Test data
xxt <- as.matrix(X[-trainindeces,])
yyt <- log(price[-trainindeces])



#Note that here, in xx, the columns 2 and 3 are levels of the categorical 
#covariate. We therefore force these levels into each model to fit:
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

#However, regsubsets is not suited for categorical covariates. Hence,
#a vector of realdimensions for the ACTUAL complexity of the models are 
#created:
realdimension <- c(rep(2,2),3)  # There are 2 models with real dimension 2

#When force.in is used, the order of covariates is rearranged by regsubsets 
#Hence, the categorical dummy variables will be first. 
#This needs to be taken into account, hence:
#reorder columns of xx and xxt by the order in cleaps$which
col.order <- c(colnames(cleaps$which))
xx <- xx[,col.order[-1]]   # order columns xx = order cleaps$which
xxt <- xxt[,col.order[-1]] # order columns xxt = order cleaps$which

pmses<-rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
  #training data is fit 
  mmr<-lm(yy~xx[,cleaps$which[jj,-1]==T])
  #Select "active variables" TEST data to
  #get the given model's design matrix
  design <- xxt[,cleaps$which[jj,-1]==T]
  #include column with ones (intercept)
  design <- cbind(rep(1,dim(xxt)[1]),design)
  PEcp<-sum((yyt-design%*%mmr$coef)^2)/length(yyt) #pMSE current model
  #store pMSE in vector:
  pmses[jj]<-PEcp
}
pmses

grDevices::windows()
plot(realdimension,pmses) # plot: model's pmses vs real dimensions 

pmses
#realdimension == 2. 
#find smallest pmse for mod with dim == 2
pmses[realdimension==2]

cleaps$which[1,]  # having pmse = 0.272191


#compare observed data in test set with predictions from our model
chosenmod <- lm(yy~xx[,cleaps$which[1,-1]==T])  #fit chosen model with training data 
betachosen <- chosenmod$coefficients  # parameter estimates 
#Take TEST data "active variables"  
designchosen <- xxt[,cleaps$which[1,-1]==T]
#+ column for ones
designchosen <- cbind(rep(1,dim(xxt)[1]),designchosen)
ypred <- designchosen%*%betachosen  # covariates test set to get predictions

#plot
grDevices::windows()
plot(yyt,ypred)  # plot: predict vs test observations
abline(0,1)  


#-------------- OLS package ---------------------------------------------------#
#CON: Cannot set size for training and test data - not even split...
#Instead, package contains other measures (AIC, Cp, etc).
#Hence, do not get model through out-of-sample pred

modols<-lm(log(price)~AirBNB$dist+r_type+number_of_reviews,data=AirBNB)
out<-ols_step_all_possible(modols) #regression of all subsets.


grDevices::windows()
plot(out) #Decide model after Mallow's Cp.
#Look in out for the best model:
out

#------------------------------------------------------------------------------#
#Here the same procedure is done again for new model. Here, covariates are
#roomtype, dist, number of reviews, and minimum nights (1 cat, 3 cont).
#The procedure is otherwise the same as before, completely.

truemod<-lm(log(price)~r_type+dist+number_of_reviews+minimum_nights,x=TRUE)
summary(truemod)

#Design matrix
X1 <- truemod$x  
X1 <- X1[,-c(1)] 

#70% training/30% test
set.seed(123)
frac <- 0.7
trainindeces <- sample(seq(1,dim(X1)[1]),round(dim(X1)[1]*frac))

xx <- as.matrix(X1[trainindeces,]) 
yy <- log(price[trainindeces])



xxt <- as.matrix(X1[-trainindeces,])
yyt <- log(price[-trainindeces])

head(xx)
dim(xx)
xx

#Force in cat levels
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

#Real dim
realdimension <- c(rep(2,3),rep(3,3),4) 


#Order after cleaps$which
col.order <- c(colnames(cleaps$which))
xx <- xx[,col.order[-1]]   
xxt <- xxt[,col.order[-1]] 

pmses<-rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
  
  mmr<-lm(yy~xx[,cleaps$which[jj,-1]==T])
  
  design <- xxt[,cleaps$which[jj,-1]==T]
  
  design <- cbind(rep(1,dim(xxt)[1]),design)
  PEcp<-sum((yyt-design%*%mmr$coef)^2)/length(yyt)
  
  pmses[jj]<-PEcp
}
pmses
realdimension

#Find best model with pmse
grDevices::windows()
plot(realdimension,
     pmses,
     main = "Prediction MSE",
     xlab = "Number of covariates",
     ylab = "pMSE")

pmses

pmses[realdimension==2]

cleaps$which

cleaps$which[1,]

#Compare test vs predicted
chosenmod <- lm(yy~xx[,cleaps$which[1,-1]==T]) 
betachosen <- chosenmod$coefficients 

designchosen <- xxt[,cleaps$which[1,-1]==T]

designchosen <- cbind(rep(1,dim(xxt)[1]),designchosen)
ypred <- designchosen%*%betachosen  

grDevices::windows()
plot(yyt,ypred) 
abline(0,1)   

#---------------------------------- Interactions ------------------------------#
#Here we also take into account interactions between the covariates. We look 
#here at interaction between roomtype and distance. 

full_interactions<-lm(log(price)~dist+r_type+dist:r_type,x=TRUE)
summary(full_interactions)

#To see if interactions are needed, we do a partial F test, where
#the full model contains the interaction term, and the reduced does not.
red_interactions <- lm(log(price)~dist+r_type)
summary(full_interactions)


grDevices::windows()
plot(room_type,log(price))


plot3d(dist,r_type,log(price))

#Anova is used for this partial F test (it works because the models
# are nested). H0 is that the reduced model will not increase SSerror enough
#for it to be problematic (so H0 = reduced model ok)
anova(red_interactions,full_interactions)
qf(1-0.05,1,length(price)-4)
#Fobs>Fstat -> reject H0 and continue on with full model.

#----------------------------------Interactions pred---------------------------#
#Here we use our full model for the prediction.

truemod1<-lm(log(price)~r_type+dist+dist:r_type,x=TRUE)
summary(truemod1)

#Design matrix
X1 <- truemod1$x  
X1 <- X1[,-c(1)] 

#Training and test data
set.seed(123)
frac <- 0.7 
trainindeces <- sample(seq(1,dim(X1)[1]),round(dim(X1)[1]*frac))



xx <- as.matrix(X1[trainindeces,]) 
yy <- log(price[trainindeces])



xxt <- as.matrix(X1[-trainindeces,])
yyt <- log(price[-trainindeces])

head(xx)
dim(xx)

#Force in
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

#Real dim
realdimension <- c(rep(2,2),3)  

#Order:
col.order <- c(colnames(cleaps$which))
xx <- xx[,col.order[-1]] 
xxt <- xxt[,col.order[-1]] 

#pMSE calc
pmses<-rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
  
  mmr<-lm(yy~xx[,cleaps$which[jj,-1]==T])
  
  design <- xxt[,cleaps$which[jj,-1]==T]
  
  design <- cbind(rep(1,dim(xxt)[1]),design)
  PEcp<-sum((yyt-design%*%mmr$coef)^2)/length(yyt)
  
  pmses[jj]<-PEcp
}
pmses
realdimension

grDevices::windows()
plot(realdimension,pmses) #Compare models by pMSE

pmses

pmses[realdimension==2]

cleaps$which

cleaps$which[3,] 

#Pred vs test
chosenmod <- lm(yy~xx[,cleaps$which[3,-1]==T]) 
betachosen <- chosenmod$coefficients  

designchosen <- xxt[,cleaps$which[3,-1]==T]

designchosen <- cbind(rep(1,dim(xxt)[1]),designchosen)
ypred <- designchosen%*%betachosen  

grDevices::windows()
plot(yyt,
     ypred,
     main = "Test data analysis: y_pred vs y_test",
     xlab = "y_test",
     ylab = "y_pred")  
abline(0,1,col="red")   
