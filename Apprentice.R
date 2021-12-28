# The code for the apprentice dataset 

# Set the working directory (Set your own)
setwd("~/MVE190_Data_Management/git_folder_mve190_project/MVE190_Project")

# Load libraries
library(MASS)
library(vioplot)
library(AER)

# Read the .txt file with the apprentice data
apprentice <- read.table('apprentice.txt', sep='\t')

# Set column names and call dataset "data"
data.0 <- setNames(apprentice, c("county",
                               "distance",
                               "apprentices",
                               "population",
                               "urbanization",
                               "direction"))
# Explanation of variables 

# • county: name of the county
# • distance: distance of the county in km (using some hypothetical centroid in
#   the county) from Edinburgh
# • apprentices: number of apprentices for the given county who moved to Edinburgh.
# • population: county population in thousands;
# • urbanization: percentage of county urbanization (that is percentage of 
#   county population living in urban settlements);
# • direction: (categorical) direction of the county compared to Edinburgh, 
#   1=North, 2=West, 3=South (I guess East is not considered since Edinburgh is 
#   on the east coast). Use North as baseline.

# Change names of levels of the categorical variable direction
direction <- factor(data.0$direction, labels = c("North", "West", "South"))

# Replace the old direction variable 
data <- data.frame(data.0[,1:5])
data$direction <- direction

#### Get overview of data ####
# County
county.fac <- as.factor(data$county)
plot(data$apprentices ~ county.fac)

# distance
plot(data$apprentices ~ data$distance)
plot(I(log(data$apprentices)) ~ data$distance)
plot(data$apprentices ~ I(log(data$distance)))
plot(data$apprentices ~ I(sqrt(data$distance)))
plot(data$apprentices ~ I(1/(data$distance)))

# population
plot(data$apprentices ~ data$population)
plot(I(log(data$apprentices)) ~ data$population)
plot(I(log(data$apprentices)) ~ I(log(data$population)))
plot(data$apprentices ~ I(log(data$population)))
plot(data$apprentices ~ I(sqrt(data$population)))
plot(data$apprentices ~ I(1/(data$population)))

# urbanization
plot(data$apprentices ~ data$urbanization)
plot(I(log(data$apprentices)) ~ data$urbanization)
plot(data$apprentices ~ I(log(data$urbanization)))
plot(data$apprentices ~ I(sqrt(data$urbanization)))
plot(data$apprentices ~ I(1/(data$urbanization)))

# Direction
plot(data$apprentices ~ data$direction)
plot(I(log(data$apprentices)) ~ data$direction)
vioplot(data$apprentices ~ data$direction, 
        col = "turquoise3",
        cex = 1.1,
        cex.axis = 1.3,
        cex.main = 1.5,
        cex.lab = 2,
        xlab = "Direction",
        ylab = "Nr. Apprentices",
        main = "Distribution of Apprentices w.r.t Directions from Edinburgh",)

# Apply transformation to the variables population and distance to get similar 
# scale, then add to data.frame
data$population.t <- log(data$population)
data$distance.t <- log(data$distance)

# Print data to check
data

########################################################################
#######################NPoisson Regression #############################

# Attach the data frame
attach(data)

#### Try different poisson models ####

# Larger model
p.model.a <-
  glm(apprentices ~ urbanization + direction + population.t + distance.t,
      family = "poisson")
summary(p.model.a)

# Test interaction effects
p.model.b <-
  #glm(apprentices ~ urbanization + direction + population.t + distance.t + population.t* distance.t,
      #family = "poisson")
    # glm(apprentices ~ urbanization + direction + population.t + distance.t + distance.t* direction,
    # family = "poisson")
  # glm(apprentices ~ urbanization + direction + population.t + distance.t + distance.t* urbanization,
  #     family = "poisson")
  # glm(apprentices ~ urbanization + direction + population.t + distance.t + population.t* direction,
  #     family = "poisson")
   glm(apprentices ~ urbanization + direction + population.t + distance.t + population.t* urbanization,
       family = "poisson")

summary(p.model.b)

# Remove urbanization
p.model.c <-
  glm(apprentices ~  direction + population.t + distance.t,
      family = "poisson")
summary(p.model.c)

# Remove direction
p.model.d <-
  glm(apprentices ~ urbanization + population.t + distance.t,
      family = "poisson")
summary(p.model.d)

# Remove population
p.model.e <-
  glm(apprentices ~ urbanization + direction  + distance.t,
      family = "poisson")
summary(p.model.e)

# Remove distance
p.model.f <-
  glm(apprentices ~ urbanization + direction + population.t ,
      family = "poisson")
summary(p.model.f)

# Urbanization and direction appear to be the least significant 
# Try a model without them
p.model.g <- glm(apprentices ~ population.t + distance.t, family = "poisson")
summary(p.model.g)

# Remove everything except distance
p.model.h <- glm(apprentices ~ distance.t, family = "poisson")
summary(p.model.h)

# Likelihood-ratio tests for deciding if direction and urbanization 
# should be in model. 

# The chi squared statistics for p = 0.05 and 1 or 2 degrees of freedom
chi.square.stat.1 <- qchisq(0.95, 1)
chi.square.stat.2 <- qchisq(0.95, 2)

# Full model: apprentices ~ urbanization + direction + population.t + distance.t
# First reduced model: apprentices ~  direction + population.t + distance.t
if (anova(p.model.c, p.model.a)[2, 4] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Second reduced model: apprentices ~ urbanization + population.t + distance.t
if (anova(p.model.d, p.model.a)[2, 4] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Third reduced model: apprentices ~ population.t + distance.t
if (anova(p.model.g, p.model.a)[2, 4] > chi.square.stat.2) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Now test the third reduced model against the larger first reduced model
if (anova(p.model.g, p.model.c)[2, 4] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Test the third reduced model against the larger second reduced model
if (anova(p.model.g, p.model.d)[2, 4] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Conclusion of likelihood-ratio tests is that the full model (urbanization,
# direction, population.t and distance.t covariates) should be used. 
# Rename the full model to p.model (p for Poisson) 
p.model <- p.model.a

# To check if the assumption for the Poisson distribution is satisfied do a
# dispersion test. If alternative hypothesis is true the poisson 
# distribution is not likely
dispersiontest(p.model, alternative="two.sided")

# The covariance matrix for the parameters
p.cov.mat <- summary(p.model)$cov.unscaled

# The standard errors of Poisson model
p.SE.b0 <- sqrt(p.cov.mat[1,1]) 
p.SE.b1 <- sqrt(p.cov.mat[2,2]) 
p.SE.b2 <- sqrt(p.cov.mat[3,3]) 
p.SE.b3 <- sqrt(p.cov.mat[4,4]) 
p.SE.b4 <- sqrt(p.cov.mat[5,5]) 
p.SE.b5 <- sqrt(p.cov.mat[6,6]) 

# 95% Confidence interval for parameters
confint.default(p.model)

# Estimate of the linear predictor
p.Xb.hat <- predict(p.model, se.fit = T)

# Estimated response (mu_i) (same as mu_hat <- exp(Xb_hat$fit))
p.mu.hat <- predict(p.model, type = "response")

# 95% confidence interval for linear predictor X_i*b
# (se.fit is the standard error of eta)
p.ciXb.low <- p.Xb.hat$fit + qnorm(0.05/2) * p.Xb.hat$se.fit
p.ciXb.high <- p.Xb.hat$fit - qnorm(0.05/2) * p.Xb.hat$se.fit

# Exponentiatiate to get the interval for mu_i
p.cimu.low <- exp(p.ciXb.low)
p.cimu.high <- exp(p.ciXb.high)

# Influence measure
p.influence <- influence(p.model)

# Pearson's residuals
p.pearsons.r <- p.influence$pear.res

# Standardized Pearson's residuals
p.pearsons.r.std <- p.pearsons.r/ sqrt(1 - p.influence$hat)

# Multiple comparisons correction for number of observations

# Number of observations
n <- length(apprentices)

# Significance level
alpha <- 0.05

# Significance level corrected for number of observations
alpha.cor <- alpha / n

# Find the cut-offs using the quantile function
z <- qnorm(alpha.cor / 2)

# Plot Standardized Pearson's residuals for poisson model
plot(p.pearsons.r.std, 
     xlab = "Observation i", 
     ylab = expression(r[i]), 
     ylim = c(-5, 5),
     main = "Std. Pearson Residuals for Poisson Model",
     sub = "apprentices ~ urbanization + direction + log(population) + log(distance)",
     cex = 1.3,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
abline(h = 0,
       lwd = 2)
abline(h = -z,
       col = "red3",
       lwd = 2)
abline(h = z, 
       col = "red3",
       lwd = 2)

# Cook's distance
p.c.d <- cooks.distance(p.model)

# Plot the Cook's distance
plot(p.c.d, 
     xlab = "Observation i", 
     ylab = "D",
     main = "Cook's distance for Poisson Model",
     sub = "apprentices ~ urbanization + direction + log(population) + log(distance)",
     cex = 1.3,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)

########################################################################
################## Negative Binomial Regression ########################
attach(data)

#### Try different NB models (same order as for Poisson to be able to compare)
# Larger model
nb.model.a <-
  glm.nb(apprentices ~ urbanization + direction + population.t + distance.t)
summary(nb.model.a)

# Test interaction effects
nb.model.b <-
  #glm.nb(apprentices ~ urbanization + direction + population.t + distance.t + population.t* distance.t)
  # glm.nb(apprentices ~ urbanization + direction + population.t + distance.t + distance.t* direction)
  # glm.nb(apprentices ~ urbanization + direction + population.t + distance.t + distance.t* urbanization)
  # glm.nb(apprentices ~ urbanization + direction + population.t + distance.t + population.t* direction)
  glm.nb(apprentices ~ urbanization + direction + population.t + distance.t + population.t* urbanization)

summary(nb.model.b)

# Remove urbanization
nb.model.c <- glm.nb(apprentices ~  direction + population.t + distance.t)
summary(nb.model.c)

# Remove direction
nb.model.d <- glm.nb(apprentices ~ urbanization + population.t + distance.t)
summary(nb.model.d)

# Remove population
nb.model.e <- glm.nb(apprentices ~ urbanization + direction  + distance.t)
summary(nb.model.e)

# Remove distance
nb.model.f <- glm.nb(apprentices ~ urbanization + direction + population.t)
summary(nb.model.f)

# Urbanization and direction appear to be the least significant 
# Try a model without them
nb.model.g <- glm.nb(apprentices ~ population.t + distance.t)
summary(nb.model.g)

# The distance appears to be most significant 
nb.model.h <- glm.nb(apprentices ~ distance.t)
summary(nb.model.h)

### Likelihood-ratio tests for deciding if direction, urbanization or 
# population.t should be in model. 

# The chi squared statistics for p = 0.05 and 1, 2 or 3 degrees of freedom
chi.square.stat.1 <- qchisq(0.95, 1)
chi.square.stat.2 <- qchisq(0.95, 2)
chi.square.stat.3 <- qchisq(0.95, 3)

# Full model: apprentices ~ urbanization + direction + population.t + distance.t
# Reduced model: apprentices ~  direction + population.t + distance.t
if (anova(nb.model.c, nb.model.a)[2, 7] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Full model: apprentices ~ urbanization + direction + population.t + distance.t
# Reduced model: apprentices ~ urbanization + population.t + distance.t
if (anova(nb.model.d, nb.model.a)[2, 7] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Full model: apprentices ~ urbanization + direction + population.t + distance.t
# Reduced model: apprentices ~ population.t + distance.t
if (anova(nb.model.g, nb.model.a)[2, 7] > chi.square.stat.2) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Full model: apprentices ~ urbanization + direction + population.t + distance.t
# Reduced model: apprentices ~  distance.t
if (anova(nb.model.h, nb.model.a)[2, 7] > chi.square.stat.3) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Full model: apprentices ~  direction + population.t + distance.t
# Reduced model: apprentices ~ population.t + distance.t
if (anova(nb.model.g, nb.model.c)[2, 7] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Full model: apprentices ~ urbanization + population.t + distance.t
# Reduced model: apprentices ~ population.t + distance.t
if (anova(nb.model.g, nb.model.d)[2, 7] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Full model: apprentices ~ population.t + distance.t
# Reduced model: apprentices ~ distance.t
if (anova(nb.model.h, nb.model.g)[2, 7] > chi.square.stat.1) {
  sprintf("Reject H0 -> go for larger model")
} else {
  sprintf("Accept H0 -> go for smaller model")
}

# Conclusion of likelihood-ratio tests is that either 
# apprentices ~ urbanization + population.t + distance.t
# or 
# apprentices ~ direction + population.t + distance.t
# is best. The model with urbanization has a lower LR statistic which indicates
# that less unexplained variance (?) is gained reducing from full model to that 
# one with direction. So go with 
# apprentices ~ urbanization + population.t + distance.t

# Rename the chosen model to nb.model (p for Poisson) 
nb.model <- nb.model.d

# The covariance matrix for the parameters
nb.cov.mat <- summary(nb.model)$cov.unscaled

# The standard errors of Poisson model
nb.SE.b0 <- sqrt(nb.cov.mat[1,1]) 
nb.SE.b1 <- sqrt(nb.cov.mat[2,2]) 
nb.SE.b2 <- sqrt(nb.cov.mat[3,3]) 
nb.SE.b3 <- sqrt(nb.cov.mat[4,4]) 

# 95% Confidence interval for parameters
confint.default(nb.model)

# Estimate of the linear predictor
nb.Xb.hat <- predict(nb.model, se.fit = T)

# Estimated response (mu_i) (same as mu_hat <- exp(Xb_hat$fit))
nb.mu.hat <- predict(nb.model, type = "response")

# 95% confidence interval for linear predictor X_i*b
# (se.fit is the standard error of eta)
nb.ciXb.low <- nb.Xb.hat$fit + qnorm(0.05/2) * nb.Xb.hat$se.fit
nb.ciXb.high <- nb.Xb.hat$fit - qnorm(0.05/2) * nb.Xb.hat$se.fit

# Exponentiatiate to get the interval for mu_i
nb.cimu.low <- exp(nb.ciXb.low)
nb.cimu.high <- exp(nb.ciXb.high)

# Influence measure
nb.influence <- influence(nb.model)

# Pearson's residuals
nb.pearsons.r <- nb.influence$pear.res

# Standardized Pearson's residuals
nb.pearsons.r.std <- nb.pearsons.r/ sqrt(1 - nb.influence$hat)

# Multiple comparisons correction for number of observations

# Number of observations
n <- length(apprentices)

# Significance level
alpha <- 0.05

# Significance level corrected for number of observations
alpha.cor <- alpha / n

# Find the cut-offs using the quantile function
z <- qnorm(alpha.cor / 2)

# Plot Standardized Pearson's residuals for NB model
plot(nb.pearsons.r.std, 
     xlab = "Observation i", 
     ylab = expression(r[i]), 
     ylim = c(-5, 5),
     main = "Std. Pearson Residuals for Negative Binomial Model 1",
     sub = "apprentices ~ urbanization + log(population) + log(distance)",
     cex = 1.3,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
abline(h = 0,
       lwd = 2)
abline(h = -z,
       col = "red3",
       lwd = 2)
abline(h = z, 
       col = "red3",
       lwd = 2)

# Cook's distance
nb.c.d <- cooks.distance(nb.model)

# Plot the Cook's distance
plot(nb.c.d, 
     xlab = "Observation i", 
     ylab = "D",
     main = "Cook's Distance for Negative Binomial Model",
     sub = "apprentices ~ urbanization + log(population) + log(distance)",
     cex = 1.3,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
text(x = c(8, 9, 28, 29),
     y = c(0.20, 0.10, 0.20, 0.10),
     labels= c("Selkirk", "Lanark", "Inverness", "Ross"),
     pos = 4)
text(x = c(1, 3),
     y = c(0.16, 0.125),
     labels= c("Midlothian", "East Lothian"),
     pos = 4)
symbols(x = c(28, 29),
        y = c(0.208, 0.11),
        circles = c(0.5, 0.5),
        add = T, 
        inches = F, 
        lwd = 2,
        fg = c("red3", "red3"))

# Influence measure for nb.model.a the model with 
# apprentices ~ urbanization + direction + population.t + distance.t
# which is same as the chosen model for Poisson to be able to compare with each 
# other.
nb.influence.a <- influence(nb.model.a)

# Pearson's residuals
nb.pearsons.r.a <- nb.influence.a$pear.res

# Standardized Pearson's residuals
nb.pearsons.r.std.a <- nb.pearsons.r.a/ sqrt(1 - nb.influence.a$hat)

# Multiple comparisons correction for number of observations

# Plot Standardized Pearson's residuals for NB model
plot(nb.pearsons.r.std.a, 
     xlab = "Observation i", 
     ylab = expression(r[i]), 
     ylim = c(-5, 5),
     main = "Std. Pearson Residuals for Negative Binomial Model 2",
     sub = "apprentices ~ urbanization + direction + log(population) + log(distance)",
     cex = 1.3,
     cex.lab = 1.3,
     cex.main = 1.5,
     pch=21,
     bg = 1)
abline(h = 0,
       lwd = 2)
abline(h = -z,
       col = "red3",
       lwd = 2)
abline(h = z, 
       col = "red3",
       lwd = 2)

################################################################################
###################### Likelihood ratio test ###################################
# Likelihood ratio test for testing the Poisson vs the negative binomial 
# regression model. Since the chosen models for the two distributions have 
# different covariates we test the equivalent models from each distribution but
# also the "best" model from each distribution (!!!check if thid is okay!!!)

# The chi squared statistics for p = 0.05 and 1 or 2 degrees of freedom
chi.square.stat.1 <- qchisq(0.95, 1)
chi.square.stat.2 <- qchisq(0.95, 2)

# Both distributions:
# apprentices ~ urbanization + direction + population.t + distance.t
D.diff.a <- -2 * (logLik(p.model.a)[1] - logLik(nb.model.a)[1])

if (D.diff.a > chi.square.stat.1) {
  sprintf("Reject H0 at 0.05 significance -> go for NB model")
} else {
  sprintf("Accept H0 at 0.05 significance -> go for Poisson model")
}

# Both distributions:
# apprentices ~ urbanization +  population.t + distance.t
D.diff.d <- -2 * (logLik(p.model.d)[1] - logLik(nb.model.d)[1])

if (D.diff.d > chi.square.stat.1) {
  sprintf("Reject H0 at 0.05 significance -> go for NB model")
} else {
  sprintf("Accept H0 at 0.05 significance -> go for Poisson model")
}

# Poisson distribution:
# apprentices ~ urbanization + direction + population.t + distance.t
# Negative binomial distribution:
# apprentices ~ urbanization +  population.t + distance.t
D.diff <- -2 * (logLik(p.model.a)[1] - logLik(nb.model.d)[1])

if (D.diff > chi.square.stat.2) {
  sprintf("Reject H0 at 0.05 significance -> go for NB model")
} else {
  sprintf("Accept H0 at 0.05 significance -> go for Poisson model")
}

# Did you find any influential observation using diagnostics?









