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

# data$direction.fac <- direction.fac

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
vioplot(data$apprentices ~ data$direction, col = "turquoise3")

# Apply transformation to the variables population and distance, add to data.frame
data$population.t <- log(data$population)
data$distance.t <- log(data$distance)

data

#### Poisson Regression ####

attach(data)

#### Try different poisson models ####

# To check if the assumption for the Poisson distribution is satisfied run
# dispersiontest(model). If alternative hypothesis is true the poisson 
# distribution is not likely

# Larger model
p.mod.1 <-
  glm(apprentices ~ urbanization + direction + population.t + distance.t,
      family = "poisson")
summary(p.mod.1)
dispersiontest(p.mod.1)

# Confidence interval (For conf. interval for expected respose see lecture 12,
# slide 32)
confint.default(p.mod.1,level=0.05)

# Standard error (diagonal of matrix)
sqrt(summary(p.mod.1)$cov.unscaled) 

# Test interaction effects
p.mod.2 <-
  glm(apprentices ~ urbanization + direction + population.t + distance.t + population.t* distance.t,
      family = "poisson")
summary(p.mod.2)
dispersiontest(p.mod.2)

# Remove urbanization
p.mod.3 <-
  glm(apprentices ~  direction + population.t + distance.t,
      family = "poisson")
summary(p.mod.3)
dispersiontest(p.mod.3)

# Remove direction
p.mod.4 <-
  glm(apprentices ~ urbanization + population.t + distance.t,
      family = "poisson")
summary(p.mod.4)
dispersiontest(p.mod.4)

# Remove population
p.mod.5 <-
  glm(apprentices ~ urbanization + direction  + distance.t,
      family = "poisson")
summary(p.mod.5)
dispersiontest(p.mod.5)

# Remove distance
p.mod.6 <-
  glm(apprentices ~ urbanization + direction + population.t ,
      family = "poisson")
summary(p.mod.6)
dispersiontest(p.mod.6)

# Urbanization appear to be the least significant followed by direction 
# (especially the west direction) Try a model without them
p.mod.7 <-
  glm(apprentices ~ population.t + distance.t,
      family = "poisson")
summary(p.mod.7)
dispersiontest(p.mod.7)

# Computer appropriate residuals for Poisson regression

#### Negative Binomial Regression ####

#### Try different NB models ####
attach(data)
# Larger model
nb.mod.1 <-
  glm.nb(apprentices ~ urbanization + direction + population.t + distance.t)
summary(nb.mod.1)

# Test interaction effects
nb.mod.2 <-
  glm(apprentices ~ urbanization + direction + population.t + distance.t + population.t* distance.t,
      family = "poisson")
summary(nb.mod.2)

# Remove urbanization
nb.mod.3 <-
  glm(apprentices ~  direction + population.t + distance.t,
      family = "poisson")
summary(nb.mod.3) 

# Remove direction
nb.mod.4 <-
  glm(apprentices ~ urbanization + population.t + distance.t,
      family = "poisson")
summary(nb.mod.4)

# Remove population
nb.mod.5 <-
  glm(apprentices ~ urbanization + direction  + distance.t,
      family = "poisson")
summary(nb.mod.5)

# Remove distance
nb.mod.6 <-
  glm(apprentices ~ urbanization + direction + population.t ,
      family = "poisson")
summary(nb.mod.6)

#####

# Computer appropriate residuals for negative binomial regression

#### Likelihood ratio test ####
# Likelihood ratio test for testing the Poisson vs the negative binomial 
# regression model. 




# Did you find any influential observation using diagnostics?









