################################################ Digital camera ##############################################################
install.packages('summarytools')
install.packages('plotly')
install.packages('psych')
install.packages("oslrr")
library(psych)
library(summarytools)
library(plotly)

install.packages("olsrr")
library(olsrr)
install.packages('gvlma')
library(gvlma)
install.packages('jtools')
library(jtools)

library(readxl)
Cameras <- read_excel("C:/Users/shiva/Desktop/3RD SEM/Term 2/Predictive/Cases/Cameras.xlsx")
View(Cameras)

#===============================================================================================
Q1.
#Option 1 for summary
summary(Cameras)

#Option 2 for summary at a granular level
mydatasummary <- round(summarytools::descr(Cameras),2)
mydatasummary

#Option 3 for summary at a granular level by Camera type
psych::describeBy(Cameras, Cameras$Brand)

#===============================================================================================
Q2.
library(plotly)

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2992018/ ( check link )

scatter.price <- plot_ly(data = Cameras, x = ~`Price ($)`, y = ~Score)
scatter.price
scatter.pixel <- plot_ly(data = Cameras, x = ~Megapixels, y = ~Score)
scatter.pixel
scatter.weight <- plot_ly(data = Cameras, x = ~`Weight (oz.)`, y = ~Score)
scatter.weight

#additional graphs
p <- plot_ly(data = Cameras, x = ~Megapixels, y = ~Score, type = "box", boxpoints = "all", jitter = 0.3,
             pointpos = -1.8)
p

q <- plot_ly(data = Cameras, x = ~Megapixels, y = ~Score, type = "box", boxpoints = "all", jitter = 0.3,
             pointpos = -1.8)
q


#================================================================================================
Q3.
model.price <- lm(Score ~ `Price ($)`, data = Cameras)
summary(model.price)
ols_plot_cooksd_bar(model.price)

par(mfrow=c(2,2))
plot(model.price, which=1:4)

#================================================================================================
#Q4. 
unique(Cameras$Brand)
Camera.canon <- Cameras[Cameras$Brand == 'Canon',]

model.canon <- lm(Score ~ `Price ($)`, data = Camera.canon)
summary(model.canon)
ols_plot_cooksd_bar(model.price)

p <- plot_ly(ggplot2::diamonds, y = ~Cameras$`Price ($)`, color = ~Cameras$Brand, type = "box")
p
#=================================================================================================

#This was to understand the recommendations
modeeee<- lm(Score ~ Observation + Brand, data = Cameras)
summary(modeeee)

delll<- lm(Score ~ `Price ($)`+ Megapixels + `Weight (oz.)`, data = Cameras)
summary(delll)




###############################################
library(dplyr)
p <- Cameras %>% mutate(price_squared = Cameras$`Price ($)`*Cameras$`Price ($)`)

m1 <- lm(p$Score ~ p$`Price ($)` + I(p$`Price ($)`^2) , data = p)

m2<- lm(Cameras$Score ~ poly(Cameras$`Price ($)`,2, raw= TRUE), data = Cameras)
m3<- lm(p$Score ~ poly(p$`Price ($)`,3, raw= TRUE), data = p)
m4 <- lm(Camera.canon$Score ~ poly(Camera.canon$`Price ($)`,2, raw= TRUE), data = Camera.canon) 
m5 <- lm(Camera.canon$Score ~ poly(Camera.canon$`Price ($)`,5, raw= TRUE), data = Camera.canon) 

summ(m2)
summ(m3)
summ(m4)
summ(m5)
install.packages(lmerMod)
library(lmerMod)
library(car)
influencePlot(m2, id.method="identify", main="Influence Plot")
influencePlot(m4, id.method="identify", main="Influence Plot")

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(m4)
par(mfrow=c(1,1)) # Change back to 1 x 1







# Car value ##############################################################

install.packages("dummies")
library(dummies)

library(readxl)
CarValues <- read_excel("C:/Users/shiva/Desktop/3RD SEM/Term 2/Predictive/Cases/CarValues.xlsx")
View(CarValues)

summary(CarValues)

# Creating Dummy variables
CarValues.1 <- cbind(CarValues, dummy(CarValues$Size))
View(CarValues.1)

#Removing additional variables
CarValues.12 <- CarValues.1[,-c(2,9)]

#Model one with dummy variables
model.1 <- lm(`Value Score` ~ `CarValuesFamily Sedan`+`CarValuesUpscale Sedan`, data = CarValues.12)
summary(model.1)
ols_plot_cooksd_bar(model.1)

#Model with all variables followed by stepwise elimination
model.2 <- lm(`Value Score` ~ `Cost/Mile`+`Road-Test Score`+`Predicted Reliability`+`Predicted Reliability`
              +`CarValuesFamily Sedan`+`CarValuesUpscale Sedan`, data = CarValues.12)
summary(model.2)

# Deleting variable with "0.05 ‘.’" significance level or less (looking at the summary of model.2)
model.3 <- lm(`Value Score` ~ `Cost/Mile`+`Road-Test Score`+`Predicted Reliability`, data = CarValues.12)
summary(model.3)
#The model Rsquares decreases slightly but reduction in the number of dimensions might reduce over fitting, and will perform better on the hold out sample

# "Smaller cars provide better value than larger cars" ??

#Need to answer this

#Regression equation Road-test Score
model.5 <- lm(`Value Score` ~ `Road-Test Score`, data = CarValues.12)
summary(model.5)

#equation
#`Value Score` = 0.902038 + 0.005783 * `Road-Test Score`

#Regression equation Predicted reliability

model.6 <- lm(`Value Score` ~ `Predicted Reliability`, data = CarValues.12)
summary(model.6)

#equation
#`Value Score` = 0.76293 + 0.17332 * `Predicted Reliability`

# Need to answer the last question for recommendation