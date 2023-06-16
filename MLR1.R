#Multiple linear regression model for Real estate market

# Load Package "flexmix"
install.packages("flexmix")
# Load Package "OLSRR"
install.packages("olsrr")
library("olsrr")
require(flexmix)
require(data.table)
require(smef) 

#Smef is a very useful package implemented by Luca Scrucca, 
#professor at the University of Perugia.
#Upload the data set and drop the first column

real_estate = fread("Rfile/real_estate.csv")
View(real_estate)  
real_estate = real_estate[,-1]

#The dataset is on Singapore's real estate market .
#All variables are quantitative. 

#X1 transaction date                    
#X2 house age                             
#X3 distance to the nearest MRT station 
#X4 number of convenience stores           
#X5 latitude                              
#X6 longitude                            
#Y house price of unit area  

#I rename some variables. 

colnames(real_estate)[c(1:7)] = c("Date", "H_Age", "Dist_MRT_station", "N.conv_store", "Latitude", 
                                  "Longitude", "Price")


head(real_estate) 


smef::describe(real_estate, detailed = TRUE) 

max(real_estate$Date) ;min(real_estate$Date)

#obs has been recorder from 2012 to 2013 in Singapore 
#no NAs in the dataset  
# The Dataset has 414 Rows.
#The Dataset has 7 Columns (the first one has been removed (NO #of obs)) 
#the house prices is response variable 

#some graphic representation of the main quantitative predictors 

y = real_estate$Price 
par(mfrow=c(1,2))
hist(y, breaks = 55, freq = TRUE, xlim = c(5, 80),col = "light green", main = "House Price Freq.(Y)", xlab = "House Prices")
abline(v = mean(y), col= 1, lwd = 2, lty =2) 

x2 = real_estate$H_Age 

hist(x2, breaks = 55, freq = TRUE, col = "light green", main = "House Age Freq", xlab = "House Age")
abline(v = mean(x2), col= 1, lwd = 2, lty =2)  

DT = data.frame(real_estate[,1:6])
lgy= log(y)  #log transformation of the price 
par(mfrow=c(2,3)) 


for(i in  1:ncol(DT)){
  plot(DT[,i], y, xlab = c("x",i), col = " dark blue")
} 

for(i in  1:ncol(DT)){
  plot(DT[,i], lgy, xlab = c("x",i), col = " dark blue")
}

#whole model MLR including the main effect of all predictors 
model = lm(Price ~ . ,data = real_estate) 
summary(model) 
(beta_vector0 = model$coefficients)
#all the predictor seems to be significant except the "Longitude" 
#the overall F-test has p-value smaller than the 0.05
#meaning that at least one  predictor has a statistically significant 
#impact in the model fit 
#the adj r squared  = 0.5762 

model1 = lm(log(Price) ~ ., data = real_estate)
summary(model1);summary(model0)  
  
(beta_vector1 = model1$coefficients) 

#log tranforming the Price we have an increase of the adj r squared
#equal to = 0.6811 
#Longitude seem to be not statistically significant 
#for both models previously fitted the Date of home purchase 
#latitude and the number of convenience stores has a stat. significative 
#positive impact on the Price 
#viceversa the House age, the distance from the MRT station has a negative 
#and a statistically significant impact on the price 

#i perform a STEPWISE SELECTION (sequential replacement)

mod_stpw = leaps::regsubsets(log(Price) ~ ., data = real_estate, method = "seqrep",
                             nbest = 1)

smod_stpw = summary(mod_stpw); smod_stpw  

DT = as.data.table(smod_stpw[c("rss", "rsq", "adjr2", "cp", "bic")])
DT[, subset:=1:nrow(DT)] 

DT[, subsetSize:= apply(smod_stpw$which,1,sum) -1 ] #intercept not counted
plot1 = ggplot(DT,  aes(x = subsetSize, y = adjr2)) + labs(x = "Subset size", y  = "Adj R^2")+
  scale_x_continuous(breaks = DT$subsetSize) +
  geom_line() +geom_point(pch = ifelse(DT$adjr2 == max(DT$adjr2), 19,1))

plot2 = ggplot(DT,  aes(x = subsetSize, y = cp)) + labs(x = "Subset size", y  = "Cp")+
  scale_x_continuous(breaks = DT$subsetSize) +
  geom_line() +geom_point(pch = ifelse(DT$cp == min(DT$cp), 19,1))

plot3 = ggplot(DT,  aes(x = subsetSize, y = bic)) + labs(x = "Subset size", y  = "BIC")+
  scale_x_continuous(breaks = DT$subsetSize) +
  geom_line() +geom_point(pch = ifelse(DT$bic == min(DT$bic), 19,1))

grid.arrange(plot1, plot2, plot3, nrow = 1)


# perform a forward selection  

mod_frwrd = leaps::regsubsets(log(Price) ~ ., data = real_estate, method = "forward",
                             nbest = 1)

smod_frwrd = summary(mod_frwrd); smod_frwrd  

DT = as.data.table(smod_frwrd[c("rss", "rsq", "adjr2", "cp", "bic")])
DT[, subset:=1:nrow(DT)] 

DT[, subsetSize:= apply(smod_frwrd$which,1,sum) -1 ] #intercept not counted
plot1 = ggplot(DT,  aes(x = subsetSize, y = adjr2)) + labs(x = "Subset size", y  = "Adj R^2")+
  scale_x_continuous(breaks = DT$subsetSize) +
  geom_line() +geom_point(pch = ifelse(DT$adjr2 == max(DT$adjr2), 19,1))

plot2 = ggplot(DT,  aes(x = subsetSize, y = cp)) + labs(x = "Subset size", y  = "Cp")+
  scale_x_continuous(breaks = DT$subsetSize) +
  geom_line() +geom_point(pch = ifelse(DT$cp == min(DT$cp), 19,1))

plot3 = ggplot(DT,  aes(x = subsetSize, y = bic)) + labs(x = "Subset size", y  = "BIC")+
  scale_x_continuous(breaks = DT$subsetSize) +
  geom_line() +geom_point(pch = ifelse(DT$bic == min(DT$bic), 19,1))

grid.arrange(plot1, plot2, plot3, nrow = 1)

#we obtain the same as before with the stepwise selection 



#optimal subset size choice for the model fitting is 5 since we have the lowest 
#bic and Mallow's CP and the higher Adj R^2    

#for the model fitting we will include only the predictors highlighted in  the 
#regsubsets summary table

smod_stpw 

# i include the Date, H_age, Dist_MRT_station, N.conv_store, Latitude   

#optimal subset regression 

mod1 = lm(log(Price) ~ Date + H_Age + Dist_MRT_station +  N.conv_store + Latitude, data =real_estate  )
summary(mod1);r1 = summary(mod1)$adj.r.squared 


#previous model with the response log tranformed beacuse of the nature of the response   
#we try to include also interactions 

mod2 = lm(log(Price) ~ Date + H_Age + Dist_MRT_station +
            N.conv_store + Latitude + N.conv_store:Dist_MRT_station ,
          data =real_estate  )
summary(mod2);r2 = summary(mod2)$adj.r.squared 

#including orhogonal polinomials (cubic effect)
mod3  = lm(log(Price) ~ Date + H_Age + Dist_MRT_station + I(H_Age^3) + 
             N.conv_store + Latitude, data =real_estate)
summary(mod3); r3 = summary(mod3)$adj.r.squared 

#including orhogonal polinomials (cubic effect) and interactions between  

mod4  = lm(log(Price) ~ Date + H_Age + Dist_MRT_station +
             N.conv_store + Latitude + I(H_Age^3)  + N.conv_store:Dist_MRT_station ,
           data =real_estate) 

summary(mod4);r4 = summary(mod4)$adj.r.squared 

#basically the previous model but with I(H_Age^5) 
mod5 = lm(log(Price) ~ Date + H_Age + Dist_MRT_station +
            N.conv_store + Latitude + I(H_Age^5)  + N.conv_store:Dist_MRT_station ,
          data = real_estate) 

summary(mod5);r5 = summary(mod5)$adj.r.squared 

#introduct the interaction between the house age and the Dist_MRT_station

mod6 = lm(log(Price) ~ Date + H_Age + Dist_MRT_station +
            N.conv_store + Latitude + I(H_Age^3)  + N.conv_store:Dist_MRT_station +
            H_Age:Dist_MRT_station,
          data = real_estate) 

summary(mod6);r6 = summary(mod6)$adj.r.squared  


#we select the model that have all significant predictor(alpha = 0.05) and have 
#lowest BIC and highest Adj R^2 

BICs = c(BIC(mod1), BIC(mod2), BIC(mod3), BIC(mod4), BIC(mod5), BIC(mod6))

modname  = 1:6   

Adj.Rsquared = c(r1,r2,r3,r4,r5,r6) 

DT = data.table(Model = modname, Adj_R2 = Adj.Rsquared, BICs = BICs);DT  


#Model 4 seems to have a good Adj R squared and the lowest BIC 
#i choose it to perform model diagnostic  

summary(mod4)  ## Adjusted R-squared:  0.7274  # BIC = -91.43 

##all the predictors are significant (p-values< 0.001)

#Date has a positive impac of the price this is basically related to the presence 
#of a trend in the house prices time series  (reasonable)

# H_age predictor has a negative impact on the house prices  this is practically 
#reasonable an old house has lower price (reasonable)

#Dist_MRT_station in average shows a negative effect on the price, a larger distances 
#from the MRT station leads averagely to a reductin of the price (reasonable)  

# N.conv_store shows a averagely negative impact on price, less minimarkets in the 
#area (= less services) lead to a reduction of the price. 

#Latitude. Singapore is above the equator meaning that increasing latitude means that 
#we are going to norhtern regions. Larger Values for the latitude (going to north) 
#leads averagely to a an increase of the house prices 

#the interaction between the predictors Dist_MRT_station:N.conv_store has a negative 
#effect on the response. 

#i'm going to perform a model diagnostic in order to see if the LR assumption 
#are respected or not. 

#Diagnostic plots

# 1- Linearity of the data

par(mfrow=c(1,1))

plot(mod4, 1, col = "dark blue")   

#there is no pattern in the residual plot. This suggests that we can assume 
#linear relationship between the predictors and the outcome variables.  


#Homogeneity of variance (homoschedasticity)

#This plot shows if residuals are spread equally along the ranges of predictors.

plot(mod4, 3, col = "dark blue")   

#no megaphone structure 

#let's perform a Breush pagan test  
install.packages("lmtest")
require(lmtest)
lmtest::bptest(mod4) 
 
# p-value = 0.2838 we don't reject the null h of contant variance 

#normality of the errors graphical inspection
residuals = mod4$residuals
plot(mod4,2)   
x = seq(-1.5, 1, by = 0.01)
hist(residuals, breaks = 32, probability = TRUE, col = "dark green", 
     border = "white",  main = "Residuals Frequency", font.main = 3,
     sub = "Alternative graph. way to check normality", font.sub	= 2) 
abline(v = mean(mod4$residuals), lty = 2, lwd = 3,col = "purple")

#Normality Test
ols_test_normality(mod4) 
shapiro<-shapiro.test(residuals)   

#residual are not normally distributed


#Outliers, high levarage points and influential point

  

# Residuals vs Leverage
plot(mod4, 5)
#The plot above highlights the top 3 most extreme points
#149, 36,114


# Cook's distance
plot(mod4, 4)

#further analysis should be done to investigate the non normality of the errors   
#and the presence of influential points as shown by the previous graphical inspection.  






 











