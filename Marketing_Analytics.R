#******************************************************************#
#****Modeling Customer life time value with linear regression***#

#How can you decide which customers are most valuable for your business? 
#Learn how to model the customer lifetime value using linear regression.#



# ##Customer Lifetime Value in R
# Statistical method applied to field of marketing
# 1) customer life time value usig linear regression
# customer churn using logistic regression
# survival analysis to predict time until customer churns
# PCA to handle high dimension marketing data
# 
# CLV is a forecast hence there are several challenges 
# 1) predicted future net profit
# 2) identify promising net customers
# 3) prioritize customers according to future margins
# 4) no further customer segmentation
# 
# This can help us to target customer s accordng to future target.
# We want to find the drivers affecting the net margin however there is one tricky aspect about this
# 
# We need a model that uses current information in order to predict the future margin.table(
#   
#   Therefore we apply a two step procedure
#   first we take the explanotry variable from year one and use them to predict
#   the dependent variable in year two.
#   
#   This is the model specification step done on the dataset called 'clvData1'
#   When the model is specified we ove to step 2
#   we take the explanatory variables for year two and make predictions for
#   future margin at year 3
#   information about year 2 is stored in cldData2
#   
#   Dataset 'clvData1' holds 13 aggregated metrics on the ordering behavior of
#   4200 customers for a certain year
#   
#   AS a first matter of finding relationships in our data we will look at correlations.
#   
#   to do this we take all the information from year 1 and correlate it with
#   future margin from year 2
#   
#   We can visualize the correlation by the 'cor' function in the *stats*
#     package using the 'corrplot' function from the 'corrplot' package.
#   
#   Between the margin of current year and future margin we observe a stronger
#   positive correlation.
#   
#   the days since the last order and the retun ratio are 
#   moderately nnegative correlated with the future margin plotted in Orange

#packages
library(readr)
library(dplyr)    
library(corrplot)  
library(ggplot2)
library(rms)

  
#dataload

# The dataset salesData is loaded in the workspace. 
#It contains information on customers for the months one to three. Only the sales of month four are included. 
#The following table gives a description of some of the variables whose meaning is less obvious.
# 
# Variable	Description
# id	            identification number of customer
# mostFreqStore	  store person bought mostly from
# mostFreqCat	    category person purchased mostly
# nCats	          number of different categories
# preferredBrand	brand person purchased mostly
# nBrands	        number of different brands

salesData <- read.csv("salesData.csv")

#structure of DataSet
str(salesData, give.attr = FALSE)

# 'data.frame':	5122 obs. of  14 variables:
#   $ id                   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ nItems               : int  1469 1463 262 293 108 216 174 122 204 308 ...
# $ mostFreqStore        : Factor w/ 10 levels "Boston","Colorado Springs",..: 10 10 2 2 2 1 3 9 6 9 ...
# $ mostFreqCat          : Factor w/ 10 levels "Alcohol","Baby",..: 1 1 10 3 4 1 8 10 3 1 ...
# $ nCats                : int  72 73 55 50 32 41 36 31 41 52 ...
# $ preferredBrand       : Factor w/ 10 levels "Akar","Alekto",..: 10 10 3 10 3 3 3 3 3 3 ...
# $ nBrands              : int  517 482 126 108 79 98 78 62 99 103 ...
# $ nPurch               : int  82 88 56 43 18 35 34 12 26 33 ...
# $ salesLast3Mon        : num  2742 2791 1530 1766 1180 ...
# $ salesThisMon         : num  1284 1243 683 730 553 ...
# $ daysSinceLastPurch   : int  1 1 1 1 12 2 2 4 14 1 ...
# $ meanItemPrice        : num  1.87 1.91 5.84 6.03 10.93 ...
# $ meanShoppingCartValue: num  33.4 31.7 27.3 41.1 65.6 ...
# $ customerDuration     : int  821 657 548 596 603 673 612 517 709 480 ...


# Visualization of correlations - Rplot1
salesData %>% select_if(is.numeric) %>%
  select(-id) %>%
  cor() %>% corrplot()

# Frequent stores - Rplot2
ggplot(salesData) +
  geom_boxplot(aes(x = mostFreqStore, y = salesThisMon))

# Preferred brand - Rplot3
ggplot(salesData) +
  geom_boxplot(aes(x = preferredBrand, y = salesThisMon))

#Estimating Simple Linear Regression
# Model specification using lm
salesSimpleModel <- lm(salesThisMon ~ salesLast3Mon, 
                       data = salesData)

# Looking at model summary
summary(salesSimpleModel)

# Call:
#   lm(formula = salesThisMon ~ salesLast3Mon, data = salesData)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -570.18  -68.26    3.21   72.98  605.58 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   99.690501   6.083886   16.39   <2e-16 ***
#   salesLast3Mon  0.382696   0.004429   86.40   <2e-16 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 117.5 on 5120 degrees of freedom
# Multiple R-squared:  0.5932,	Adjusted R-squared:  0.5931 
# F-statistic:  7465 on 1 and 5120 DF,  p-value: < 2.2e-16

#Since the regression coefficient is greater than 0, there exists a positive relationship between the explanatory variable salesLast3Mon and the dependent variable salesThisMon. 
#It explains almost 60 percent of the variation in the sales of this month.

#Omitted Variable bias is when a variable which is correlated with both
#explanotory varibale and target variable is not included in the model.


# Multicollinearity is when one explanatory variable can be explained by other 
#explanatory variables. Then thestandard error report by model is underestimated.

#Avoiding multicollinearity
# Estimating the full model
salesModel1 <- lm(salesThisMon ~ . - id, 
                  data = salesData)


# Checking variance inflation factors
#remove variables with higher VIF values
vif(salesModel1)
# > vif(salesModel1)
# nItems 
# 11.772600 
# mostFreqStoreColorado Springs 
# 1.478098 
# mostFreqStoreColumbus 
# 1.746101 
# mostFreqStoreDenver 
# 1.289203 
# mostFreqStoreHonolulu 
# 1.338330 
# mostFreqStoreJersey 
# 1.317158 
# mostFreqStoreOrlando 
# 1.401396 
# mostFreqStoreSan Diego 
# 1.219922 
# mostFreqStoreSeattle 
# 1.794891 
# mostFreqStoreStockton 
# 1.070250 
# mostFreqCatBaby 
# 1.456921 
# mostFreqCatBakery 
# 1.246035 
# mostFreqCatBeverages 
# 1.079007 
# mostFreqCatClothes 
# 1.156841 
# mostFreqCatFresh food 
# 1.069987 
# mostFreqCatFrozen food 
# 1.296358 
# mostFreqCatPackaged food 
# 1.268000 
# mostFreqCatPets 
# 1.077488 
# mostFreqCatShoes 
# 1.417807 
# nCats 
# 8.402073 
# preferredBrandAlekto 
# 3.844176 
# preferredBrandBo 
# 41.075930 
# preferredBrandKatram 
# 1.632978 
# preferredBrandKellest 
# 1.713510 
# preferredBrandMedeia 
# 6.120384 
# preferredBrandMoone 
# 4.591570 
# preferredBrandNilima 
# 22.714376 
# preferredBrandTanvi 
# 1.885777 
# preferredBrandVeina 
# 20.739114 
# nBrands 
# 14.150868 
# nPurch 
# 3.083952 
# salesLast3Mon 
# 8.697663 
# daysSinceLastPurch 
# 1.585057 
# meanItemPrice 
# 1.987665 
# meanShoppingCartValue 
# 2.247579 
# customerDuration 
# 1.004664 


# Estimating new model by removing information on brand
salesModel2 <- lm(salesThisMon ~ . - id - preferredBrand - nBrands, 
                  data = salesData)

# Checking variance inflation factors
vif(salesModel2)
# nItems 
# 6.987456 
# mostFreqStoreColorado Springs 
# 1.470508 
# mostFreqStoreColumbus 
# 1.737790 
# mostFreqStoreDenver 
# 1.283222 
# mostFreqStoreHonolulu 
# 1.335457 
# mostFreqStoreJersey 
# 1.299889 
# mostFreqStoreOrlando 
# 1.398318 
# mostFreqStoreSan Diego 
# 1.213865 
# mostFreqStoreSeattle 
# 1.788777 
# mostFreqStoreStockton 
# 1.052065 
# mostFreqCatBaby 
# 1.412755 
# mostFreqCatBakery 
# 1.236939 
# mostFreqCatBeverages 
# 1.077907 
# mostFreqCatClothes 
# 1.105054 
# mostFreqCatFresh food 
# 1.067089 
# mostFreqCatFrozen food 
# 1.270953 
# mostFreqCatPackaged food 
# 1.235165 
# mostFreqCatPets 
# 1.072278 
# mostFreqCatShoes 
# 1.384861 
# nCats 
# 5.813494 
# nPurch 
# 3.069046 
# salesLast3Mon 
# 8.412520 
# daysSinceLastPurch 
# 1.579426 
# meanItemPrice 
# 1.925494 
# meanShoppingCartValue 
# 2.238410 
# customerDuration 
# 1.002981 


#Fitness of model
#Coefficient of Determination R-Squared
#Range of R- square is {0, 1}
#Higher the R square value more variance in y explained by x variables

#F-Test
#if p-vaue of F test is less than 0 then hypothesis of R- sqaure = 0 is rejected

#Methods to avoid overfitting
#>>AIC() from stats package - penalize every additional explanatory variable, so that you can control for overfitting while developing a model.
#When comapiring 2 models, the AIC minimizing model is preferred
#>>stepAIC() Automatic model selection can be done using stepAIC() from the MASS package.
#>>Other methods to avoid overfitting lie out-of-sample model validation 
# cross validation are also explained in depth in chapter of logisitic regression.



# salesData2_4 contains information on the customers for the months two to four. 
#We want to use this information in order to predict the sales for month 5.
salesData2_4 <- read.csv("salesDataMon2To4.csv")

# predicting sales
predSales5 <- predict(salesModel2, newdata = salesData2_4)

# calculating mean of future sales
mean(predSales5)
#>>625.1438

#***********************************************************************#


#*********Logistic Regression for Churn Prevention**********************#
#Predicting if a customer will leave your business, or churn, is important for targeting valuable customers and retaining those who are at risk. 
#Learn how to model customer churn using logistic regression.

#since gaining new customers is expensive than keeping existing ones,
# you will like to distinguish loyal customers from the one's who only buy once.

# the tools we use to predict returning of customers returning to an online shop is called binary logistic regression. IMAGE - 1

#loading dataset.
#This dataset is about bank customers and will be used to predict if customers will default on their loan payments.
defaultData <- read.csv("defaultData.csv", header = TRUE)

# Analyze the balancedness of dependent variable
ggplot(defaultData, aes(x = PaymentDefault)) +
  geom_histogram(stat = "count")

#glm() function in order to model the probability that a customer will default on his payment by using a logistic regression. 
# Build logistic regression model
logitModelFull <- glm(PaymentDefault ~ limitBal + sex + education + marriage +
                        age  + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                        billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                        payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, 
                      family = binomial, data = defaultData)

# Take a look at the model
summary(logitModelFull)

# Take a look at the odds
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)

library(MASS)
#Build the new model
logitModelNew <- stepAIC(logitModelFull, trace = 0) 

#Look at the model
summary(logitModelNew) 

# Save the formula of the new model (it will be needed for the out-of-sample part) 
formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit

#fitting full model
# Make predictions using the full Model
defaultData$predFull <- predict(logitModelFull, type = "response", na.action = na.exclude)

library(SDMTools)
# Construct the in-sample confusion matrix
confMatrixModelFull <- confusion.matrix(defaultData$PaymentDefault, defaultData$predFull, threshold = 0.5)
confMatrixModelFull

# Calculate the accuracy for the full Model
accuracyFull <- sum(diag(confMatrixModelFull)) / sum(confMatrixModelFull)
accuracyFull

#Fitting restricted model
# Calculate the accuracy for 'logitModelNew'
# Make prediction
defaultData$predNew <- predict(logitModelNew, type = "response", na.action = na.exclude)

# Construct the in-sample confusion matrix
confMatrixModelNew <- confusion.matrix(defaultData$PaymentDefault,defaultData$predNew, threshold = 0.5)
confMatrixModelNew

# Calculate the accuracy...
accuracyNew <- sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew)
accuracyNew
#Accuracy of logitModelFull is 78.5222%

# and compare it to the full model's accuracy
accuracyFull
#Accuracy of logitModelFull is 78.52778%

#Finding the optimal threshold
# Prepare data frame with threshold values and empty payoff column
payoffMatrix <- data.frame(threshold = seq(from = 0.1, to = 0.5, by = 0.1),
                           payoff = NA) 
payoffMatrix 

for(i in 1:length(payoffMatrix$threshold)) {
  # Calculate confusion matrix with varying threshold
  confMatrix <- confusion.matrix(defaultData$PaymentDefault,
                                 defaultData$predNew, 
                                 threshold = payoffMatrix$threshold[i])
  # Calculate payoff and save it to the corresponding row
  payoffMatrix$payoff[i] <- confMatrix[1,1]*250 + confMatrix[1,2]*(-1000)
}
payoffMatrix
#(1, 1) is true negative
#(1, 2) is false negative
# Optimal threshold is 0.3
# threshold  payoff
# 1       0.1  207500
# 2       0.2  513500
# 3       0.3  658500
# 4       0.4  307500
# 5       0.5 -190500


#To avoid over fitting of data we use out of simple model fit or K cross validation to
# divide the data set into train and test data.

# Out of simple model fit
# Split data in train and test set
set.seed(534381) 
defaultData$isTrain <- rbinom(nrow(defaultData), 1, 0.66)
train <- subset(defaultData, isTrain == 1)
test <- subset(defaultData, isTrain  == 0)

logitTrainNew <- glm(formulaLogit, family = binomial, data = train) # Modeling
test$predNew <- predict(logitTrainNew, type = "response", newdata = test) # Predictions

# Out-of-sample confusion matrix and accuracy
confMatrixModelNew <- confusion.matrix(test$PaymentDefault, test$predNew, threshold = 0.3) 
sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew) # Compare this value to the in-sample accuracy
#Accuracy - 0.7648


#Cross Validation
library(boot) #cv.glm()
# Accuracy function
costAcc <- function(r, pi = 0) {
  cm <- confusion.matrix(r, pi, threshold = 0.3)
  acc <- sum(diag(cm)) / sum(cm)
  return(acc)
}

# Cross validated accuracy for logitModelNew
set.seed(534381)
cv.glm(defaultData, logitModelNew, cost = costAcc, K = 6)$delta[1]
#Accuracy - 0.7712

########################################################################################################

#************Modeling time to Reorder with Survival Analysis************************************
#Survival Analysis allows us to obtain deep insights into customer relations 
#since it is possible to model and event when will take place not just if it will take place

dataNextOrder <- read.csv("survivalDataExercise.csv")
#Data about customers of an online shop in order to practice survival analysis. But now it's not about the time until churn, but about the time until the second order.

#The data is stored in the object dataNextOrder. 
#The variable boughtAgain takes the value 0 for customers with only one order and 1 for customers who have placed a second order already. 
#If a person has ordered a second time, you see the number of days between the first and second order in the variable daysSinceFirstPurch. 
#For customers without a second order, daysSinceFirstPurch contains the time since their first (and most recent) order.

# Look at the head of the data
head(dataNextOrder)

# Plot a histogram  - Rplot5
ggplot(dataNextOrder) +
  geom_histogram(aes(x = daysSinceFirstPurch,
                     fill = factor(boughtAgain))) +
  facet_grid( ~ boughtAgain) + # Separate plots for boughtAgain = 1 vs. 0
  theme(legend.position = "none") # Don't show legend

#We observe that there are more customers in the data who bought a second time. 
#Apart from that, the differences between the distributions are not very large.

library(survival) # Surv()
# Create survival object
survObj <- Surv(dataNextOrder$daysSinceFirstPurch, dataNextOrder$boughtAgain)

# Look at structure
str(survObj) # there is a mark of + if the second order has not been placed yet.

#the data contains an additional covariate called voucher. This categorical variable tells you if the customer used a voucher in her first order. 
#It contains the value 0 or 1.

# Compute and print fit
fitKMSimple <- survfit(survObj ~ 1)
print(fitKMSimple)

# Plot fit - Rplot6
plot(fitKMSimple,
     conf.int = FALSE, xlab = "Time since first purchase", ylab = "Survival function", main = "Survival function")

# Compute fit with categorical covariate
fitKMCov <- survfit(survObj ~ voucher, data = dataNextOrder)

# Plot fit with covariate and add labels - Rplot7
plot(fitKMCov, lty = 2:3,
     xlab = "Time since first purchase", ylab = "Survival function", main = "Survival function")
legend(90, .9, c("No", "Yes"), lty = 2:3)

#We  analyzed how voucher usage is related to the survival curve! Customers using a voucher seem to take longer to place their second order. 
#They are maybe waiting for another voucher?

#Cox Proportional Hazard Model
#we are going to compute a Cox Proportional Hazard model on the online shop data. 
#Our data stored in dataNextOrder now contains four additional variables: the shoppingCartValue of the first order in dollars, whether the customer used a voucher, whether the order was returned, and the gender.

library(rms)
# Determine distributions of predictor variables
dd <- datadist(dataNextOrder)
options(datadist = "dd")

# Compute Cox PH Model and print results
fitCPH <- cph(Surv(daysSinceFirstPurch, boughtAgain) ~ shoppingCartValue + voucher + returned + gender,
              data = dataNextOrder,
              x = TRUE, y = TRUE, surv = TRUE)
print(fitCPH)

# Interpret coefficients
exp(fitCPH$coefficients)

# Plot results - RPlot8
plot(summary(fitCPH), log = TRUE) 

#We can see that a shopping cart value increase of 1 dollar decreases the hazard to buy again by a factor of only slightly below 1 - but the coefficient is significant, as are all coefficients. 
#For customers who used a voucher, the hazard is 0.74 times lower, and for customers who returned any of the items, the hazard is 0.73 times lower. 
#Being a man compared to a women increases the hazard of buying again by the factor 1.11.

# Check proportional hazard assumption and print result
testCPH <- cox.zph(fitCPH)
print(testCPH)

# Plot time-dependent beta - RPlot9
plot(testCPH, var = "gender=male")

# Load rms package
library(rms)

# Validate model
validate(fitCPH, method = "crossvalidation",
         B = 10, dxy = TRUE, pr = FALSE)
# Unfortunately, the explanatory power of your model is rather low. 
# We could try to collect more explanatory variables.

# Create data with new customer
newCustomer <- data.frame(daysSinceFirstPurch = 21, shoppingCartValue = 99.90, gender = "female", voucher = 1, returned = 0, stringsAsFactors = FALSE)

# Make predictions - RPlot10
pred <- survfit(fitCPH, newdata = newCustomer)
print(pred)
plot(pred)

# Correct the customer's gender
newCustomer2 <- newCustomer
newCustomer2$gender <- "male"

# Redo prediction
pred2 <- survfit(fitCPH, newdata = newCustomer2)
print(pred2)

#The correction of the gender decreased the predicted median time until the second order from 47 to 44 days.


#############################################################################################

#*************Reducing dimensionality with Principal Component Analysis***************#
#load data
library(miceadds) #load.Rdata()
local <- environment()
newsData <- load.Rdata("newsData.RData", "newsData")
head(newsData)
str(newsData)

library(dplyr)
library(corrplot)

# Overview of data structure:
str(newsData, give.attr = FALSE)

# Correlation structure: RPlot11
newsData %>% cor() %>% corrplot()

# Standardize data
newsData <- newsData %>% scale() %>% as.data.frame()

# Compute PCA
pcaNews <- newsData %>% prcomp()

# Eigenvalues - Variance of the components
pcaNews$sdev ^ 2


# Screeplot: - RPlot12
screeplot(pcaNews)

# Cumulative explained variance:
summary(pcaNews)

# Kaiser-Guttmann (number of components with eigenvalue larger than 1):
sum(pcaNews$sdev ^ 2 > 1)


# The screeplot suggests 4 or 6 components. To explain 70% of the variance, you need as many as 10 components. 
# The Kaiser-Guttmann criterion suggests 8 components, but the eigenvalues of PC7 and PC8 are already very close to 1. 
# Therefore, 6 would be a good compromise.

# The loadings are stored in the rotation element of the pca object pcaNews. 
# They are the correlations between the original variables and the components.

# Print loadings of the first six components
pcaNews$rotation[, 1:6] %>% round(2)

# PC1   PC2   PC3   PC4   PC5   PC6
# n_tokens_title             -0.05 -0.10  0.01 -0.10  0.20 -0.28
# n_tokens_content            0.23 -0.17 -0.38  0.12  0.15 -0.02
# n_unique_tokens             0.00  0.00  0.00  0.01  0.01  0.06
# num_hrefs                   0.26 -0.16 -0.42 -0.03  0.07  0.11
# num_self_hrefs              0.20 -0.07 -0.39  0.06  0.12  0.08
# num_imgs                    0.14 -0.15 -0.43 -0.06  0.04  0.08
# num_videos                  0.09 -0.20  0.04 -0.19  0.16 -0.14
# num_keywords                0.07  0.11 -0.25  0.14 -0.42 -0.30
# is_weekend                  0.05 -0.01 -0.12 -0.02 -0.10 -0.16
# kw_avg_min                  0.03  0.01 -0.05 -0.25 -0.65  0.07
# kw_avg_avg                  0.02 -0.15 -0.06 -0.61 -0.31  0.17
# kw_avg_max                 -0.10 -0.21  0.10 -0.50  0.35  0.26
# average_token_length        0.39 -0.02  0.19  0.19 -0.01  0.14
# global_subjectivity         0.45 -0.01  0.23 -0.04 -0.03  0.03
# global_sentiment_polarity   0.25  0.55 -0.03 -0.19  0.11  0.13
# global_rate_positive_words  0.33  0.25  0.14 -0.08  0.04 -0.09
# global_rate_negative_words  0.15 -0.47  0.23  0.11 -0.10 -0.21
# avg_positive_polarity       0.42  0.09  0.17 -0.06  0.02  0.10
# avg_negative_polarity      -0.25  0.37 -0.20 -0.04  0.08  0.06
# title_subjectivity          0.07 -0.03  0.01 -0.27  0.07 -0.61
# title_sentiment_polarity    0.07  0.24 -0.11 -0.24  0.15 -0.42

# Here are some ideas for the interpretation: PC1 reflects "Subjectivity" (high global_subjectivity and avg_positive_polarity, negative loading on avg_negative_polarity). 
# PC2 contains "Positivity" (high global_sentiment_polarity, low global_rate_negative_words; even negative words are not very negative as you can see from the positive loading on avg_negative_polarity). 
# Here you meet again the group of intercorrelated variables from the corrplot, but they split into two components.

#Biplot - RPlot13
pcaNews %>% biplot(cex = 0.5)

#We can see a separated small group of articles with low values on PC1 and low variance in their PC2 values. These articles have a low subjectivity and are neither positive nor negative (which makes sense, because a very positive or negative article would probably also be higher in subjectivity).


# Predict log shares with all original variables
mod1 <- lm(logShares ~ ., data = newsData)

# Create dataframe with log shares and first 6 components
dataNewsComponents <- cbind(logShares = newsData[, "logShares"],
                            pcaNews$x[, 1:6]) %>%
  as.data.frame()

# Predict log shares with first six components
mod2 <- lm(logShares ~ ., data = dataNewsComponents)

# Print adjusted R squared for both models
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared

# The R squared decreased only from 8% to 5% although the number of variables decreased from 21 to 6. 
# However, even 8% of variance explained is not very good. 
# That means, in order to get a good prediction for the (log) number of shares, you would probably need to collect additional variables or compute new variables from the existing ones.

