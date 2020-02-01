# Installing data visualization packages 
install.packages("DataExplorer")
install.packages("colorspace")
install.packages("ksvm")
install.packages("arulesViz")
install.packages("Rgraphviz")
install.packages('e1071')
install.packages('caTools')
install.packages('ElemStatLearn')
library(ElemStatLearn)
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(arules,arulesViz)
library(arulesViz)
library(colorspace)
library(Rgraphviz)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(party)
library(e1071)
library(caTools)
range(satisfactionData2$X..of.Flight.with.other.Airlines)
## Loading the data set
satisfactionData <- read.csv("~/Downloads/Satisfaction Survey.csv")
satisfactionData2 <- read.csv("~/Downloads/Satisfaction Survey.csv")
View(satisfactionData)
# Checking if there are missing values
sapply(satisfactionData, function(x) sum(is.na(x)))
# here is a way to visualize the percentage of NAs in the whole day ina nice way. Hamza
missingvalues <- plot_missing(satisfactionData)
# There are three variables contain missing values which are Departure.Delay.in.Minutes, Arrival.Delay.in.Minutes and Flight.time.in.minutes.
# imputation techniques for missing values
# Because these three variables are all related to 'minutes' which is a numeric variable, we decide to replace the missing values by the mean for each corresponding variable.
satisfactionData$Departure.Delay.in.Minutes[is.na(satisfactionData$Departure.Delay.in.Minutes)] = mean(satisfactionData$Departure.Delay.in.Minutes, na.rm = T)
satisfactionData$Arrival.Delay.in.Minutes[is.na(satisfactionData$Arrival.Delay.in.Minutes)] = mean(satisfactionData$Arrival.Delay.in.Minutes, na.rm = T)
satisfactionData$Flight.time.in.minutes[is.na(satisfactionData$Flight.time.in.minutes)] = mean(satisfactionData$Flight.time.in.minutes, na.rm = T)
# Making sure all missing values are all imputated
sapply(satisfactionData, function(x) sum(is.na(x)))
plot_missing(satisfactionData)
View(satisfactionData)
# Descriptive statistics
summary(satisfactionData)
# For the numeric variables, we check its boxplot to detect outliers
str(satisfactionData)
# this is a better way to display the structure of the data (check it out,it is a nice visualization technique for the structure of the data, definitely nice for customer experience) Hamza. 
plot_str(satisfactionData)

# Change the 'Satisfaction' variable from factor to numeric
satisfactionData$Satisfaction <- as.numeric(satisfactionData$Satisfaction)
range(satisfactionData$Satisfaction)
# For the 'Age' variable
boxplot(satisfactionData$Age)
summary(satisfactionData$Age)
# For the 'Price.Sensitivity' variable
boxplot(satisfactionData$Price.Sensitivity)
summary(satisfactionData$Price.Sensitivity)
# Deal with outliers
boxplot.stats(satisfactionData$Price.Sensitivity)$out <- mean(satisfactionData$Price.Sensitivity, na.rm = T)
# For the 'No.of.Flights.p.a.' variable
boxplot(satisfactionData$No.of.Flights.p.a.)
summary(satisfactionData$No.of.Flights.p.a.)
boxplot.stats(satisfactionData$No.of.Flights.p.a.)$out
# For the 'No..of.other.Loyalty.Cards' variable
boxplot(satisfactionData$No..of.other.Loyalty.Cards)
summary(satisfactionData$No..of.other.Loyalty.Cards)
# For the 'Shopping.Amount.at.Airport' variable
boxplot(satisfactionData$Shopping.Amount.at.Airport)
summary(satisfactionData$Shopping.Amount.at.Airport)
# For the 'Eating.and.Drinking.at.Airport' variable
boxplot(satisfactionData$Eating.and.Drinking.at.Airport)
summary(satisfactionData$Eating.and.Drinking.at.Airport)
# For the 'Scheduled.Departure.Hour' variable
boxplot(satisfactionData$Scheduled.Departure.Hour)
summary(satisfactionData$Scheduled.Departure.Hour)
# For the 'Departure.Delay.in.Minutes' variable
boxplot(satisfactionData$Departure.Delay.in.Minutes)
summary(satisfactionData$Departure.Delay.in.Minutes)
# For the 'Arrival.Delay.in.Minutes' variable
boxplot(satisfactionData$Arrival.Delay.in.Minutes)
summary(satisfactionData$Arrival.Delay.in.Minutes)
# For the 'Flight.time.in.minutes' variable
boxplot(satisfactionData$Flight.time.in.minutes)
summary(satisfactionData$Flight.time.in.minutes)
# For the 'Flight.Distance' variable
boxplot(satisfactionData$Flight.Distance)
summary(satisfactionData$Flight.Distance)
# For the categorical variables
boxplot(Satisfaction ~ Airline.Status, data = satisfactionData, main = "Satisfaction by Airline.Status",
        xlab = "Airline.Status", ylab ='Satisfaction', notch = TRUE, col = c('red','blue','yellow','green'))
boxplot(Satisfaction ~ Gender, data = satisfactionData, main = "Satisfaction by Gender", xlab = "Gender", 
        ylab = "Satisfaction", notch = F, col = c('red', 'blue'))
boxplot(Satisfaction ~ Type.of.Travel, data = satisfactionData, main = "Satisfaction by Type of Travel",
        xlab = "Type of Travel", ylab ='Satisfaction', notch = TRUE, col = c('red','blue','yellow'))
boxplot(Satisfaction ~ Class, data = satisfactionData, main = "Satisfaction by Class",
        xlab = "Class", ylab ='Satisfaction', notch = TRUE, col = c('red','blue','yellow'))
boxplot(Satisfaction ~ Flight.cancelled, data = satisfactionData, main = "satisfaction by Flight Cancelled", xlab = "Flight Cancelled", 
        ylab = "Satisfaction", notch = T, col = c('red', 'blue'))
boxplot(Satisfaction ~ Arrival.Delay.greater.5.Mins, data = satisfactionData, main = "satisfaction by Arrival.Delay.greater.5.Mins", xlab = "Arrival.Delay.greater.5.Mins", 
        ylab = "Satisfaction", notch = T, col = c('red', 'blue'))


# plot the means of groups with error bars. Plotting the mean standard deviation of groups (Airlines.Status and Gender). Hamza
plotdata <- satisfactionData %>% group_by(Airline.Status, Gender) %>% summarize(n = n(),
                                                                                mean = mean(Satisfaction), sd = sd(Satisfaction),
                                                                                se = sd/sqrt(n))

ggplot(plotdata, aes(x = Airline.Status, y = mean,
                     group=Gender,
                     color=Gender)) + geom_point(size = 3) +
  geom_line(size = 1) + geom_errorbar(aes(ymin =mean - se,
                                          ymax = mean+se), width = .1)

plotdata <- satisfactionData %>% group_by(Class, Gender) %>% summarize(n = n(),
                                                                                mean = mean(Satisfaction), sd = sd(Satisfaction),
                                                                                se = sd/sqrt(n))

ggplot(plotdata, aes(x = Class, y = mean,
                     group=Gender,
                     color=Gender)) + geom_point(size = 3) +
  geom_line(size = 1) + geom_errorbar(aes(ymin =mean - se,
                                          ymax = mean+se), width = .1)

# Linear Regression Model
# Deleting some variables which we believe are useless for a linear regression model
satisfactionData_lm <- satisfactionData[,-15:-21]
# investigating correlation between selected variables. Hamza
plot_correlation(satisfactionData, type = 'continuous','Review.Date')
# we notice that delay in departure and delay on arrival are highly correlated (0.97). As such, we decide to remove delay in arrival from the regression analysis. 
satisfactionData_lm <- satisfactionData_lm[,-17]
satisfactionData_lm <- satisfactionData_lm[,-18]

# Encoding categorical data
str(satisfactionData_lm)

# Converting categorical variables to dummy variables
satisfactionData_lm$Airline.Status <- factor(satisfactionData_lm$Airline.Status, levels = c('Blue','Gold','Silver','Platinum'), labels = c(1,2,3,4))
str(satisfactionData_lm$Airline.Status)
satisfactionData_lm$Gender <- factor(satisfactionData_lm$Gender, levels = c('Female','Male'), labels = c(1,2))
str(satisfactionData_lm$Gender)
satisfactionData_lm$Type.of.Travel <- factor(satisfactionData_lm$Type.of.Travel, levels = c('Business travel','Personal Travel','Mileage tickets'), labels = c(1,2,3))
str(satisfactionData_lm$Type.of.Travel)
satisfactionData_lm$Class <- factor(satisfactionData_lm$Class, levels = c('Business','Eco','Eco Plus'), labels = c(1,2,3))
str(satisfactionData_lm$Class)
satisfactionData_lm$Flight.cancelled <- factor(satisfactionData_lm$Flight.cancelled, levels = c('Yes','No'), labels = c(1,2))
str(satisfactionData_lm$Flight.cancelled)
satisfactionData_lm$Arrival.Delay.greater.5.Mins <- factor(satisfactionData_lm$Arrival.Delay.greater.5.Mins, levels = c('yes','no'), labels = c(1,2))
str(satisfactionData_lm$Arrival.Delay.greater.5.Mins)


# Building a linear regression model
lm_model <- lm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity + No.of.Flights.p.a. + Type.of.Travel
                + Class + Flight.cancelled + Arrival.Delay.greater.5.Mins , data = satisfactionData_lm)
summary(lm_model)

# lm_model is a model which all variables are significant, so it is the optimized model we want to get here.
# Its Multiple R-squared: 0.4066, Adjusted R-squared: 0.4065.

# Visulizing the linear regression results


# Support Vector Machines (SVMs)
# Creating a new variable called 'happyCust' where Satisfaction is higher or eqaul to 6.
satisfactionData_SVMs <- satisfactionData
range(satisfactionData_SVMs$Satisfaction)
satisfactionData_SVMs$happyCust <- ifelse(satisfactionData_SVMs$Satisfaction >= 6, 'happy', 'notHappy')
satisfactionData_SVMs$happyCust <- factor(satisfactionData_SVMs$happyCust, levels = c('happy', 'notHappy'), labels = c(1,0))

# Splitting the dataset into the training set and testing set
set.seed(101)
x <-sample.split(satisfactionData_SVMs$happyCust, SplitRatio = 0.70)
training_SVMs = subset(satisfactionData_SVMs, x == TRUE)
test_SVMs = subset(satisfactionData_SVMs, x == FALSE)

# Building a support vector model using all variables to predict a happy customer
SVMs_model0 <- svm(formula = happyCust ~ ., data = training_SVMs, type = 'C-classification', kernel = 'linear')

# Predicting the test set results
SVMs_model0_pred <- predict(SVMs_model0, newdata = test_SVMs, type = 'votes')

# Building the confusion matrix
cm0 <- table(test_SVMs$happyCust, SVMs_model0_pred)
cm0
accuracy0 <- (cm0[1,1]+cm0[2,2])/(cm0[1,1]+cm0[1,2]+cm0[2,1]+cm0[2,2])
accuracy0

# Building a support vector model using two of the variables to predict a happy customer and calculte its accuracy
# Here, we randomly pick up two variables with high significance level in linear regression model.
SVMs_model1 <- svm(formula = happyCust ~  Airline.Status + Age + Gender + Price.Sensitivity + Class + X..of.Flight.with.other.Airlines, 
                   data = training_SVMs, type = 'C-classification', kernel = 'linear')
SVMs_model1_pred <- predict(SVMs_model1, newdata = test_SVMs, type = 'votes')
cm1 <- table(test_SVMs$happyCust, SVMs_model1_pred)
accuracy1 <- (cm1[1,1]+cm1[2,2])/(cm1[1,1]+cm1[1,2]+cm1[2,1]+cm1[2,2])
accuracy1

SVMs_model2 <- svm(formula = happyCust ~ No.of.Flights.p.a. + Price.Sensitivity, data = training_SVMs, type = 'C-classification', kernel = 'linear')
SVMs_model2_pred <- predict(SVMs_model2, newdata = test_SVMs, type = 'votes')
cm2 <- table(test_SVMs$happyCust, SVMs_model2_pred)
accuracy2 <- (cm2[1,1]+cm2[2,2])/(cm2[1,1]+cm2[1,2]+cm2[2,1]+cm2[2,2])
accuracy2
kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot')


# Visualising the Training set results

set <- training_SVMs[,5:7]
set <- set[,-2]
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c('Price.Sensitivity', 'No.of.Flights.p.a.')
y_grid <- predict(SVMs_model2, newdata = grid_set)
plot(set, main = 'Classifier(Training Set)', xlab = 'Price.Sensitivity', ylab = 'No.of.Flights.p.a.', xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(test_SVMs$happyCust == 1, 'green4', 'red3'))

  
# Association rules mining. Hamza
str(satisfactionData)
# Creatin various buckets in the significant variables
myfunction1 <- function(vec) {
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec >= 7] <- "High"
  vBuckets[vec < 7] <- "Low"
  return(vBuckets)
}
myfunction2 <- function(vec) {
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}
myfunction3 <- function(vec) {
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec >=3] <- "High"
  vBuckets[vec < 3] <- "Low"
  return(vBuckets)
}
# Applying functions to the different variables
satisfactionData$Satisfaction <- myfunction1(satisfactionData$Satisfaction)
satisfactionData$Age <- myfunction2(satisfactionData$Age)
range(satisfactionData$Price.Sensitivity)
satisfactionData$Price.Sensitivity <- myfunction3(satisfactionData$Price.Sensitivity)
str(satisfactionData$Price.Sensitivity)
satisfactionData$No.of.Flights.p.a. <- myfunction2(satisfactionData$No.of.Flights.p.a.)
satisfactionData$Arrival.Delay.in.Minutes <- myfunction2(satisfactionData$Arrival.Delay.in.Minutes)
satisfactionData$X..of.Flight.with.other.Airlines <- myfunction2(satisfactionData$X..of.Flight.with.other.Airlines)
satisfactionData$Satisfaction <- as.factor(satisfactionData$Satisfaction)
satisfactionData$Age <- as.factor(satisfactionData$Age)
satisfactionData$Price.Sensitivity <- as.factor(satisfactionData$Price.Sensitivity)
satisfactionData$No.of.Flights.p.a. <- as.factor(satisfactionData$No.of.Flights.p.a.)
satisfactionData$Arrival.Delay.in.Minutes <- as.factor(satisfactionData$Arrival.Delay.in.Minutes)
satisfactionData$X..of.Flight.with.other.Airlines <- as.factor(satisfactionData$X..of.Flight.with.other.Airlines)
# Filtering the data by the relevant variables in the analysis
View(satisfactionData)
str(satisfactionData)
satisfactionData <- satisfactionData[,-14:-25]
satisfactionData <- satisfactionData[,-10:-12]
satisfactionData <- satisfactionData[,-6]
satisfactionData <- satisfactionData[,-10:-11]
# Converting data into a sparse matrix
SatisfactionSurveyX <- as(satisfactionData,"transactions")
View(satisfactionData)
summary(SatisfactionSurveyX)
inspect(SatisfactionSurveyX)
# Exploring items frequencies 
itemFrequency(SatisfactionSurveyX)
itemFrequencyPlot(SatisfactionSurveyX, support= 0.5)
# Searching for strong associations by controlling for satisfaction is "High"
apriori(SatisfactionSurveyX, parameter = list(support= 0.01, confidence = 0.4), appearance = list(rhs=c('Satisfaction=High'),default="lhs"))
ruleset <- apriori(SatisfactionSurveyX, parameter = list(support= 0.01, confidence = 0.4), appearance = list(rhs=c('Satisfaction=High'),default="lhs"))
ruleset
inspect(ruleset)
goodrules <- ruleset[quality(ruleset)$lift> 4]
inspect(goodrules)
# We obtain 37 association rules by choosing the paramter support = 0.005 and confidence = 0.5
# regular plotting of the rulest
plot(goodrules)
# plotting a 3D matrix for the ruleset with respcet to lift
plot(goodrules, method = "matrix3D", measure = "lift")
# A graph based visualization

plot(goodrules, method="graph", control=list(type="items"))

plot(ruleset, method="paracoord", control=list(reorder=TRUE))

# observation: 
# there is a strong association between low price elasticity and high satisfaction. Similary customers who have a platinum package are more likely to have high level of satisfaction. Finally, it seems like customers who are travelling for business (business people) have higher level of satisfaction. 
# Symmetrically, it seems like customers who don't have platinum package, traveil in econ class are not highly satisfied. The company needs to focus on increasing their satisfactions. 
# This is my understanding let me know, if you have a better interpretation of this.

# Final algorithm: decision tree algorithm (CART)

iris_ctree1 <- ctree(Satisfaction ~ Price.Sensitivity + Gender + X..of.Flight.with.other.Airlines, data=satisfactionData)
print(iris_ctree1)
plot(iris_ctree1)
plot(iris_ctree, type = "simple")

table(satisfactionData$Satisfaction)
table(satisfactionData$Class)
hist(as.numeric(satisfactionData$Satisfaction))
str(satisfactionData$Satisfaction)
