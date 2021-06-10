library(dplyr)
library(plyr)
library(class)
library(ggplot2)
library(caret)
library(lda)
library(MASS)
library(boot)
library(tree)
library(leaps)
library(tidyverse)
library(pls)
library(gbm)
library(glmnet)
library(randomForest)
library(maptree)

#Read in data to be used for project
People = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/People.csv")
Players = People[People$birthYear > 1894, ]
CareerAppearances = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/CareerAppearances.csv")
CareerAwards = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/CareerAwards.csv")
CareerBatting = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/CareerBatting.csv")
CareerPitching = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/CareerPitching.csv")
SeasonBatting = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/SeasonBatting.csv")
SeasonPitching = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/SeasonPitching.csv")
AvgPitching = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/AvgPitching.csv")
Salary = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/Salary.csv")
Salary2 = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/Salary2.csv")
HOF = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/HOF.csv")
HOF2 = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/HOF2.csv") 
ASG = read.csv("C:/Users/Kevin Chonko/Documents/Grad School/Baseball/ASG.csv")

#Create dataframe containing season-long stats for players where salary info exists
SeasonStats = left_join(SeasonBatting, SeasonPitching, by = c("playerID" = "playerID", "yearID" = "yearID"))
SeasonStats = subset(SeasonStats, select = -c(X.x, stint.x, teamID.x, lgID.x, X.y, stint.y, teamID.y, lgID.y))
SeasonStats[is.na(SeasonStats)] = 0
SeasonStats = left_join(SeasonStats, Salary2, by = c('playerID' = 'playerID', 'yearID' = 'yearID'))
SeasonStats = SeasonStats[!is.na(SeasonStats$salary),]

#Create loop to distinguish between hitters and pitchers based on # of At-Bats and Outs Recorded
SeasonStats$Hitter = ""
for (i in 1:31053) {
  if (SeasonStats$AB[i] > SeasonStats$IPoutsP[i]){
    SeasonStats$Hitter[i] = 1
  } else {
    SeasonStats$Hitter[i] = 0
  }
}
SeasonStats = SeasonStats %>% relocate (Hitter, .after = playerID)
SeasonStats = transform(SeasonStats, Hitter = as.numeric(Hitter))



#merge files together as needed, and create master file for all players and HOF-eligible players
ASGAwards = merge(CareerAwards, ASG, by = "playerID")
names(ASGAwards)[14] = 'ASG'

MasterData = left_join(Players, CareerBatting, by = 'playerID')
MasterData = left_join(MasterData, CareerPitching, by = 'playerID')
MasterData = left_join(MasterData, ASGAwards, by = 'playerID')
MasterData = left_join(MasterData, HOF2, by = 'playerID')
MasterData = left_join(MasterData, Salary, by = 'playerID')
MasterData = subset(MasterData, select = -c(X.x, bats, throws, debut, finalGame, yearID.x, stint, Hitter, yearID.y, yearID, teamID, lgID, X.y))
names(MasterData)[42] = 'MaxSalary'
MasterData[is.na(MasterData)] = 0

#Create master data frame for all Hall of Fame eligible players
HOFPitch = right_join(CareerPitching, HOF2, by = "playerID")
HOFPitch = subset(HOFPitch, select = -c(X))
HOFHit = right_join(CareerBatting, HOF2, by = "playerID")
HOFHit = subset(HOFHit, select = -c(X))
HOFMaster = left_join(HOFHit, HOFPitch, by = 'playerID')
HOFMaster = left_join(HOFMaster, ASGAwards, by = 'playerID')
HOFMaster = subset(HOFMaster, select = -c(Hitter.y, inducted.y))
names(HOFMaster)[12] = 'Hitter'
names(HOFMaster)[13] = 'Inducted'
HOFMaster = HOFMaster %>% relocate (Hitter, .after = playerID)
HOFMaster = HOFMaster %>% relocate (Inducted, .after = last_col())
HOFMaster[is.na(HOFMaster)] = 0
#Removed any Hall of Fame inductees that played before the Dead Ball Era
HOFMaster = HOFMaster[HOFMaster$GH > 0,]

#Remove playerID column since these are unique to each player
HOFBlind = subset(HOFMaster, select = -c(playerID))

#Create Train/Test split for HOFMaster data for future models
set.seed(1)
train = sample(1:nrow(HOFBlind), 795)
HOF.train = HOFBlind[train,]
HOF.test = HOFBlind[-train,]

#Remove playerID column from SeasonStats since playerID is unique variable
#Then, create Train/Test split for SeasonStats for future models
SSBlind = subset(SeasonStats, select = -c(playerID))
set.seed(1)
train2 = sample(1:nrow(SSBlind), 23290)
SS.train = SSBlind[train2,]
SS.test = SSBlind[-train2,]

#Linear Regression model for salary prediction
SScor = cor(SSBlind)
write.csv(SScor, file = 'SScor.csv')
#Created correlation matrix to find which variables have highest correlation values with salary


#Classification Model
#LogisticRegression
HOFcor = cor(HOFBlind)
write.csv(HOFcor, file = 'HOFcor.csv')

#Train/Test Split for LogReg
glmHOF = glm(Inducted ~ ASG + TotalAwards + BBMagAllStar + X3B + RegMVP, data = HOF.train, family = "binomial")
summary(glmHOF)
HOFtestprob = predict(glmHOF, type = 'response', newdata = HOF.test)
HOFpred2 = rep('No', length(HOFtestprob))
HOFpred2[HOFtestprob > 0.5] = 'Yes'
table(HOFpred2, HOF.test$Inducted)
#The model predicted 235/266 players correctly (88.3%).
#The model correctly predicted 214/219 players that didn't make the Hall (97.7%).
#The model correctly predicted 21/47 players that made the Hall (44.7%).
#This model is once again better suited for predicting players that didn't make it.

#QDA
qda1 = qda(factor(Inducted) ~ ASG + TotalAwards + BBMagAllStar + X3B + RegMVP, data = HOF.train)
qda1prob = predict(qda1, type = "response", newdata = HOF.test)
qda1class = qda1prob$class
table(qda1class, HOF.test$Inducted)
#The model predicted 226/266 players correctly (85.0%)
#The model correctly predicted 198/219 players that didn't make the Hall (90.4%)
#The model correctly predicted 28/47 players that did make the Hall (59.6%)
#The QDA model is much better at predicting Hall of Fame members than the 
#Logistic Regression.

#KNN
HOF.train$Inducted = as.factor(HOF.train$Inducted)
HOF.test$Inducted = as.factor(HOF.test$Inducted)
str(HOF.train)

nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

HOF.train.norm = as.data.frame(lapply(HOF.train[,2:37], nor))
HOF.test.norm = as.data.frame(lapply(HOF.test[,2:37], nor))
HOFtraintarget = HOF.train[,38]
HOFtesttarget = HOF.test[,38]

HOFknn = knn(HOF.train.norm, HOF.test.norm, cl = HOFtraintarget, k = 2)
table(HOFknn, HOFtesttarget)
#After normalizing all variables in the HOF data set, the model predicted 229/266
#players correctly (86.1%).
#The model correctly predicted 200/219 players that didn't make the Hall (91.3%).
#The model correctly predicted 29/47 players that made the Hall (61.7%).
#This appears to be the best model so far, since the accuracy for players that 
#made the Hall is the highest.

#LeaveOneOutCrossValidation
glm.fit = glm(salary ~ yearID, data = SSBlind)
coef(glm.fit)
cv.err = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(salary~poly(yearID, i), data = SSBlind)
  cv.err[i] = cv.glm(SSBlind, glm.fit)$delta[1]
}
cv.err
#The LOOCV method is a terrible way to analyze this data set.
#Runtime is slow and error values are in the billions of dollars.
#A different method should be used to analyze this data.

#k-fold CV
set.seed(1)
cv.err.10 = rep(0,10)
for (i in 1:10) {
  glm.fit = glm(salary~poly(yearID, i), data = SSBlind)
  cv.err.10[i] = cv.glm(SSBlind, glm.fit, K = 10)$delta[1]
}
cv.err.10
#The k-fold Cross-Validation method is a terrible way to analyze this data set.
#With error values in the billions, this method is not good for this analysis.

#Bootstrap
attach(SSBlind)
set.seed(1)
boot.fn = function(data, index)
  return(coef(lm(salary~yearID + HR + RBI + R, data = data, subset = index)))
boot.fn(SSBlind, 1:31053)

boot(SSBlind, boot.fn, 100)
#Using the bootstrap method to help create a linear regression model gives a good 
#insight into the standard errors of every variable considered. Salary had the highest 
#standard error by far, which is to be expected, but the standard errors for 
#yearID, HR, RBI, and R were all in the thousands, which is very good considering 
#the large salaries being dealt with in this analysis. We also get the coefficients
#for each variable in the linear regression model, which is a plus.

#Principal Components Regression
set.seed(1)
SSxtrain = model.matrix(salary~., SS.train)[,-1]
SSxtest = model.matrix(salary~., SS.test)[,-1]
SSytrain = SS.train$salary
SSytest = SS.test$salary
pcr.fit = pcr(salary~., data = SS.train, scale = TRUE, validation = 'CV')
summary(pcr.fit)
validationplot(pcr.fit, val.type = 'MSEP')
#For this principal components regression analysis, the validation plot shows that 
#the optimal model occurs when M = 20. The mean squared error may be lower for models
#where M > 20, but only marginally. Therefore, a model with M = 20 components will
#suffice for this application.
pcr.pred = predict(pcr.fit, SSxtest,ncomp = 20)
mean((pcr.pred - SSytest)^2)
#The MSE for the PCR regression was 8.496e12.
testavg = mean(SS.test$salary)
pcrr2 = 1 - mean((pcr.pred - SS.test$salary)^2) / mean((testavg - SS.test$salary)^2)
pcrr2
#The R^2 value for the PCR regression was found to be 0.283. This is a decent improvement over
#the R^2 value for the linear regression model previously calculated.

#Best Subset Selection
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
k = 10
set.seed(1)
folds=sample (1:k,nrow(SSBlind),replace=TRUE)
cv.errors =matrix (NA,k,37, dimnames =list(NULL , paste (1:37)))
for(j in 1:k){
  best.fit=regsubsets (salary ~ .,data=SSBlind [folds!=j,],nvmax=37)
  for(i in 1:37){
    pred=predict (best.fit , SSBlind[folds ==j,],id=i)
    cv.errors[j,i]= mean((SSBlind$salary[ folds==j]-pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors, xlab = "Number of variables", ylab = "CV error", type = "l")
#The ideal number of variables for this method is 12.
#The cross-validated MSE is roughly 8.2e12.

#Lasso
set.seed(1)
lasso.mod = glmnet(SSxtrain, SSytrain, alpha = 1)
cv.out = cv.glmnet(SSxtrain, SSytrain, alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
#The best lambda value for the lasso method was 1269.
lasso.pred = predict(lasso.mod, s = bestlam, newx = SSxtest)
mean((lasso.pred - SSytest)^2)
#The mean squared error for the lasso method was 8.23e12, which was slightly more
#than the ridge regression model.
out = glmnet(SSxtrain, SSytrain, alpha = 1)
lasso.coef = predict(out, type = "coefficients", s = bestlam)[1:37,]
lasso.coef[lasso.coef!=0]
#There are 29 coefficients that are nonzero for this model.

#Ridge Regression
cv.out = cv.glmnet(SSxtrain, SSytrain, alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
#The best lambda value was found to be 124001.
ridge.mod = glmnet(SSxtrain, SSytrain, alpha = 0, lambda = bestlam)
ridge.pred = predict(ridge.mod, s = bestlam, newx = SSxtest)
mean((ridge.pred - SSytest)^2)
#From the plot, the cross-validated MSE for the ridge model was approximately 8.2e12.

#PLS
set.seed(1)
pls.fit = plsr(salary~., data = SS.train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
#From the plot, this time keeping 10 components will suffice.
pls.pred = predict(pls.fit, SSxtest, ncomp = 10)
mean((pls.pred - SSytest)^2)
#The mean squared error for the PLS test is 8.22e12, which is right around
#the MSE value for the ridge regression and lasso methods.

testavg = mean(SS.test$salary)
pcrr2 = 1 - mean((pcr.pred - SS.test$salary)^2) / mean((testavg - SS.test$salary)^2)
pcrr2
ridger2 = 1 - mean((ridge.pred - SS.test$salary)^2) / mean((testavg - SS.test$salary)^2)
ridger2
lassor2 = 1 - mean((lasso.pred - SS.test$salary)^2) / mean((testavg - SS.test$salary)^2)
lassor2
plsr2 = 1 - mean((pls.pred - SS.test$salary)^2) / mean((testavg - SS.test$salary)^2)
plsr2
#The R^2 value of the PLS model is 0.306, which is the highest R^2 value of any 
#model tested so far. The R^2 value of the lasso method is very close to this value,
#and the R^2 of the PCR and ridge regression models are not far behind.

#Decision Tree/ Random Forest
set.seed(1)
tree.HOF = tree(Inducted~., HOF.train)
summary(tree.HOF)
#Misclassification error rate is 43/795, or 5.4%.
#There are 26 terminal nodes in this tree.
draw.tree(tree.HOF, cex = 0.45)
predHOF = predict(tree.HOF, HOF.test, type = 'class')
table(predHOF, HOF.test$Inducted)
#The most important predictor seems to be number of All-Star Games played (ASG).
#The accuracy rate is 86.5%. Therefore, the test error rate is 13.5%
set.seed(2)
cv.HOF = cv.tree(tree.HOF, FUN = prune.misclass)
cv.HOF
#The optimal decision tree has 8 terminal nodes, since there are only 99 cross-validation
#errors. Other decision tree sizes also have 99 cross-validation errors, but
#8 nodes is the smallest tree that can be produced that still yields ideal results.

#Bagging/ Random Forest
set.seed(1)
bag.HOF = randomForest(Inducted~., data = HOF.train, mtry = 37, importance  = TRUE)
bag.HOF
#The out-of-bag estimate of error rate was 10.06%.
#The class error for class 0 (not inducted) was 4.84%.
#The class error for class 1 (inducted) was 35.8%.
#The Mean of Squared Residuals was 0.0744
#The R^2 value was 46.89, making this by far the best model to date.

set.seed(1)
rf.HOF= randomForest(Inducted~., data = HOF.train, mtry = 6, importance = TRUE)
importance(rf.HOF)
yhat.rf = predict(rf.HOF, newdata = HOFBlind[-train,])
Hall.test = HOFBlind[-train, 'Inducted']
mean((yhat.rf - Hall.test)^2)
#The mean of squared residuals was 0.0769, slightly worse than the previous model.
varImpPlot(rf.HOF)
#The three most important variables according to the MSE are BBMagAllStar,
#ASG, and X3B.

#Boosting
set.seed(1)
boost.HOF = gbm(Inducted~., data = HOF.train, distribution = 'gaussian',
                n.trees = 5000, interaction.depth = 4)
summary(boost.HOF)
#The most important variables in the boosted tree are GH, X3B, ASG, and AB.
plot(boost.HOF, i = 'GH')
plot(boost.HOF, i = 'X3B')
plot(boost.HOF, i = 'ASG')
yhat.boost = predict(boost.HOF, newdata = HOFBlind[-train,], n.trees = 5000)
mean((yhat.boost - Hall.test)^2)
#The test MSE obtained is 0.1037, again slightly more than the previous models.
boost.HOF2 = gbm(Inducted~., data = HOF.train, distribution = 'gaussian',
                n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost2 = predict(boost.HOF2, newdata = HOFBlind[-train,], n.trees = 5000)
mean((yhat.boost2 - Hall.test)^2)
#The test MSE is 0.1046, again slightly less than the previous model. 