# 1. TARGET_B

#loading libraries
library(caret)
library(gains)
library(rpart)
library(rpart.plot)

#loading files
fund.df<-read.csv("Fundraising.csv")
futurefund.df<-read.csv("FutureFundraising.csv")
futurefundraising.df<-read.csv("FutureFundraising.csv") # to enter final predicted values

#check for missing values if any
is.null(fund.df)
is.null(futurefund.df)

#understanding the data
View(fund.df)
sapply(fund.df, class)
head(fund.df)
levels(as.factor(fund.df$WEALTH))
levels(as.factor(fund.df$INCOME))
summary(fund.df)
summary(futurefund.df)

#changing class of few columns
cols <- c(3, 4, 5, 6, 7, 9, 10, 11, 23)
fund.df[,cols] <- lapply(fund.df[,cols], factor)
futurefund.df[,cols] <- lapply(futurefund.df[,cols], factor)
sapply(fund.df, class)

#normalising datasets
normalize_min_maz<- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
fund.df[,c(12:22)] <- normalize_min_maz(fund.df[,c(12:22)])
futurefund.df[,c(12:22)] <- normalize_min_maz(futurefund.df[,c(12:22)])

#creating training and validation sets
set.seed(1109)
train.index <- sample(nrow(fund.df), nrow(fund.df)*0.6)
train.df <- fund.df[train.index, -c(1,2,24)]
valid.df <- fund.df[-train.index, -c(1,2,24)]


#building the model - decision tree
class.tree <- rpart(TARGET_B ~., data = train.df, method = "class")
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)


#assess model's performance on training data (confusion matrix and lift chart)
class.tree.pred.train <- predict(class.tree, train.df, type = "class")
confusionMatrix(class.tree.pred.train, as.factor(train.df$TARGET_B))

class.tree.pred.train_p <- predict(class.tree, train.df)
class.tree.pred.train_p <- as.data.frame(class.tree.pred.train_p)
train.df$prob1 <- class.tree.pred.train_p[,2]
train.df$prob0 <- class.tree.pred.train_p[,1]

lift.train1 <- lift(relevel(as.factor(TARGET_B), ref = "1") ~ prob1, data = train.df)
xyplot(lift.train1, plot = "gain", main = "Lift chart of Donor on training data")

lift.train0 <- lift(relevel(as.factor(TARGET_B), ref = "0") ~ prob0, data = train.df)
xyplot(lift.train0, plot = "gain", main = "Lift chart of non-donor on training data")


#assess model's performance on validation data (confusion matrix and lift chart)
class.tree.pred.valid <- predict(class.tree, valid.df, type = "class")
confusionMatrix(class.tree.pred.valid, as.factor(valid.df$TARGET_B))

class.tree.pred.valid_p <- predict(class.tree, valid.df)
class.tree.pred.valid_p <- as.data.frame(class.tree.pred.valid_p)
valid.df$prob1 <- class.tree.pred.valid_p[,2]
valid.df$prob0 <- class.tree.pred.valid_p[,1]

lift.valid1 <- lift(relevel(as.factor(TARGET_B), ref = "1") ~ prob1, data = valid.df)
xyplot(lift.valid1, plot = "gain", main = "Lift chart of Donor on validation data")

lift.valid0 <- lift(relevel(as.factor(TARGET_B), ref = "0") ~ prob0, data = valid.df)
xyplot(lift.valid0, plot = "gain", main = "Lift chart of non-donor on validation data")


#making predictions of TARGET_B on futurefundraising
class.tree.pred.futurefund <- predict(class.tree, futurefund.df, type = "class")
class.tree.pred.futurefund <- as.data.frame(class.tree.pred.futurefund)
futurefund.df$TARGET_B <- class.tree.pred.futurefund[,1]


# 2. TARGET_D

#creating dataframe with only donor(TARGET_B = 1) from fundraiser
funddonor.df <- fund.df[fund.df$TARGET_B == 1,]
sapply(funddonor.df, class)

#creating training and validation sets
set.seed(1199)
train2.index <- sample(nrow(funddonor.df), nrow(funddonor.df)*0.5)
train2.df <- funddonor.df[train2.index, -c(1,2,23)]
valid2.df <- funddonor.df[-train2.index, -c(1,2,23)]


#building multiple regression model
funddonor.lm <- lm(TARGET_D ~., data = train2.df)
options(scipen = 999)
summary(funddonor.lm)


#assessing model's performance on training data
library(forecast)
funddonor.lm.pred.t <- predict(funddonor.lm, train2.df, na.action = na.pass)
View(funddonor.lm.pred.t)
options(scipen = 999, digits = 0)

options(scipen = 999, digits = 3)
accuracy(funddonor.lm.pred.t, train2.df$TARGET_D)

res.train <- train2.df$TARGET_D - funddonor.lm.pred.t
hist.train <- hist(res.train, breaks = 25, xlab = "Residuals on Training Data", xlim = c(-50,50), main = "Histogram of Training Dataset Residuals" )

#assessing model's performance on validation data
funddonor.lm.pred.v <- predict(funddonor.lm, valid2.df, na.action = na.pass)
View(funddonor.lm.pred.v)
options(scipen = 999, digits = 0)


options(scipen = 999, digits = 3)
accuracy(funddonor.lm.pred.v, valid2.df$TARGET_D)

res.valid <- valid2.df$TARGET_D - funddonor.lm.pred.v
hist.train <- hist(res.valid, breaks = 25, xlab = "Residuals on Validation Data", xlim = c(-50,50), main = "Histogram of Validation Dataset Residuals" )

#predicting TARGET_D
futurefunddonor.df <- futurefund.df[futurefund.df$TARGET_B == 1,]
futurefunddonor.df <- futurefunddonor.df[, -c(1,2,23)]

futurefunddonor.lm.pred <- predict(funddonor.lm, futurefunddonor.df, na.action = na.pass)
futurefunddonor.lm.pred <- as.data.frame(futurefunddonor.lm.pred)

futurefunddonor.df$TARGET_D <- futurefunddonor.lm.pred[,1]


futurefund.df$TARGET_D[futurefund.df$TARGET_B==1] <- futurefunddonor.lm.pred[,1]
futurefund.df$TARGET_D[futurefund.df$TARGET_B==0] = 0


#puttinf values of TARGET_D in original futurefundraising dataset
futurefundraising.df$TARGET_B <- futurefund.df$TARGET_B
futurefundraising.df$TARGET_D <- futurefund.df$TARGET_D

View(futurefundraising.df)
length(futurefundraising.df$TARGET_B[futurefundraising.df$TARGET_B==1])

#Exporting final updated file
write.csv(futurefundraising.df, "~/Desktop//FutureFundRaising.csv", row.names = TRUE)