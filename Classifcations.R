library(tidyverse)
library(randomForest)
library(caret)
library(class)
library(e1071)

#reading in the Dataset
path3<-"/Users/njaccard/Documents/uMiami/spring2022/statLearning/streaming.csv"
dataframe<-read.csv(path3,header=TRUE)
summary(dataframe)
dataframe$Service=as.factor(dataframe$Service)
#class imbalance
one<-which(dataframe$Service==1)
two<-which(dataframe$Service==2)
three<-which(dataframe$Service==3)
four<-which(dataframe$Service==4)

#down sampling to 1450
one.downsample<-sample(one,1450)
two.downsample<-sample(two,1450)
three.downsample<-sample(three,1450)
four.downsample<-sample(four,1450)
df1<-dataframe%>%slice(one.downsample)
df2<-dataframe%>%slice(two.downsample)
df3<-dataframe%>%slice(three.downsample)
df4<-dataframe%>%slice(four.downsample)
dataframe<-rbind(df1,df2,df3,df4)

#spilitng the data
split <- sample.split(dataframe, SplitRatio = 0.8)
train_cl <- subset(dataframe, split == "TRUE")
test_cl <- subset(dataframe, split == "FALSE")

# generate a random number of  80 percent of everything
randomNum <- sample(1:nrow(dataframe), 0.8 * nrow(dataframe))

# everything you need for my knn process
# just checking for NA, then pulling out all irrelevant information
# we might have done this slightly differently, ahh whateverr
rmNaDf <- na.omit(dataframe)
rmNaDfTestTrain <- rmNaDf[,c(8:46)]

# The test factors isolated to make an easy table later
dfTargCat <- rmNaDf[randomNum, 8]

# adding noise because of similar ties error (can go into more detail about that)
# if ya need!
corrupt <- rbinom(length(rmNaDfTestTrain),1,0.1)    # choose an average of 10% to corrupt at random
corrupt <- as.logical(corrupt)
noise <- rnorm(sum(corrupt),.01,.001) # generate the noise to add
rmNaDfTestTrain[corrupt] <- rmNaDfTestTrain[corrupt] + noise

# this is our holdout, we basically disclude 20% of data to use for testing
dfTrain <- rmNaDfTestTrain[randomNum,]
dfTest <- rmNaDfTestTrain[-randomNum,]

# set a seed idk its good practice
set.seed(9009)

# so this is the whole "cv" part, i make a trainControl object
# which basically is telling it 5 fold cross validation
trnctrl <- trainControl(method = "cv", 5)

# train said model
trainModel <- train(Service~., data = dfTrain, method = "knn", trControl = trnctrl)
# plot(trainModel)

#truncating the variables (theyre always above their value, never below, so this works)
knnPredData <- as.integer(predict(trainModel, dfTest))
#knnPredData
#dfTestCat
knnTable <- table(knnPredData, dfTestCat)
#knnTable

# accuracy function
accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}
accuracy(knnTable)

#The end, onto svm

#resetting this so there isn't any noise
rmNaDf <- na.omit(dataframe)
rmNaDfTestTrain <- rmNaDf[,c(8:46)]

dfTrain <- rmNaDfTestTrain[randomNum,]

# same cv as above, just moved it down
trnctrl <- trainControl(method = "cv", 5)
#trained it
svm1 <- train(Service ~., data = rmNaDfTestTrain, method = "svmRadial", trControl = trnctrl)
# svmPredData <- predict(svm1, cutPieceOneRow)


svmTable <- table(svmPredData, dfTestCat)
svmTable
accuracy(svmTable)

##########################################################
# everything after here is my thought process, your work is below it!!

rmNaDf <- na.omit(dataframe)
rmNaDf <- rmNaDf[,c(8:46)]

#heres me making the example tv show
oneRow <- slice_head(rmNaDf)
oneRow$title <- "The Best Project Ever"
oneRow$date_added <- "April 19, 2022"
oneRow$release_year <- 2022L
oneRow$duration <- "2 Seasons"
oneRow$comedy <- 0L
oneRow$sitcom <- 1L
oneRow$drama <- 1L
oneRow$action <- 1L
oneRow$ContentType <- 0L

cutPieceOneRow <- oneRow[,c(9:46)]
cut


dfTrain <- rmNaDf[randomNum,]
dfTest <- rmNaDf[-randomNum,]

dfTargCat <- rmNaDf[randomNum, 8]
dfTestCat <- rmNaDf[-randomNum, 8]

as.numeric(dfTrain)

#knn function!
#running CV to optimize for K value
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1)
#runs the test
knn_fit <- train(Service~.-title-duration-listed_in-date_added-rating-release_year, data = dataframe, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
#use the best K value
pr <- knn(dfTrain, dfTest, cl = dfTargCat, k = 13)

#issue at hand, too many ties, basically it is too clearly 0 or too clearly 1
#figuring how to make these floating points and to give them slight noise

corrupt <- rbinom(length(rmNaDf),1,0.1)    # choose an average of 10% to corrupt at random
corrupt <- as.logical(corrupt)
noise <- rnorm(sum(corrupt),.01,.001) # generate the noise to add
rmNaDf[corrupt] <- rmNaDf[corrupt] + noise      # about 10% of x has been corrupted

#we ride again
pr <- knn(dfTrain, dfTest, cl = dfTargCat, k = 13)
pr
tab <- table(pr, dfTestCat)
tab
dfTestCat

accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}
accuracy(table(pred, dfTestCat))
#my test got a 71% accuracy

# Support vector Machine
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,         # do 5 repetitions of cv
                     summaryFunction=twoClassSummary, 
                     classProbs=TRUE)
set.seed(1)
#tunning for the best Cost value 
tune.out=tune(svm,STAT_CAUSE ~ ., data = training_set,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),trControl=ctrl)
bestmod=tune.out$best.model
summary(bestmod)

svmfit <- svm(Service~.-title-duration-listed_in-date_added-rating-release_year-ContentType, data = dfTrain, scale = F, kernel = "radial", cost = 20, type="C-classification")
pred <- predict(svmfit, newdata = dfTest)
svmfit$fitted

table(pred, dfTestCat)

prd <- predict(svmfit)
dfTestCat
tabl <- table(pred, dfTestCat)
accuracy(tabl)
tabl <- table(dfTestCat, svmfit$fitted)

table(svmfit$fitted)

table(table(dfTestCat), table(svmfit$fitted))

val <- 0
for (i in predict(svmfit)) {
  if(i == 1){
    val <- val + 1
  }
}
predict(svmfit) == 2
# 4 = 9668, 3 = 8807, 2 = 1450, 1 = 3073

vals <- c(9668, 8807, 1450, 3073)
vals / 22998




#linear
lm.fit1 = lm(as.numeric(Service)~.-title-duration-listed_in-date_added-rating,data=dfTrain)
summary(lm.fit1)
#predicting
lm.probs = predict(lm.fit1,dataframe, type="response")
plot(lm.probs)

#Classifcation tree
tree.stream=tree(Service~.-title-duration-listed_in-date_added-rating,dfTrain)
summary(tree.stream)
plot(tree.stream) # plotting the tree
text(tree.stream, pretty = 0, cex=0.75)
pred = predict(tree.stream, type="class")
table(pred)

#random forest
#preforming 10 fold CV 3 times using random search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(1)
#tunning for the mtry value
rf_random <- train(Service~.-title-duration-listed_in-date_added-rating, data=dfTrain, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
#preforming 10 fold CV 3 times using grid search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(1)
tunegrid <- expand.grid(.mtry=c(1:15))

rf_gridsearch <- train(Service~.-title-duration-listed_in-date_added-rating, data=dfTrain, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
BestModel1 <- randomForest(Service~.-title-duration-listed_in-date_added-rating, data = dfTrain,subset=train,mtry=3, importance = TRUE)
#Naive bayes
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(Service~.-title-duration-listed_in-date_added-rating, data = dfTrain)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$Service, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)




