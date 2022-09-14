library(ISLR)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)


#call the Carseats data frame from the ISLR package
df <- Carseats
str(df)
head(df)

#set a threshold for the sales data set to enable a binary conversion.

hist(df$Sales)
summary(df$Sales)

#average is ~7.5 rounded up as 8. We can set a threshold where,
# using the ifelse function 


df$sales.binary <- ifelse(df$Sales<=8, 'Low','High') 
df$sales.binary <- as.factor(df$sales.binary)

#removing the sales variable because its redundant at this point.

df <- select(df, -Sales)

#split df data frame using CaTools

library(caTools)

set.seed(101) 

sample = sample.split(df$sales.binary, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#you can use the tree or rpart package to create a decision tree.
#tree.carseats = tree(High~.-Sales, data=carseats)
tree <- rpart(sales.binary ~.,method='class',data = train)
summary(tree)

tree.preds <- predict(tree,test)
tree.preds

tree.preds <- as.data.frame(tree.preds)
# Lots of ways to do this

#tree.preds$sales.binary <- ifelse(tree.preds<=8, 'Low','High') 

joiner <- function(x){
  if (x<=0.5){
    return('Low')
  }else{
    return("High")
  }
}

tree.preds$sales.binary <- sapply(tree.preds$Low,joiner)

table(tree.preds$sales.binary,test$sales.binary)

library(rpart.plot)
prp(tree)

#poor performance. calculate misclassifcation but the plot shows a misclassification. 
#That's good enough for me.


library(randomForest)

rf.model <- randomForest(sales.binary ~ . , data = train,importance = TRUE)
rf.model$confusion
rf.model$importance

p <- predict(rf.model,test)
table(p,test$sales.binary)
