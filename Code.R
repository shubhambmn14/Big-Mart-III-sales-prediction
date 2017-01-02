rm(list = ls())
setwd("D://Sunstone//kaggle//BigMart")
install.packages("VIM") #For running kNN imputations
install.packages("missForest")# for running missForest to impute missing values
install.packages("ggplot2")
library(missForest)
library(VIM)
library(randomForest)
library(plyr)
library(ggplot2)
#Reading the training data
train <- read.csv("train.csv")
#Reading the test data
test <- read.csv("test.csv")
#Lets have a look at the data set in the train.
str(train)
#Shows that we have 12 variables (coloumns) with 8523 obs. (Rows)
summary(train)
#Summay shows that there are many Item-weight entries which are not available (NA). Also there are some values in Outlet_Size which are
#not having any factor associated with them (i.e. either high, medium or low)
#lets have a look at test data as well.
str(test)
summary(test)
#Test data has 5681 rows with 11 variables.
#Again we see that there are lot of not available (NAs) values for item_weight variable. Similarly Outlet_size has some missing values
#Lets combine the data together for the imputations...
#Lets propogate new variable Item_Outlet_Sales in the test data before going ahead.
test$Item_Outlet_Sales <- 0
combi <- rbind(train,test)
#checking the names of the rows
str(combi)
#----------combi has 14204 rows and 12 coloumns-----------#
summary(combi)
#--------Imputing the missing and incorrect data.---------#
# We can clearly see that Item fat content has some dicey factors. Based on cursory look, its
#clear that the LF, low fat are same as Low Fat.
#Also the reg is actual Regular fat content category.
#converting the factor to character variable for the manipulation
combi$Item_Fat_Content <- as.character(combi$Item_Fat_Content)
combi$Item_Fat_Content[combi$Item_Fat_Content == 'reg'] <- 'Regular'
revalue(combi$Item_Fat_Content, c("LF" = "Low Fat", "low fat" = "Low Fat"))#Using the plyr package
combi$Item_Fat_Content[combi$Item_Fat_Content == 'LF'] <- 'Low Fat'
combi$Item_Fat_Content[combi$Item_Fat_Content == 'low fat'] <- 'Low Fat'
combi$Item_Fat_Content <- factor(combi$Item_Fat_Content)
#now checking if the above transformation has taken place
summary(combi$Item_Fat_Content)
#Examining closely the data, we can see the Item identifier starts with Either DR or a NC or a FD.
#By observing the Item type corresponding to each of them, we can safely predict that,
#DR = Drinks, FD = Foods, NC = Non-consumables
#changing the Item_type.
#converting the factor to character variable
combi$Item_Type <- as.character(combi$Item_Type)
combi$Item_Type[combi$Item_Type %in% c('Fruits and Vegetables','Canned', 'Snack Foods', 'Frozen Foods','Baking Goods', 'Breads','Meat', 'Seafood','Frozen Foods','Starchy Foods', 'Breakfast')] <- 'Foods'
combi$Item_Type[combi$Item_Type %in% c('Health and Hygiene','Household','Others')] <- 'Non-Consumable'
combi$Item_Type[combi$Item_Type %in% c('Dairy','Soft Drinks','Hard Drinks')] <- 'Drinks'
#Checking if the changes are committed
combi$Item_Type <- factor(combi$Item_Type)
#Now we have only three factors for the Item_type variable
summary(combi)
#We also see that there are lot of items which have 0 visibility in the stores.
#unless and untill they are stored in the warehouses, this dont make any sense.
#So converting the 0 values to NA too. This will help the function to impute their values aswell
combi$Item_Visibility[combi$Item_Visibility == 0] <- NA
combi$Outlet_Size <- as.character(combi$Outlet_Size)
combi$Outlet_Size[combi$Outlet_Size == ' '] <- NA
combi$Outlet_Size <- factor(combi$Outlet_Size)
boxplot(Item_Weight ~ Outlet_Identifier, data = combi, las=1, xlab="Store Name", ylab = "Item Weight")
#We can see that only for outlet 19 and 27 the data is missing.
#Also we see that the item_weights are equally distributed among the stores and the products whose values are missing for these stores can be found elsewhere.
#Hence we can safely impute those variables.
#running kNN on the missing Item_weight
combi2 <- kNN(combi, variable = "Item_Weight", k = 5)
boxplot(Item_Weight ~ Outlet_Identifier, data = combi2, las=1, xlab="Store Name", ylab = "Item Weight")
#looks the values are not imputed correctly.
combi3 <- missForest(combi[,-1], ntree = 100, maxiter = 10)
boxplot(Item_Weight ~ Outlet_Identifier, data = combi3$ximp, las=1, xlab="Store Name", ylab = "Item Weight")
summary(combi3$ximp$Item_Weight) #check imputed values
combi3$OOBerror #check imputation error
#the box plot looks terrible for the Item weight and the Outlet Identifier
#Hence keeping the kNN imputation values for the missing item_weights
#Checking the structure of combi data again....
combi$Item_Weight <- combi2$Item_Weight
summary(combi)
boxplot(Item_Weight ~ Outlet_Identifier, data = combi, las=1, xlab="Store Name", ylab = "Item Weight")
boxplot(Item_Outlet_Sales ~ Outlet_Type, data = combi, las=1, xlab="Outlet Type", ylab = "Item Sales")
#Clearly sales are quite different based on store types: Grocery having the least sales 
#while Supermarket type 3 having the highest sales.
#We also see that there are lot of Outliers especiallyin the Supermarket type 3 and 1

#Looking at the Fat content, we can see that for non-conumables also we have 
#fat content as low fat.
#This doesnt make any sense. Making a new factor called none for those rows.
table(combi$Item_Type, combi$Item_Fat_Content)
combi$Item_Fat_Content <- as.character(combi$Item_Fat_Content)
combi$Item_Fat_Content[combi$Item_Type == 'Non-Consumable'] <- 'None'
combi$Item_Fat_Content <- factor(combi$Item_Fat_Content)
summary(combi)
#Imputing the missing values for Item_Weight
# Kernel Density Plot
d <- density(combi$Item_MRP) # returns the density data 
plot(d) # plots the results

ggplot(combi, aes(x=Item_MRP)) + 
  geom_density(color = "blue", adjust=1/10) +
  geom_vline(xintercept = 69, color="red")+
  geom_vline(xintercept = 136, color="red")+
  geom_vline(xintercept = 203, color="red") + 
  ggtitle("Density of Item MRP")

# Clearly, there are four different price categories
# We'll introduce a new factor MRP_Level to mark
# those groups

combi$MRP_Level <- as.factor(
  ifelse(combi$Item_MRP < 69, "Low",
         ifelse(combi$Item_MRP < 136, "Medium",
                ifelse(combi$Item_MRP < 203, "High", "Very_High"))))

#Changing the year of establishment varibale to years of existense (2013-Establishment year)c
combi$Outlet_Age <- 2013 - as.numeric(combi$Outlet_Establishment_Year)
summary(combi)
#Plotting the outlet vs item visibility box plot
boxplot(Item_Visibility ~ Outlet_Identifier, data = combi, xlab = "Outllet Identifier", ylab="Item-visibility")
#Clearly shows that OUT010 and OUT019 are having more space and lesser items
 # this seems to be the case for Grocery stores
#again using kNN function to impute the missing Item_visibility
combi3 <- kNN(combi, variable = "Item_Visibility", k = 5)
boxplot(Item_Visibility ~ Outlet_Identifier, data = combi3, xlab = "Outllet Identifier", ylab="Item-visibility")
combi$Item_Visibility <- combi3$Item_Visibility
summary(combi)
combi$Outlet_Size <- as.character(combi$Outlet_Size)
sum(is.na(combi$Outlet_Size))
#How often each outlet appears in the data
table(combi$Outlet_Identifier)
aggregate(combi$Outlet_Identifier, by=list(Category=combi$Outlet_Identifier), FUN=length)
#----------Lets analyze the data for some trends----------#

# boxplot of  Sales vs. Outlet identifier
ggplot(combi[1:nrow(train),], aes(Outlet_Identifier, Item_Outlet_Sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet identifier") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet identifier")

combi$Outlet_Size <- factor(combi$Outlet_Size)

#Which stores is the Outlet size missing? Lets explore it further
plot(combi$Outlet_Identifier, combi$Outlet_Size)
#This shows that the outlet 10, 17 and 45 are missing
aggregate()
combi$Outlet_Size[combi$Outlet_Size == ""] <- NA
combi2 <- kNN(combi, variable = "Outlet_Size", k = 5)
table(combi$Outlet_Size, combi$Outlet_Identifier)
combi$Outlet_Size <- factor(combi$Outlet_Size)
#We see that KNN has generated below:

##########OUT010 OUT013 OUT017 OUT018 OUT019 OUT027 OUT035 OUT045 OUT046 OUT049
#High        0   1553      0      0      0      0      0      0      0      0
#Medium    366      0   1134   1546      0   1559      0    611      0   1550
#Small     559      0    409      0    880      0   1550    937   1550      0

#This is certainly not what we wanted. Lets make OUT010 to small, OUT017 to Medium
#Out045 to Small.
combi$Outlet_Size <- as.character(combi$Outlet_Size)
combi$Outlet_Size[combi$Outlet_Identifier == 'OUT010'] <- 'Small'
combi$Outlet_Size[combi$Outlet_Identifier == 'OUT017'] <- 'Medium'
combi$Outlet_Size[combi$Outlet_Identifier == 'OUT045'] <- 'Small'
# boxplot of  Sales vs. Outlet Type
ggplot(combi[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Establishment_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet Type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Type")

combi$Outlet_Size <- combi2$Outlet_Size
table(combi$Outlet_Size, combi$Outlet_Identifier)
summary(combi)
#Now complete data is NA free and adequate to run the random forest algorithm to predict sales
train_mod <- combi[1:8523, -c(6,8)]
test_mod <- combi[8524:14204, -c(6,8)]
#-------------------------------------------------------------------#
#|               Running Random Forest Model                        |
#-------------------------------------------------------------------#  
#Running the random forest on the training data.
set.seed(2016)
fit <- randomForest(Item_Outlet_Sales ~ ., data = train_mod[,-1], importance = T, ntree = 500)
plot(fit)
#plot shows that the the error after tree numbers of 300 the error decrease is almost negligible
varImpPlot(fit, sort = T, main = "Variable Importance", n.var = 6)
train_mod$Predicted_Sales <- predict(fit, train_mod)
#Measuring the correlation between the actual and the predicted value
train.corr <- round(cor(train_mod$Predicted_Sales, train_mod$Item_Outlet_Sales),2)
#Measuring the RMSE
train.RMSE <- round(sqrt(mean(train_mod$Item_Outlet_Sales - train_mod$Predicted_Sales)^2),2)
train.MAE <- round(mean(abs(train_mod$Item_Outlet_Sales- train_mod$Predicted_Sales)))
c(train.corr,train.RMSE,train.MAE)
#0.85   1.88   642.00
test_mod$Item_Outlet_Sales <- predict(fit, test_mod)
write.csv(test_mod, file = "RandomForest1.csv", row.names = FALSE)

# The result shows a RMSE of 1182 - Rank 262
#Running one more iteration
fit1 <- randomForest(Item_Outlet_Sales ~ MRP_Level+ Outlet_Identifier + Outlet_Type + Item_Visibility + Item_Weight + Outlet_Age, data = train_mod, importance = T, ntree = 300)
plot(fit1)
print(fit1)
print(importance(fit1,type = 2)) 
varImpPlot(fit1, sort = T, main = "Variable Importance", n.var = 6)
train_mod$Predicted_Sales <- predict(fit1, train_mod)
train.RMSE <- round(sqrt(mean(train_mod$Item_Outlet_Sales - train_mod$Predicted_Sales)^2),2)
train.MAE <- round(mean(abs(train_mod$Item_Outlet_Sales- train_mod$Predicted_Sales)))
c(train.corr,train.RMSE,train.MAE)
write.csv(test_mod, file = "RandomForest2.csv", row.names = FALSE)
#Removing the Outlet_Age variable from the model.
#Running the model again using only top 5 important variables
fit2 <- randomForest(Item_Outlet_Sales ~ MRP_Level+ Outlet_Identifier + Outlet_Type + Item_Visibility + Item_Weight, data = train_mod, importance = T, ntree = 100)
plot(fit2)
print(fit2)
train_mod$Predicted_Sales <- predict(fit2, train_mod)
train.corr <- round(cor(train_mod$Predicted_Sales, train_mod$Item_Outlet_Sales),2)
train.RMSE <- round(sqrt(mean(train_mod$Item_Outlet_Sales - train_mod$Predicted_Sales)^2),2)
train.MAE <- round(mean(abs(train_mod$Item_Outlet_Sales- train_mod$Predicted_Sales)))
c(train.corr,train.RMSE,train.MAE)
test_mod$Item_Outlet_Sales <- predict(fit2, test_mod)
write.csv(test_mod, file = "RandomForest3.csv", row.names = FALSE)
rm(fit)
#-----------------------------------------------------------------#
#           Running Gradient Boosting Machine Learning Algo.
#-----------------------------------------------------------------#

install.packages("xgboost")
install.packages("Matrix")
install.packages("caret")
library(caret)
library(Matrix)
library(xgboost)
require(xgboost)

# one-hot-encoding categorical features
ohe_features = c('Item_Fat_Content', 'Item_Type', 'Outlet_Identifier', 'Outlet_Size', 'Outlet_Type', 'Outlet_Location_Type')
dummies <- dummyVars(~ Item_Fat_Content + Item_Type + Outlet_Identifier + Outlet_Size + Outlet_Type + Outlet_Location_Type, data = combi)
df_all_ohe <- as.data.frame(predict(dummies, newdata = combi))
df_all_combined <- cbind(combi[,-c(which(colnames(combi) %in% ohe_features))],df_all_ohe)
train_xg <- df_all_combined[1:8523, -1]
test_xg <- df_all_combined[8524:14204, -1]
train_xg <- train_xg[,-c(4,6)]#removed the Sales data from train
test_xg <- test_xg[,-c(4,6)]
train_xg <- train_xg[,-22]
test_xg <- test_xg[,-22]

#train_xg <- train_xg[,-4]#remove the establishment year
train_xg <- train_xg[c(1,2,3,5:ncol(train_xg),4)] # Reordering the coloumns
#Sales data is removed from the train_xg dataset
train_xg$Item_Outlet_Sales <- NULL
test_xg$Item_Outlet_Sales <- NULL
#Based on the principal component analysis PCA, it is important 
#that features have maximum variance for maximum uniqueness, so that 
#each feature is as distant as possible (as orthogonal as possible) 
#from the other features.
zero.var = nearZeroVar(train_xg, saveMetrics=TRUE)
zero.var
#From the data we can see no variable has zero variance. 

# convert data to matrix
train_matrix <- as.matrix(train_xg)
test_matrix <- as.matrix(test_xg)
mode(train_matrix) = "numeric"
# convert outcome from factor to numeric matrix 
# xgboost takes multi-labels in [0, numOfClass)
y <- as.matrix(as.integer(y))
levels(y)
xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=50, 
               objective = "reg:linear"
               )
# predict values in test set
test_mod$PredictedSales <- predict(xgb, test_matrix)

write.csv(test_mod,file = "Xgboost1.csv", row.names = FALSE)
#Got a score worse than the Random Forest algorithms rmse = 1192.43
#Lets make the nround to half i.e. 25
y = as.matrix(as.integer(y))

xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=25, 
               objective = "reg:linear"
)
# predict values in test set
test_mod$PredictedSales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod,file = "Xgboost2.csv", row.names = FALSE)
#The rmse score improved to 1169. This might be due to overfitting done in the previous model
print(xgb)

#----Iteration 3----------#
install.packages("DiagrammeR")
library(DiagrammeR)
xgb.plot.tree(model = xgb)
model <- xgb.dump(xgb, with.stats = T)
model[1:10] 
# Get the feature real names
names <- dimnames(data.matrix(train_xg))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:20,])

#Tuning the parameters for the xgboost.
xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=20, 
               objective = "reg:linear"
)

# predict values in test set
test_mod$PredictedSales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod,file = "Xgboost3.csv", row.names = FALSE)
#for nround = 20, the results actually came down slightly.
#lets increase the rounds to 30 and see if there is any changes in rmse

xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=23, 
               objective = "reg:linear"
)

# predict values in test set
test_mod$PredictedSales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod,file = "Xgboost4.csv", row.names = FALSE)
# we have a new lowest rmse = 1167.31. Nice!! 

#lets run one more iteration by changing the nrounds as 22
xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=22, 
               objective = "reg:linear"
)

# predict values in test set
test_mod$PredictedSales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod,file = "Xgboost5.csv", row.names = FALSE)
#rmse = 1170

#Lets add one more parameter to the tree eval_metric = "auc"
xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=10, 
               objective = "reg:linear"
              )

cv.res = xgb.cv(data = train_matrix, nfold = 5, label = y, nround = 50,objective = "reg:linear", early_stopping_rounds = 20, sub_sample = 0.7)

# predict values in test set
test_mod$PredictedSales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod,file = "Xgboost6.csv", row.names = FALSE)
#with nrounds = 10, rsme = 1169 which is not better then nrounds = 23

xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=25, 
               objective = "reg:linear",
               max_depth = 6,
               sub_sample = 0.9,
               eta = 0.3
)
# predict values in test set
test_mod$PredictedSales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod,file = "Xgboost12.csv", row.names = FALSE)
#Again we are getting same results, rsme = 1169. NO IMPROVEMENTS!!
xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround=10, 
               objective = "reg:linear",
               max_depth = 6,
               sub_sample = 0.9,
               eta = 0.3
)
test_mod$Item_Outlet_Sales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod[,c(1,6,13)],file = "Xgboost13.csv", row.names = FALSE)

names <- dimnames(data.matrix(train_xg))[[2]]

# Compute feature importance matrix
class(importance_matrix) <- xgb.importance(names, model = xgb)
# Nice graph
#getting the last 5 factors based on least importance.
names_Limp <- importance_matrix[20:24, 1]
xgb.plot.importance(importance_matrix[1:20,])

#removing the above coloums for the computations.
train_xg1 <- train_xg[-which(colnames(train_xg) %in% names_Limp$Feature)]
test_xg1 <- test_xg[-which(colnames(test_xg) %in% names_Limp$Feature)]

# convert data to matrix
train_matrix <- as.matrix(train_xg1)
test_matrix <- as.matrix(test_xg1)
mode(train_matrix) = "numeric"
# convert outcome from factor to numeric matrix 

#Running again the model which gave us the best score of 1167.

xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround= 9, 
               sub_sample = 0.9,
               objective = "reg:linear"
)

# predict values in test set
test_mod$Item_Outlet_Sales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod,file = "Xgboost14.csv", row.names = FALSE)
#Finally the score improved to 1166.14!! New better score.

#-------------Running another iteration-------------#

xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround= 10, 
               sub_sample = 0.8,
               objective = "reg:linear"
)

# predict values in test set
test_mod$Item_Outlet_Sales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod[,c(1,6,11)],file = "Xgboost15.csv", row.names = FALSE)

#We have a new high score of 1163.72. We are now at 191th position.


#-------------Running another iteration-------------#

xgb <- xgboost(data = train_matrix, 
               label = y, 
               nround= 10, 
               sub_sample = 0.7,
               objective = "reg:linear"
)

# predict values in test set
test_mod$Item_Outlet_Sales <- predict(xgb, test_matrix)
#writing the file back to local disk
write.csv(test_mod[,c(1,6,11)],file = "Xgboost16.csv", row.names = FALSE)
#The result is exactly same at the earlier: rmse = 1163.72


