#Load Test and Train file
LP_Test <- read.csv("https://datahack-prod.s3.ap-south-1.amazonaws.com/test_file/test_Y3wMUE5_7gLdaTN.csv", header = TRUE, sep = ",")

LP_Train <- read.csv("https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv", header = TRUE, sep = ",")

#Check Structure and Summary
str(LP_Train)
str(LP_Test)

summary(LP_Train)
summary(LP_Test)

#Get no. of missing values
sum(is.na(LP_Train))
sum(is.na(LP_Test))


#check how many Married and emplyed and graduate took Loan
#table(LP_Train$Gender, LP_Train$Loan_Status)

#table(LP_Train$Married, LP_Train$Loan_Status)

#table(LP_Train$Education, LP_Train$Loan_Status)

#table(LP_Train$Property_Area, LP_Train$Loan_Status)

#table(LP_Train$Self_Employed, LP_Train$Loan_Status)

#table(LP_Train$Dependents, LP_Train$Loan_Status)

#prop.table(table(LP_Train$Credit_History, LP_Train$Loan_Status),1)

#Combine train and Test Data

#Plots
#hist(LP_Train$ApplicantIncome, breaks = 40)

#boxplot(LP_Train$ApplicantIncome ~ LP_Train$Education)

#hist(LP_Train$LoanAmount)

#boxplot(LP_Train$LoanAmount)

#barplot(table(LP_Train$Credit_History))

#barplot(prop.table(table(LP_Train$Credit_History)))


#Load Caret library
library(caret)

#Lets treat missing values by imputing them with knn at the same time centering and scaling the numeric variables
LP_preProcValue <- preProcess(LP_Train, method = c("knnImpute","center","scale"))
LP_preProcValueTest <- preProcess(LP_Test, method = c("knnImpute","center","scale"))

#Load RANN library for faster KNN
install.packages("RANN")
library(RANN)

LP_Train_Proc <- predict(LP_preProcValue, LP_Train)
LP_Test_Proc <- predict(LP_preProcValueTest, LP_Test)

#check data again for missing vlaues
sum(is.na(LP_Train_Proc))
sum(is.na(LP_Test_Proc))
#Removing 1st column of Loan Id as it's not required
#getColNum <- grep("Loan_ID", names(LP_Test))
id_Train <- LP_Train_Proc$Loan_ID
id_Test <- LP_Test_Proc$Loan_ID

LP_Test_Proc$Loan_ID <- NULL
LP_Train_Proc$Loan_ID <- NULL
#LP_Test <- LP_Test[,-getColNum]
#LP_Train_Proc <- LP_Train_Proc[,-getColNum]

str(LP_Train_Proc)

#Create dummy variables by One hot encoding and changing each categorical variable to numeric
dummy <- dummyVars( ~., data = LP_Train_Proc, fullRank = T) #fullRank=T creates n-1 variables for a variable with n different levels
LP_Tran_Transformed <- data.frame(predict(dummy, LP_Train_Proc))

dummy <- dummyVars( ~., data = LP_Test_Proc, fullRank = T)
LP_Test_Transformed <- data.frame(predict(dummy, LP_Test_Proc))

#Married didn't have a missing data in Test so it has only one varible converting it to two
library(dummies)
LP_Test_Transformed <- dummy.data.frame(LP_Test_Transformed, names = "Married.Yes", sep =".")

getColIndex <- grep("Married.Yes.0", names(LP_Test_Transformed))
names(LP_Test_Transformed)[getColIndex] <- "Married.No"

getColInd <- grep("Married.Yes.1", names(LP_Test_Transformed))
names(LP_Test_Transformed)[getColInd] <- "Married.Yes"
str(LP_Tran_Transformed)

#Chaging Dependend variable back to categorical
LP_Tran_Transformed$Loan_Status.Y <- as.factor(LP_Tran_Transformed$Loan_Status.Y)


#Dividing Training data into 75-25 to create a validation set
getColIndex <- createDataPartition(LP_Tran_Transformed$Loan_Status.Y, p=.75, list = FALSE)

trainData <- LP_Tran_Transformed [getColIndex,]
testData <- LP_Tran_Transformed [-getColIndex,]

#Build a Initial Decision Tree without Data Exploration or Feature Engineering
library(rpart)
D.Tree1 <- rpart(Loan_Status.Y ~ ., data = trainData, method = "class")

#plot first tree
plot(D.Tree1)
text(D.Tree1)

summary(D.Tree1)

#prediction 1
tree1.predict <- predict(D.Tree1, testData, type = "class")


mean(tree1.predict==testData$Loan_Status.Y)

prop.table (table(tree1.predict,testData$Loan_Status.Y))

#First Submission
sub1.tree1.predict <- predict(D.Tree1, newdata = LP_Test_Transformed, type = "class")
sub_file_DTree <- data.frame(Loan_ID=LP_Test$Loan_ID, Loan_Status=ifelse(sub1.tree1.predict ==1,"Y","N"))

write.csv(sub_file_DTree,"Decision_Tree_LoanStatus.csv", col.names = TRUE,sep = ",")


