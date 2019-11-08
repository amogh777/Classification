getwd()
setwd("C:/RIT-Stuff/BigData/faulty-steel-plates")
###### Used only For comparisons of results using R and my fuction
install.packages("caret",dependencies = TRUE)
install.packages('e1071', dependencies=TRUE)
install.packages("tibble")
install.packages("ggplot2")
install.packages("gmodels")
 
library(caret)##for comparision with lib
library(tibble)##to make one column
library(e1071) 
library("ggplot2")
library(gmodels)
##### Used only For comparisons of results using R and my fuction
##-------------------Note on time complexity------  O(nd^2c)

#Reading  dataset 
Auto <- read.csv("faults_main.csv",header=T,na.strings="?")     #remove null values
summary(Auto)


#Removing duplicate data from dataset
Data=unique(Auto)
New_data=Data[!duplicated(Data), ]
summary(New_data)

#Removing the missing values from  dataset
New_Data1=na.omit(New_data)
summary(New_Data1)

#Removing the attributes which is not required
New_Data1$TypeOfSteel_A400 <- NULL

dat<-(cbind(New_Data1$Pastry,New_Data1$Z_Scratch,New_Data1$K_Scatch,New_Data1$Stains,New_Data1$Dirtiness,New_Data1$Bumps,New_Data1$Other_Faults))
cor(New_Data1$Pastry,dat)
plot.new()
plot.window(c(0,nrow(dat)),range(dat))
matplot(dat) 
##if a column is pastry it iso nly pastry hence combining all the 7 and cor is negative for other classes
##columns into 1 named type
for(i in 27:33)
{
  for(j in 1:nrow(New_Data1))
    if (New_Data1[j,i]==1)
      New_Data1[j,i] <- colnames(New_Data1[i])
}

New_Data1 <- add_column(New_Data1,0)
colnames(New_Data1)[34] <- c("type")

for(i in 27:33)
{
  for(j in 1:nrow(New_Data1))
    if (New_Data1[j,i] != 0)
      New_Data1[j,34] <- New_Data1[j,i]
}

New_Data1 <- New_Data1[,-c(27:33)]
New_Data1[27] <- lapply(New_Data1[27], as.factor)

 
 

#seperate training and testing data
smp.size = floor(0.8*nrow(New_Data1)) 
set.seed(1029)                     
train.ind = sample(seq(nrow(New_Data1)), smp.size)
train = New_Data1[train.ind, ] 
test = New_Data1[-train.ind, ] 


 
#splitting each row by its column
classSplit<-list()
 
  #print(split(train,train[i]))
classSplit= c(classSplit,split(train,train[27]))
 
print((classSplit))
#-----------------------------find mean and sd which takes O(d) where d is the dimensionatiy of the data---------------------------
#creating meanArray and sdArray to store mean and standard devation of 26 columns of each row
meanArray<-matrix(nrow=7, ncol = 27)  #initialise meanArray
sdArray<-matrix(nrow=7 , ncol = 27)   #initialise sdArray

 
  for(j in 0:26){     #scanning through all the attibutes from 0 to 26 for Pastry
     
      meanVal = mean(data.matrix(classSplit$Pastry[j]))
      sd = sd(data.matrix(classSplit$Pastry[j]))
      meanArray[1,j]<-meanVal    #store meanval as a matrix
      sdArray[1,j]<-sd 
    
  }
meanArray[1,27]<-nrow(classSplit$Pastry)     #store length of each class
sdArray[1,27]<-nrow(classSplit$Pastry)
for(j in 0:26){     #scanning through all the attibutes from 0 to 26 for z_scratch
  
  meanVal = mean(data.matrix(classSplit$Z_Scratch[j]))
  sd = sd(data.matrix(classSplit$Z_Scratch[j]))
  meanArray[2,j]<-meanVal    #store meanval as a matrix
  sdArray[2,j]<-sd 
  
  
  
}
meanArray[2,27]<-nrow(classSplit$Z_Scratch)     #store length of each class
sdArray[2,27]<-nrow(classSplit$Z_Scratch)
for(j in 0:26){     #scanning through all the attibutes from 0 to 26 for k_scatch
  
  meanVal = mean(data.matrix(classSplit$K_Scatch[j]))
  sd = sd(data.matrix(classSplit$K_Scatch[j]))
  meanArray[3,j]<-meanVal    #store meanval as a matrix
  sdArray[3,j]<-sd 
  
  
  
}
meanArray[3,27]<-nrow(classSplit$K_Scatch)    #store length of each class
sdArray[3,27]<-nrow(classSplit$K_Scatch)
for(j in 0:26){     #scanning through all the attibutes from 0 to 26 for stains
  
  meanVal = mean(data.matrix(classSplit$Stains[j]))
  sd = sd(data.matrix(classSplit$Stains[j]))
  meanArray[4,j]<-meanVal    #store meanval as a matrix
  sdArray[4,j]<-sd 
  
  
  
}
meanArray[4,27]<-nrow(classSplit$Stains)    #store meanval as a matrix
sdArray[4,27]<-nrow(classSplit$Stains)
for(j in 0:26){     #scanning through all the attibutes from 0 to 26 for dirtiness
  
  meanVal = mean(data.matrix(classSplit$Dirtiness[j]))
  sd = sd(data.matrix(classSplit$Dirtiness[j]))
  meanArray[5,j]<-meanVal    #store meanval as a matrix
  sdArray[5,j]<-sd 
  
  
  
}
meanArray[5,27]<-nrow(classSplit$Dirtiness)    #store meanval as a matrix
sdArray[5,27]<-nrow(classSplit$Dirtiness)
for(j in 0:26){     #scanning through all the attibutes from 0 to 26 for bumps
  
  meanVal = mean(data.matrix(classSplit$Bumps[j]))
  sd = sd(data.matrix(classSplit$Bumps[j]))
  meanArray[6,j]<-meanVal    #store meanval as a matrix
  sdArray[6,j]<-sd 
  
  
  
}
meanArray[6,27]<-nrow(classSplit$Bumps)    #store length of each class
sdArray[6,27]<-nrow(classSplit$Bumps)
for(j in 0:26){     #scanning through all the attibutes from 0 to 26 for other faults
  
  meanVal = mean(data.matrix(classSplit$Other_Faults[j]))
  sd = sd(data.matrix(classSplit$Other_Faults[j]))
  meanArray[7,j]<-meanVal    #store meanval as a matrix
  sdArray[7,j]<-sd 
  
  
  
}
meanArray[7,27]<-nrow(classSplit$Other_Faults)    #store length of each class
sdArray[7,27]<-nrow(classSplit$Other_Faults)
print(meanArray)
 print(sdArray)
##------------------------------end----------
#Calculating Gaussian Probabilty Density function

 ##----------------find probabilty-----------------------
probabilityFunction <- function(x, mean , stdev){
  result= exp(-(`^`(x-mean, 2))/(2*(`^`(stdev,2))))
  return((1/(sqrt(2*pi)*stdev)) * result)
}
 
 #creating proArray to store the provality of each class
nrowTest=(nrow(test))
probArray<-matrix(0,nrow=nrowTest , ncol = 7)

# finding the probailty ot]f each class which takes O(cd) c is number of class,d is the dimensions 
Findprob <- function(meanArray,sdArray,testval,rowVal,column,probArray){
   
  for (ik in 1:7){
    probArray[rowVal,ik]<-(meanArray[ik,27]/as.double(nrow(train)))
    for(i in 1:26){
     
        probArray[rowVal,ik]<-probArray[rowVal,ik] * probabilityFunction(testval, meanArray[ik,column], sdArray[ik,column])
      
    }
    
  }
  
  return(probArray)
}

##get all the test values and find the probality
#time complexity O(nd^2c) where n is the number of test data, the find probabilty takes O(dc)
gettestset <- function(meanArray, sdArray,test,probArray){
  
  
  for(p in 1:nrow(test)){
      
    for(q in 1:26){
      
      probArray= Findprob(meanArray,sdArray,test[p,q],p,q,probArray)
       
      
    }
    
    
  }
  return(probArray)
  
}
 



probArray=gettestset(meanArray, sdArray,test,probArray )##probArray stores the probality of each class
print(probArray)



class<-c("Pastry", "Z_Scratch","K_Scatch",
         "Stains","Dirtiness","Bumps","Other_Faults") #creating a listof class names
#find the class based on the maximum probabilty 
findBestClass<-function(){
  
  max=-34028234663 #max val contains -34028234663
  index=0
  
  ##finds the class with the best probabilty
  for(i in 1:nrow(test)){
    
     for(j in 1:7){
       
       if(probArray[i,j]> max){
          max=probArray[i,j]
          index=j
          
           
       }
        
       
     }
    
    
    test[i,28]<<-class[index] ## store the class value at test column 28
    #reset values
    index=0
    max=-34028234663
    
  }
 
}
 
findBestClass() # calling findBestClass which finds the class with max probabilty
 
#finds the accuracy of each class,
#Also finds the count of the actaul predicted class values
accuracy <- function(){
  correct = 0
  correctClass<-matrix(0,nrow=7,ncol=2) ##contains correct class value and its count
  accu<-0
  accuracyClass<-matrix(0,nrow=7,ncol=1) #stores the accuracy % of each class
  for(i in c(1:nrow(test))){
    #if acutal and predicted is equal themn check which class it belongs to and 
    #increment count of that class
    if(test[i,27]=="Pastry"){
      
      if(test[i,27] == test[i,28]){
        correctClass[1,1] = correctClass[1,1]+1
      }
      correctClass[1,2] = correctClass[1,2]+1
    }
    else if(test[i,27]=="Z_Scratch"){
      if(test[i,27] == test[i,28]){
        correctClass[2,1] = correctClass[2,1]+1
      }
      correctClass[2,2] = correctClass[2,2]+1
      
      
    }
    else if(test[i,27]=="K_Scatch"){
      if(test[i,27] == test[i,28]){
        correctClass[3,1] = correctClass[3,1]+1
      }
      correctClass[3,2] = correctClass[3,2]+1
      
      
    }
    else if(test[i,27]=="Stains"){
      if(test[i,27] == test[i,28]){
        correctClass[4,1] = correctClass[4,1]+1
      }
      correctClass[4,2] = correctClass[4,2]+1
      
      
    }
    else if(test[i,27]=="Dirtiness"){
      if(test[i,27] == test[i,28]){
        correctClass[5,1] = correctClass[5,1]+1
      }
      correctClass[5,2] = correctClass[5,2]+1
      
      
    }
    else if(test[i,27]=="Bumps"){
      if(test[i,27] == test[i,28]){
        correctClass[6,1] = correctClass[6,1]+1
      }
      correctClass[6,2] = correctClass[6,2]+1
      
    }
    else if(test[i,27]=="Other_Faults"){
      if(test[i,27] == test[i,28]){
        correctClass[7,1] = correctClass[7,1]+1
      }
      correctClass[7,2] = correctClass[7,2]+1
      
    }
    
    
  }
  
  print("Frequencey of Actual vs predicted")
  print(correctClass)
  
  
  for(i in c(1:nrow(correctClass))){
    accuracyClass[i] = correctClass[i,1]/correctClass[i,2] * 100
  }
  print("acc class")
  print( accuracyClass)
  total=0
  for(i in 1:length(accuracyClass)){
    accu = accu+accuracyClass[i]
  }
   
  accu=(accu/nrow(test))
  print(paste0("Average Accuracy: ",accu))
  print("Percentage of predcitions of each class")
  print(accuracyClass)
  return(accuracyClass)
}

 accuracy()
#comparing the result of actual and the predictd data
(table(test$V28,test$type))
ctable<-table(test$V28,test$type)
ggplot(test,aes(x=test$type,y=test$V28))+geom_bar(stat="identity") 
 

############ With R Library ###########

Data=unique(Auto)
New_data=Data[!duplicated(Data), ]
summary(New_data)

#Removing the missing values from  dataset
New_Data1=na.omit(New_data)
summary(New_Data1)

#Removing the attributes which is not required
New_Data1$TypeOfSteel_A400 <- NULL

for(i in 27:33)
{
  for(j in 1:nrow(New_Data1))
    if (New_Data1[j,i]==1)
      New_Data1[j,i] <- colnames(New_Data1[i])
}

New_Data1 <- add_column(New_Data1,0)
colnames(New_Data1)[34] <- c("type")

for(i in 27:33)
{
  for(j in 1:nrow(New_Data1))
    if (New_Data1[j,i] != 0)
      New_Data1[j,34] <- New_Data1[j,i]
}

New_Data1 <- New_Data1[,-c(27:33)]
New_Data1[27] <- lapply(New_Data1[27], as.factor)
print(New_Data1)

#seperate training and testing data
set.seed(123)
sample <- sample.int(n = nrow(New_Data1), size = floor(.75*nrow(New_Data1)), replace = F)
trainL <- New_Data1[sample, ]
testL  <- New_Data1[-sample, ]

print(trainL)
print(testL)

anyNA(Auto)

##k folds cross validation of training data
trCntrl<- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(222)

print(New_Data1)

 
 
#calling naiveBayes Classifier
classifier<-naiveBayes(type ~.,data=train)
classifier
 
#calling predict fnction
NB_Predictions=predict(classifier,newdata = test[-27],type = 'class')
#comparing the result of actual and the predictd data
confusionMatrix(table(NB_Predictions,test$type))
plot(NB_Predictions)

CrossTable(test$type, NB_Predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
 

 
