getwd()
setwd("C:/RIT-Stuff/BigData/faulty-steel-plates")
install.packages("caret",dependencies = TRUE)
install.packages('e1071', dependencies=TRUE)
install.packages("tibble")
install.packages("DSL")
install.packages("ggplot2")
 
library(caret)
library(tibble)
library(DSL)
library("ggplot2")
 
 

################ Without R library ########


##-------------------Note on time complexity------  O(ntest*ntrain*k)



#where ntest is number of test data and nTrain is the number of training data
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
print(New_Data1)

#seperate training and testing data
smp.size = floor(0.8*nrow(New_Data1)) 
set.seed(1029)                     
train.ind = sample(seq(nrow(New_Data1)), smp.size)
train = New_Data1[train.ind, ] 
test = New_Data1[-train.ind, ] 

print(train)
print(test)

names(train) <- NULL
names(test) <- NULL

#this fucntion finds the k nearest neighbour and uses the sapply to fid the euclidean distance 
# O(kn) where k is the loop till nearest nieghbour and n is the trainig data count to find the eculidena distanct

findNeighbour<- function(traningData,testData,k,testindex,train){#parameters->training data, k value test row value
  ##index and training data without the last coulmn removed
 
  distances<-train 
  col=ncol((traningData))+1
  length=ncol(testData)-1
  train.row<-nrow(traningData)
  mat <- order(apply(traningData, 1, function(x) sum((x - testData)^2)))[1:k]
  
   
  dis <- sapply(apply((apply(data.frame(matrix(rep(testData,each=train.row),nrow=train.row)),2,as.numeric) - traningData)^2,1,sum),sqrt)
  
  dis<-data.frame(dis)#convert distance to dataframe 
 
  distances[,28]<-dis# store distance in the new column
  
   
  neighbor<-list()

   distances<-distances[order(distances$NA.26),]#orders the data according 
   ##to the incresing oreder of distance found
   names(distances)<- NULL


  for(j in 1:k){#finds the neigbhours and adds to the list

    neighbor<-c(neighbor,list(distances[j,1:28]))

  }


  return(neighbor)#returns the list of neighbours
}
getResponse<-function(neighbor,testIndex){
   
  
  count<-0
  #class name list
  class<-c("Pastry", "Z_Scratch","K_Scatch",
           "Stains","Dirtiness","Bumps","Other_Faults")
  #create a map of class name and its count
  list<-DList( Pastry = 0,
               Z_Scratch = 0,K_Scatch=0,Stains=0,Dirtiness=0,Bumps=0,Other_Faults=0)
  #ocveting the mapped key pair to list for ease of access
  foo <- function( keypair )
    list( key = paste("next_", keypair$key, sep = ""), value =gsub("first", "mapped", keypair$value) )
  dlm <- DMap( x = list, MAP = foo)
  newList<-as.list(dlm)
   
  len<-ncol(test)
  
  #get the max nereast neighbour from the list
  for(i in 1:length(neighbor)){
   Values<-data.frame(neighbor[[i]])
   
  
      
      
     response<-Values[27]
      
      #if the repsone is name of the class increase the count of that class
     if(response=="Pastry"){
       newList$Pastry<-(strtoi(newList$Pastry)+1)
       
       
     }
    else if(response=="Z_Scratch"){
       newList$Z_Scratch<-(strtoi(newList$Z_Scratch)+1)
       
       
     }
     else if(response=="K_Scatch"){
       newList$K_Scatch<-(strtoi(newList$K_Scatch)+1)
       
       
     }
    else if(response=="Stains"){
       newList$Stains<-(strtoi(newList$Stains)+1)
      
       
     }
      else if(response=="Dirtiness"){
       newList$Dirtiness<-(strtoi(newList$Dirtiness)+1)
        
       
     }
   else if(response=="Bumps"){
       newList$Bumps<-(strtoi(newList$Bumps)+1)
        
     }
     else if(response=="Other_Faults"){
       newList$Other_Faults<-(strtoi(newList$Other_Faults)+1)
        
     }
     
     
  }
  #convert the mapped list to data frame
  list<-data.frame(newList)
  #sort the data frame in dcresing oreder
  val<-sort(list, decreasing = TRUE)
  #the first column is our predicted value
  test[testIndex,28]<<-colnames(val[1]) 
   
   
}

#this function runs till the number of data in test and 
# calls find neghbours and get respone function 
mainFunction<- function(){
  train1<-train
  train1[27]<-NULL
  train1<-as.matrix(train1)
  test1<-test
  test1[27]<-NULL
  test1<-as.matrix(test1)
  
  k=3 # k value to find the 
  for(i in 1:nrow(test)){
    ##finds the k neighborus
    neighbors=findNeighbour(train1,test1[i,],k,i,train)
    ##gets the closest neighbours
    getResponse(neighbors,i)
    
    
    
  }
  
}
#finds the accuracy of each class,
#Also finds the count of the actaul predicted class values
accuracy <- function(){
  correct = 0
  correctClass<-matrix(0,nrow=7,ncol=2) ##contains correct class value and its count
  accu<-0
  accuracyClass<-matrix(0,nrow=7,ncol=1)#stores the accuracy % of each class
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
  
  #find the percentage of accuracy
  for(i in c(1:nrow(correctClass))){
    accuracyClass[i] = correctClass[i,1]/correctClass[i,2] * 100
  }
   
   for(i in 1:length(accuracyClass)){
      accu = accu+correctClass[i,1]
   }
  #overalla average of the accuracy
  accu=(accu/nrow(test))
  print(paste0("Average Accuracy: ",accu))
  print("Percentage of predcitions of each class")
  print(accuracyClass)
}

mainFunction()
print(accuracy())
confusionMatrix(table(test[,28],test[,27]))

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
#k fold cross validiton 
trCntrl<- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(222)

#using train fucntion in package data 
knn_fit <- train(type ~., data = trainL, method = "knn",
                 trControl=trCntrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 15)
 
knn_fit

plot(knn_fit) 
# predicting the knn using the traing and plotting the values
knnPredict <- predict(knn_fit, newdata = testL )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knn_fit)



