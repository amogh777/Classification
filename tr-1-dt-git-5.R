###### Used only For comparisons of results using R and my fuction
install.packages("caret",dependencies = TRUE)
install.packages('e1071', dependencies=TRUE)
install.packages("tibble")
install.packages("ggplot2")
install.packages("tree")
install.packages("rpart")
install.packages("rpart")
install.packages("C50")
install.packages("datasets")
install.packages("gmodels")
 
library(tibble)##to make single column
library(rpart)##for comparision with lib
library(tree)##for comparision with lib
library(rpart.plot) ##to draw a plot 
library(C50)
library(datasets)
library(gmodels)
 

getwd()
setwd("C:/RIT-Stuff/BigData/faulty-steel-plates")

#----------- Note on time complexity time O(Nkd). Actually  can be somewhere in between being in O(NklogN) and O(N2k).

##The uncertainty here is due to the non-deterministic way in which decision trees are built, 
#always splitting data based on locally optimal thresholds with close to no consideration for overall balance.-------------------


#Reading  dataset 
Auto <- read.csv("faults.csv",header=T,na.strings="?")     #remove null values
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


#finds the gini index
giniIndexMeasure <- function(value){
  
  if(length(value) == 0){ 
    
    return(0) #return 0 if length of value is 0
  }
  else{
    result <- table(value)/length(value) #else find the gini
    1-sum(result^2)
  }
}
#function to find the data variace
Data_Variance <- function(value){
  if(length(value) <= 1){
    
    return(0)
  }
  else{
     var(value)#function to find variance
  }
}

#functon to find the entropy
measure_of_Entropy <- function(value){
  
  if(length(value)==0){
    
   return(0)
  }
  else{
    result <- table(value)/length(value)
    addition <-sum(result*log2(result+1e-9))
  }
}

#creating class to store fileds of the decions tree 
DT <- setRefClass("DT",fields = list(lowered_val="function",
                                     knowledge_acquire="numeric",
                                     least_stem_dim="numeric",
                                     highest_magnitude="numeric",
                                     origin = "MainBranch"),
        methods = list(
          initialize = function(...){
              base <- list(lowered_val=giniIndexMeasure,
                           knowledge_acquire=1e-3,
                           least_stem_dim=20,
                           highest_magnitude=3,
                           origin=NULL)
              
              P <- list(...)
              fields <- names(getRefClass()$fields())
              for( domain in fields){
                if (!(domain %in% names(P))) {
                  P[[domain]] <- base[[domain]]
                }
              }
              for( criteria in names(P)){
                do.call("<<-",list(criteria, P[[criteria]]))
              }
            
          },
          suit = function(qualitys,select){
            origin <<- MainBranch$new(a=qualitys,
                                      b=select,
                                      lowered_val=lowered_val,
                                      knowledge_acquire=knowledge_acquire,
                                      least_stem_dim=least_stem_dim,
                                      highest_magnitude=highest_magnitude
            )
            origin$divided_branch()
            
          },
          guess = function(qualitys){
            origin$guess(qualitys)
          }
        )
)


# calling decions tree from the r pacakge


dt_rpart <- rpart(type ~ .,data=train)
dt_tree <- tree(type ~ .,data=train)
rpart.plot(dt_rpart, box.col=c("red", "green")) 
 
Prediction1 <- predict(dt_rpart,newdata=test[-27],type = 'class')
confusionMatrix(Prediction1,test$type)

print(dt_rpart)
cat("\n\n")
print(dt_tree)

#pruning decision tree
printcp(dt_rpart)

opt  <-  which.min(dt_rpart$cptable[,'xerror'])

cp <-  dt_rpart$cptable[opt, 'CP']
pruned_model <-  prune(dt_rpart,cp)
rpart_pruned_predict <- predict(pruned_model, newdata=test[-27],type = 'class')
rpart.plot(pruned_model, box.col=c("red", "green"))
mean1 <- mean(rpart_pruned_predict==test$type)
mean1

#confusion Matrix for  
confusionMatrix(rpart_pruned_predict,test$type)

#uses of c5.0 package to check the results as the dataset is large 

cmodel <- C5.0(train[-27], train$type)
cmodel_pred <- predict(cmodel, test)
table(cmodel_pred)
CrossTable(test$type, cmodel_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

