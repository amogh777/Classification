READ ME

Inpu file used is faults_main.csv

Cointains setwd(..) might have to change to the existing cd if case of any errors


FOR NAive Bayes

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

For Knn

install.packages("caret",dependencies = TRUE)
install.packages('e1071', dependencies=TRUE)
install.packages("tibble")
install.packages("DSL")
install.packages("ggplot2")
 
library(caret)
library(tibble)
library(DSL)
library("ggplot2")

For Decions Tree

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



Test Cases for all these are the data accuracy and the freuqncy of the all the data that are predcited.
Since we are using a seed  training and testing data are procded at random hence the acutal test case show in the 
report might be produced. Accuracy of our results produced is compared to the results procuded from the pacakages. 
That is how the results are verified.