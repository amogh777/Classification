getwd()
setwd("C:/Users/John/Desktop/ppp using R")

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

label=New_Data1[27:33]
summary(label)

#feature=New_Data1[0:26]
#summary(feature)