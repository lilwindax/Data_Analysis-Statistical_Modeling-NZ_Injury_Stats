# Data analysis and Predictive Modeling for NZ Injury Statistics 


#pip install.packages("ggplot2")  
#pip install.packages(tidyverse)

library("ggplot2") # These will be needed to be downloaded - Use the above commands to do so 
library(tidyverse) # These will be needed to be downloaded - Use the above commands to do so 

# Load the data 
NewInjuryIndicators <- serious.injury.outcome.indicators.2000.19.csv
NewInjuryIndicators$X

for(i in 1:nrow(NewInjuryIndicators)) {
  
  
  #Go through severity column and change values to number Serious non-fatal = 1, Serious = 2, Fatal = 3
  if(NewInjuryIndicators[i, 13]=="Serious non-fatal"){
    NewInjuryIndicators[i, 13] <- 1
  }
  else if(NewInjuryIndicators[i, 13]=="Serious"){
    NewInjuryIndicators[i, 13] <- 2
  }
  else {
    NewInjuryIndicators[i, 13] <- 3
  }
  
  
  #Go through age column and change values to number 0-14 years = 1, 0-74 years = 2, 75+ Years = 3, All ages = 4
  if(NewInjuryIndicators[i, 12]=="0-14 years"){
    NewInjuryIndicators[i, 12] <- 1
  }
  else if(NewInjuryIndicators[i, 12]=="0-74 years"){
    NewInjuryIndicators[i, 12] <- 2
  }
  else if(NewInjuryIndicators[i, 12] =="75+ years"){
    NewInjuryIndicators[i, 12] <- 3
  }
  else {
    NewInjuryIndicators[i, 12] <- 4
  }
  
  
  #Go through population column and change values to number Maori = 1, Children = 2, Whole pop = 3
  if(NewInjuryIndicators[i, 11]=="Maori"){
    NewInjuryIndicators[i, 11] <- 1
  }
  else if(NewInjuryIndicators[i, 11]=="Children"){
    NewInjuryIndicators[i, 11] <- 2
  }
  else {
    NewInjuryIndicators[i, 11] <- 3
  }
  
  
  #Go through Validation column and change values to number Provisional = 1, Validated = 2
  if(NewInjuryIndicators[i, 10]=="Provisional"){
    NewInjuryIndicators[i, 10] <- 1
  }
  else {
    NewInjuryIndicators[i, 10] <- 2
  }
  
  
  #Go through cause column and change values to number Assault = 1, Falls = 2, Work = 3, Intentional self-harm = 4, 
  #Intentional = 5, Motor vehicle traffic crashes = 6, Car Occupant = 7, Pedestrian = 8, Drowning = 9, All = 10
  if(NewInjuryIndicators[i, 9]=="Assault"){
    NewInjuryIndicators[i, 9] <- 1
  }
  else if(NewInjuryIndicators[i, 9]=="Falls"){
    NewInjuryIndicators[i, 9] <- 2
  }
  else if(NewInjuryIndicators[i, 9]=="Work"){
    NewInjuryIndicators[i, 9] <- 3
  }
  else if(NewInjuryIndicators[i, 9]=="Intentional self-harm"){
    NewInjuryIndicators[i, 9] <- 4
  }
  else if(NewInjuryIndicators[i, 9]=="Intentional"){
    NewInjuryIndicators[i, 9] <- 5
  }
  else if(NewInjuryIndicators[i, 9]=="Motor vehicle traffic crashes"){
    NewInjuryIndicators[i, 9] <- 6
  }
  else if(NewInjuryIndicators[i, 9]=="Car occupant"){
    NewInjuryIndicators[i, 9] <- 7
  }
  else if(NewInjuryIndicators[i, 9]=="Pedestrian"){
    NewInjuryIndicators[i, 9] <- 8
  }
  else if(NewInjuryIndicators[i, 9]=="Drowing"){
    NewInjuryIndicators[i, 9] <- 9
  }
  else {
    NewInjuryIndicators[i, 9] =  10
  }
  
  
  #Go through Indicator column and change values to number Number = 1, Age standardised rate = 2, Rate per billion km = 3, Rate per thousand registered vehicles = 4
  if(NewInjuryIndicators[i, 8]=="Number"){
    NewInjuryIndicators[i, 8] <- 1
  }
  else if(NewInjuryIndicators[i, 8]=="Age-standardised rate"){
    NewInjuryIndicators[i, 8] <- 2
  }
  else if(NewInjuryIndicators[i, 8]=="Rate per billion km"){
    NewInjuryIndicators[i, 8] <- 3
  }
  else {
    NewInjuryIndicators[i, 8] <- 4
  }
  
  
  #Go through Units column and change values to number Injuries = 1, Per 100,000 people = 2, Per 100,000 FTE's = 3, 
  #Per Billion km = 4, Per thousand registered vehicles = 5
  if(NewInjuryIndicators[i, 7]=="Injuries"){
    NewInjuryIndicators[i, 7] <- 1
  }
  else if(NewInjuryIndicators[i, 7]=="Per 100,000 people"){
    NewInjuryIndicators[i, 7] <- 2
  }
  else if(NewInjuryIndicators[i, 7]=="Per 100,000 FTEs"){
    NewInjuryIndicators[i, 7] <- 3
  }
  else if(NewInjuryIndicators[i, 7]== "Per billion km"){
    NewInjuryIndicators[i, 7] <- 4
  }
  else {
    NewInjuryIndicators[i, 7] <- 5
  }

  
  #Go through Type column and change values to number Moving average = 1, Single year = 2
  if(NewInjuryIndicators[i,3]=="Moving average"){
    NewInjuryIndicators[i,3] <- 1
  }
  else {
    NewInjuryIndicators[i,3] <- 2
  }
}

#Create new df to do visualizations on
ModifiedInjuryIndicators <- NewInjuryIndicators

#Convert all columns that were changed to integers
ModifiedInjuryIndicators$Type <- as.integer(ModifiedInjuryIndicators$Type)
ModifiedInjuryIndicators$Units <- as.integer(ModifiedInjuryIndicators$Units)
ModifiedInjuryIndicators$Indicator <- as.integer(ModifiedInjuryIndicators$Indicator)
ModifiedInjuryIndicators$Cause <- as.integer(ModifiedInjuryIndicators$Cause)
ModifiedInjuryIndicators$Validation <- as.integer(ModifiedInjuryIndicators$Validation)
ModifiedInjuryIndicators$Population <- as.integer(ModifiedInjuryIndicators$Population)
ModifiedInjuryIndicators$Age <- as.integer(ModifiedInjuryIndicators$Age)
ModifiedInjuryIndicators$Severity <- as.integer(ModifiedInjuryIndicators$Severity)


#Get a summary of overall dataframe
summary(ModifiedInjuryIndicators)

#Create tables showing totals for Population, Ages, Cause, Severity
table(ModifiedInjuryIndicators$Population)
table(ModifiedInjuryIndicators$Age)
table(ModifiedInjuryIndicators$Cause)
table(ModifiedInjuryIndicators$Severity)

#Create tables comparing between Cause and Severity, Age and Severity, and Population and Severity
table(ModifiedInjuryIndicators$Cause, ModifiedInjuryIndicators$Severity)
table(ModifiedInjuryIndicators$Age, ModifiedInjuryIndicators$Severity)
table(ModifiedInjuryIndicators$Population, ModifiedInjuryIndicators$Severity)


#Splits NewInjStats based on severity column
SplitInjIndicators <- split(ModifiedInjuryIndicators, ModifiedInjuryIndicators$Severity)
lapply(names(SplitInjIndicators), function(x){
  write_csv(SplitInjIndicators[[x]], path = paste(x, ".csv", sep = ""))
})

#Then taking the subsets made from splitting between severity before, 
# creates tables comparing population and cause then age and cause for each subset
table(SplitInjIndicators[[1]]$Population, SplitInjIndicators[[1]]$Cause)#Serious non-fatal tables
table(SplitInjIndicators[[1]]$Age, SplitInjIndicators[[1]]$Cause)#Serious non-fatal tables

table(SplitInjIndicators[[2]]$Population, SplitInjIndicators[[2]]$Cause)#Serious Tables
table(SplitInjIndicators[[2]]$Age, SplitInjIndicators[[2]]$Cause)#Serious tables

table(SplitInjIndicators[[3]]$Population, SplitInjIndicators[[3]]$Cause)#Fatal tables
table(SplitInjIndicators[[3]]$Age, SplitInjIndicators[[3]]$Cause)#Fatal tables
#Create bar graphs comparing Cause and Severity,


graph2 <- ggplot(data=ModifiedInjuryIndicators, aes(x=Age, y=Severity, fill=Age)) + geom_bar(fun = "mean", stat="summary", color="red")
graph2 + ggtitle("Figure 1 - Age vs Severity")
graph5 <- ggplot(data=ModifiedInjuryIndicators, aes(x=Cause, y=Severity, fill=Cause)) + geom_bar(fun = "mean", stat="summary", color="red")
graph5 + ggtitle("Figure 2 - Cause vs Severity")


# Predictive Modeling using K Nearest Neighbor 

# Load the data for the model training 
dataframe <- ModifiedInjuryIndicators 


head(dataframe)
str(dataframe)

# Filter the attributes to include only important features 

dataframe.subset <- dataframe[c('Indicator', 'Cause', 'Validation', 'Population', 'Age', 'Severity')]
head(dataframe.subset)

# Randomise the dataset 
head(dataframe.subset)
set.seed(42)
rows <- sample(nrow(dataframe.subset))
dataframe_shuffle <- dataframe.subset[rows, ]
head(dataframe_shuffle)

# Split into Train and Test 


# Independent 
X <- dataframe_shuffle[0:5]
head(X)

x_train <- X[1:1824,]
head(x_train)

x_test <- X[1824:2606,]
head(x_test)


# Dependent 
Y <- dataframe_shuffle[6:6]
head(Y)

y_train <- Y[1:1824,]
head(y_train)

y_test <- Y[1824:2606,]
head(y_test)

# Load package 
#pip install.packages(class)
library(class)

# Call the KNN Function 

model <- knn(train = x_train, test = x_test, cl = y_train, k = 51)
model
table(model, y_test)
