                                        ## Titanic Data Set ###

## Load Libraries
library(titanic)
library(tidyverse)
library(corrplot)
library(RColorBrewer)

## Load Datasets

test_set<-titanic_test
training_set<- titanic_train

                                        ## Data Wrangling ##

## Combine test and Validation Set
total<- bind_rows(test_set,training_set)

## View Structure and Summary Stats of Data Frame

str(total)
summary(total)

## Lets start with numeric columns and then work on character columns 

## First we search for NA counts by Column
colSums(is.na(total))

## Lots of Missing values in Survived & Age
## Lets start by swithing over all the missing survived to desceased 

total$Survived[is.na(total$Survived==TRUE)]<- 0

## Now Lets use the mean age to fill in the empty values of the age column

total$Age[is.na(total$Age)]<- mean(total$Age, na.rm=TRUE)

## Lets also Fix the Fare which has one missing value

total$Fare[is.na(total$Fare)] <- mean(total$Fare, na.rm=TRUE)

## Lets take a look at the clean numeric data again

colSums(is.na(total))

## Switching over to character data

## Now lest inspect missing string data
colSums(total=="")

## Now Lets fill in the missing Embarked
total$Embarked[total$Embarked==""]<- "C"

## Missing lots of data for the cabin room can be relaxed, we wont use this variable
## so we can actually drop it before we build a model

## Changing columns to factors 

string_cols<- c("Sex", "Survived","Embarked","Pclass")

for(i in string_cols){
  total[,i]<- as.factor(total[,i])
} 

## Look at the structure again
str(total)


                                  ## Exploratory Data Analysis ##


## Lets generate some graphs to understand which variables have an impact on the chances of survival

total %>% ggplot(aes(Sex,fill=Survived)) + geom_bar(position="fill") + labs(title="Impact of Gender on Survival") + theme(plot.title = element_text(hjust = 0.5))

## Looks to be that females had a much greater chance of survival just like the movie showed


## Now lets look at Fare

total %>% ggplot(aes(Fare,fill=Survived)) + geom_histogram(position="fill") + labs(title= "Impact of Fare on Survival") + theme(plot.title= element_text(hjust = 0.5))

## Looks like there is are some extreme outliers with Fares that we may want to remove from the model
## aside from that there seems to be a positive relationship between fare and the chances of survival

## Lets do the same thing with Age

total %>% ggplot(aes(Age,fill=Survived)) + geom_histogram(position="fill") + labs(title = "Impact of Age on Survival") + theme(plot.title = element_text(hjust = 0.5))

## This shows that younger individuals had a better chance of survival than lower except for some that were 80, but how many people

total %>% ggplot(aes(Age, fill=Survived)) + geom_histogram()

## The regular histogram shows that only a few people over that age of 60 are in the dataset, a majority are in the 20-40 Range

## Now lets look at Class and Cabin 

total %>% ggplot(aes(Embarked, fill=Survived)) + geom_bar(position="fill") + facet_wrap(~Pclass)

## No crystal clear correlation here but it seems that Class has an overall impact on the chances of survival


## Lets build a correlation Matrix to see which numeric values correlate with each other

num_fields<- select_if(total,is.numeric)

corr_matrix<-cor(num_fields)

corrplot(corr_matrix,main="\n\nCorrelation Plot for Numerical Variables", method="circle", type="upper",col=brewer.pal(n=8, name="RdBu"),tl.col="black")

## Aside from cabin assignment and class there are no numeric fields that are highly correlated with each other.

pairs(~ Survived +  Fare + Age + Parch + Pclass, data = total)
