# Insuarance Analysis
# Fahad Reda
# 09.10.2020
#General info for the dataset:
#In order for insurance companies to achieve high profits,
#they must charge higher premiums than the amount paid to 
#the insured person. As a result, insurance companies 
#invest a lot of time, effort and money in creating 
#models that accurately predict health care costs.
# Load packages
library(tidyverse)

# Read in the data----
df <- read_csv("insurance.csv")
#structure of Data----
str(df)
#Here we can see that we have 1338 datapoints and 7 features
#The first 6 Rows of the dataset ----
head(df)
#The last 6 Rows of the dataset----
tail(df)
#Checking for Missing Values ----
#we can see that there are no missing values
is.na.data.frame(df)
#General Statistics ----
summary(df)
#Summary stat for Age ----
summary(df$age)
#Summary Stat for Charges ----
summary(df$charges)
#Since the Mean(13270)is greater than Median(9382),then we can
#say that the distribution of insurance expenses is right skewed
#let's draw a histogram and check
library(ggplot2)
hist(df$charges, main = "Histogram of charges", col = "red")
#Let's Explore Other variables----
#how many males and females do we have?----
df %>% count(df$sex)
#Bar chart that shows how many males and females----
barplot(table(df$sex), main = "Gender")
#we have more males than femals in our dataset
#how many smokers do we have in our dataset?(how can i show the value on the bar plot)----
barplot(table(df$smoker), main = "smoker",beside = TRUE)

#Apparently most of our dataset are Non-Smokers
barplot(table(df$sex),
        main = "Smokers by Sex",
        xlab = "Smoker",
        col = c("red","green") 
)
legend("topleft",
       c("Female","Male"),
       fill = c("red","green")
)
#how many males are smokers(Couldn't answer it in R, but i did in python)
smok_male <- df$smoker == "male"
smok_male 
smok_female <- df$smoker == "female"
#histogram of the charges (right skwed)----
hist(df$charges, main = "Histogram of charges", col = "lightblue")
#Bar plot for Sex - Smoker - Region ----
par(mfrow = c(1,3))
barplot(table(df$sex), main = "sex")
barplot(table(df$smoker), main = "smoker")
barplot(table(df$region), main = "region")
#we can tell that most of our dataset are from southwest
#Checking for outliers using Boxplots----
par(mfrow = c(1,3))
boxplot(df$age, main = "Histogram of age")
boxplot(df$bmi, main = "Histogram of bmi")
boxplot(df$children, main = "Histogram of children")
#we can see that BMI has outliers
#Charges for Males and Females ----
df %>%
  ggplot(aes(x= sex, y = charges)) + 
  geom_boxplot() + 
  ggtitle("Charges for Males vs Females")
# we can tell from the boxplot 
#that there is no difference between
#the charges of males and females----
#
df %>%
  ggplot(aes(x=as.factor(children), y = charges)) + 
  geom_boxplot() + 
  ggtitle("Charges vs Number of Children")
#the weird thing is that people who has 3 and 4 children
#pay more than the one who has 5 children
#The End ----


