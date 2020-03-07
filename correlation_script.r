#installed.packages("corrplot")
library(corrplot)
getwd()
setwd("C:/Users/Lasha/Desktop/Lasha/Education/`UW DS/I Semester/Unsupervised Learning/Clustering Paper HW/Clustering_Project")
raw.data <- read.csv("retailMarketingDI.csv")

raw.data <- raw.data[!is.na(raw.data$AmountSpent),]
head(raw.data$Age, 10)

cor.data <- raw.data
levels(raw.data$Age)
cor.data$Age <- ifelse(cor.data$Age == 'Young', 0,
                        ifelse(cor.data$Age == 'Middle',1,2))

levels(raw.data$Gender)
cor.data$Gender <- ifelse(cor.data$Gender == "Female", 0 ,1)


levels(raw.data$OwnHome)
cor.data$OwnHome <- ifelse(cor.data$OwnHome == "Rent", 0 ,1)


levels(raw.data$Married)
cor.data$Married <- ifelse(cor.data$Married == "Single", 0 ,1)

levels(raw.data$Location)
cor.data$Location_close <- ifelse(cor.data$Location == "Far", 0 ,1)

cor.data$History<- NULL
cor.data$Location<- NULL


str(cor.data)

cor.maxtrix<- cor(cor.data, method = "pearson", use = "complete.obs")

corrplot(cor.maxtrix)
#explain the matrix, high correlation, low correlation etc. 


library(ggplot2)

par(mfrow=c(1,7))

barplot(table(raw.data$Age), main="Age", col = "#69b3a2")
barplot(table(raw.data$Gender), main="Gender", col = "#A9A9A9")
barplot(table(raw.data$OwnHome), main="Own Home?", col = "#69b3a2")
barplot(table(raw.data$Married), main="Married", col = "#A9A9A9")
barplot(table(raw.data$Location), main="Location", col = "#69b3a2")
barplot(table(raw.data$Children), main="Children", col = "#A9A9A9")
barplot(table(raw.data$Catalog), main="Catalog", col = "#69b3a2")

par(
  mfrow=c(1,2),
  mar=c(4,4,1,0)
)
hist((raw.data$AmountSpent), xlab="", main="Amount Spent", col = "#69b3a2")
hist((raw.data$Salary), xlab="", ylab="", main="Salary", col = "#A9A9A9")

