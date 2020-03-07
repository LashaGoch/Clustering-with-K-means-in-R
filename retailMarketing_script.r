library(dplyr)
library(ggplot2)
library(data.table)
#install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("pastecs")
library(pastecs)



setwd("D:/Semester Winter 2019/Unsupervised Learning/Clustering_Project")
list.files()
raw.data <- read.csv("retailMarketingDI.csv")
View(raw.data)

str(raw.data)
#There are 10 variables: ...  
#and 1000 records (before cleaning the data)


########################## 1st PART : cleaning and orginazing the data ########################## 
str(raw.data)

table(is.na(raw.data$Age)) #no NA
table(is.na(raw.data$Gender)) #no NA
table(is.na(raw.data$OwnHome)) #no NA
table(is.na(raw.data$Married)) #no NA
table(is.na(raw.data$Location)) #no NA
table(is.na(raw.data$Salary)) #no NA
table(is.na(raw.data$Children)) #no NA
table(is.na(raw.data$History)) #There are 303 NAs, which I will replace with 'Unknown'
table(is.na(raw.data$Catalogs)) #no NA
table(is.na(raw.data$AmountSpent)) ##There are 6 NAs, records which I can remove OR do forcast on them - we will decide soon 

#first I will replace the NAs of History with 'Unknown':
raw.data$History <- as.character(raw.data$History)
raw.data$History[is.na(raw.data$History)] <- 'Unknown'
raw.data$History <- factor(raw.data$History)
table((raw.data$History)) # worked succefuly 

#first I will remove the 6 NAs with no amount spent
retail.df <- raw.data[!is.na(raw.data$AmountSpent),]

# I will factorize the Children veriable
retail.df$Children <- factor(retail.df$Children)  


View(retail.df %>%
       group_by(Catalogs) %>%
       summarise(mean_of_amount = mean(AmountSpent),numebr_of_appirances = n()))
#By looking at the table above we can see that variabele Catalogs is actually a factor variable where 6 is the 'low_end' prices and 24 is 'high_end' products
#Where 12 and 16 are the mid_range products
#therefore I will change the notation to more intuitive notation (althoth there is no real change in the content) : 

retail.df<- (retail.df %>%
       mutate(Catalog = ifelse (Catalogs ==6, 'low_end',
                                (ifelse(Catalogs == 12, "low_midrange", 
                                        (ifelse(Catalogs == 18, "high_midrange", "high_end")))))))

#I will factorize the new variable
retail.df$Catalog <- as.factor(retail.df$Catalog)

#And remove the old one:
retail.df$Catalogs <- NULL

str(retail.df) # we are left with 10 variables, 8 of them are factors and 2 are integers (salary + amount spent)

############# 2nd PART : getting to know the Data (summary statistics and EDA) ################ 

###################################################################################
#########go to correlation_script and explain the correlation matrix###############
###################################################################################

#I will show the distribution of each categorical veriables
lapply( retail.df %>%
          select(c("Age", "Gender", "OwnHome", "Married", "Location", "Children", "History","Catalog"))
        ,table)

ggplot(data = retail.df, aes(x = Salary))+
  geom_histogram(bins = 50, colour = 'white', fill = 'darkblue')+
  scale_x_continuous(breaks = seq(0,150000,25000))+
  scale_y_continuous(breaks = seq(0,70,10))+
  xlab("Salary")+
  ylab("Frequency")+
  ggtitle("Distribution of salaries")+
  geom_vline(xintercept = mean(retail.df$Salary), color = 'red')+
  labs(subtitle  = 'red line represent average salary')
  

mean_salary_female <- mean(retail.df$Salary[retail.df$Gender =="Female"])
mean_salary_male <- mean(retail.df$Salary[retail.df$Gender =="Male"])


ggplot(data = retail.df, aes(x = Salary))+
  geom_histogram(bins = 50, colour = 'white', fill = 'darkblue')+
  scale_x_continuous(breaks = seq(0,150000,35000))+
  scale_y_continuous(breaks = seq(0,70,10))+
  xlab("Salary")+
  ylab("Frequency")+
  ggtitle("Distribution of salaries faceted by gender")+
  geom_vline(xintercept = mean_salary_female, color = 'pink',size=1.5)+
  geom_vline(xintercept = mean_salary_male, color = 'red', alpha= 0.6)+
  labs(subtitle  = "red line is male's average salary, and pink's female's")+
  facet_wrap(~Gender)

#explain the distributions, males is more normally looking dist, while womens is right skued

mean_AmountSpent_female <- mean(retail.df$AmountSpent[retail.df$Gender =="Female"])
mean_AmountSpent_male <- mean(retail.df$AmountSpent[retail.df$Gender =="Male"])

ggplot(data = retail.df, aes(x = AmountSpent))+
  geom_histogram(bins = 50, colour = 'white', fill = 'lightgreen')+
  scale_x_continuous()+
  scale_y_continuous()+
  xlab("Amount Spent")+
  ylab("Frequency")+
  ggtitle("Distribution of Amount Spent faceted by gender")+
  labs(subtitle  = "red line is male's average spent, and pink's female's")+
  facet_wrap(~Gender)+
  geom_vline(xintercept = mean_AmountSpent_female, color = 'pink',size=1.5)+
  geom_vline(xintercept = mean_AmountSpent_male, color = 'red', alpha= 0.6)

#Again explain the distributions, males is more normally looking dist, while womens is right skued
