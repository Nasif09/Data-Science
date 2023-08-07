#Name: Md. Nasifur Rahman
#ID: 20-43651-2
#-----------------------------------------------#

library(dplyr)
dataframe <- read.csv("D:/Data Science/Dataset_midterm_Section(B).csv",header = TRUE,sep=',')
summary(dataframe)
str(dataframe)

#----------------------Gender--------------------#
which(is.na(dataframe$Gender))

mood_tb <- table(dataframe$Gender)
mood_gender<-names(which.max(mood_tb))
print(paste("Mode: ",mood_gender))

dataframe<-dataframe %>% mutate(Gender = ifelse(is.na(Gender), mood_gender , Gender))
dataframe$Gender <- as.numeric(dataframe$Gender)
summary(dataframe$Gender)

hist(dataframe$Gender,xlab = "Gender",ylab = "Frequency",main = "Histogram of Gender")

#----------------------Age--------------------#
which(is.na(dataframe$age))

mean_age<-mean(dataframe$age, na.rm = TRUE)
median_age<-median(dataframe$age, na.rm = TRUE)
mood_tage <- table(dataframe$age)
mood_age<-names(which.max(mood_tage))

print(paste("Mean: ",mean_age))
print(paste("Median: ",median_age))
print(paste("Mode: ",mood_age))


dataframe<-dataframe %>%mutate(age = ifelse(is.na(age), median(age, na.rm = TRUE) , age)) 
summary(dataframe$age)

out <- boxplot.stats(dataframe$age)$out
boxplot(dataframe$age, col = "blue", ylab = "Age", main = "Boxplot of Age")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

hist(dataframe$age,xlab = "Age",ylab = "Frequency",main = "Histogram of Age")

summary(dataframe$age)

#----------------------sibsp--------------------#
which(is.na(dataframe$sibsp))

mood_tb <- table(dataframe$sibsp)
mood_sibsp<-names(which.max(mood_tb))
print(paste("Mode: ",mood_sibsp))

summary(dataframe$sibsp)

hist(dataframe$sibsp,xlab = "Sibsp",ylab = "Frequency",main = "Histogram of Sibsp")

#----------------------parch--------------------#
which(is.na(dataframe$parch))

mood_tb <- table(dataframe$parch)
mood_parch<-names(which.max(mood_tb))
print(paste("Mode: ",mood_parch))

summary(dataframe$parch)

hist(dataframe$parch,xlab = "Parch",ylab = "Frequency",main = "Histogram of Parch")


#----------------------fare--------------------#
which(is.na(dataframe$fare))

mean_fare<-mean(dataframe$fare, na.rm = TRUE)
median_fare<-median(dataframe$fare, na.rm = TRUE)
mood_tage <- table(dataframe$fare)
mood_fare<-names(which.max(mood_tage))

print(paste("Mean: ",mean_fare))
print(paste("Median: ",median_fare))
print(paste("Mode: ",mood_fare))

summary(dataframe$fare)

sds_fare<-sd(dataframe$fare, na.rm = TRUE)
sds_fare
dataframe$zScore <- ((dataframe$fare-mean_fare)/sds_fare)

minZ<-min(dataframe$zScore)
maxZ<-max(dataframe$zScore)
print(paste("Minimum Z-Score: ",minZ))
print(paste("Maximum Z-Score: ",maxZ))

dataframe <- subset(dataframe, dataframe$zScore<=abs(minZ))
hist(dataframe$fare,xlab = "Fare",ylab = "Frequency",main = "Histogram of Fare")

summary(dataframe$fare)
dataframe$zScore<-NULL

#----------------------Embarked--------------------#
dataframe$embarked<-as.numeric(factor(dataframe$embarked,levels = c("C","Q","S"), labels = c(1,2,3)))
which(is.na(dataframe$embarked))

df<- factor(dataframe$embarked,levels = c(1,2,3),labels = c("C","Q","S"))
mood_tb <- table(df)
mood_embarked<-names(which.max(mood_tb))

print(paste("Mode: ",mood_embarked))

dataframe<-dataframe %>% mutate(embarked = ifelse(is.na(embarked), mood_embarked , embarked))

summary(dataframe$embarked)

hist(dataframe$embarked,xlab = "embarked",ylab = "Frequency",main = "Histogram of Embarked")

#----------------------class--------------------#
dataframe$class<-as.numeric(factor(dataframe$class,levels = c("First","Second","Third"), labels = c(1,2,3)))
which(is.na(dataframe$class))

df<- factor(dataframe$class,levels = c(1,2,3),labels = c("First","Second","Third"))
mood_tb <- table(df)
mood_class<-names(which.max(mood_tb))

print(paste("Mode: ",mood_class))

dataframe<-dataframe %>% mutate(class = ifelse(is.na(class), mood_class , class))

dataframe$class <- as.numeric(dataframe$class)
summary(dataframe$class)

hist(dataframe$class,xlab = "class",ylab = "Frequency",main = "Histogram of Class")

#----------------------who--------------------#
dataframe$who <- ifelse(substr(dataframe$who, 1, 1) == "m", 0, 1)
which(is.na(dataframe$who))

df<- factor(dataframe$who,levels = c(0,1,3),labels = c("man","woman","child"))
mood_tb <- table(df)
mood_who<-names(which.max(mood_tb))

print(paste("Mode: ",mood_who))

summary(dataframe$who)

hist(dataframe$who,xlab = "who",ylab = "Frequency",main = "Histogram of Who")


#----------------------alone--------------------#
dataframe$alone<-as.numeric(factor(dataframe$alone,levels = c("TRUE","FALSE"), labels = c(1,2)))
which(is.na(dataframe$alone))

df<- factor(dataframe$alone,levels = c(1,2),labels = c("TRUE","FALSE"))
mood_tb <- table(df)
mood_alone<-names(which.max(mood_tb))

print(paste("Mode: ",mood_alone))

summary(dataframe$alone)
hist(dataframe$alone,xlab = "Alone",ylab = "Frequency",main = "Histogram of Alone")

#----------------------survived--------------------#
which(is.na(dataframe$survived))

mood_tb <- table(dataframe$survived)
mood_survived<-names(which.max(mood_tb))
print(paste("Mode: ",mood_survived))

summary(dataframe$survived)

hist(dataframe$survived,xlab = "Survived",ylab = "Frequency",main = "Histogram of Survived")









