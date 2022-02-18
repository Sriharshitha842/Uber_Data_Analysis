#Step-1 : Importing the essential libraries and loading the dataset
#import essential libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(caTools)
library(caret)
library(class)

#get the current path
getwd()

#set the path
setwd("C:/Users/D SRIHARSHITHA/OneDrive/Desktop/Fall_Sem_21-22")

#read the dataset
df <- read.csv('uber.csv')

#Step-2 : data preprocessing
#(1)data description
#(2)data reduction
#(3)data cleaning
#(4)data visualization

#(1)data description
sprintf("The dataset contains %d columns (features) and %d rows (samples)", ncol(df), nrow(df))
colnames(df)
head(df)
summary(df)

#(2)data reduction
df = df[, !(colnames(df) %in% c("id","source","destination","cab_type","surge_multiplier","temperature","apparentTemperature","short_summary","precipIntensity","precipProbability","humidity", "windSpeed","visibility","temperatureHigh","temperatureLow","dewPoint","windBearing","cloudCover","uvIndex","visibility.1","moonPhase","weekday"))]
head(df)

#(3)data cleaning
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)
subset(df, !complete.cases(df))
df[,"price"][is.na(df[,"price"])]<- mean(df[,"price"],na.rm=T)
na_count1 <-sapply(df, function(y) sum(length(which(is.na(y)))))
data.frame(na_count1)
df$month_num <- df$month
df$month <-month(df$month, label=T)
df$year <- c(rep(2020,nrow(df)))
df <- df[,c(1:2,7,3,8,5:6,4)]
dim(df)
glimpse(df)
head(df)
tail(df)
summary(df)

write.csv(df, 'UBER_cleaned.csv', row.names = FALSE)
df <- read.csv('UBER_cleaned.csv')
print(head(df))

#(4)data visualization
#trips by the hours in a day
hour_data <- df %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous()

#trips during every day of the month
day_group <- df %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "maroon") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous()

#Trips taking place during months in a year
month_group <- df %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 

ggplot(month_group,aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous()

#Trips by vehicle name
name_group <- df %>%
  group_by(name) %>%
  dplyr::summarize(Total = n()) 

ggplot(name_group,aes(name, Total, fill = name)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by vehicle name") +
  theme(legend.position = "none") +
  scale_y_continuous()



#Step - 3 : Splitting data into train and test data
set.seed(42)
split <- sample.split(df, SplitRatio = 0.80)
train_cl <- subset(df, split == "TRUE")
test_cl <- subset(df, split == "FALSE")

#Step - 4: Feature Scaling & Normalization
train_scale <- scale(train_cl[, c(6,7)])
test_scale <- scale(test_cl[,c(6,7)])

#Step - 5: Building Machine Learning Model
knn.26 <- knn(train=train_scale, test=test_scale, cl=train_cl$name, k=26)
knn.27 <- knn(train=train_scale, test=test_scale, cl=train_cl$name, k=27)
print(knn.26)
print(knn.27)

#Step - 6: Model Evaluation
#Calculate the proportion of correct classification for k = 26, 27
ACC.26 <- 100 * sum(test_cl$name == knn.26)/NROW(test_cl$name)
ACC.27 <- 100 * sum(test_cl$name == knn.27)/NROW(test_cl$name)
print(ACC.26)
table(knn.26 ,test_cl$name)

print(ACC.27)
table(knn.27 ,test_cl$name)


#Step - 7:Optimization
i=1
k.optm=1
for (i in 1:30){
  knn.mod <- knn(train=train_scale, test=test_scale, cl=train_cl$name, k=i)
  k.optm[i] <- 100 * sum(test_cl$name == knn.mod)/NROW(test_cl$name)
  k=i
  cat(k,'=',k.optm[i],'\n')
}
#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")