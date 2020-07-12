

####
library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
library(GGally)
####

house=read.csv('houses.csv', header=T)
dim(house)
glimpse(house)
#separating date and 'T00000'-s 
house=separate(house, date, c('date', 'athing'), sep = 'T', remove=T)
stri_sub(house$athing, 0,1)='T'
#checking if there are any different value in 'athing' column
thing=('T00000')
sum(thing==house$athing)
#dropping the 'athing' column because it does not make sense
house=subset(house,select = -athing )
#NA check
sum(is.na(house))


##Calculating correlation
house$date=as.numeric(house$date)
cor(house)
cor(house[-3], house$price)
#easy way to see the level of correlation
ggcorr(house, label = TRUE)
##the top 5 variables correlated to price are: sqft_living, grade, sqft_living15, sqft_above, bathrooms. 

#Prepare dataset for prediction


str(house)
##80% train & 20% test data
set.seed(4)
trainA=sample(nrow(house), round(nrow(house)/100*80,0), replace=F)
train80=house[trainA,]
test20=house[-trainA,]

##60% train & 40% test data
set.seed(4)
trainB=sample(nrow(house), round(nrow(house)/100*60,0), replace=F)
train60=house[trainB,]
test40=house[-trainB,]
View(train80)
View(train60)

#Build 2 models using any 5 features. One
#model for each case (A and B) specified above. Perform prediction function for the test
#data

corCheck1=house[,c(3,5,6,12,13,20)]
cor(corCheck)
###sqft_living has 0.88 corr coefficient with sqft_above. i change sqft_above with sqft_basement
corCheck2=house[,c(3,5,6,12,14,20)]
cor(corCheck2)


###
# Case A
A=lm(price~sqft_living+grade+sqft_living15+sqft_basement+bathrooms,train80) 
summary(A)
test20$linear_price_prediction=predict(A, test20)
View(test20)

#Checking the quality of predictions
RMSEa=sqrt((sum((test20$price-test20$linear_price_prediction)^2))/length(test20$linear_price_prediction))
RMSEa

##the function from lab session
prediction_quality <- function(predictions, real_values){
  diff <- sum((real_values - predictions)^2)
  mse <- diff/length(predictions)
  rmse <- sqrt(mse)
  mae <- sum(abs(real_values - predictions))/length(predictions)
  print(paste("mean squared error is ", mse))
  print(paste("root mean squared error is ", rmse))
  print(paste("mean absolute error is ", mae))
}

prediction_quality(test20$linear_price_prediction, test20$price)


## Case B
B=lm(price~sqft_living+grade+sqft_living15+sqft_basement+bathrooms,train60)
summary(B)
test40$linear_price_prediction=predict(B, test40)
View(test40)

#rmse
RMSEb=sqrt((sum((test40$price-test40$linear_price_prediction)^2))/length(test40$linear_price_prediction))
RMSEb
prediction_quality(test40$linear_price_prediction, test40$price)

#comparing 
RMSEa-RMSEb
#the first train&test data show better results comparing to the second case
