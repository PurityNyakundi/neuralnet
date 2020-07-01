dataset = read.table("NN3D_data.csv", sep =",", header = TRUE)

m <- length(unique(dataset$x))
n <- length(unique(dataset$y))
nz <- matrix(dataset$z, nrow=m, ncol=n, byrow = FALSE)

persp(unique(dataset$x),unique(dataset$x),nz,theta=30, phi=30, expand=0.5,col="lightblue", zlab="z=f(x,y)", xlab = "x", ylab = "y", ticktype="detailed", shade=.75, lphi=45, ltheta=135)


m
nz
n

head(dataset)

dim(dataset)

str(dataset)

library(ggplot2)
library(GGally)
library(psych)

hist(
  dataset$x,
  main = "check X distribution",
  xlab = "X",
  col = "Blue"
)

hist(
  dataset$y,
  main = "check Y distribution",
  xlab = "Y",
  col = "Red"
)

#All is well
library(tidyverse)
#check the nulls

colSums(is.na(dataset))
#there are no missing values

#check for outliers
boxplot(dataset$x)
boxplot(dataset$y)
boxplot(dataset$z)

anyDuplicated(dataset)
#there are no duplicates
# install package
install.packages("neuralnet")

#split train and test data
library(caret)

index <-createDataPartition(dataset$z,p = 0.8,list = F)

train_data<-dataset[index,]
test_data<-dataset[-index,]

require(neuralnet)

# fit neural network
scaleddata<-scale(dataset)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(dataset, normalize))
#Neural Network
head(maxmindf)

trainNN = maxmindf[index , ]
testNN = maxmindf[-index , ]


set.seed(2)
NN <-neuralnet(z~., trainNN, hidden = 4 , linear.output = T )

# plot neural network
plot(NN)


predict_testNN = compute(NN, testNN)
predict_testNN = (predict_testNN$net.result * (max(dataset$z) - min(dataset$z))) + min(dataset$z)

#par(mfrow = c(1,2))
plot(test_data$z, predict_testNN, col='blue', pch=16, ylab = "predicted z NN", xlab = "real z")

abline(0,1,lwd = 2)
#ggplot(aes(y = predict_testNN,x = test_data$z))+
  #geom_abline()+
  #geom_point()
rsme.nn = sqrt(mean((test_data$z - predict_testNN)^2))
rsme.nn
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((test_data$z - predict_testNN)^2) / nrow(test_data)) ^ 0.5

RMSE.NN
#check on the data
head(predict_testNN)
head(dataset$z)
test_data$predictedz = predicted_z

#create 
#mean(test_data$z==predict_testNN)
z = test_data$z
predicted_z = test_data$predictedz
sub<-cbind(z,predicted_z)
write.csv(sub,file = "SubmissionFile.csv",row.names = F)
SubmissionFile.csv<-read.csv("SubmissionFile.csv",header = T)
view(SubmissionFile.csv)
```
```{r}
#The given report 
library(readr)
SubmissionFile <- read_csv("SubmissionFile.csv")
View(SubmissionFile)

