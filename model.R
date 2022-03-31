### Import libraries
library(randomForest)
library(caret)
library(AER) #AER package has our creditcard dataset

# Importing CreditCard dataset
mydat <- read.csv("mydat.csv")

# Randomly split the dataset using caret package functions
#sed.seed(123)

index <- createDataPartition(mydat$card, p = 0.75, list = FALSE)

training <- mydat[index,]
testing <- mydat[-index,]

dim(training)
dim(testing)
str(training)
str(testing)

training$card <- as.factor(training$card)
training$owner <- as.factor(training$owner)
training$selfemp <- as.factor(training$selfemp)

testing$card <- as.factor(testing$card)
testing$owner <- as.factor(testing$owner)
testing$selfemp <- as.factor(testing$selfemp)

write.csv(training, "training.csv")
write.csv(testing, "testing.csv")
#training <- training[, -1]
#testing <- testing[, -1]
trainingset <- read.csv("training.csv", header = TRUE)
trainingset <- trainingset[, -1]
trainingset$card <- as.factor(trainingset$card)
trainingset$owner <- as.factor(trainingset$owner)
trainingset$selfemp <- as.factor(trainingset$selfemp)
str(trainingset)
#write.csv(training, "training.csv")
#write.csv(testing, "testing.csv")
## Random Forest model building
model <- randomForest(formula = card ~ age + income + owner + selfemp,
                         data = trainingset,
                         ntree = 500,
                         importance = TRUE)

saveRDS(model, "model.rds")
