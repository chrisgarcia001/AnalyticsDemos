#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About:  This builds and evaluates a model (random forest) to predict customer churn.
#----------------------------------------------------------------------------------------------------

library(caret)
set.seed(25714)

# Read in data
dataset <- read.csv("data/churn_data.csv")

# Separate data into training and test sets 
trainp <- 0.7
inTrain <- createDataPartition(y = dataset$Churn, p = trainp, list = FALSE)
training <- dataset[inTrain,]
testing <- dataset[-inTrain,]

# Define the model structure
model <- Churn ~ Gender + Age + Income + FamilySize + Education + Calls + Visits

# Fit the model
message("Model Fitting...")
start.time <- Sys.time()
modelFit <- train(model, data=training, method="rf")
end.time <- Sys.time()
message("Done!")
message(paste("Elapsed Time (sec):", end.time - start.time))
message("")

# Predict churn on test data
predictions <- predict(modelFit,newdata=testing)

# Evaluate and print the quality of prediction on test set
print(confusionMatrix(predictions,testing$Churn))
cmp <- data.frame(actual=testing$Churn, predicted=predictions)
edit(cmp)