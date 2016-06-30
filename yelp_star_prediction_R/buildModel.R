# ------------------------------------------------------------------------------------------------
# Author: cgarcia
# About: This builds the model and datasets. After this is run the R workspace can be saved
#        as a snapshot so that variable stack.predictf contains the prediction model and can be 
#        applied to new datasets. 
#
# Note: This may take 25-45 minutes to complete. It is best to run once and save the R workspace.
# ------------------------------------------------------------------------------------------------

source("classFunctions.R")

message('Building Dataset...')
curr.data <- balanced.traintest.data("./yelp_data/review_data_100k.csv", 25000, trainp=0.7, sparsity.cutoff = 0.93) 
#curr.data <- balanced.traintest.data("./yelp_data/review_data_15k.csv", 5000, trainp=0.7, sparsity.cutoff = 0.93) # Smaller test example
message('Building Model Stack...')
inSub <- createDataPartition(y = curr.data$training$target.stars, p = 0.6, list = FALSE)
train.sub <- curr.data$training[inSub,]
train.top <- curr.data$training[-inSub,]
stack.predictf <- model.stack.predictorf(train.sub, train.top, top.model="rf", sub.models=c("qda","gbm","rf"))
head(stack.predictf(curr.data$testing))
print(confusionMatrix(stack.predictf(curr.data$testing), curr.data$testing$target.stars))

raw.data <- read.csv('./yelp_data/review_data_100k.csv')	
