#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About:  This contains the core functions for creating a classification model to predict Yelp stars
#         from the review text.
#----------------------------------------------------------------------------------------------------

library(tm)
library(caret)
library(MASS)


# Builds a dataset from the specified file. The target is "target.stars". This
# returns a dataframe document-term matrix with stemming, stop-words and 
# punctuation removed, and using TF-IDF weighting.
build.dataset <- function(filepath, sparsity.cutoff = 0.98, min.word.length = 3) {
	dtm.control <- list(
		tolower = TRUE, 
		removePunctuation = TRUE,
		removeNumbers = TRUE,
		stopwords = stopwords("english"),
		stemming = TRUE, # false for sentiment
		weighting = function(x){weightTfIdf(x, normalize = FALSE)},
		minDocFreq = 5,
		wordLengths = c(min.word.length, "inf"))
	d1 <- read.csv(filepath)
	d1$text <- as.character(d1$text)
	d1 <- subset(d1, nchar(text) >= min.word.length)
	corp <- Corpus(VectorSource(d1$text))
	dtm <- DocumentTermMatrix(corp, control = dtm.control)
	sparsedtm <- removeSparseTerms(dtm, sparsity.cutoff)
	dtm.df <- as.data.frame(as.matrix(sparsedtm))
	dtm.df$target.stars <- as.factor(d1$stars)
	dtm.df
}

# Prints a summary of star distribution for a given raw dataset.
summarize.stars <- function(ds) {
	s1 <- subset(ds, target.stars == 1)
	s2 <- subset(ds, target.stars == 2)
	s3 <- subset(ds, target.stars == 3)
	s4 <- subset(ds, target.stars == 4)
	s5 <- subset(ds, target.stars == 5)
	sizes <- paste("1:",nrow(s1),"2:",nrow(s2),"3:",nrow(s3),"4:",nrow(s4),"5:",nrow(s5))
	message(paste("Distribution of Stars:", sizes))
}

# Provides a balanced sampling of stars.
balanced.sample <- function(fileobj, sample.size, sparsity.cutoff = 0.98, min.word.length = 3) {
	ds <- fileobj
	if(is.character(fileobj)) {
		ds <- build.dataset(fileobj, sparsity.cutoff, min.word.length)
	}
	s1 <- subset(ds, target.stars == 1)
	s2 <- subset(ds, target.stars == 2)
	s3 <- subset(ds, target.stars == 3)
	s4 <- subset(ds, target.stars == 4)
	s5 <- subset(ds, target.stars == 5)
	n = min(sample.size / 5, nrow(s1) ,nrow(s2) ,nrow(s3), nrow(s4), nrow(s5))
	d2 <- s1[sample(1:nrow(s1), n),]
	d2 <- rbind(d2, s2[sample(1:nrow(s2), n),])
	d2 <- rbind(d2, s3[sample(1:nrow(s3), n),])
	d2 <- rbind(d2, s4[sample(1:nrow(s4), n),])
	d2 <- rbind(d2, s5[sample(1:nrow(s5), n),])
	d2
}

# Builds a balanced set of split training/test data of the specified proportion. 
# Returns a list containing the training and test sets.
balanced.traintest.data <- function(filename, sample.size, sparsity.cutoff = 0.98, min.word.length = 3, trainp = 0.7, seed = -1) {
	dataset <- balanced.sample(filename, sample.size, sparsity.cutoff, min.word.length)
	if(seed != -1) {set.seed(seed)}
	inTrain <- createDataPartition(y = dataset$target.stars, p = trainp, list = FALSE)
	training <- dataset[inTrain,]
	testing <- dataset[-inTrain,]	
	list(training=training, testing=testing)
}

# Construct the model.
build.model <- function(training.data, method) {
	# Build the model
	start.time <- Sys.time()
	modelFit <- train(target.stars ~ .,data=training.data, method=method)
	end.time <- Sys.time()
	message(paste("Model Build - Elapsed Time:", end.time - start.time))
	modelFit
}

# Performs testing and prints key performance statistics. Returns a list containing:
# 1) model, 2) train.data, 3) test.data, 4) actual, and 5) predicted.
test.model <- function(dataset, method, trainp = 0.7, seed = -1) {
	# Build train and test datasets
	if(seed != -1) {set.seed(seed)}
	inTrain <- createDataPartition(y = dataset$target.stars, p = trainp, list = FALSE)
	training <- dataset[inTrain,]
	testing <- dataset[-inTrain,]	
	modelFit <- build.model(training, method=method)
	message(class(modelFit))
	predictions <- predict(modelFit, newdata=testing)	
	print(confusionMatrix(predictions, testing$target.stars))
	message("All Done!")
	list(model=modelFit, train.data=training, test.data=testing, 
	     actual=as.numeric(testing$target.stars), predicted=as.numeric(predictions))
}

# Builds a predictor function based on the specified model stack. The default stacking model is a 
# random forest, and a QDA is the single default sub-model. 
model.stack.predictorf <- function(sub.training.data, top.training.data, top.model="rf", sub.models=c("qda")) {
	mods.list <- vector("list", length(sub.models))
	for(i in 1:length(sub.models)) {
		modelFit <- build.model(sub.training.data, method=sub.models[i])
		pfunc <- function(ds) {predict(modelFit, newdata=ds)}
		mods.list[[i]] <- pfunc
	}
	make.top.df <- function(ds) {
		curr.data <- data.frame(target.stars=ds$target.stars)
		for(i in 1:length(mods.list)) {
			predictions <- mods.list[[i]](ds)
			message(paste('---------- SUB-MODEL ', i, 'DIAGNOSTICS: ----------'))
			print(confusionMatrix(predictions, ds$target.stars))
			curr.data[paste('X', i, sep='')] <- predictions
		}
		curr.data
	}
	topFit <- build.model(make.top.df(top.training.data), method=top.model)
	function(ds) {head(make.top.df(ds)); predict(topFit, newdata=make.top.df(ds))}
}