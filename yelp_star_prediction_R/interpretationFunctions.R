#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About:  This contains the core functions for model evaluation.
#----------------------------------------------------------------------------------------------------


np.percentileFinder <- function(vals, percentile, use.upper.bound = FALSE) {
	if(use.upper.bound) { return(sort(v)[ceiling(length(vals) * percentile)]) }
	sort(v)[length(vals) * percentile];
}

# Add NA to end of vals, up to specified length
pad <- function(vals, to.length) {
	while(length(vals) < to.length) { vals[length(vals) + 1] <- NA }
	vals
}

# Build a summary data frame. Each column is actual number of stars and each 
# row is a prediction.
build.summary.df <- function(actual, predicted) {
	rd <- data.frame(actual=as.numeric(actual), predicted=as.numeric(predicted))
	rc <- list(s1=subset(rd, actual == 1)$predicted,
			   s2=subset(rd, actual == 2)$predicted,
			   s3=subset(rd, actual == 3)$predicted,
			   s4=subset(rd, actual == 4)$predicted,
			   s5=subset(rd, actual == 5)$predicted)
	maxlen <- max(length(rc$s1), length(rc$s2), length(rc$s3), length(rc$s4), length(rc$s5))
	data.frame(a1=pad(rc$s1, maxlen),
			   a2=pad(rc$s2, maxlen),
			   a3=pad(rc$s3, maxlen),
			   a4=pad(rc$s4, maxlen),
			   a5=pad(rc$s5, maxlen))
}

# Construct a boxplot of results given actual and predicted star values.
plot.results <- function(actual, predicted, save.as.png="", col=c("red","palevioletred1","yellow", "orange", "green")) {
	results <- build.summary.df(actual=as.numeric(actual), predicted=as.numeric(predicted))
	if(save.as.png != "") {png(file = save.as.png)}
	boxplot(results, las=2, xlab="Actual Stars", ylab="Predicted Stars",
	        main="Comparison of Actual vs. Predicted Stars from Text",
			names=c("1-Star","2-Star","3-Star","4-Star","5-Star"),
			col=col)
	if(save.as.png != "") {dev.off()}			  
}


