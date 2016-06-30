#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About:  This contains some functions for exploratory text analysis.
#----------------------------------------------------------------------------------------------------


library(tm)
library(wordcloud)

# Build a word cloud from a document-term matrix.
build.wordcloud.from.dtm <- function(doc.term.matrix, colors=brewer.pal(9, "Paired")) {
	freqs <- sort(colSums(as.matrix(doc.term.matrix)), decreasing = TRUE)
	words <- names(freqs)
	wordcloud(words, freqs, random.order=FALSE,  
		  use.r.layout=FALSE, colors=colors)
}

# Build a word cloud from a file object (either a data frame or file path).
build.wordcloud <- function(fileobj, sparsity.cutoff = 0.98, min.word.length=3, 
						    max.percentile=1.0, colors=brewer.pal(9, "Paired")) {
	dtm.control <- list(
		tolower = TRUE, 
		removePunctuation = TRUE,
		removeNumbers = TRUE,
		stopwords = stopwords("english"),
		stemming = TRUE, # false for sentiment
		weighting = function(x){weightTfIdf(x, normalize = FALSE)},
		minDocFreq = 5,
		wordLengths = c(min.word.length, "inf"))
	d1 <- fileobj
	print(class(fileobj))
	if(class(fileobj) == class("abc")) {
		message(paste("Loading file:", fileobj))
		d1 <- read.csv(fileobj)
		message("Done!")
	}
	d1$text <- as.character(d1$text)
	d1 <- subset(d1, nchar(text) >= min.word.length)
	corp <- Corpus(VectorSource(d1$text))
	dtm <- DocumentTermMatrix(corp, control = dtm.control)
	sparsedtm <- removeSparseTerms(dtm, sparsity.cutoff)
	#freqs <- sort(colSums(as.matrix(sparsedtm)), decreasing = TRUE)
	freqs <- sort(colSums(as.matrix(sparsedtm)), decreasing = FALSE)
	freqs <- sort(freqs[1:(max.percentile * length(freqs))], decreasing = TRUE)
	message(paste("Freqs Class:", class(freqs)))
	print(head(freqs))
	words <- names(freqs)
	wordcloud(words, freqs, random.order=FALSE,  
		  use.r.layout=FALSE, colors=colors)
}

# Build a comparison word cloud for differing numbers of stars.
stars.compare.wordplot <- function(fileobj, use.stars=c(1,3,5), layout=c(1, length(use.stars)), 
                                   colors=brewer.pal(9, "Paired"), save.as.png="", sparsity.cutoff = 0.98, 
								   min.word.length=3, max.percentile=1.0) {
	d1 <- fileobj
	print(class(fileobj))
	if(class(fileobj) == class("abc")) {
		message(paste("Loading file:", fileobj))
		d1 <- read.csv(fileobj)
		message("Done!")
	}
	if(save.as.png != "") {png(file = save.as.png)}
	par(mfrow = layout)
	for(i in use.stars) {
		td <- subset(d1, stars == i)
		print(head(td))
		build.wordcloud(td, sparsity.cutoff=sparsity.cutoff, min.word.length=min.word.length, 
						max.percentile=max.percentile, colors=colors)
	}
	if(save.as.png != "") {dev.off()}
}

# -------- SOME EXAMPLE COLOR PALATES: ----------------
# colors=brewer.pal(4, "Dark2")
# colors=brewer.pal(9,"BuGn")
# colors=brewer.pal(4, "Set3")
# colors=brewer.pal(9, "Paired")

# AN EXAMPLE: 
#build.wordcloud.from.dtm(sparsedtm, colors=brewer.pal(4, "Set3"))