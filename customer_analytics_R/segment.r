#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About:  This demo shows how to segment customers into groups based on shopping habits. It
#         uses the EM algorithm which also finds the optimal number of clusters.
#----------------------------------------------------------------------------------------------------

library(mclust)
library(cluster)

# Load data
dataset = read.csv("data/purchases.csv", row.names=1)

# Build latent cluster model and show results
fit <- Mclust(dataset)
print(fit$BIC) # Show the fit summary
classif <- fit$classification # The segment classification vector - class of each person

# Build classification frame
classifications <- cbind(classif)
print(classifications) # Show the classes of each person
rownames(classifications) <- rownames(dataset)

fitdata <- cbind(dataset, classif=classif) # Add the classes to the data as a new column

# Display cluster plot based on two latent components and show cross-var plots
plot(fitdata)
dev.new()
clusplot(dataset, classifications, color=TRUE, shade=TRUE, labels=2, lines=0)







