install.packages("slam")
install.packages("RTextTools")
install.packages("maxent")
install.packages("SnowballC")
install.packages("ggplot2")
install.packages("tm")
install.packages("NLP")


library(RTextTools)
imdb_reviews <- read.csv('/Users/leiguo/Desktop/HW6_IMDB.csv',stringsAsFactors = FALSE)
View(imdb_reviews)
library(NLP)
library(tm)
library(maxent)

library(SnowballC)
library(ggplot2)

imdb_reviews$Review.Text <- as.character(imdb_reviews$Review.Text)
matrix <- create_matrix(imdb_reviews$Review.Text, language="english", removeSparseTerms = 0.95, removeStopwords=TRUE, removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightBin)
View(matrix)
matrix <- create_matrix(imdb_reviews$Review.Text, language="english", removeSparseTerms = 0.95, removeStopwords=FALSE, removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightBin)

matrix <- create_matrix(imdb_reviews$Review.Text, language="english", removeSparseTerms = 0.90, removeStopwords=TRUE, removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightBin)
mat <- as.matrix(matrix)
test_instn = sample(nrow(mat), 0.3*nrow(mat))
IMDB_test <- mat[test_instn,]
IMDB_train <- mat[-test_instn,]
mat

#Question 2
model <- maxent(IMDB_train, imdb_reviews[-test_instn,2])
Predictions <- predict(model, IMDB_test)
table(imdb_reviews[test_instn,2],as.factor(Predictions[,1]),dnn=c("Actual", "Predicted"))
recall_accuracy(imdb_reviews[test_instn,2], Predictions)

#Question 3
not_N <- IMDB_test[(imdb_reviews[test_instn,2]=="N" & imdb_reviews[test_instn,2]!=as.factor(Predictions[,1])),]
not_N[1:10,1]

#Qestion 4:
