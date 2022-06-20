# Czyszczenie srodowiska
rm(list=ls())

# Biblioteki
library(class)
library(kernlab)
library(randomForest)
library(neuralnet)
library(adabag)
library(ClusterR)
library(cluster)
library(naivebayes)
library(kernlab)


haberman <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data",header = F, sep = ',')
View(haberman)
# https://archive.ics.uci.edu/ml/datasets/Haberman%27s+Survival


southGerman <- read.table("SouthGermanCredit.asc", header = TRUE) 
# https://archive.ics.uci.edu/ml/datasets/South+German+Credit


# metoda random Forest


# zbior 1

haberman <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/
                       haberman/haberman.data",header = F, sep = ',')

folds <- cut(seq(1, nrow(haberman)), breaks = 10, labels = FALSE)
rows <- which(folds == 10, arr.ind=TRUE)
haberman.train <- haberman[-rows,]
haberman.test <- haberman[rows,]
model.rf <- randomForest(x = haberman.train[,-4], y = haberman.train[,4], ntree = 2000, do.trace = 100)

rf.result <- predict(model.rf, newdata = haberman.test[,-4])
plot(model.rf)


# zbior 2

southGerman <- read.table("SouthGermanCredit.asc", header = TRUE) 

folds <- cut(seq(1, nrow(southGerman)), breaks = 10, labels = FALSE)
rows <- which(folds == 10, arr.ind = TRUE) 
southGerman.train <- southGerman[-rows,]
southGerman.test <- southGerman[rows,]
model.rf <- randomForest(x = southGerman.train[,-21], y = southGerman.train[,21], ntree = 2000, do.trace = 100)

rf.result <- predict(model.rf, newdata = southGerman.test[,-21])
plot(model.rf)


# knn
# zbior 1

haberman <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/
haberman/haberman.data",header = F, sep = ',')

rows.haberman <- sample.int(nrow(haberman), size = round(nrow(haberman) / 2), replace = F)
haberman.train <- haberman[-rows.haberman, -4]
haberman.test <- haberman[rows.haberman, -4]
cl.train.haberman <- haberman[-rows.haberman, 4]
cl.test.haberman <- haberman[rows.haberman, 4]
for(i in 1:10){
  knn.result.haberman <- knn(train = haberman.train, test = haberman.test, cl =
                               cl.train.haberman, k = i) # tu ilosc sasiadow
  error.haberman <- sum(cl.test.haberman != knn.result.haberman) / length(knn.result.haberman)
  print(paste("Dla k =", i, " Blad:", round(error.haberman, 2), "% Dokladnosc: ", 
              1 - round(error.haberman, 2), "%"))
}

# zbior 2
southGerman <- read.table("SouthGermanCredit.asc", header = TRUE) 

rows.southGerman <- sample.int(nrow(southGerman), size = round(nrow(southGerman) / 2), replace = F)
train.set <- southGerman[-rows.southGerman, -21]
test.set <- southGerman[rows.southGerman, -21]
cl.train.southGerman <- southGerman[-rows.southGerman, 21]
cl.test.southGerman <- southGerman[rows.southGerman, 21]
for(i in 1:15){
  knn.result.southGerman <- knn(train = train.set, test = test.set, cl =
                                  cl.train.southGerman, k = i) # tu ilosc sasiadow
  error.southGerman <- sum(cl.test.southGerman != knn.result.southGerman) / 
    length(knn.result.southGerman)
  print(paste("Dla k =", i, " Blad:", round(error.southGerman, 2), "% Dokladnosc:", 
              1 - round(error.southGerman, 2), "%"))
}



# metoda Naive Bayesa

# zbior 1
haberman <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data",header = F, sep = ',')

folds <- cut(seq(1,nrow(haberman)), breaks=2, labels=FALSE)
rows <- which(folds == 2, arr.ind = TRUE)
train.set <- haberman[-rows,]
test.set <- haberman[rows,]
model.bayes <- naiveBayes(x = train.set[,-4], y = train.set[,4])
bayes.result <- predict(model.bayes, test.set[,-4])
error.bayes <- 1 - sum(bayes.result == test.set[,4]) / length(test.set[,4])
print(paste("Blad:", round(error.bayes * 100, 2), "%"))
print(paste("Dokladnosc: ", (sum(bayes.result == test.set[,4]) / length(test.set[,4])) * 100, "%"))
plot(bayes.result)

# zbior 2
southGerman <- read.table("SouthGermanCredit.asc", header = TRUE)

folds <- cut(seq(1,nrow(southGerman)), breaks=5, labels=FALSE)
rows <- which(folds==5, arr.ind=TRUE)
southGerman.train <- southGerman[-rows,]
test.set <- southGerman[rows,]
model.bayes <- naiveBayes(x = southGerman.train[,-21], y = southGerman.train[,21])
bayes.result <- predict(model.bayes, test.set[,-21])
error.bayes <- 1-sum(bayes.result == test.set[,21])/length(test.set[,21])
print(paste("Blad:", round(error.bayes * 100, 2), "%"))
print(paste("Dokladnosc: ",(sum(bayes.result == test.set[,21]) / length(test.set[,21])) * 100, "%"))


# neuralnet

# zbior 1
haberman <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data",header = F, sep = ',')

rows.haberman <- sample.int(nrow(haberman), size = round(nrow(haberman)/2), replace = F)
haberman.train <- haberman[-rows.haberman,]
haberman.test <- haberman[rows.haberman,]
model <- neuralnet(formula = V4 ~., data = haberman.train, linear.output = F)
result <- predict(model, haberman.test)
print(model$result.matrix)
plot(model)



# zbior 2
southGerman <- read.table("SouthGermanCredit.asc", header = TRUE)

rows.southGerman <- sample.int(nrow(southGerman), size = round(nrow(southGerman)/4), replace = F)
train.set <- southGerman[-rows.southGerman,]
test.set <- southGerman[rows.southGerman,]
model <- neuralnet(formula = laufkont~., data = train.set, linear.output = F)
result <- predict(model, test.set)
model$result.matrix
plot(model)


# K means

#zbior 1



# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(haberman, centers = 2, nstart = 20)
kmeans.re

# Confusion Matrix
cm <- table(haberman$V4, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(haberman[c("V1", "V2")], 
     col = kmeans.re$cluster, 
     main = "K-means with 2 clusters")

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(haberman[, c("V1", "V2")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster Haberman's Survival Data Set"),
         xlab = 'V1',
         ylab = 'V2')


# zbior 2

# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(southGerman, centers = 4, nstart = 20)
kmeans.re

# Confusion Matrix
cm <- table(southGerman$laufkont, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(southGerman[c("laufzeit", "rate")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(southGerman[, c("laufzeit", "rate")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster South German Credit Data Set"),
         xlab = 'laufzeit',
         ylab = 'rate')

