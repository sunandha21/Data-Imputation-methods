data("iris")
library(Amelia)
library(missForest)

#10% missing values at random, percentage can be changed by changing value of noNA
iris.mis<-prodNA(iris,noNA = 0.1)
#iris.mis <- prodNA(iris, noNA = 0.02) #for 2% 
#iris.mis <- prodNA(iris, noNA = 0.05) #for 5%
#iris.mis <- prodNA(iris, noNA = 0.15) #for 15%
#iris.mis <- prodNA(iris, noNA = 0.2)  #for 20%
#iris.mis <- prodNA(iris, noNA = 0.25) #for 25%

amelia_fit <- amelia(iris.mis, m=1, parallel = "multicore",noms = "Species")
###colnames(amelia_fit$imputations[[2]])
iris.org<-subset(iris, select = -c(Species))
iris.imp<-subset(amelia_fit$imputations[[1]],select=-c(Species))
library(caret)
RMSE(iris.org,iris.imp)

#knn
newtrain = data.frame(sepal_length = iris$Sepal.Length,sepal_width = iris$Sepal.Width,petal_length = iris$Petal.Length,petal_width = iris$Petal.Width)
newtest = data.frame(sepal_length = iris.imp$Sepal.Length,sepal_width = iris.imp$Sepal.Width,petal_length = iris.imp$Petal.Length,petal_width = iris.imp$Petal.Width)
library(class)
iris.imp_species = knn(train = newtrain, test = newtest,cl=iris$Species,k=3) 
iris.imp$species<-iris.imp_species
cm<-confusionMatrix(iris$Species,iris.imp$species)
cm
