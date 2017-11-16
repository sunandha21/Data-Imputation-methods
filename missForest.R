data("iris")

library(missForest)
iris.mis<-prodNA(iris,noNA = 0.1)
iris.mis <- subset(iris.mis, select = -c(Species))

summary(iris.mis)
iris.imp<-missForest(iris.mis)
iris.imp$ximp
summary(iris.imp)
iris.org<-subset(iris, select = -c(Species))
RMSE(iris.imp$ximp,iris.org)

#knn
newtrain = data.frame(sepal_length = iris$Sepal.Length,sepal_width = iris$Sepal.Width,petal_length = iris$Petal.Length,petal_width = iris$Petal.Width)
newtest = data.frame(sepal_length = iris.imp$ximp$Sepal.Length,sepal_width = iris.imp$ximp$Sepal.Width,petal_length = iris.imp$ximp$Petal.Length,petal_width = iris.imp$ximp$Petal.Width)
iris.imp_species = knn(train = newtrain, test = newtest,cl=iris$Species,k=3) 
iris.imp$ximp$species<-iris.imp_species
cm<-confusionMatrix(iris$Species,iris.imp$ximp$species)
cm

