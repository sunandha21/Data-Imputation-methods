data("iris")
library(missForest)
##10% missing values at random, percentage can be changed by changing value of noNA
iris.mis <- prodNA(iris, noNA = 0.25)   #for 10%
#iris.mis <- prodNA(iris, noNA = 0.02) #for 2% 
#iris.mis <- prodNA(iris, noNA = 0.05) #for 5%
#iris.mis <- prodNA(iris, noNA = 0.15) #for 15%
#iris.mis <- prodNA(iris, noNA = 0.1)  #for 20%
#iris.mis <- prodNA(iris, noNA = 0.1) #for 25%
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)
library(mice)
#mice implementation m-no.of imputed datasets, maxit- no. of iterations taken to impute missing values, method-method used in imputation. 
imputed_Data <- mice(iris.mis, m=1, maxit = 50, method = 'pmm', seed = 500)
##imputed_Data$imp$Sepal.Width
##imputed_Data$imp
summary(imputed_Data)
imputed_Data <- complete(imputed_Data,1)
summary(imputed_Data)
iris.org<-subset(iris, select = -c(Species))
library(caret)
#RMSE calculation
RMSE(iris.org,imputed_Data)
#knn 
newtrain = data.frame(sepal_length = iris$sepal_length,sepal_width = iris$sepal_width,petal_length = iris$petal_length,petal_width = iris$petal_width)
newtest = data.frame(sepal_length = imputed_Data$sepal_length,sepal_width = imputed_Data$sepal_width,petal_length = imputed_Data$petal_length,petal_width = imputed_Data$petal_width)
library(class)
imputed_Data_species = knn(train = newtrain, test = newtest,cl=iris$species,k=3) 
imputed_Data$species<-imputed_Data_species
cm<-confusionMatrix(iris$species,imputed_Data$species)
cm #gives accuracy of model


