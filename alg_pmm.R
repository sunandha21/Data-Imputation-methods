data("iris")
##data classifies only as setosa
iris.s<-iris[with(iris,Species=="setosa"), ] 
summary(iris.s)
mean.s.sl<-mean(iris.s$Sepal.Length) #mean of sepal length of species classifies as setosa
mean.s.sw<-mean(iris.s$Sepal.Width)  #mean of sepal width of species classifies as setosa
mean.s.pl<-mean(iris.s$Petal.Length) #mean of petal length of species classifies as setosa
mean.s.pw<-mean(iris.s$Petal.Width)  ##mean of petal width of species classifies as setosa

##iris.s[1,]
threshold=0.2
for(i in 1:length(iris.s))
{ 
 if(abs(mean.s.sl-iris.s$Sepal.Length[i])>threshold){
   iris.s$Sepal.Length[i]<-NA
 }
}
for(i in 1:length(iris.s))
{ 
  if(abs(mean.s.sw-iris.s$Sepal.Width[i])>threshold){
    iris.s$Sepal.Width[i]<-NA
  }
}
for(i in 1:length(iris.s))
{ 
  if(abs(mean.s.pl-iris.s$Petal.Length[i])>threshold){
    iris.s$Petal.Length[i]<-NA
  }
}
for(i in 1:length(iris.s))
{ 
  if(abs(mean.s.pw-iris.s$Petal.Width[i])>threshold){
    iris.s$Petal.Width[i]<-NA
  }
}
summary(iris.s)


##data classified only as versicolor
iris.v<-iris[with(iris,Species=="versicolor"), ]
summary(iris.v)
mean.v.sl<-mean(iris.v$Sepal.Length)
mean.v.sw<-mean(iris.v$Sepal.Width)
mean.v.pl<-mean(iris.v$Petal.Length)
mean.v.pw<-mean(iris.v$Petal.Width)

for(i in 1:length(iris.v))
{ 
  if(abs(mean.v.sl-iris.v$Sepal.Length[i])>threshold){
    iris.v$Sepal.Length[i]<-NA
  }
}
for(i in 1:length(iris.v))
{ 
  if(abs(mean.v.sw-iris.v$Sepal.Width[i])>threshold){
    iris.v$Sepal.Width[i]<-NA
  }
}
for(i in 1:length(iris.v))
{ 
  if(abs(mean.v.pl-iris.v$Petal.Length[i])>threshold){
    iris.v$Petal.Length[i]<-NA
  }
}
for(i in 1:length(iris.v))
{ 
  if(abs(mean.v.pw-iris.v$Petal.Width[i])>threshold){
    iris.v$Petal.Width[i]<-NA
  }
}
summary(iris.v)

##data classified only as virginica
iris.vi<-iris[with(iris,Species=="virginica"), ]
summary(iris.vi)
mean.vi.sl<-mean(iris.vi$Sepal.Length)
mean.vi.sw<-mean(iris.vi$Sepal.Width)
mean.vi.pl<-mean(iris.vi$Petal.Length)
mean.vi.pw<-mean(iris.vi$Petal.Width)
for(i in 1:length(iris.vi))
{ 
  if(abs(mean.v.sl-iris.vi$Sepal.Length[i])>threshold){
    iris.vi$Sepal.Length[i]<-NA
  }
}
for(i in 1:length(iris.vi))
{ 
  if(abs(mean.vi.sw-iris.vi$Sepal.Width[i])>threshold){
    iris.vi$Sepal.Width[i]<-NA
  }
}
for(i in 1:length(iris.vi))
{ 
  if(abs(mean.vi.pl-iris.vi$Petal.Length[i])>threshold){
    iris.vi$Petal.Length[i]<-NA
  }
}
for(i in 1:length(iris.vi))
{ 
  if(abs(mean.v.pw-iris.vi$Petal.Width[i])>threshold){
    iris.vi$Petal.Width[i]<-NA
  }
}
summary(iris.vi)

##combine all the three data subsets
iris.mis<-rbind(iris.s,iris.v,iris.vi)
summary(iris.mis)
iris.mis <- subset(iris.mis, select = -c(Species))
library(mice)
imputed_Data <- mice(iris.mis, m=1, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)
imputed_Data <- complete(imputed_Data,1)
iris.org<-subset(iris, select = -c(Species))
library(caret)
RMSE(iris.org,imputed_Data)  ##RMSE claculation

#knn
newtrain = data.frame(sepal_length = iris$Sepal.Length,sepal_width = iris$Sepal.Width,petal_length = iris$Petal.Length,petal_width = iris$Petal.Width)
newtest = data.frame(sepal_length = imputed_Data$Sepal.Length,sepal_width = imputed_Data$Sepal.Width,petal_length = imputed_Data$Petal.Length,petal_width = imputed_Data$Petal.Width)
library(class)
imputed_Data_species = knn(train = newtrain, test = newtest,cl=iris$Species,k=3) 
imputed_Data$species<-imputed_Data_species
#confusion matrix, to claculate accuracy of model
cm<-confusionMatrix(iris$Species,imputed_Data$species)
cm
