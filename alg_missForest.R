data("iris")
##data classifies only as setosa
iris.s<-iris[with(iris,Species=="setosa"), ] 
summary(iris.s)
mean.s.sl<-mean(iris.s$Sepal.Length)
mean.s.sw<-mean(iris.s$Sepal.Width)
mean.s.pl<-mean(iris.s$Petal.Length)
mean.s.pw<-mean(iris.s$Petal.Width)

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

##dat classified only as virginica
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
library(missForest)
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

