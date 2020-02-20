#Charlie Huemmler

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

irisdf <- iris

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

versi <- iris[iris$Species == "versicolor", ]

x <- c("Sepal.Length","Petal.Length","Sepal.Length")
y <- c("Sepal.Width","Petal.Width","Petal.Length")

lm.out <- list()

for(i in 1:3){
  lm.out[[i]] <- lm(versi[ ,y[i]] ~ versi[ ,x[i]])
}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))


#iris left
#height right

irisdf<-left_join(x= irisdf, y = height, by = "Species")
irisdf$Petal.Width/irisdf$Height.cm



#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data = irisdf, aes(y = Sepal.Width, x= Sepal.Length))+
  geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data = irisdf, aes(y = Sepal.Width, x= Sepal.Length))+
  geom_point()+
  theme_bw()

#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size

ggplot(data = irisdf, aes(y = Sepal.Width, x= Sepal.Length, color = Species))+
  geom_point(size =2.5)+
  theme_bw()

  #####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		