 #load data
data(iris)

#Calculate petal length mean
petal_length_mean <- mean(iris$Petal.Length)

#Plot distribution of petal length with a histogram
hist(iris$Petal.Length)

#install ggplot2 library
install.packages("ggplot2")
library(ggplot2)

iris
#Plot distribution of petal length with a histogram but make it pretty
ggplot(iris, aes(Petal.Length, fill = Species)) +
  geom_histogram()



#can plot other types of graphs depending on what the purpose of the graph is, as well as what variable(s) you might want to focus on