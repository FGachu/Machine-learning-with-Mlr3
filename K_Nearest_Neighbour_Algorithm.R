#necessary packages
library(tidyverse)
library("ggplot2")
library("kit")

data <- iris
View(data)

#visualizing the data
ggplot(data = data, aes(x = Sepal.Length, 
                        y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Scatterplot of Sepal Length vs. Sepal Width", 
       x = "Sepal Length", y = "Sepal Width")

ggplot(data = data, aes(x = Petal.Length, 
                        y = Petal.Width, color = Species)) +
  geom_point() +
  labs(title = "Scatterplot of Petal Length vs. Petal Width", 
       x = "Petal Length", y = "Petal Width")

ggplot(data = iris, aes(x = Sepal.Length/Sepal.Width,
                        y = Petal.Length/Petal.Width, color = Species)) +
  geom_point() +
  labs(title = "Iris data", 
       x = "Sepal Proportion", y = "Sepal proportion")+
   theme(plot.title = element_text(hjust = 0.5))


#splitting data into training and test datasets

set.seed(10) #for reproducability
test_idx = sample(1:nrow(data), size = 0.8*nrow(data), 30)
train_idx <- setdiff(1:150, test_idx)


x_test <- data[test_idx, -5]
y_test <- data[test_idx, 5]

x_train <- data[train_idx, -5]
y_train <- data[train_idx, 5]

head(x_train)


#function dist(p, q) that takes two observations and calculates the euclidean distance
dist <- function(p,q) {
  sqrt(sum(p-q)**2)
}

v_dist = nrow(data)
v_dist = vector(mode="numeric", length = 120L)
for (i in 1:120) {
  v_dist[i] <- dist(x_test[1,], x_train[i,])
}

v_dist%>%head()

View(v_dist)

#species of the 3 nearest neighbors

nn <- topn(v_dist, n=3, decreasing = FALSE)
nn

nn_spec= y_train[nn]
nn_spec

# Predict the Species of the first test observation.
sort_tab = table(nn_spec) %>% sort(decreasing = TRUE)
prediction = names(sort_tab[1])
prediction == y_test[1]


nearest_species <- y_train[nearest_indices]