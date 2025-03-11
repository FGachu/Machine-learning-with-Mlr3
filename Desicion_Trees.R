data("mtcars")
View(mtcars)

library(tidyverse)

##visualizing the data
ggplot(data=mtcars, aes(y=mpg, x= hp, colour = disp))+ geom_point() + 
  labs(x="horsepower", y="miles per gallon")


ggplot(data=mtcars, aes(x=disp/cyl, y= mpg, colour = gear))+ geom_point() + 
  labs(y="miles per gallon", x="Displacement per cylinder")

ggplot(data=mtcars, aes(x=cyl, y=mpg, color=wt))+ geom_point() +
  scale_color_viridis_b()



##splitting the data into 2
data<- mtcars
x_var <- data[,-1]
y_var <- data[,1]

partition<-function(X,s_var,s_val){
  r1<-X[,s_var] < s_val
  r2<-X[,s_var] >=s_val
  list(r1,r2)
}

#idx<-partition(X,1,5); idx

##calculating the ssr

loss<-function(y){
  (y-mean(y))** 2%>% sum
}
loss_part<-function(y,idx){
  loss(y[idx[[1]]])+loss(y[idx[[2]]])
}
#loss_part(y,idx)

## To find the best split of the feature space

var_splits<-function(var){
  u_s<-var%>% unique%>%sort
  splits<-vector(mode="numeric",length=length(u_s)-1)
  for(i in 1:(length(u_s)-1)){
    splits[i]<-u_s[i+ 1]-(u_s[i+ 1]-u_s[i]) / 2
  }
  splits
}
all_splits<-function(X){
  apply(X,2,var_splits)
}
#splits<-all_splits(X);splits

##finding the respective losses
  
all_losses<-function(X,y, splits){
  losses <-splits
  for(j in 1:length(splits)) {
    for(k in 1:length(splits[[j]])){
      losses[[j]][k]<-partition(X,j,splits[[j]][k])%>%
        loss_part(y=y,idx=.)
    }
  }
  losses
}
#losses <-all_losses(X,y, splits) 
  
#a function get_var_split(splits,losses) that given the list of all possible splits
#splits as well as the list of all their corresponding losses losses
#find the variable name and value corresponding to the best split.

get_first_split<-function(X,y){
  splits<-all_splits(X)
  losses <-all_losses(X,y,splits)
  get_var_split(splits,losses)
}
get_first_split(X,y)
  
##############################################################

#using the packages tree to get a decision tree of the mtcars dataset

##############################################################

library(tree)

tree.mtcars <- tree(formula = mpg ~. , data = mtcars )

tree.mtcars

#plot the tree using the plot and text fxns

plot(tree.mtcars)
text(tree.mtcars, pretty = 0, cex = 0.69)


summary(tree.mtcars)  
  

