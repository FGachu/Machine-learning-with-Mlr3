library(mlr3verse)
library(tidyverse)

#Iris Task ####

task <- tsk("iris") 
task

#alternative
#task <- mlr_tasks$get("iris")
#task <- TaskClassif$new("iris", iris, "Species") #prone to errors
  
autoplot(task)

#to make it more useful

autoplot(task, type= "pairs")


#defining a knn learner and train it on the task ####

lrn.knn <- mlr_learners$get("classif.kknn")
lrn.knn$train(task=task)

#lrn.knn$param_set

lrn.knn$model

lrn.knn$train(task=task)

#performance evaluation ####

pred <- learner$predict(task)
pred

msr("classif.acc")
pred$score(msr("classif.acc"))

#change the learner to output probabilities instead of just class predictions ####

learner$predict_type <- "prob"
learner$train(task)
pred <- learner$predict(task)
pred

#find a measure that can utilize class prob and evaluate the learner ####

mlr_measures %>%
  as.data.table %>%
  filter(task_type == "classif") %>%
  filter(task_type == "prob") %>%
  filter(task_properties != "twoclass")%>%
  view()

measure <- msr("classif.mbrier")
pred$score(measures=measure)

#finding the optimal value of k ####

learner.knn <- lrn("classif.kknn", k=1)
pred <- learner.knn$predict(task)
#> Warning: Paket 'kknn' wurde unter R Version 4.4.2 erstellt
pred$score(msrs(list("classif.acc", "classif.mbrier")))


  