library(tidyverse)
library(mlr3verse)
library(mlr3pipelines)
library(mlr3tuning)
library(iml)

task <- tsk('penguins')
task

head(task)
summary(task)

autoplot(task)

task %>% autoplot

#Define a decision tree learner and find its performance####

learner <- lrn('classif.rpart')
lrn()

#setting seed
set.seed(0xC0FFEE)

resample <- rsmp("cv", folds = 10)
resample$instantiate(task)

rr <-resample(task=task, learner=learner, resampling = resample)
rr$aggregate()


#Evaluate a random forest learner on the dataset####

#to see the available learners in mlr3
mlr_learners %>%
  as.data.table() %>%
  view()

learner_2 <- lrn('classif.ranger')

rr_2 <-resample(task=task, learner=learner_2, resampling = resample)
rr_2$aggregate()

# We get an error that data is missing! 
# In order to apply this learner, the dataset will need to be modified.

#using imputatuions to impute the missing values

graph_ranger <- 
  po("imputemedian") %>>%  
  po("imputemode") %>>% 
  lrn("classif.ranger")   

learner_ranger_gr <- as_learner(graph_ranger)

rr_ranger <- resample(task=task,learner_ranger_gr, resample)

rr_ranger$aggregate()

#Evaluating using the default svm ####

learner_svm<-lrn("classif.svm")
resample(task,learner_svm,resampling)
#Error! there are factors, need to transform the 
#factor columns using one-hot-encoding

graph_svm <-
  po("imputemedian") %>>%  
  po("imputemode") %>>% 
  po("encode") %>>%
  lrn("classif.ranger") 

learner_svm_gr <- as_learner(graph_ranger)
rr_svm <- resample(task=task, 
                      learner = learner_svm_gr, 
                      resampling = resample)

rr_svm$aggregate()

#benchmarking the learners ####

learners <- list(
  learner,
  learner_2,
  learner_ranger_gr,
  learner_svm_gr,
  lrn('classif.featureless')
)

 
resampling <- rsmp("cv", folds = 10) 
set.seed(123L)
benchmark_design <- benchmark_grid(task=task, learner=learners, resamplings = resample)

bmr_2 <- benchmark(benchmark_design)
autoplot(bmr_2)

