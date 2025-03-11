library(mlr3verse)
library(tidyverse)
library(mlr3pipelines)
library(mlr3tuning)
library(paradox)

#Task moneyball ####

task = tsk("moneyball")

view(task)

task$data()%>% head %>% view

autoplot(task)

#creating a suitable SVM learner and tune some parameters ###

learner.svm = lrn(
            "regr.svm",
            type="nu-regression",
            kernel="radial"
)

#use pipeline robustify to impute missing values

svm_graph <- ppl("robustify", task = task, learner = learner.svm) %>>% learner.svmf

svm_graph %>%
  plot

#create a new svm learner

graph.learner <- as_learner(svm_graph)
graph.learner$id <- "g.svm"

#benchmark multiple learners including one created above ####

set.seed(69)
resampling_cv <- rsmp("cv", folds=10) 
resampling_cv$instantiate(task)

learners <- list(graph.learner, lrn("regr.rpart"), 
                 lrn("regr.featureless"))

design <- benchmark_grid (learner = learners,
                          task = task,
                          resamplings = resampling_cv
                          )

bmr <- benchmark(design)
bmr$aggregate()

#define an AutoTuner with the svm learner, optimizing cost and gamma parameters ####


#search_space<-ps(
#  regr.svm.cost= p_dbl(lower =1e-5,upper=1e5,logscale=TRUE),
#  regr.svm.gamma= p_dbl(lower=1e-5,upper=1e5,logscale=TRUE)
#)

auto.svm <- AutoTuner$new(
  learner = graph.learner,
  resampling = rsmp('holdout'),
  measure = msr("regr.rmse"),
  search_space = ps(
    c = p_dbl(lower = 10 ** -5, upper = 10 ** 5, 
              trafo = function(x) 10 ** x),
    kernel = p_fct(levels = c("polydot", "rbfdot"))
  ),
  terminator = trm("evals", n_evals = 100),
  tuner = tnr("cmaes")
)

#same but with random search tuner

auto.random <- AutoTuner$new(
  learner = graph.learner,
  resampling = rsmp('holdout'),
  measure = msr("regr.rmse"),
  search_space = ps(
    c = p_dbl(lower = 10 ** -5, upper = 10 ** 5, 
              trafo = function(x) 10 ** x),
    kernel = p_fct(levels = c("polydot", "rbfdot"))
  ),
  terminator = trm("evals", n_evals = 100),
  tuner = tnr("random_search")
)


auto.random

#new benchmark with added learners ####

learners.summary <- list(graph.learner,
                 auto.svm,
                 auto.random,
                 lrn("regr.rpart"), 
                 lrn("regr.featureless"))

design.summary <- benchmark_grid (learner = learners,
                          task = task,
                          resamplings = resampling_cv
)

bmr <- benchmark(design)

autoplot(bmr)

bmr$aggregate()
