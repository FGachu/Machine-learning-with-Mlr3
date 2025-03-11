library(mlr3verse)
library(tidyverse)
library(mlr3pipelines)
library(mlr3tuning)
library(paradox)
library(mlr3hyperband)
library(mlr3misc)
library(mlr3mbo)
library(bbotk)
library(future.apply)

#Bike sharing task ####

task <- tsk("bike_sharing")
autoplot(task)
summary(task)

task$missings()

#Training a desicion tree on the task ####

learner_rpart<-lrn("regr.rpart")
learner_rpart$train(task)

#need to preprocess the data with robustify

graph <- ppl("robustify", task = task, learner = learner_rpart)
plot(graph)

#create a new learner ####

gl_rpart <- GraphLearner$new(
  graph %>>% po("subsample") %>>% learner_rpart
)

#define a search space that tunes the minsplit and cp parameters ####

search_space <- ps(
  regr.rpart.cp = p_dbl(1e-2,1, logscale = TRUE),
  regr.rpart.minsplit = p_int(1, 10),
  subsample.frac = p_dbl(1e-2,1, tags = "budget")
)

#optimize the graph learner with the hyperband tuner ####

set.seed(420)

instance <- TuningInstanceBatchSingleCrit$new(
  task = task,
  learner = gl_rpart,
  resampling = rsmp("holdout"),
  measure = msr("regr.rmse"),
  terminator = trm("none"),
  search_space = search_space
)

tuner <- tnr("hyperband", eta = 3)
tuner$optimize(instance)

as.data.table(instance$archive)
autoplot(instance)


##########################################################
#bayesian optimization
##########################################################


#Building a Bayesian Autotuner ####

learner_xgb <- lrn("regr.xgboost",
             eta = to_tune(1e-4, 1, logscale = TRUE),
             nrounds = to_tune(1, 10),
             max_depth = to_tune(1, 20))

graph_xgb <- as_learner(ppl("robustify",
                         learner = learner_xgb,
                         task = task) %>>%
                       learner_xgb)

graph_xgb$id <- "gl_xgb"

# loop function
loop_function <- bayesopt_ego

# surrogate model
surrogate <- srlrn(lrn("regr.ranger"))

# acquisition function
acq_function <- acqf("ei")

# acquisition optimizer
optimizer_bo <- opt("random_search")
terminator_bo <- trm("evals")
acq_optimizer <- acqo(optimizer_bo, terminator_bo)

# tuner
tuner_bo <- tnr("mbo",
              loop_function = loop_function,
              surrogate = surrogate,
              acq_function = acq_function,
              acq_optimizer = acq_optimizer)

autotuner_bo_xgb <- AutoTuner$new(
  learner = graph_xgb,
  tuner = tuner_bo,
  terminator = trm("evals", n_evals = 20),
  measure = msr("regr.rmse"),
  resampling = rsmp("holdout")
)

autotuner_bo_xgb$id<-"at_bo_xgb"

#Benchmark Bayesian Optimization, Random Search and untuned xgboost ####

autotuner_random_xgb<-AutoTuner$new(
 learner=graph_xgb,
 resampling=rsmp("holdout"),
 terminator=trm("evals",n_evals=20),
 tuner= tnr("random_search"),
 measure= msr("regr.rmse")
 )

autotuner_random_xgb$id<-"at_rs_xgb"
 
untuned_xgb <-as_learner(ppl("robustify",
 learner=lrn("regr.xgboost"),
 task=task)%>>%
 lrn("regr.xgboost"))

untuned_xgb$id<-"xgboost"
 learners<-list(
 lrn("regr.featureless"),
 untuned_xgb,
 autotuner_random_xgb,
 autotuner_bo_xgb
 )
 
resampling<-rsmp("cv",folds=3)

learners<-list(
  lrn("regr.featureless"),
  untuned_xgb,
  autotuner_random_xgb,
  autotuner_bo_xgb
)

#you might want to use parallelizaion to speed up the benchmark

future::plan(list("sequential","multisession"))
graph <- benchmark_grid(task,learners,resampling)

bmr <- benchmark(graph)
bmr$aggregate()
autoplot(bmr)
