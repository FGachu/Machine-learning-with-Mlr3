library(mlr3verse)
library(tidyverse)
library(mlr3tuning)
library(mlr3fselect)

#spam data set ####

data(spam)

task <- tsk("spam")
autoplot(task)

task$missings()

#creating an importance feature filter ####

lrn.ranger = lrn("classif.ranger" , importance = "impurity")

filter = flt("importance", learner = lrn.ranger)

set.seed(0xc0FEE)

filter$calculate(task)
filter$scores

as.data.table(filter)

lrn.rpart = lrn("classif.rpart", importance = "impurity")

filter = flt("importance", learner = lrn.ranger)
filter$calculate(task)
as.data.table(filter)


#intergrate the importance feature with the random forest ####

learner2 <- as_learner(
  po("filter", flt("importance", 
                    learner = lrn.ranger)) %>>%
    lrn("classif.ranger", predict_type = "prob")
)

tuner_nfeat = AutoTuner$new(
  learner = learner2,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  search_space = ps(
    importance.filter.nfeat = p_int(1, 57, logscale = FALSE)
  ),
  terminator = trm("none"),
  tuner =  tnr("grid_search", resolution = 10)
  )

tuner_nfeat$train(task)

tuner_nfeat$tuning_instance %>%
  autoplot(cols_x = c("importance.filter.nfeat"))


#Creating a sequential wrapper with a decision tree learner. ####
set.seed(123L)

resampling = rsmp("holdout")
resampling$instantiate(task)

instance = FSelectInstanceBatchSingleCrit$new(
    task = task,
    learner = lrn("classif.rpart", predict_type = "prob"),
    resampling = resampling,
    measure = msr("classif.auc"),
    terminator = trm("stagnation", iters = 10),
    store_models = TRUE
    )

fselector <- fs("sequential")
fselector$optimize(instance)

instance$result_feature_set %>%
  as_tibble()

as.data.table(instance$archive) %>% as_tibble()


# Create an AutoFSelector with the setting from above ####


auto_fs = AutoFSelector$new(
  fs("sequential"),
  learner = lrn("classif.rpart", predict_type = "prob"),
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  terminator = trm("stagnation", iters = 10), #100 in the question
  store_models = TRUE
  )


#Benchmarking the filter adn wrapper approaches against each other #### 

learner3 = list(tuner_nfeat, auto_fs, learner = lrn("classif.rpart", predict_type = "prob"))
r.cv = rsmp("cv")


benchmark_learners <- benchmark(
  benchmark_grid(task=task, learner=learner3, r.cv))

benchmark_learners$aggregate(msr("classif.auc"))
benchmark_learners %>% autoplot(msr("classif.auc")) #doesn't make sense

autoplot(benchmark_learners)


