library("mlr3verse")
library("tidyverse")
library("mlr3tuning")
library("mlr3fselect")

#sonar task ####

task <- tsk("sonar")

task

autoplot(task)


#Creating two AutoFSelector objects ####

set.seed(123L)
inner_resampling = rsmp("cv", folds =3)
terminator = trm("evals", n_evals = 10)

auto_fs_random = AutoFSelector$new(
  fs("random_search"),
  learner = lrn("classif.kknn", predict_type = "prob"),
  resampling = inner_resampling,
  measure = msr("classif.auc"),
  terminator = terminator, 
  store_models = TRUE
)
auto_fs_random$id = "random_fs"

auto_fs_genetic = AutoFSelector$new(
  fselector = fs("genetic_search", 
                 popSize = 100L,
                 elitism = 2L,
                 zeroToOneRatio = 2L
                 ),
  learner = lrn("classif.kknn"),
  resampling = resampling,
  measure = msr("classif.bacc"),
  terminator = terminator, 
  store_models = TRUE
)

auto_fs_random$id = "random_fs"

#Benchmarking a list of 4 learners including above ####

outer_resampling = rsmp("cv")

learners = list(
  auto_fs_random,
  auto_fs_genetic,
  lrn("classif.kknn"),
  lrn("classif.featureless")
)

design = benchmark_grid(
  task = task,
  learners = learners,
  resamplings = outer_resampling
)

bmr = benchmark(design, store_models = TRUE)

autoplot(bmr)