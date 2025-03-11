library(tidyverse)
library(mlr3verse)

#benchmarking multiple learners on iris task ####

task <-tsk("german_credit")

autoplot(task)

#autoplot(task, type="pairs")

#defining the learners ####

learners <- lrns (
  c("classif.rpart", "classif.ranger", "classif.kknn", 
    "classif.featureless"), 
  predict_type = "prob"
  )

##also

learners <- list(
  lrn("classif.rpart", predict_type = "prob"),
  lrn("classif.ranger", predict_type = "prob")
)


#bootstrap resampling #### 

rsmp("bootstrap", repeats = 100)

#performing the benchmark ####

rsmp.boot = rsmp("bootstrap", repeats = 100)

design = benchmark_grid(
   tasks = list(task),
   learners = learners,
   resamplings = rsmp.boot
   )

bmr<- benchmark(design)


bmr$aggregate(msrs(list("classif.acc", "classif.bacc")))

bmr %>% autoplot(measure = msr("classif.acc"))
bmr %>% autoplot(measure = msr("classif.bacc"))
