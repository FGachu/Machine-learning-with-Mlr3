library(tidyverse)
library(mlr3verse)
library(xgboost)
library(iml)

#German credit task ####

task = tsk("german_credit")
task %>% view()
autoplot(task)
task$missings()

learner = lrn("classif.xgboost")

mlr_learners %>%
  as.data.table %>%
  View()

measure = msr("classif.auc")

graph <- ppl(
  "robustify",
  learner = lrn("classif.xgboost"),
  task = task) %>>%
  lrn("classif.xgboost", predict_type = "prob")
  

gl <- GraphLearner$new(graph)

#tuning hyperparameters

search_space <- ps(
  classif.xgboost.nrounds = p_int(1,1000)
)
??search_space
plot(graph)

instance <-TuningInstanceBatchSingleCrit$new(
  task = task,
  learner = gl,
  resampling = rsmp("holdout"),
  measure = measure,
  terminator = trm("none"),
  search_space = search_space
)

tuner <- tnr("grid_search", resolution = 100)

tuner$optimize(instance)

archive <- instance$archive %>%
  as.data.table() %>%
  arrange(-classif.auc)

archive %>% view("archive")

ggplot(archive) + geom_line(aes(x=classif.xgboost.nrounds, y = classif.auc))

gl$param_set$values$classif.xgboost.nrounds = 250
gl$train(task)


#creating a predictor object ####
task
predictor <- Predictor$new(
  model = gl,
  data = task$data(), 
  y = task$credit_risk,
)

#using partial dependancy plots ####

pdp_dur <- FeatureEffect$new(
            predictor = predictor,
            feature = "duration",
            method = "pdp"); pdp_dur %>% plot()

ale_dur <- FeatureEffect$new(
  predictor = predictor,
  feature = "duration",
  method = "ale"); ale_dur %>% plot()

#Individual Conditional Expectations ####

ice <- FeatureEffect$new(
  predictor = predictor,
  feature = "duration",
  method = "pdp+ice"); ice %>% plot()

ice_center <- FeatureEffect$new(
  predictor = predictor,
  feature = "duration",
  method = "pdp+ice",
  center.at = 0); ice_center %>% plot()

#Shapely ####

shapley <- Shapley$new(
  predictor = predictor,
  x.interest = as.data.frame(task$data())
  ); shapley$plot()


df = shapley$results %>%
  group_by(class) %>%
  summarise(sum = sum(phi))

shapley$y.hat.average
  