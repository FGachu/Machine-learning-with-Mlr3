library(iml)
library(mlr3verse)
library(tidyverse)
library(mlr3fselect)
library(mlr3filters)
library(mlr3mbo)

#creating a mlr3 pipeline

filters =  list(
      mrmr = po("filter",flt("mrmr")), #mrmr - minimum redundancy maximum relevancy
      information_gain = po("filter", flt("information_gain")),
      nop = po("nop")
      )


learners = list(
  classif.rpart = lrn("classif.rpart", predict_type = "prob"),
  classif.kknn = lrn("classif.kknn", predict_type = "prob")
)

graph = (
  ppl("branch", filters, prefix_branchops = "filters_") %>>%
  ppl("branch", learners, prefix_branchops = "learner_")
)

plot(graph)

#Creating a graph learner from the pipeline ####

graph_learner = GraphLearner$new(graph)

#alternative graph_learner <- as_learner(graph)

# Defining the search space ####
graph$param_set

search_space = ps(
  filter_branch.selection = 
    p_fct(c("mrmr", "information_gain", "nop")),
  
  learner_branch.selection = 
    p_fct(c("classif.rpart", "classif.kknn")),
  
  mrmr.filter.nfeat = p_int(1,57, logscale = TRUE, ###logscale is the type of distribution to be used to select features
                      depends = (filter_branch.selection == "mrmr")),
  
  information_gain.filter.nfeat = p_int(1, 57, 
                        logscale = TRUE, 
                        depends = (filter_branch.selection == "information_gain")),

  classif.rpart.minsplit = p_int(lower = 1, upper = 100, 
                                  logscale = TRUE, 
                                  depends = learner_branch.selection == "classif.rpart"),
  
)

search_space = ps(
  filter_branch.selection = 
    p_fct(c("mrmr", "information_gain", "nop")),
  
  learner_branch.selection = 
    p_fct(c("classif.rpart", "classif.kknn")),
  
  mrmr.filter.nfeat = p_int(1, 57, logscale = TRUE, ###logscale is the type of distribution to be used to select features
                            depends = (filter_branch.selection == "mrmr")),
  
  information_gain.filter.nfeat = p_int(1, 57, 
                                        logscale = TRUE, 
                                        depends = (filter_branch.selection == "information_gain")),
  
  classif.rpart.minsplit = p_int(lower = 1, upper = 100, 
                                 logscale = TRUE, 
                                 depends = learner_branch.selection == "classif.rpart")
)

# Optimizing the pipeline on the task spam using random search ####
set.seed(42)
task = tsk("spam")


instance_tuner = TuningInstanceBatchSingleCrit$new(
  task = task,
  resampling = rsmp("cv", folds = 3),
  learner = graph_learner,
  measure = msr("classif.auc"),
  terminator =  trm("evals", n_evals = 10),
  search_space = search_space,
)

tuner_random = tnr("random_search")

tuner_random$optimize(instance_tuner)

# Extracting the three best configurations found ####

best_configs<-instance$archive$data%>%
  arrange(-classif.auc)%>%
  head(3)%>%
  select(x_domain)

copy_learner<-function(i){
  params<-best_configs$x_domain[[i]]
  learner_copy<-graph_learner$clone(deep=TRUE)
  learner_copy$param_set$values<-params
  learner_copy$id<-paste0("best_learner_",i)
  return(learner_copy)
}

best_learners<-lapply(1:3, copy_learner)

#An ensemble learner that uses the three best configurations defined before ####

ensemble_graph<-gunion(best_learners) %>>%
  po("classifavg")

ensemble_learner<-GraphLearner$new(ensemble_graph)

#Benchmarking the three best tuned learners ####

learners <- c(
  best_learners,
  ensemble_learner,
  lrn("classif.kknn", predict_type = "prob"),
  lrn("classif.rpart", predict_type = "prob")
)
bmr <- benchmark(benchmark_grid(task, learners, rsmp("cv")))
bmr$aggregate(measure = msr("classif.auc"))
bmr %>% autoplot(measure = msr("classif.auc"))