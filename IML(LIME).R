library(iml)
library(mlr3verse)
library(tidyverse)


# Boston Task ####

data("Boston", package = "MASS")
Boston %>% view()

#alternatives for loading the task
#task <- data("BostonHousing", package = "mlbench") not so good
#task <- data("Boston", package = "MASS")

ggplot (
  data = Boston,
  aes(y=crim, x=medv))+ geom_point()


ggplot (Boston)+
  geom_density(aes(x=medv))

task = TaskRegr$new(
  Boston,
  target = "medv",
  id = "Boston"
)


#learner and benchmarking ####

learner = lrn("regr.ranger")

measure = msr("regr.rmse")

bmr = benchmark(benchmark_grid(task, c(learner, lrn("regr.featureless"), lrn("regr.rpart")),rsmp("cv")))
bmr$aggregate(measure = measure)

bmr %>%  autoplot(measure = measure)

#Create a predictor with the trained model ####

learner$train(task)
predictor = Predictor$new(model = learner,
                          data = task$data(),
                          y = task$target_names)

#FeatureImp object

importance = FeatureImp$new(predictor=predictor, 
                            loss = "rmse", 
                            n.repetitions = 20 
                            )
plot(importance)

iml::loc
??LocalMOdel

#Installing packages needed for LIME and using LIME #### 
install.packages("glmnet")
install.packages("gower")

lime <- LocalModel$new(predictor, x.interest = as.data.frame(task$data()))

# Select a single observation to explain (let's say the first row)
x_explain <- as.data.frame(task$data()[1, ])

# Create the LIME model for this single observation
lime <- LocalModel$new(predictor, x.interest = x_explain)

# You can then plot the explanation
plot(lime)
