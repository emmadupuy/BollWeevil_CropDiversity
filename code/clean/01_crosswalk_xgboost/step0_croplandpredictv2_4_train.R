## Code for Cluster Training ##
## In this file we train the model ##

rm(list=ls())
source("clean/paths.R")

# for the parallelization
e <- new.env()
e$libs <- .libPaths()

list_packages <- list("parallel","dplyr","xgboost","mlr",
                      "parallelMap","stringr","purrr",
                      "httr","SpaDES","stringr")
lapply(list_packages, require, character.only = TRUE)
numCores <- detectCores()
## module requirements: gcc/6.1.0 for xgboost, for raster: geos/3.6.5, gdal/2.2.2, boost/1.72

# Function to load Rdata without having them named with their past object's name
loadRdata <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

mat_trans <- loadRdata(build_file("xgboost", "model", "mat_trans.Rdata"))

## Drop Year
mat_trans <- subset(mat_trans, select=-year)
mat_trans <- mat_trans[which(!is.na(mat_trans$mean)),] # 10 rows dropped

#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#*#   Fourth -#-  Tuning #*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#*

RNGkind("L'Ecuyer-CMRG")
set.seed(42)

# Random Grid Search 
# create tasks
traintask <- makeRegrTask (data = mat_trans, target = "mean")
# create learner
lrn <- makeLearner("regr.xgboost", predict.type = "response")
lrn$par.vals <- list( objective="reg:linear", eval_metric="rmse", nrounds=100L, eta=0.1)
#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = F,iters=4L)
#search strategy
ctrl <- makeTuneControlRandom(maxit = 30L)
#parameter tuning
mytune <- mlr::tuneParams(learner = lrn, task = traintask, resampling = rdesc,
                     par.set = params, control = ctrl, show.info = T)

# Building the model with tuned hyperparameters
#set hyperparameters
lrn_tune <- setHyperPars(lrn, par.vals=mytune$x)
#train model
xgmodel <- mlr::train(learner=lrn_tune, task=traintask)


#*#*#*#*#*#*#*#*#*#*#*#*#*#
#*#    Fifth  -#-  Train #*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#*

xgb_save <- list(xgmodel)
save(xgb_save, file=build_file("xgboost", "model", "xgboost_v2.Rdata"))


### --- OLD --- ###

#trainTask <- makeRegrTask(data = mat_trans, target = "mean")
# Create an xgboost learner 
#xgb_learner <- makeLearner(
#  "regr.xgboost",
  #predict.type = "response",
#  par.vals = list(
#    objective = "reg:logistic",
#    eval_metric = "rmse",
#    nrounds = 300
#  )
#)

## Now we can train the hyper-parameters
#xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
#  makeIntegerParam("nrounds", lower = 100, upper = 500),
  # number of splits in each tree
#  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
#  makeNumericParam("eta", lower = .1, upper = .5),
  # L2 regularization - prevents overfitting
#  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
#)

#control <- makeTuneControlRandom(maxit = 50) # random search
# evaluation of the randomly-chosen parameters is done using cross-validation
#resample_desc <- makeResampleDesc("CV", iters = 4, stratify = F)

#tuned_params <- tuneParams(
#  learner = xgb_learner,
#  task = trainTask,
#  resampling = resample_desc,
#  par.set = xgb_params,
#  control = control
#)

# Create a new model using tuned hyperparameters
#xgb_tuned_learner <- setHyperPars(
#  learner = xgb_learner,
#  par.vals = tuned_params$x
#)

#*#*#*#*#*#*#*#*#*#*#*#*#*#
#*#    Fifth  -#-  Train #*#
#*#*#*#*#*#*#*#*#*#*#*#*#*#*

# Re-train parameters using tuned hyperparameters (and full training set)
#xgb_model <- train(xgb_tuned_learner, trainTask)
#xgb_save <- list(xgb_model)
