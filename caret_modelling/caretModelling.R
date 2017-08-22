## CARET Modeling similar to Scikit Implementation

library(data.table)
library(caret)
library(doParallel)


### Reading in the data
data = fread('political_demographics.csv')

### Splitting into two sets of models: One for voter preference, one for voter turnout
go_cols = 'preference'
pre_cols = 'GetOut'

go = houston[,!go_cols, with = FALSE]
pre = houston[, !pre_cols, with = FALSE]


## Train Test Split
set.seed(1234)
index_go = createDataPartition(y = data$GetOut, p = 0.9, list = FALSE)
index_pre = createDataPartition(y = data$preference, p = 0.9, list = FALSE)

train_go = go[index_go,]
test_go = go[-index_go,]

train_pre = pre[index_pre,]
test_pre = pre[-index_pre,]

## All models are 10 Fold cross validated
cntrl <- trainControl(method = "cv", number = 10,
                      classProbs = TRUE, summaryFunction = twoClassSummary)
###### GOTV MODELS ##############

# Random Forest

## parallel implementation, model with the highest AUC was selected as the best model
registerDoParallel(cores = detectCores() -1)
rf.grid <- expand.grid(.mtry= 4:12) 

ptm = proc.time()
train.rf = train(GetOut ~ .,
                 data = train_go,
                 method = "rf",
                 metric = 'ROC',
                 ntree = 1500,
                 trControl = cntrl,
                 tuneGrid = rf.grid
)
proc.time() - ptm
stopImplicitCluster()

### performance metrics
rf.test = predict(train.rf, test_go)

## Test results
rf.test = predict(train.rf, test_go)
## Accuracy
sum(diag(table(rf.test, test_go$GetOut))) / nrow(test_go)

## Plotting for Convergence
plot(train.rf$finalModel)

## Variable Importance
train.rf$finalModel$importance

# XGBoost

## parallel implementation
registerDoParallel(cores = detectCores() -1)
xgb.grid <- expand.grid(.eta = c(0.1, 0.05, 0.75), .max_depth = c(3,5),
                        .subsample = c(0.5, 1), .nrounds = c(50,200, 300),
                        .gamma = 0, .colsample_bytree = 0.5,
                        .min_child_weight = 1)

ptm = proc.time()
train.xgb = train(GetOut ~ . ,
                  data = train_go,
                  method = "xgbTree",
                  metric = 'ROC',
                  trControl = cntrl,
                  tuneGrid = xgb.grid)
proc.time() - ptm
stopImplicitCluster()

## Test results
xgb.test = predict(train.xgb, test_go)
## Accuracy
sum(diag(table(xgb.test, test_go$GetOut))) / nrow(test_go)


# Boosted Logistic Regression

registerDoParallel(cores = detectCores() -1)

lmt.grid = expand.grid(.nIter = c(50,200,500,1000))
ptm = proc.time()

train.logBoost = train(GetOut ~ . ,
                  data = train_go,
                  method = "LogitBoost",
                  metric = 'ROC',
                  trControl = cntrl,
                  tuneGrid = lmt.grid)

proc.time() - ptm
stopImplicitCluster()

## Test results
logBoost.test = predict(train.logBoost, test_go)
## Accuracy
sum(diag(table(logBoost.test, test_go$GetOut))) / nrow(test_go)


# Regularized Logistic Regression 

registerDoParallel(cores = detectCores() -1)

glmnet.grid = expand.grid(.alpha = c(0,1),
                          .lambda = c(0, 0.01,0.05,0.1,0.5,0.75))
ptm = proc.time()

train.glmnet = train(GetOut ~ . ,
                       data = train_go,
                       method = "glmnet",
                       metric = 'ROC',
                       trControl = cntrl,
                       tuneGrid = glmnet.grid)

proc.time() - ptm
stopImplicitCluster()

## Test results
glmnet.test = predict(train.glmnet, test_go)
## Accuracy
sum(diag(table(glmnet.test, test_go$GetOut))) / nrow(test_go)




###### PREFERENCE MODELS ############

# Random Forest

## parallel implementation
registerDoParallel(cores = detectCores() -1)
rf.grid <- expand.grid(.mtry= 4:12) 

ptm = proc.time()
train.rf = train(preference ~ .,
                 data = train_pre,
                 method = "rf",
                 metric = 'ROC',
                 ntree = 1500,
                 trControl = cntrl,
                 tuneGrid = rf.grid
)
proc.time() - ptm
stopImplicitCluster()

### performance metrics
rf.test = predict(train.rf, test_pre)

## Test results
rf.test = predict(train.rf, test_pre)
## Accuracy
sum(diag(table(rf.test, test_pre$preference))) / nrow(test_pre)

## Plotting for Convergence
plot(train.rf$finalModel)

## Variable Importance
train.rf$finalModel$importance

# XGBoost

## parallel implementation
registerDoParallel(cores = detectCores() -1)
xgb.grid <- expand.grid(.eta = c(0.1, 0.05, 0.75), .max_depth = c(3,5),
                        .subsample = c(0.5, 1), .nrounds = c(50,200, 300),
                        .gamma = 0, .colsample_bytree = 0.5,
                        .min_child_weight = 1)

ptm = proc.time()
train.xgb = train(preference ~ . ,
                  data = train_pre,
                  method = "xgbTree",
                  metric = 'ROC',
                  trControl = cntrl,
                  tuneGrid = xgb.grid)
proc.time() - ptm
stopImplicitCluster()

## Test results
xgb.test = predict(train.xgb, test_pre)
## Accuracy
sum(diag(table(xgb.test, test_pre$preference))) / nrow(test_pre)


# Boosted Logistic Regression

registerDoParallel(cores = detectCores() -1)

lmt.grid = expand.grid(.nIter = c(50,200,500,1000))
ptm = proc.time()

train.logBoost = train(preference ~ . ,
                       data = train_pre,
                       method = "LogitBoost",
                       metric = 'ROC',
                       trControl = cntrl,
                       tuneGrid = lmt.grid)

proc.time() - ptm
stopImplicitCluster()

## Test results
logBoost.test = predict(train.logBoost, test_pre)
## Accuracy
sum(diag(table(logBoost.test, test_pre$preference))) / nrow(test_pre)


# Regularized Logistic Regression 

registerDoParallel(cores = detectCores() -1)

glmnet.grid = expand.grid(.alpha = c(0,1),
                          .lambda = c(0, 0.01,0.05,0.1,0.5,0.75))
ptm = proc.time()

train.glmnet = train(preference ~ . ,
                     data = train_pre,
                     method = "glmnet",
                     metric = 'ROC',
                     trControl = cntrl,
                     tuneGrid = glmnet.grid)

proc.time() - ptm
stopImplicitCluster()

## Test results
glmnet.test = predict(train.glmnet, test_pre)
## Accuracy
sum(diag(table(glmnet.test, test_pre$preference))) / nrow(test_pre)