library(tidyverse)
library(dplyr)

# Generalized Linear Model without Regularization
Generalize.Linear.Model = function(data, response, familiy, predictors, subset){
  formula = Get.Formula(response, predictors)
  glm(formula, family = familiy, data = data, subset = subset)
}

## Transformation Helper
Transformation = function(var, power){
  ifelse(power == 0, log(var), var^power)
}

## Formula Helper
Get.Formula = function(response, predictors){
  response %>% paste("~") %>% 
    paste(paste(predictors, collapse = "+")) %>% 
    as.formula()
}

# Cross Validation
GLM.Cross.Validation = function(fold, data, response, family, predictors){
  n = nrow(data)
  set.seed(1102)
  shuffle.data = data[sample(1:n),]
  fold.size = map_int(1:fold, ~ifelse(.x <= n%%fold, floor(n/fold) + 1, floor(n/fold)))
  fold.index = map2_int(1:fold, 1:fold.size, ~rep(.x, .y))
  index.data = mutate(shuffle.data, fold = fold.index)
  fold.glm = map(1:fold, ~Generalize.Linear.Model(index.data, response, 
                                                      familiy, predictors, subset = fold != .x))
  fold.test.data = map(1:fold, ~subset(index.data, fold == .x))
  fold.mse = map_dbl(fold.glm, fold.test.data, 
                     ~(predict(.x, select(.y, -response), type = "response") - .y[[response]])^2)
  cv.error = mean(fold.mse)
  cv.error
}

# Elastic Net (Lasso and Ridge included)
library(glmnet)
Best.glmnet = function(family, data, response, fold, test){
  y = data[[response]]
  x = as.matrix(select(data, -response))
  y.test = test[[response]]
  x.test = as.matrix(select(test, -response))
  if (family == "binomial"){
    models = map(seq(0, 1, 0.1), ~cv.glmnet(x, y, type.measure = "class", alpha = .x, family = family, nfolds = fold))
  }
  else{
    models = map(seq(0, 1, 0.1), ~cv.glmnet(x, y, type.measure = "mse", alpha = .x, family = family, nfolds = fold))
  }
  lambdas = map_dbl(models, "lambda.1se")
  cvms = map2_dbl(models, lambdas, ~.x[["cvm"]][which(.x[["lambda"]] == .y)])
  best.glmnet = models[[which.min(cvms)]]
  alpha = (which.min(cvms) - 1)/10
  lambda = lambdas[which.min(cvms)]
  if (!is.null(test)){
    prediction = predict(best.glmnet, s=model$lambda.1se, x.test)
    list(alpha, lambda,
         coef = coef.glmnet(glmnet(x, y, family, alpha, lambda = seq(lambda, lambda - 1.5, -0.1)))[,1], 
         prediction)
  }
  else{
    list(alpha, lambda,
         coef = coef.glmnet(glmnet(x, y, family, alpha, lambda = seq(lambda, lambda - 1.5, -0.1)))[,1])
  }
}

# Tree models
