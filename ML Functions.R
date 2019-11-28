library(tidyverse)
library(dplyr)

# Linear Model
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
