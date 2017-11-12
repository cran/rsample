## ----setup, include = FALSE----------------------------------------------
options(digits = 3)

## ----ames-data, message=FALSE--------------------------------------------
library(AmesHousing)
ames <- make_ames()
names(ames)

## ----form, eval = FALSE--------------------------------------------------
#  log10(Sale_Price) ~ Neighborhood + House_Style + Year_Sold + Lot_Area

## ----build---------------------------------------------------------------
library(ggplot2)
theme_set(theme_bw())
ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram(binwidth = 5000, col = "red", fill ="red", alpha = .5)

## ----hood----------------------------------------------------------------
ggplot(ames, aes(x = Neighborhood)) + geom_bar() + coord_flip() + xlab("")

## ----rec_setup, message=FALSE, warning=FALSE-----------------------------
library(recipes)

rec <- recipe(Sale_Price ~ Neighborhood + House_Style + Year_Sold + Lot_Area, 
              data = ames) %>%
  # Log the outcome
  step_log(Sale_Price, base = 10) %>%
  # Collapse rarely occurring jobs into "other"
  step_other(Neighborhood, House_Style, threshold = 0.05) %>%
  # Dummy variables on the qualitative predictors
  step_dummy(all_nominal()) %>%
  # Unskew a predictor
  step_BoxCox(Lot_Area) %>%
  # Normalize
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
rec

## ----recipe-all----------------------------------------------------------
rec_training_set <- prep(rec, training = ames, retain = TRUE, verbose = TRUE)
rec_training_set

## ----baked---------------------------------------------------------------
# By default, the selector `everything()` is used to 
# return all the variables. Other selectors can be used too. 
bake(rec_training_set, newdata = head(ames))

## ----juiced--------------------------------------------------------------
juice(rec_training_set) %>% head

## ----boot----------------------------------------------------------------
library(rsample)
set.seed(7712)
bt_samples <- bootstraps(ames)
bt_samples
bt_samples$splits[[1]]

## ----col-pred------------------------------------------------------------
library(purrr)

bt_samples$recipes <- map(bt_samples$splits, prepper, recipe = rec, retain = TRUE, verbose = FALSE)
bt_samples
bt_samples$recipes[[1]]

## ----cols-fit------------------------------------------------------------
fit_lm <- function(rec_obj, ...) 
  lm(..., data = juice(rec_obj, everything()))

bt_samples$lm_mod <- 
  map(
    bt_samples$recipes, 
    fit_lm, 
    Sale_Price ~ .
  )
bt_samples

## ----cols-pred-----------------------------------------------------------
pred_lm <- function(split_obj, rec_obj, model_obj, ...) {
  mod_data <- bake(
    rec_obj, 
    newdata = assessment(split_obj),
    all_predictors(),
    all_outcomes()
  ) 
  
  out <- mod_data %>% select(Sale_Price)
  out$predicted <- predict(model_obj, newdata = mod_data %>% select(-Sale_Price))
  out
}

bt_samples$pred <- 
  pmap(
    lst(
      split_obj = bt_samples$splits, 
      rec_obj = bt_samples$recipes, 
      model_obj = bt_samples$lm_mod
    ),
    pred_lm 
  )
bt_samples

## ----cols-rmse-----------------------------------------------------------
rmse <- function(dat) 
  sqrt(mean((dat$Sale_Price - dat$predicted)^2))
bt_samples$RMSE <- map_dbl(bt_samples$pred, rmse)
summary(bt_samples$RMSE)

