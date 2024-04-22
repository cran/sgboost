## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  install.packages("sgboost")

## ----eval = FALSE-------------------------------------------------------------
#  remotes::install_github(
#    "FabianObster/sgboost",
#    build_vignettes = TRUE, dependencies = TRUE
#  )

## ----warning=FALSE, message=FALSE---------------------------------------------
library(mboost)
library(sgboost)
library(dplyr)
library(ggplot2)

## ----warning=FALSE,message=FALSE, eval=TRUE-----------------------------------
set.seed(10)
n <- 100
p <- 200
X <- matrix(data = rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
beta_star <- c(
  rep(5, 5), c(5, -5, 2, 0, 0), rep(-5, 5),
  c(2, -3, 8, 0, 0), rep(0, (p - 20))
)
groups <- rep(1:(p / 5), each = 5)

# Linear regression model
eps <- rnorm(n, mean = 0, sd = 1)
y <- X %*% beta_star + eps

# Logistic regression model
pr <- 1 / (1 + exp(-X %*% beta_star))
y_binary <- as.factor(rbinom(n, 1, pr))

# Input data.frames
df <- X %>%
  as.data.frame() %>%
  mutate_all(function(x) {
    as.numeric(scale(x))
  }) %>%
  mutate(y = as.numeric(y), y_binary = y_binary)
group_df <- data.frame(
  group_name = groups,
  variable_name = head(colnames(df), -2)
)

## ----eval=TRUE----------------------------------------------------------------
sgb_formula_linear <- create_formula(
  alpha = 0.4, group_df = group_df, outcome_name = "y", intercept = FALSE,
  group_name = "group_name", var_name = "variable_name",
)
sgb_formula_binary <- create_formula(
  alpha = 0.4, group_df = group_df, outcome_name = "y_binary", intercept = FALSE,
  group_name = "group_name", var_name = "variable_name",
)

## ----eval=TRUE----------------------------------------------------------------
sgb_model_linear <- mboost(
  formula = sgb_formula_linear, data = df,
  control = boost_control(nu = 1, mstop = 600)
)
# cv_sgb_model_linear <- cvrisk(sgb_model_linear,
#                               folds = cv(model.weights(sgb_model_linear),
#                                          type = 'kfold', B = 10))
sgb_model_binary <- mboost(
  formula = sgb_formula_binary, data = df, family = Binomial(),
  control = boost_control(nu = 1, mstop = 600)
)
# cv_sgb_model_binary <- cvrisk(sgb_model_binary,
#                               folds = cv(model.weights(sgb_model_linear),
#                                          type = 'kfold', B = 10))
# mstop(cv_sgb_model_linear)
# mstop(cv_sgb_model_binary)
# plot(cv_sgb_model_linear)
# plot(cv_sgb_model_binary)
sgb_model_linear <- sgb_model_linear[320]
sgb_model_binary <- sgb_model_binary[540]

## ----eval=TRUE, fig.align='center', fig.height=5, fig.width=7-----------------
get_varimp(sgb_model = sgb_model_linear)$varimp %>% slice(1:10)
get_varimp(sgb_model = sgb_model_linear)$group_importance
plot_varimp(sgb_model = sgb_model_linear, n_predictors = 15)

## ----eval=TRUE, fig.align='center', fig.height=5, fig.width=7-----------------
get_coef(sgb_model = sgb_model_linear)$aggregate %>% slice(1:10)
get_coef(sgb_model = sgb_model_linear)$raw %>% slice(1:10)

## ----eval=TRUE, fig.align='center', fig.height=5, fig.width=7-----------------
plot_effects(sgb_model = sgb_model_binary, n_predictors = 10, base_size = 10)
plot_effects(sgb_model = sgb_model_binary, n_predictors = 10, plot_type = "clock", base_size = 10)
plot_effects(sgb_model = sgb_model_binary, n_predictors = 10, plot_type = "scatter", base_size = 10)

## ----eval=TRUE, fig.align='center', fig.height=5, fig.width=7-----------------
plot_path(sgb_model = sgb_model_linear[100])
plot_path(sgb_model = sgb_model_binary[100])

