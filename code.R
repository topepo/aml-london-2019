# ------------------------------------------------------------------------------
# Applied Machine Learning - London, 2019
# Max Kuhn (max@rstudio.com)

# ------------------------------------------------------------------------------
# Part 1

library(tidymodels)

# ------------------------------------------------------------------------------

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

# ------------------------------------------------------------------------------
# Some Example Data Manipulation Code (slide 9)

library(tidyverse)

ames_prices <- "http://bit.ly/2whgsQM" %>%
  read_delim(delim = "\t", guess_max = 2000) %>%
  rename_at(vars(contains(' ')), list(~gsub(' ', '_', .))) %>%
  dplyr::rename(Sale_Price = SalePrice) %>%
  dplyr::filter(!is.na(Electrical)) %>%
  dplyr::select(-Order, -PID, -Garage_Yr_Blt)

ames_prices %>%
  group_by(Alley) %>%
  summarize(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )

# ------------------------------------------------------------------------------
# Examples of purrr::map* (slide 10)

mini_ames <- ames_prices %>%
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>%
  dplyr::filter(!is.na(Alley))
head(mini_ames, n = 5)

by_alley <- split(mini_ames, mini_ames$Alley)
# map(.x, .f, ...)
map(by_alley, head, n = 2)

# ------------------------------------------------------------------------------
# Examples of purrr::map* (slide 11)

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)

# ------------------------------------------------------------------------------
# purrr and list-columns (slide 12)

ames_lst_col <- nest(mini_ames, data = c(Sale_Price, Yr_Sold))
ames_lst_col

ames_lst_col %>%
  mutate(
    n_row = map_int(data, nrow),
    max   = map_dbl(data, ~ max(.x$Sale_Price))
  )

# ------------------------------------------------------------------------------
# Hands-on: Quick Data Investigation (slide 13)

library(AmesHousing)
ames <- make_ames()


ggplot(ames, aes(x = Gr_Liv_Area)) + 
  geom_histogram() +
  scale_x_log10()

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram()+
  scale_x_log10()


ggplot(ames, aes(x = TotRms_AbvGrd, y = Sale_Price)) + 
  geom_point() +
  scale_x_log10()

ggplot(ames_train, aes(x = Sale_Price)) + 
  geom_line(stat = "density", trim = TRUE)+ 
  geom_line(data = ames_test, stat = "density", trim = TRUE, col = "red")+
  scale_x_log10()


# ------------------------------------------------------------------------------
# Part 2: Data Splitting, Models, and Performance

# ------------------------------------------------------------------------------
# Loading (slide 3)

library(tidymodels)
library(AmesHousing)

# ------------------------------------------------------------------------------
# Ames Housing Data (slide 6)

ames <- 
  make_ames() %>% 
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")
ames_train <- training(data_split)
ames_test  <- testing(data_split)
nrow(ames_train)/nrow(ames)

# ------------------------------------------------------------------------------
# Ames Housing Data (slide 7)

data_split
training(data_split)

# ------------------------------------------------------------------------------
# A Linear Regression Model (slide 12)

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

# ------------------------------------------------------------------------------
# parsnip in Action (slide 15)

spec_lin_reg <- linear_reg()
spec_lin_reg

spec_lm <- set_engine(spec_lin_reg, "lm")
spec_lm

fit_lm <- fit(
  spec_lm,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)
fit_lm

# ------------------------------------------------------------------------------
# parsnip in Action (slide 16)

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))
fit_xy(
  spec_lm,
  y = ames_train_log$Sale_Price_Log,
  x = ames_train_log %>% dplyr::select(Latitude, Longitude)
)

# ------------------------------------------------------------------------------
# Alternative Engines (slide 17)

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

coef(fit_stan$fit)

coef(fit_lm$fit)

# ------------------------------------------------------------------------------
# Different models (slide 18)

fit_knn <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn") %>% 
  fit(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
fit_knn

# ------------------------------------------------------------------------------
# Predictions (slide 20)

# Numeric predictions always in a df
# with column `.pred`
test_pred <- 
  fit_lm %>%
  predict(ames_test) %>%
  bind_cols(ames_test) %>%
  mutate(log_price = log10(Sale_Price))

test_pred %>% 
  dplyr::select(log_price, .pred) %>% 
  slice(1:3)

# ------------------------------------------------------------------------------
# Estimating Performance (slide 21)

# yardstick loaded by tidymodels
perf_metrics <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test_pred  %>% 
  perf_metrics(truth = log_price, estimate = .pred)

# ------------------------------------------------------------------------------
# Part 3: Feature Engineering

# ------------------------------------------------------------------------------
# Reipes (slide 13)

mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10)

# ------------------------------------------------------------------------------
# Recipes and Categorical Predictors (slide 14)

mod_rec <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  step_log(Sale_Price, base = 10) %>%
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

mod_rec

# ------------------------------------------------------------------------------
# Preparing the Recipe (slide 16)

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)

# ------------------------------------------------------------------------------
# Preparing the Recipe (slide 17)

mod_rec_trained

# ------------------------------------------------------------------------------
# Getting the Values (slide 18)

ames_test_dummies <- bake(mod_rec_trained, new_data = ames_test)
names(ames_test_dummies)

# ------------------------------------------------------------------------------
# Hands-On: Zero-Variance Filter (slide 21)

# Instead of using step_other(), take 10 minutes and research how to eliminate
# any zero-variance predictors using the recipe reference site.

# Re-run the recipe with this step.

# What were the results?
  
# Do you prefer either of these approaches to the other?


# Stuff that you need: 

library(tidymodels)
library(AmesHousing)

ames <- 
  make_ames() %>% 
  dplyr::select(-matches("Qu"))
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")
ames_train <- training(data_split)
ames_test  <- testing(data_split)

mod_rec <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  step_log(Sale_Price, base = 10) %>%
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05, id = "other") %>%
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())




mod_rec_zv <- 
  recipe(
    Sale_Price ~ Longitude:Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  
  step_log(Sale_Price, base = 10)%>% 
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>% 
  prep()


starts_with("Central_Air"):Year_Built:Lot_Area

# ------------------------------------------------------------------------------
# Interactions (slide 23)

price_breaks <- (1:6)*(10^5)
ggplot(ames_train, aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  geom_smooth(method = "loess")

# ------------------------------------------------------------------------------
# Interactions (slide 24)

library(MASS) # to get robust linear regression model
ggplot(ames_train, aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm")

# ------------------------------------------------------------------------------
# Interactions (slide 25)

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air, data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air,
           data = ames_train)
anova(mod1, mod2)

# ------------------------------------------------------------------------------
# Interactions in Recipes (slide 26)

recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)

# ------------------------------------------------------------------------------
# Code to make bivariate data

load("RData/bivariate_data.RData")
library(ggthemes)

# ------------------------------------------------------------------------------
# More Recipe Steps (slide 30)

bivariate_rec <- recipe(Class ~ ., data = bivariate_data_train) %>%
  step_BoxCox(all_predictors())
bivariate_rec <- prep(bivariate_rec, training = bivariate_data_train, verbose = FALSE)
inverse_test_data <- bake(bivariate_rec, new_data = bivariate_data_test)

# ------------------------------------------------------------------------------
# Back to the Bivariate Example - Transformed Data (slide 34)

ggplot(inverse_test_data, 
       aes(x = 1/PredictorA, 
           y = 1/PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlab("1/A") + ylab("1/B") 

# ------------------------------------------------------------------------------
# Back to the Bivariate Example - Recipes (slide 35)

bivariate_pca <- 
  recipe(Class ~ PredictorA + PredictorB, data = bivariate_data_train) %>%
  step_BoxCox(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors()) %>%
  prep(training = bivariate_data_test, verbose = FALSE)

pca_test <- bake(bivariate_pca, new_data = bivariate_data_test)

# Put components axes on the same range
pca_rng <- extendrange(c(pca_test$PC1, pca_test$PC2))

ggplot(pca_test, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = .2, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlim(pca_rng) + ylim(pca_rng) + 
  xlab("Principal Component 1") + ylab("Principal Component 2")

# ------------------------------------------------------------------------------
# Back to the Bivariate Example - Recipes (slide 36)

ggplot(pca_test, 
       aes(x = PC1, 
           y = PC2,
           color = Class)) +
  geom_point(alpha = .2, cex = 1.5) + 
  theme(legend.position = "top") +
  scale_colour_calc() +
  xlim(pca_rng) + ylim(pca_rng) + 
  xlab("Principal Component 1") + 
  ylab("Principal Component 2") 

# ------------------------------------------------------------------------------
# Longitude (slide 38)

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# ------------------------------------------------------------------------------
# Longitude (slide 39)

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::ns(x, df = 5), 
    se = FALSE
  ) + 
  scale_y_log10()

# ------------------------------------------------------------------------------
# Linear models again (slide 40)

ames_rec <- 
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_ns(Longitude, Latitude, deg_free = 5)

# ------------------------------------------------------------------------------
# Combining the Recipe with a Model (slide 41)

ames_rec <- prep(ames_rec)
fit_lm <- 
  spec_lm %>% 
  fit(Sale_Price ~ ., data = juice(ames_rec))   # The recipe puts Sale_Price on the log scale
glance(fit_lm$fit)

holdout_data <- bake(ames_rec, ames_test, all_predictors())

# but let's not do this
# predict(fit_lm, new_data = holdout_data)

# ------------------------------------------------------------------------------
# Part 4: Resampling and Grid Search

# ------------------------------------------------------------------------------
# Cross-Validating Using rsample (slide 10)

cv_splits <- vfold_cv(ames_train) #10-fold is default
cv_splits

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% analysis() %>% dim()

cv_splits$splits[[1]] %>% assessment() %>% dim()

# ------------------------------------------------------------------------------
# Resampling a 5-NN model (slide 13)

five_nn <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

five_nn %>% fit(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

# ------------------------------------------------------------------------------
# Resampling a 5-NN model (slide 14)

cv_splits_knn <-
  cv_splits %>%
  mutate(
    fits = 
      map(
        splits,
        ~ five_nn %>% fit(log10(Sale_Price) ~ Longitude + Latitude, data = analysis(.x))    
      )
  ) 
cv_splits_knn %>% slice(1:3)

# ------------------------------------------------------------------------------
# Predictions for each model (slides 15-22)

knn_pred <-                                              
  map2_dfr(cv_splits_knn$fits, cv_splits_knn$splits,     
           ~ predict(.x, assessment(.y)),                
           .id = "fold")                                 
prices <-  
  map_dfr(cv_splits_knn$splits,  
          ~ assessment(.x) %>% dplyr::select(Sale_Price)) %>%  
  mutate(Sale_Price = log10(Sale_Price))

rmse_estimates <- 
  knn_pred %>%  
  bind_cols(prices) %>% 
  group_by(fold) %>% 
  do(rmse = rmse(., Sale_Price, .pred)) %>% 
  unnest(cols = c(rmse)) 

mean(rmse_estimates$.estimate)

# ------------------------------------------------------------------------------
# Easy resampling using the {tune} package (slide 26)

library(tune)
easy_eval <- 
  fit_resamples(
    log10(Sale_Price) ~ Longitude + Latitude,
    five_nn,
    resamples = cv_splits,
    control = control_resamples(save_pred = TRUE)
  )

collect_predictions(easy_eval) %>% 
  mutate(Sale_Price = 10^Sale_Price, .pred = 10^.pred) %>% 
  group_by(id) %>% do(rmse(., Sale_Price, .pred)) %>% 
  pull(.estimate) %>%
  mean()

easy_eval_extract <- 
  fit_resamples(
    log10(Sale_Price) ~ Longitude + Latitude,
    five_nn,
    resamples = cv_splits,
    control = control_resamples(save_pred = TRUE, extract = function(x) x$model)
  )

purrr::map(easy_eval_extract$.extracts, ~ .x$.extracts[[1]])

# ------------------------------------------------------------------------------
# Getting the statistics and predictions (slide 27)

collect_predictions(easy_eval) %>% 
  arrange(.row) %>% 
  slice(1:5)

collect_metrics(easy_eval)

collect_metrics(easy_eval, summarize = FALSE) %>% 
  slice(1:10)

# ------------------------------------------------------------------------------
# Making Regular Grids (slide 35)

penalty()

mixture()

glmn_param <- parameters(penalty(), mixture())

glmn_grid <- grid_regular(glmn_param, levels = c(10, 5))
glmn_grid %>% slice(1:4)

# ------------------------------------------------------------------------------
# Non-Regular Grids (slide 36)

set.seed(7454)
glmn_sfd <- grid_max_entropy(glmn_param, size = 50)
glmn_sfd %>% slice(1:4)

# ------------------------------------------------------------------------------
# Modifying Parameter Sets (slide 37)

# The names can be changed:
glmn_set <- parameters(lambda = penalty(), mixture())

# The ranges can also be set by their name:
glmn_set <-  update(glmn_set, lambda = penalty(c(-5, -1)))

# Some parameters depend on data dimensions:
mtry()

rf_set <- parameters(mtry(), trees())

rf_set

# Sets the range of mtry to be the number of predictors
finalize(rf_set, mtcars %>% dplyr::select(-mpg))

# How to find which parameters names in parsnip map to which model values:

boost_tree(learn_rate = .1, loss_reduction = 4) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression") %>% 
  translate()

# ------------------------------------------------------------------------------
# Hands-On: K-NN Grids (slide 38)

# Looking at the help file ?nearest_neighbors and find the names of the three
# tuning parameters.

# Create a parameter set for these three, make at least one grid, and plot them.

library(tidymodels)

knn_param <- parameters(neighbors(), weight_func(), dist_power())

knn_param %>% 
  grid_regular(levels = c(5, 4, 5)) %>% 
  ggplot(aes(x = neighbors, y = dist_power, col = weight_func)) + 
  geom_point() + 
  facet_wrap(~weight_func)


knn_param <-
  parameters(
    neighbors(c(1, 100)), 
    weight_func(values = c("rectangular", "triangular")), 
    dist_power(c(1, 3))
  )

set.seed(9060)
knn_grid <- 
  parameters(neighbors(), weight_func(), dist_power()) %>% 
  update(neighbors = neighbors(c(1, 500))) %>% 
  grid_max_entropy(size = 300)


recipe(Sale_Price ~ ., data = ames_train) %>% 
  step_ns(Latitude,  deg_free = tune("lat df")) %>% 
  step_ns(Longitude, deg_free = tune("#$^(@#$(")) %>% 
  parameters() %>% 
  grid_max_entropy(size = 4)


# ------------------------------------------------------------------------------
# Tagging Tuning parameterss (slide 40)

library(tune)
knn_mod <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

parameters(knn_mod)

# ------------------------------------------------------------------------------
# Tagging Tuning parameterss (slide 41)

nearest_neighbor(neighbors = tune("K"), weight_func = tune("weights")) %>% 
  set_engine("kknn") %>% 
  set_mode("regression") %>% 
  parameters()

# ------------------------------------------------------------------------------
# Grid Search (slide 42)

set.seed(522)
knn_reg <- knn_mod %>% parameters() %>% grid_regular(levels = c(15, 5))
ctrl <- control_grid(verbose = TRUE)

knn_reg_search <-
  tune_grid(
    ames_rec,
    model = knn_mod,
    resamples = cv_splits,
    grid = knn_reg,
    control = ctrl
  )

# ------------------------------------------------------------------------------
# The Results (slide 44)

knn_reg_search

knn_reg_search$.metrics[[1]]

# ------------------------------------------------------------------------------
# Resampled Performance Estimates (slide 45)

knn_perf <- collect_metrics(knn_reg_search)
knn_perf %>% slice(1:10)

# ------------------------------------------------------------------------------
# Resampled Performance Estimates (slide 46)

knn_perf %>% 
  dplyr::filter(.metric == "rmse") %>% 
  ggplot(aes(x = neighbors, y = mean, col = weight_func)) + 
  geom_point() + 
  geom_line()

# ------------------------------------------------------------------------------
# Zoomed (slide 47)

knn_perf %>% 
  dplyr::filter(.metric == "rmse") %>% 
  ggplot(aes(x = neighbors, y = mean, col = weight_func)) + 
  geom_point() + 
  geom_line() + 
  ylim(c(0.08, 0.10))

# ------------------------------------------------------------------------------
# Finalizing the objects (slide 48)

best_knn <- select_best(knn_reg_search, metric = "rmse", maximize = FALSE)
best_knn

final_ames_rec <- 
  ames_rec %>% 
  prep()

final_mars_mod <- 
  knn_mod %>%
  finalize_model(best_knn) %>% 
  # Recall that sale price has been transformed by the recipe. 
  fit(Sale_Price ~ ., data = juice(final_ames_rec))

show_best(knn_reg_search, metric = "rmse", maximize = FALSE)

# ------------------------------------------------------------------------------
# Part 5: Regression Models

library(tidymodels)
library(tune)

# ------------------------------------------------------------------------------
# Hands-On: Explore the Data (slide 6)

data("Chicago")


library(lubridate)

Chicago %>% 
  mutate(dow = wday(date, label = TRUE)) %>% 
  ggplot(aes(x = date, y = ridership, col = dow)) + 
  geom_point(alpha = .3)

Chicago %>% 
  ggplot(aes(x = Clark_Lake, y = ridership)) + 
  geom_point(alpha = .3)



# ------------------------------------------------------------------------------
# A Recipe (slides 8-15)

library(stringr)

# define a few holidays
us_hol <- 
  timeDate::listHolidays() %>% 
  str_subset("(^US)|(Easter)")

chi_rec <-
  recipe(ridership ~ ., data = Chicago) %>%
  step_holiday(date, holidays = us_hol) %>%
  step_date(date) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())
# step_normalize(one_of(!!stations))
# step_pca(one_of(!!stations), num_comp = tune())

# ------------------------------------------------------------------------------
# Resampling (slide 16)

data_folds <-
  rolling_origin(
    Chicago,
    initial = 364 * 15,
    assess = 7 * 4,
    skip = 7 * 4,
    cumulative = FALSE
  )
data_folds %>% nrow()

tidy(data_folds) %>% 
  ggplot(aes(x = Row, y = Resample, fill = Data)) + 
  geom_tile() 

# ------------------------------------------------------------------------------
# Tuning the Model (slide 24)

glmn_grid <- 
  expand.grid(penalty = 10^seq(-3, -1, length = 20), mixture = (0:5)/5)

# ------------------------------------------------------------------------------
# Tuning the Model (slide 25)

# We need to normalize the predictors:
glmn_rec <- chi_rec %>% step_normalize(all_predictors())

glmn_mod <-
  linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")

# Save the assessment set predictions
ctrl <- control_grid(save_pred = TRUE)

glmn_res <-
  tune_grid(
    glmn_rec,
    model = glmn_mod,
    resamples = data_folds,
    grid = glmn_grid,
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Plotting the Resampling Profile (slide 28)

rmse_vals <- collect_metrics(glmn_res) %>% filter(.metric == "rmse")
rmse_vals %>% 
  mutate(mixture = format(mixture)) %>% 
  ggplot(aes(x = penalty, y = mean, col = mixture)) + 
  geom_line() + 
  geom_point() + 
  scale_x_log10()

# ------------------------------------------------------------------------------
# The numerically best results (slide 29)

show_best(glmn_res, metric = "rmse", maximize = FALSE)

best_glmn <- select_best(glmn_res, metric = "rmse", maximize = FALSE)
best_glmn

# ------------------------------------------------------------------------------
# Residual Analysis (slide 30)

lr_pred <- collect_predictions(glmn_res)
lr_pred %>% slice(1:10)

# ------------------------------------------------------------------------------
# Observed Versus Predicted Plot (slide 31)

# Keep the best model
lr_pred <- 
  lr_pred %>% 
  inner_join(best_glmn, by = c("penalty", "mixture")) 

ggplot(lr_pred, aes(x = .pred, y = ridership)) + 
  geom_abline(col = "green") + 
  geom_point(alpha = .3) + 
  coord_equal()

# ------------------------------------------------------------------------------
# Which training set points had the worst results? (slide 32)

large_resid <- 
  lr_pred %>% 
  mutate(resid = ridership - .pred) %>% 
  arrange(desc(abs(resid))) %>% 
  slice(1:4)

library(lubridate)

Chicago %>% 
  slice(large_resid$.row) %>% 
  mutate(day = wday(date, label = TRUE)) %>% 
  bind_cols(large_resid) %>% 
  dplyr::select(date, day, ridership, .pred, resid)

# ------------------------------------------------------------------------------
# Creating a Final Model (slide 34)

glmn_rec_final <- prep(glmn_rec)
glmn_mod_final <- finalize_model(glmn_mod, best_glmn)
glmn_mod_final

glmn_fit <- 
  glmn_mod_final %>% 
  fit(ridership ~ ., data = juice(glmn_rec_final))
glmn_fit

# ------------------------------------------------------------------------------
# Using the glmnet Object (slide 35)

library(glmnet)
plot(glmn_fit$fit, xvar = "lambda")

# ------------------------------------------------------------------------------
# A glmnet Coefficient Plot (slide 36)

library(ggrepel)

# Get the set of coefficients across penalty values
tidy_coefs <- 
  broom::tidy(glmn_fit) %>%
  dplyr::filter(term != "(Intercept)") %>% 
  dplyr::select(-step, -dev.ratio)

# Get the lambda closest to tune's optimal choice 
delta <- abs(tidy_coefs$lambda - best_glmn$penalty)
lambda_opt <- tidy_coefs$lambda[which.min(delta)]

# Keep the large values
label_coefs <- 
  tidy_coefs %>%
  mutate(abs_estimate = abs(estimate)) %>% 
  dplyr::filter(abs_estimate >= 1.1) %>% 
  distinct(term) %>% 
  inner_join(tidy_coefs, by = "term") %>% 
  dplyr::filter(lambda == lambda_opt)

# plot the paths and highlight the large values
tidy_coefs %>%
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) + 
  geom_vline(xintercept = lambda_opt, lty = 3) +
  geom_line(alpha = .4) + 
  theme(legend.position = "none") + 
  scale_x_log10() + 
  geom_text_repel(data = label_coefs, aes(x = .005))

# ------------------------------------------------------------------------------
# glmnet Variable Importance (slide 38)

library(vip)
vip(glmn_fit, num_features = 20L, 
    # Needs to know which coefficients to use
    lambda = best_glmn$penalty)

# ------------------------------------------------------------------------------
# MARS in via {parsnip} and {tune} (slide 51)

# Let MARS decide the number of terms but tune the term dimensions
mars_mod <-  mars(prod_degree = tune())

# We'll decide via search:
mars_mod <-  
  mars(
    num_terms = tune("mars terms"),
    prod_degree = tune(),
    prune_method = "none"
  ) %>%
  set_engine("earth") %>%
  set_mode("regression")


mars_rec <- 
  chi_rec %>% 
  step_normalize(one_of(!!stations)) %>% 
  step_pca(one_of(!!stations), num_comp = tune("pca comps"))


# ------------------------------------------------------------------------------
# Parameter Ranges (slide 67)

chi_wflow <-
  workflow() %>%
  add_recipe(mars_rec) %>%
  add_model(mars_mod)

chi_set <-
  parameters(chi_wflow) %>%
  update(
    `pca comps`  =  num_comp(c(0, 20)), # 0 comps => PCA is not used 
    `mars terms` = num_terms(c(2, 100)))

# ------------------------------------------------------------------------------
# Running the Optimization (slide 68)

library(doMC)
registerDoMC(cores = 8)

ctrl <- control_bayes(verbose = TRUE, save_pred = TRUE)
# Some defaults:
#   - Uses expected improvement with no trade-off. See ?exp_improve().
#   - RMSE is minimized

set.seed(7891)
mars_search <-
  tune_bayes(
    chi_wflow,
    resamples = data_folds,
    iter = 25,
    param_info = chi_set,
    metrics = metric_set(rmse),
    initial = 4,
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Performance over iterations (slide 70)

autoplot(mars_search, type = "performance")

# ------------------------------------------------------------------------------
# Performance versus parameters (slide 71)

autoplot(mars_search, type = "marginals")

# ------------------------------------------------------------------------------
# Parameters over iterations (slide 72)

autoplot(mars_search, type = "parameters")

# ------------------------------------------------------------------------------
# Results (slide 73)

show_best(mars_search, maximize = FALSE)

# ------------------------------------------------------------------------------
# Assessment Set Results (Again) (slide 77)

mars_pred <- 
  mars_search %>% 
  collect_predictions() %>% 
  inner_join(
    select_best(mars_search, maximize = FALSE), 
    by = c("mars terms", "prod_degree", "pca comps")
  ) 

ggplot(mars_pred, aes(x = .pred, y = ridership)) + 
  geom_abline(col = "green") + 
  geom_point(alpha = .3) + 
  coord_equal()

# ------------------------------------------------------------------------------
# Finalizing the recipe and model (slide 78)

best_mars <- select_best(mars_search, maximize = FALSE)
best_mars

final_mars_rec <- 
  mars_rec %>% 
  finalize_recipe(best_mars) %>% 
  prep()

final_mars_mod <- 
  mars_mod %>%
  finalize_model(best_mars) %>% 
  fit(ridership ~ ., data = juice(final_mars_rec))

# ------------------------------------------------------------------------------
# Variable importance (slide 79)

library(vip)
vip(final_mars_mod, num_features = 20L, type = "gcv")

# ------------------------------------------------------------------------------
# Part 6: Classification

# ------------------------------------------------------------------------------
# Illustrative Example (slide 5)

two_class_example %>% head(4)

# ------------------------------------------------------------------------------
# Class Prediction Metrics (slide 6)

two_class_example %>% 
  conf_mat(truth = truth, estimate = predicted)

two_class_example %>% 
  accuracy(truth = truth, estimate = predicted)

# ------------------------------------------------------------------------------
# The Receiver Operating Characteristic (ROC) Curve (slide 10)

roc_obj <- 
  two_class_example %>% 
  roc_curve(truth, Class1)

two_class_example %>% roc_auc(truth, Class1)

autoplot(roc_obj) + thm

# ------------------------------------------------------------------------------
# Feature Engineering (slides 16-25)
library(textrecipes)

count_to_binary <- function(x) {
  ifelse(x != 0, "present", "absent")
}

text_rec <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%               
  step_rename_at(                                 
    starts_with("textfeature_"),                  
    fn = ~ gsub("textfeature_review_raw_", "", .)
  )  %>%
  step_tokenize(review) %>%
  step_stopwords(review)  %>%
  step_stem(review) %>%
  step_texthash(review, signed = FALSE, num_terms = 1024) %>%
  step_rename_at(       
    starts_with("review_hash"),  
    fn = ~ gsub("review_", "", .)
  ) %>% 
  step_num2factor(                                                           
    starts_with("hash"),                                                     
    transform = count_to_binary                                              
  ) %>% 
  step_zv(all_predictors())

# ------------------------------------------------------------------------------
# Resampling and Analysis Strategy (slide 26)

set.seed(8935)
folds <- vfold_cv(training_data, strata = "score")

# ------------------------------------------------------------------------------
# {recipe} and {parsnip} objects (slide 34)

basics <- names(textfeatures:::count_functions)

tree_rec <-
  recipe(score ~ product + review, data = training_data) %>%
  update_role(product, new_role = "id") %>%
  step_mutate(review_raw = review) %>%
  step_textfeature(review_raw) %>%
  step_rename_at(
    starts_with("textfeature_"),
    fn = ~ gsub("textfeature_review_raw_", "", .)
  ) %>%
  step_tokenize(review)  %>%
  step_stopwords(review) %>%
  step_stem(review) %>%
  step_texthash(review, signed = FALSE, num_terms = tune()) %>%
  step_rename_at(starts_with("review_hash"), fn = ~ gsub("review_", "", .))

# and 
cart_mod <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

ctrl <- control_grid(save_pred = TRUE)

# ------------------------------------------------------------------------------
# Model tuning (slide 35)

set.seed(2553)
cart_tune <-
  tune_grid(
    tree_rec,
    cart_mod,
    folds,
    grid = 10,
    metric = metric_set(roc_auc),
    control = ctrl
  )
show_best(cart_tune)

# ------------------------------------------------------------------------------
# Parameter profiles (slide 36)

autoplot(cart_tune)

# ------------------------------------------------------------------------------
# Plotting ROC curves (slide 37)

cart_pred <- collect_predictions(cart_tune)
cart_pred %>% slice(1:5)

cart_pred %>% 
  inner_join(select_best(cart_tune)) %>% 
  group_by(id) %>% 
  roc_curve(score, .pred_great) %>% 
  autoplot()

# ------------------------------------------------------------------------------
# A single (but approximate) ROC curve (slide 38)

auc_curve_data <- function(x) {
  collect_predictions(x) %>% 
    inner_join(select_best(x, "roc_auc")) %>% 
    roc_curve(score, .pred_great)
}
approx_roc_curves <- function(...) {
  curves <- map_dfr(list(...), auc_curve_data, .id = "model")
  default_cut <- 
    curves %>% 
    group_by(model) %>% 
    arrange(abs(.threshold - .5)) %>% 
    slice(1)
  ggplot(curves) +
    aes(y = sensitivity, x = 1 - specificity, col = model) +
    geom_abline(lty = 3) + 
    geom_step(direction = "vh") + 
    geom_point(data = default_cut) + 
    coord_equal()
}

# Use named arguments for better labels
approx_roc_curves(CART = cart_tune)

# ------------------------------------------------------------------------------
# Hands-On: Down-Sampling (slide 39)

# Looking at the ROC curve, the default cutoff may not be optimal if FP and FN
# errors are about equal.

# We could pick a better cutoff or fit another model using sub-class sampling.

# The latter approach would balance the data prior to model fitting.
 
# The most common method would be to down-sample the data.
 
# This is fairly controversial (at least in statistical circles).
 
# Let's take 20m and refit the model code above with a recipe that includes
# downsampling.

# ------------------------------------------------------------------------------
# C5.0 (slide 50)

C5_mod <- 
  boost_tree(trees = tune(), min_n = tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

# We will just modify our CART grid and add 
# a new parameter: 
set.seed(5793)

C5_grid <- 
  collect_metrics(cart_tune) %>% 
  splyr::select(min_n, num_terms) %>% 
  mutate(trees = sample(1:100, 10))

C5_tune <-
  tune_grid(
    tree_rec,
    C5_mod,
    folds,
    grid = C5_grid,
    metric = metric_set(roc_auc),
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Comparing models (slide 51)

approx_roc_curves(CART = cart_tune, C5 = C5_tune)

show_best(C5_tune)

autoplot(C5_tune)

# ------------------------------------------------------------------------------
# Finalizing the recipe and model (slide 52)

best_C5 <- select_best(C5_tune)
best_C5

final_tree_rec <- 
  tree_rec %>% 
  finalize_recipe(best_C5) %>% 
  prep()

final_C5_mod <- 
  C5_mod %>%
  finalize_model(best_C5) %>% 
  fit(score ~ ., data = juice(final_tree_rec, -product))

library(C50) # to get the print method
final_C5_mod

# ------------------------------------------------------------------------------
# Predicting the test set (slide 53)

test_features <- 
  bake(final_tree_rec, testing_data, -product, -score)
test_probs <- 
  predict(final_C5_mod, test_features, type = "prob") %>% 
  bind_cols(testing_data %>% splyr::select(score)) %>% 
  bind_cols(predict(final_C5_mod, test_features))

roc_auc(test_probs, score, .pred_great)

conf_mat(test_probs, score, .pred_class)

roc_values <- roc_curve(test_probs, score, .pred_great)
autoplot(roc_values)


# ------------------------------------------------------------------------------
# Extra Slides (as time allows)

# ------------------------------------------------------------------------------
# Naive Bayes recipe and fit (slide 66)

count_to_binary <- function(x) {
  ifelse(x != 0, "present", "absent")
}

nb_rec <- 
  tree_rec %>% 
  step_num2factor(starts_with("hash"), transform = count_to_binary) 

library(discrim)

nb_mod <- naive_Bayes() %>% set_engine("klaR")

nb_tune <-
  tune_grid(
    nb_rec,
    nb_mod,
    folds,
    grid = tibble(num_terms = floor(2^seq(8, 12, by = 0.5))),
    metric = metric_set(roc_auc),
    control = ctrl
  )

# ------------------------------------------------------------------------------
# Naive Bayes results (slide 67)

autoplot(nb_tune) +
  scale_x_continuous(trans = log2_trans())

approx_roc_curves(CART = cart_tune, C5 = C5_tune, "Naive Bayes" = nb_tune)
