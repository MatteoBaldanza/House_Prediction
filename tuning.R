#--------
#REGRESSION-----------
#---------

#########
#LM
########L

#Folds
folds<- vfold_cv(train, v = 5,repeats = 2, strata = price)

#Lm
lm_model <- linear_reg()%>%
  set_engine("lm")


####
lm_spec <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(basic_rec_lm)

lm_rs <- fit_resamples(lm_wflow, folds,metrics = yardstick::metric_set(yardstick::mae))
#Generalized error
collect_metrics(lm_rs)

lm_fit <- lm_wflow %>% fit(data = train)

#Empirical error
pred_lm=predict(lm_fit,train %>% select(-price))
Metrics::mae(actual=train$price,pred_lm$.pred)

#Validation error
pred_lm=predict(lm_fit,test %>% select(-price))
Metrics::mae(actual=test$price,pred_lm$.pred)

#######
#LM+ITERAZIONI+spline
###############

#Folds
folds<- vfold_cv(train, v = 5,strata = price)

#Lm
lm_model <- linear_reg()%>%
  set_engine("lm")


####
lm_spec <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(splines_rec)

lm_rs <- fit_resamples(lm_wflow, folds,metrics = yardstick::metric_set(yardstick::mae))

#Generalized error
collect_metrics(lm_rs)

lm_fit <- lm_wflow %>% fit(data = train)

#Empirical error
pred_lm=predict(lm_fit,train %>% select(-price))
Metrics::mae(actual=train$price,pred_lm$.pred)

#Validation error
pred_lm=predict(lm_fit,test %>% select(-price))
Metrics::mae(actual=test$price,pred_lm$.pred)

#------

model_res <- 
  lm_fit %>% 
  extract_fit_engine() %>% 
  summary()

model_res
##################################################à

#-------Rf


# A regular grid:
mtry_r=seq(4,10,1)
min_r=seq(3,10,1)
grid <- expand_grid(mtry = mtry_r, min_n = min_r)

mae_tr=c()
mae_te=c()


tic()
for(i in 1:nrow(grid)){
  rf_model <- 
    rand_forest(trees = 200,mtry = grid[i,1], min_n = grid[i,2]) %>% 
    set_engine("ranger") %>%
    set_mode("regression")
  rf_wflow <- 
    workflow() %>% 
    add_model(rf_model) %>%
    add_recipe(basic_rec) 
  
  rf_fit <- rf_wflow %>% 
    fit(data = train)
  mae_tr[i]=ris(rf_fit,train,test)[1]
  mae_te[i]=ris(rf_fit,train,test)[2]
}
toc()


grid%>%
  mutate(mae=mae_tr,mae_t=mae_te)
view(
  grid%>%
    mutate(mae=mae_tr,mae_t=mae_te))

#9 e 4
grid%>%
  mutate(mae=mae_tr,mae_t=mae_te)%>%
  mutate(seq=seq(1,nrow(grid)),1)%>%
  ggplot()+
  geom_line(aes(y=mae,seq),col="red")+
  geom_line(aes(y=mae_t,seq),col="blue")


#######
#Random Forest
###############

#Folds
folds<- vfold_cv(train, v = 5,strata = price)

rf_model <- 
  rand_forest(trees = 1000,mtry = 9, min_n =4) %>% 
  set_engine("ranger") %>%
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(alberi_rec) 

rf_fit <- rf_wflow %>% 
  fit(data = train)

rf_rs <- fit_resamples(rf_wflow, folds,metrics = yardstick::metric_set(mae))

#Generalized error
collect_metrics(rf_rs)

rf_fit <- rf_wflow %>% fit(data = train)

#Empirical error
pred_rf=predict(rf_fit,train %>% select(-price))
Metrics::mae(actual=train$price,pred_rf$.pred)

#Validation error
pred_rf=predict(rf_fit,test %>% select(-price))
Metrics::mae(actual=test$price,pred_rf$.pred)



#######################
#----SVM
#####################


# A regular grid:
mtry_r=seq(4,5,0.5)
min_r=seq(0.25,0.35,0.05)
grid <- expand.grid(mtry = mtry_r, min_n = min_r)

mae_tr=c()
mae_te=c()


tic()
for(i in 1:nrow(grid)){
  svm_r_spec <- 
    svm_rbf(cost = grid[i,1]  ,rbf_sigma = grid[i,2]) %>% 
    set_engine("kernlab") %>% 
    set_mode("regression")
  
  svm_wflow <- 
    workflow() %>% 
    add_model(svm_r_spec) %>%
    add_recipe(normalized_rec) 
  
  svm_fit <- svm_wflow %>% 
    fit(data = train)
  
  prev= ris(svm_fit,train,test)
  mae_tr[i]=prev[1]
  mae_te[i]=prev[2]
}
toc()

#4.5  0.30


grid%>%
  mutate(mae=mae_tr,mae_t=mae_te)

grid%>%
  mutate(mae=mae_tr,mae_t=mae_te)%>%
  mutate(seq=seq(1,nrow(grid)),1)%>%
  ggplot()+
  geom_line(aes(y=mae,seq),col="red")+
  geom_line(aes(y=mae_t,seq),col="blue")


########
#GRADIENT BOOSTING
######
# A regular grid:


min_x=seq(35,38,1)
tree_x=seq(9,12,1)
learn_x=seq(0.04,0.06,0.01)

grid_xg <- expand.grid(min=min_x,tree=tree_x,learn=learn_x)

mae_tr=c()
mae_te=c()

xg_recipe <- recipe(price ~.,train) %>%
  step_mutate(bathrooms=ifelse(bathrooms%%1>0.5, round(bathrooms), floor(bathrooms)))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms==33, 3)) %>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms>=8, 8)) %>%
  step_mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)) %>%
  step_mutate(year_renovated=yr_built-year_renovated) %>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms==0,1)) %>%
  step_mutate(sqft_basement=ifelse(sqft_basement>0,1,0)) %>% 
  step_log(sqft_living,sqft_lot,nn_sqft_lot, nn_sqft_living,base = 10)%>%
  step_rm(date_sold,condition)

tic()
for(i in 1:nrow(grid)){
  i=1
  
  xgboost_spec <- 
    parsnip::boost_tree(
      mode = "regression",
      trees = 500,
      min_n = 37,
      tree_depth =   15,
      learn_rate = 0.04001710,
      loss_reduction = 2.1e-06 ) %>%
    set_engine("xgboost")%>%
    set_mode("regression")
  
  xgb_wflow <- 
    workflow() %>% 
    add_model(xgboost_spec) %>%
    add_recipe(alberi_rec)
  
  xgb_fit <- xgb_wflow %>% 
    fit(data = train)
  
  a=ris(xgb_fit,train,test)
  a
  mae_tr[i]=a[1]
  mae_te[i]=a[2]
}
toc()


grid_xg%>%
  mutate(mae=mae_tr,mae_t=mae_te)

grid_xg%>%
  mutate(mae=mae_tr,mae_t=mae_te)%>%
  mutate(seq=seq(1,nrow(grid)),1)%>%
  ggplot()+
  geom_line(aes(y=mae,seq),col="red")+
  geom_line(aes(y=mae_t,seq),col="blue")


####

#######
#XGBOOST
###############

####fine hyper

######################################
detach("package:Metrics", unload=TRUE)


preprocessing_recipe <- splines_rec %>%
  prep()

# Cross validation

train_cv_folds <- 
  recipes::bake(
    preprocessing_recipe, 
    new_data = train
  ) %>%  
  rsample::vfold_cv(train, v = 5, repeats = 2, strata=price)

# XGBoost model specification
xgboost_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 500,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")


# grid specification
xgboost_params <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )

xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 20
  )
knitr::kable(head(xgboost_grid))


xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(price ~ .)

# hyperparameter tuning
tictoc::tic()
xgboost_tuned <- tune::tune_grid(
  object = xgboost_wf,
  resamples = train_cv_folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE)
)
tictoc::toc()
#16769.4 sec elapsed

#Best hyperparameter

xgboost_tuned %>%
  tune::show_best(metric = "mae") %>%
  knitr::kable()

#Isolate
xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("mae")
knitr::kable(xgboost_best_params)

#xgboost_model_final <- xgboost_model %>% 

xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

#min_n| tree_depth| learn_rate| loss_reduction|.config               |
# |-----:|----------:|----------:|--------------:|:---------------------|
#|    37|         11|  0.0500719|        2.1e-06
###Evaliate training
# 
# | min_n| tree_depth| learn_rate| loss_reduction|.config               |
#   |-----:|----------:|----------:|--------------:|:---------------------|
#   |    27|         15|   0.094999|      0.0287997|Preprocessor1_Model25 |



# | min_n| tree_depth| learn_rate| loss_reduction|.config               |
#   |-----:|----------:|----------:|--------------:|:---------------------|
#   |    30|         13|  0.0545302|          1e-07|Preprocessor1_Model10 |


train_processed <- bake(preprocessing_recipe,  new_data = train)
train_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = price ~ ., 
    data    = train_processed
  ) %>%
  # predict the sale prices for the training data
  predict(new_data = train_processed) %>%
  bind_cols(train)

xgboost_score_train <- 
  train_prediction %>%
  yardstick::metrics(price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(xgboost_score_train)


############
test_processed  <- bake(preprocessing_recipe, new_data = test)
test_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = price ~ ., 
    data    = train_processed
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(test)
# measure the accuracy of our model using `yardstick`

xgboost_score <- 
  test_prediction %>%
  yardstick::metrics(price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(xgboost_score)

##HOUSE PREDICTION RESIDUAL

house_prediction_residual <- test_prediction %>%
  arrange(.pred) %>%
  mutate(residual_pct = (price - .pred) / .pred) %>%
  select(.pred, residual_pct)
ggplot(house_prediction_residual, aes(x = .pred, y = residual_pct)) +
  geom_point() +
  xlab("Predicted Sale Price") +
  ylab("Residual (%)") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent)

##################################

#Folds
folds<- vfold_cv(train, v = 5,strata = price)

rf_model <- 
  xgboost_spec <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 500,
    min_n = 37,
    tree_depth =   15,
    learn_rate = 0.0400719,
    loss_reduction = 2.1e-06 ) %>%
  set_engine("xgboost")%>%
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(alberi_rec) 

rf_fit <- rf_wflow %>% 
  fit(data = train)

rf_rs <- fit_resamples(rf_wflow, folds,metrics = yardstick::metric_set(mae))

#Generalized error
collect_metrics(rf_rs)

rf_fit <- rf_wflow %>% fit(data = train)

#Empirical error
pred_rf=predict(rf_fit,train %>% select(-price))
Metrics::mae(actual=train$price,pred_rf$.pred)

#Validation error
pred_rf=predict(rf_fit,test %>% select(-price))
Metrics::mae(actual=test$price,pred_rf$.pred)


a=seq(0.1,1,0.01)
b=seq(0.1,1,0.01)
grid=expand.grid(a=a,b=b)
grid$som=apply(grid,MARGIN = 1,FUN = sum)
grid=grid[which(grid$som==1.0),]

for(i in 1:nrow(grid)){
  j=pred_rf$.pred*grid[i,1]+pred_lm$.pred*grid[i,2]
  prd=Metrics::mae(test$price,j)
  print(c(prd,i))
}

#########################
#KNN-----------------------
#############################
mod= train %>% #stessa info di sqft living
  filter(sqft_living<10000)%>%
  filter(sqft_lot<8e+05)%>%
  filter(nn_sqft_living<6000)%>%
  filter(nn_sqft_lot<4e+05)%>%
  filter(10^price<5*10^6)%>%
  select(price,sqft_living,lattitude,longitude,sqft_lot,view)

mod_test= test%>%
  select(price,sqft_living,lattitude,longitude,sqft_lot,view)
colnames(train)

knn_rec= recipe(price ~ .,mod)%>%
  step_normalize(all_numeric_predictors()) %>%
  step_mutate(garden=abs(sqft_lot-sqft_living))

knn_spec <-
  nearest_neighbor(neighbors = 15) %>%
  set_engine('kknn') %>%
  set_mode('regression')

knn_wflow <- 
  workflow() %>%
  add_model(knn_spec) %>%
  add_recipe(normalized_rec)

knn_fit <- knn_wflow %>% fit(data = train)
knn_fit

pred_knn=predict(knn_fit,test %>% select(-c(price)))
knn=pred_knn$.pred
Metrics::mae(actual=test$price,knn)
