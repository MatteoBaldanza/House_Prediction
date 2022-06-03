##INFORMAZIONI UTILE:

#VIENE RIPERCORSO FORMALMENTE IL PROCEDIMENTO PER L'OTTENIMENTO DELLA PREVISIONE

#SI PUO ESEGUIRE TUTTO IL CODICE INSIEME SENZA VISIONARLO OTTENENDO LE STIME IN: 220.18 secondi

#LA FASE DI TUNING E DI ANALISI ESPLORATIVA NON SONO STATE RIPORTATE


#-----------
#LIBRERIE E FUNZIONI
#---------------

library(tidymodels)
library(readr)
library(tictoc)
library(baguette)
library(ggplot2)

gg_two_plus_wrap=function(dati,x,y,fc_w){  #funzione per grafico
  ggplot(dati) +
    geom_point(aes(x=x, y=(y),col=fc_w),pch=19,show.legend = F) + 
    geom_smooth(method = lm, formula = y ~ x,aes(x=x, y=(y)),col= "red")+
    facet_wrap(fc_w)+
    theme_classic()
}

#Funzione da me creata per restiuirmi in base al modello: MAE train, MAE test e previsioni!

metric_collect=function(wk_set,dati_train,dati_test){
  wk_set=all_workflows;dati_train=train;dati_test=test
  name_mod=wk_set$wflow_id
  frm=matrix(nrow=length(name_mod),ncol=5);prev=matrix(nrow=nrow(dati_test),ncol=length(name_mod)+1)
  colnames(frm)=c("nome_modello","mae_train","mae_test","rmse_train","rmse_test");colnames(prev)=c("test",name_mod)
  prev[,1]=dati_test$price
  pb = txtProgressBar(min = 0, max = length(name_mod), initial = 0,style = 3) 
  for(i in 1:length(name_mod)){
    wf=extract_workflow(wk_set, id = name_mod[i])
    fit_mod=wf %>%
      fit(dati_train)
    previsioni_train=predict(fit_mod,train %>% select(-price))$.pred
    previsioni_test=predict(fit_mod,test %>% select(-price))$.pred
    mae_train=Metrics::mae(dati_train$price,previsioni_train)
    mae_test=Metrics::mae(dati_test$price,previsioni_test)
    rmse_train=Metrics::rmse(dati_train$price,previsioni_train)
    rmse_test=Metrics::rmse(dati_test$price,previsioni_test)
    frm[i,]=c(name_mod[i],mae_train,mae_test,rmse_train,rmse_test)
    prev[,i+1]=previsioni_test
    setTxtProgressBar(pb,i)
  }
  close(pb)
  lista=list();lista[[1]]=frm;lista[[2]]=prev
  return(lista)
}


#------------------------
#IMPORTAZIONE DATI
#-------------------------
tidymodels_prefer()
PATH <- "https://raw.githubusercontent.com/aldosolari/DM/master/docs/HomePrices/"
train = read_csv2(paste0(PATH,"home_prices_train.csv"))
test = read_csv2(paste0(PATH,"home_prices_test.csv"))
dataset=train;data_pred=test

set.seed(123)
data_split = initial_split(dataset, prop = 0.80, strata=price)
train = training(data_split);test =  testing(data_split)

#-----------------
#GRAFICI DEL MIO REPORT
#-----------------
plot_map = ggplot(train, 
                  aes(x = longitude, y = lattitude, fill = price),colour="black") +
  geom_point(alpha = 0.3,pch=21) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Data Map") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradientn(colours = c( "yellow","cyan", "red"),
                       values = scales::rescale(c(4, 5, 6.5)))+                                
  labs(color = "Price", size = "Sqft_living")+
  theme_classic()

{win.graph()
  plot_map
  }

#Price waterfront
{win.graph()
  attach(train)
  gg_two_plus_wrap(train,sqft_living,10^price,waterfront)
}
detach(train)

#--------------------------
#AGGIUSTAMENTI UTILI
#-----------------------------
vars_fact = c("bedrooms", "sqft_basement","zip_code","yr_built","condition","bathrooms","floors","view","waterfront","year_renovated")

metro_quadro=train %>%
  select(price,zip_code,sqft_living)%>%
  group_by(zip_code)%>%
  summarize(pr_m_qu=mean(10^price/sqft_living))

train= train %>% #tolgo outlier notati con l'esplorativa
     filter(sqft_living<10000)%>%
     filter(sqft_lot<8e+05)%>%
     filter(nn_sqft_living<6000)%>%
     filter(nn_sqft_lot<4e+05)%>%
     filter(10^price<5*10^6)

#Aggiungo informazione
train$pr_m_qu=unlist(lapply(1:nrow(train),function(i) metro_quadro[which(train[i,]$zip_code==metro_quadro$zip_code),2]))
test$pr_m_qu=unlist(lapply(1:nrow(test),function(i) metro_quadro[which(test[i,]$zip_code==metro_quadro$zip_code),2]))

#-------------------------
#RECIPIENTI PER I MODELLI
#-------------------------

basic_rec = recipe(price ~.,train) %>%
  step_mutate(age=as.integer(format(as.Date(date_sold), "%Y"))-pmax(yr_built, year_renovated))%>%
  step_mutate(floors=ifelse(floors==1,"One","More"))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms==33, 3)) %>%
  step_mutate(bathrooms=ifelse(bathrooms%%1>0.5, round(bathrooms), floor(bathrooms)))%>%
  step_mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)) %>%
  step_mutate(garden=abs(sqft_lot-sqft_living))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms>=8, 8)) %>%
  step_mutate(diff_year=yr_built-year_renovated) %>%
  step_mutate(year_renovated=ifelse(year_renovated!=yr_built,"Yes","No")) %>%
  step_mutate(sqft_basement=ifelse(sqft_basement>0,"Yes","No"))%>% 
  step_log(sqft_living,sqft_lot,nn_sqft_lot, nn_sqft_living,base = 10)

basic_rec_lm = basic_rec %>%
  step_mutate_at(all_of(vars_fact), fn = factor)

interaction_rec <-
  basic_rec_lm %>%
  step_dummy(all_nominal_predictors())%>%
  step_interact( ~ sqft_living:starts_with("zip_code"))%>%
  step_interact( ~ sqft_living:starts_with("bedrooms"))%>%
  step_interact( ~ sqft_living:starts_with("bathrooms"))%>%
  step_interact( ~ sqft_living:starts_with("floors")) %>%
  step_interact( ~ sqft_living:starts_with("waterfront")) %>%
  step_interact( ~ sqft_living:starts_with("view")) %>%
  step_interact( ~ sqft_living:starts_with("year_renovated"))%>%
  step_interact( ~ sqft_lot:starts_with("condition"))%>%
  step_interact( ~ sqft_lot:starts_with("zip_code"))%>%
  step_interact( ~ sqft_lot:starts_with("floors")) %>%
  step_interact( ~ sqft_lot:starts_with("waterfront")) %>%
  step_interact( ~ sqft_lot:starts_with("view")) %>%
  step_interact( ~ sqft_lot:starts_with("year_renovated")) %>%
  step_interact( ~ lattitude:starts_with("bathrooms"))%>%
  step_interact( ~ lattitude:starts_with("floors")) %>%
  step_interact( ~ lattitude:starts_with("waterfront"))%>%
  step_interact( ~ longitude:starts_with("bathrooms"))%>%
  step_interact( ~ longitude:starts_with("floors")) %>%
  step_interact( ~ longitude:starts_with("waterfront")) %>%
  step_interact( ~ longitude:starts_with("view")) %>%
  step_interact( ~ longitude:starts_with("year_renovated"))%>%
  step_interact( ~ longitude:starts_with("condition"))%>%
  step_interact( ~ nn_sqft_living:starts_with("zip_code"))%>%
  step_interact( ~ nn_sqft_living:starts_with("waterfront")) %>%
  step_interact( ~ nn_sqft_living:starts_with("year_renovated"))%>%
  step_interact( ~ nn_sqft_lot:starts_with("zip_code"))%>%
  step_interact( ~ nn_sqft_lot:starts_with("floors")) %>%
  step_interact( ~ nn_sqft_lot:starts_with("waterfront")) %>%
  step_interact( ~ nn_sqft_lot:starts_with("view")) %>%
  step_interact( ~ nn_sqft_lot:starts_with("year_renovated"))%>%
  step_interact( ~ nn_sqft_lot:starts_with("condition"))


splines_rec <- 
  interaction_rec %>%
  step_ns(lattitude, deg_free = 55)%>%
  step_ns(longitude, deg_free= 25)


alberi_rec <- recipe(price ~.,train) %>%
  step_mutate(age=as.integer(format(as.Date(date_sold), "%Y"))-pmax(yr_built, year_renovated))%>%
  step_mutate(floors=ifelse(floors==1,1,2))%>%
  step_mutate(bathrooms=ifelse(bathrooms%%1>0.5, round(bathrooms), floor(bathrooms)))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms==33, 3)) %>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms>=8, 8)) %>%
  step_mutate(garden=abs(sqft_lot-sqft_living))%>%
  step_mutate(perc_cas=(sqft_above/sqft_living)*100)%>%
  step_mutate(perc_cas_2=(sqft_basement/sqft_living)*100)%>%
  step_mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)) %>%
  step_mutate(year_renovated=abs(yr_built-year_renovated)) %>%
  step_log(sqft_living,sqft_lot,nn_sqft_lot, nn_sqft_living,base = 10)%>%
  step_mutate(date_sold=as.integer(format(as.Date(date_sold), "%Y%m%d")))%>%
  step_other(zip_code, threshold = 0.05)%>%
  step_dummy(all_nominal_predictors())%>%
  step_rm(sqft_above,sqft_basement)

# #normalized_rec <- 
#   recipe(price ~.,train) %>%
#   step_rm(bedrooms,bathrooms, floors, waterfront, condition, sqft_above,sqft_basement,yr_built,
#           year_renovated, nn_sqft_living,nn_sqft_lot)%>%
#   step_normalize(all_numeric_predictors())
  
#-----------------------
#CREAZIONE MODELLI
#-----------------------

lm_spec <- linear_reg() %>% set_engine("lm")

rf_spec <- 
  rand_forest(trees = 1000,mtry = 9, min_n = 4) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# svm_r_spec <- 
#   svm_rbf(cost = 3.5  ,rbf_sigma = 0.10) %>% 
#   set_engine("kernlab") %>% 
#   set_mode("regression")

# knn_spec <-
#   nearest_neighbor(neighbors =15) %>%
#   set_engine('kknn') %>%
#   set_mode('regression')

xgboost_spec <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 500,
    min_n = 38,
    tree_depth = 15,
    learn_rate = 0.0410719,
    loss_reduction =  2.1e-06) %>%
  set_engine("xgboost", objective = "reg:squarederror")%>%
  set_mode("regression")

#------------------------
#WORKFLOW
#------------------------

# normalized <- 
#   workflow_set(
#     preproc = list(normalized = normalized_rec), 
#     models = list(SVM_radial = svm_r_spec, KNN = knn_spec))

no_normalized_simple <- 
  workflow_set(
    preproc = list(basic = alberi_rec), 
    models = list(RF = rf_spec,  boosting = xgboost_spec)
  )

lm_value <- 
  workflow_set(
    preproc = list(simple = basic_rec_lm,iter = interaction_rec, splice = splines_rec), 
    models = list(linear_reg = lm_spec)
  )

#all_workflows <-  bind_rows(no_normalized_simple, normalized, lm_value)

all_workflows <- 
  bind_rows(no_normalized_simple, lm_value)

all_workflows

#-------------------------------
#FIT DEI MODELLI
#------------------------------

#Si noti che utilizzo la mia funzione per la restituzione di tutti i valori utili!
#TEMPO MIO PC---> 139.62 sec elapsed
tic()
res=metric_collect(all_workflows,train,test)
toc()

#Risultati
res[[1]] 


#-------------------------
#ENSEMBLE DEI MODELLI
#-------------------------

previsioni=as.data.frame(res[[2]])

#ensemble con media
ens=apply(X = previsioni[,c(3,6)],MARGIN = 1,FUN = mean)
Metrics::mae(test$price,ens)

#Ricerca combinazione lineare
a=seq(0.1,1,0.01)
b=seq(0.1,1,0.01)
grid=expand.grid(a=a,b=b)
grid$som=apply(grid,MARGIN = 1,FUN = sum)
grid=grid[which(grid$som==1.0),]

ris=lapply(seq(1,nrow(grid),1),function(x) Metrics::mae(test$price,previsioni$splice_linear_reg*grid$a[x]+previsioni$basic_boosting*grid$b[x]))
ris=unlist(ris)

#Valori combinazione
grid[which.min(ris),]

#Mae con combinazione
ris[which.min(ris)]

#----------------------------
#PREVISIONI-----------------
#----------------------------

test=data_pred
#SI ESEGUA PURE TUTTO, VIENE RIPETUTO TUTTO QUELLO FATTO IN PRECEDENZA CON I NUOVI DATI DI TEST 
#E CON IL TRAIN E VALIDATION UNITI!

metro_quadro=dataset %>%
  select(price,zip_code,sqft_living)%>%
  group_by(zip_code)%>%
  summarize(pr_m_qu=mean(10^price/sqft_living))

training= dataset %>% #tolgo outlier notati con l'esplorativa
  filter(sqft_living<10000)%>%
  filter(sqft_lot<8e+05)%>%
  filter(nn_sqft_living<6000)%>%
  filter(nn_sqft_lot<4e+05)%>%
  filter(10^price<5*10^6)

training$pr_m_qu=unlist(lapply(1:nrow(training),function(i) metro_quadro[which(training[i,]$zip_code==metro_quadro$zip_code),2]))

test$pr_m_qu=unlist(lapply(1:nrow(test),function(i) metro_quadro[which(test[i,]$zip_code==metro_quadro$zip_code),2]))
##RECIPE
basic_rec = recipe(price ~.,training) %>%
  step_mutate(age=as.integer(format(as.Date(date_sold), "%Y"))-pmax(yr_built, year_renovated))%>%
  step_mutate(floors=ifelse(floors==1,"One","More"))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms==33, 3)) %>%
  step_mutate(bathrooms=ifelse(bathrooms%%1>0.5, round(bathrooms), floor(bathrooms)))%>%
  step_mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)) %>%
  step_mutate(garden=abs(sqft_lot-sqft_living))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms>=8, 8)) %>%
  step_mutate(diff_year=yr_built-year_renovated) %>%
  step_mutate(year_renovated=ifelse(year_renovated!=yr_built,"Yes","No")) %>%
  step_mutate(sqft_basement=ifelse(sqft_basement>0,"Yes","No"))%>% 
  step_log(sqft_living,sqft_lot,nn_sqft_lot, nn_sqft_living,base = 10)

basic_rec_lm = basic_rec %>%
  step_mutate_at(all_of(vars_fact), fn = factor)

interaction_rec <-
  basic_rec_lm %>%
  step_dummy(all_nominal_predictors())%>%
  step_interact( ~ sqft_living:starts_with("zip_code"))%>%
  step_interact( ~ sqft_living:starts_with("bedrooms"))%>%
  step_interact( ~ sqft_living:starts_with("bathrooms"))%>%
  step_interact( ~ sqft_living:starts_with("floors")) %>%
  step_interact( ~ sqft_living:starts_with("waterfront")) %>%
  step_interact( ~ sqft_living:starts_with("view")) %>%
  step_interact( ~ sqft_living:starts_with("year_renovated"))%>%
  step_interact( ~ sqft_lot:starts_with("condition"))%>%
  step_interact( ~ sqft_lot:starts_with("zip_code"))%>%
  step_interact( ~ sqft_lot:starts_with("floors")) %>%
  step_interact( ~ sqft_lot:starts_with("waterfront")) %>%
  step_interact( ~ sqft_lot:starts_with("view")) %>%
  step_interact( ~ sqft_lot:starts_with("year_renovated")) %>%
  step_interact( ~ lattitude:starts_with("bathrooms"))%>%
  step_interact( ~ lattitude:starts_with("floors")) %>%
  step_interact( ~ lattitude:starts_with("waterfront"))%>%
  step_interact( ~ longitude:starts_with("bathrooms"))%>%
  step_interact( ~ longitude:starts_with("floors")) %>%
  step_interact( ~ longitude:starts_with("waterfront")) %>%
  step_interact( ~ longitude:starts_with("view")) %>%
  step_interact( ~ longitude:starts_with("year_renovated"))%>%
  step_interact( ~ longitude:starts_with("condition"))%>%
  step_interact( ~ nn_sqft_living:starts_with("zip_code"))%>%
  step_interact( ~ nn_sqft_living:starts_with("waterfront")) %>%
  step_interact( ~ nn_sqft_living:starts_with("year_renovated"))%>%
  step_interact( ~ nn_sqft_lot:starts_with("zip_code"))%>%
  step_interact( ~ nn_sqft_lot:starts_with("floors")) %>%
  step_interact( ~ nn_sqft_lot:starts_with("waterfront")) %>%
  step_interact( ~ nn_sqft_lot:starts_with("view")) %>%
  step_interact( ~ nn_sqft_lot:starts_with("year_renovated"))%>%
  step_interact( ~ nn_sqft_lot:starts_with("condition"))


splines_rec <- 
  interaction_rec %>%
  step_ns(lattitude, deg_free = 50)%>%
  step_ns(longitude, deg_free= 25)


alberi_rec <- recipe(price ~.,training) %>%
  step_mutate(age=as.integer(format(as.Date(date_sold), "%Y"))-pmax(yr_built, year_renovated))%>%
  step_mutate(floors=ifelse(floors==1,1,2))%>%
  step_mutate(bathrooms=ifelse(bathrooms%%1>0.5, round(bathrooms), floor(bathrooms)))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms==33, 3)) %>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms>=8, 8)) %>%
  step_mutate(garden=abs(sqft_lot-sqft_living))%>%
  step_mutate(perc_cas=(sqft_above/sqft_living)*100)%>%
  step_mutate(perc_cas_2=(sqft_basement/sqft_living)*100)%>%
  step_mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)) %>%
  step_mutate(year_renovated=abs(yr_built-year_renovated)) %>%
  step_log(sqft_living,sqft_lot,nn_sqft_lot, nn_sqft_living,base = 10)%>%
  step_mutate(date_sold=as.integer(format(as.Date(date_sold), "%Y%m%d")))%>%
  step_other(zip_code, threshold = 0.05)%>%
  step_dummy(all_nominal_predictors())%>%
  step_rm(sqft_above,sqft_basement)


#MODELLO CON SPLINES

lm_spec <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(splines_rec)

#Fit

lm_fit <- lm_wflow %>% fit(data = training)
pred_lm=predict(lm_fit,test)


#XGBOOST

xg_model <- 
  xgboost_spec <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 500,
    min_n = 38,
    tree_depth =   15,
    learn_rate = 0.0410719,
    loss_reduction = 2.1e-06 ) %>%
  set_engine("xgboost")%>%
  set_mode("regression")

xg_wflow <- 
  workflow() %>% 
  add_model(xg_model) %>%
  add_recipe(alberi_rec) 

#fit
xg_fit <- xg_wflow %>% 
  fit(data = training)
pred_xg=predict(xg_fit,test)

#PREVISIONE FINALE
finals_prev=pred_lm$.pred*0.24+pred_xg$.pred*0.76

#write(finals_prev,"826018_Baldanza.txt",ncolumns = 1)


Metrics::mae(actual = previsione_corretta$V1,predicted = finals_prev)
