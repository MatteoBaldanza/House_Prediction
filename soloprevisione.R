##INFORMAZIONI UTILE:

#NON VIENE RIPERCORSO FORMALMENTE IL PROCEDIMENTO PER L'OTTENIMENTO DELLA PREVISIONE

#SI PUO ESEGUIRE TUTTO IL CODICE INSIEME SENZA VISIONARLO OTTENENDO LE STIME IN: 77.59 secondi

#LA FASE DI TUNING, ANALISI RISULTATI VALIDATION E DI ANALISI ESPLORATIVA NON SONO STATE RIPORTATE

#-----------
#LIBRERIE E FUNZIONI
#---------------

library(tidymodels)
library(readr)
library(tictoc)
library(baguette)
library(ggplot2)

tic()
gg_two_plus_wrap=function(dati,x,y,fc_w){  #funzione per grafico
  ggplot(dati) +
    geom_point(aes(x=x, y=(y),col=fc_w),pch=19,show.legend = F) + 
    geom_smooth(method = lm, formula = y ~ x,aes(x=x, y=(y)),col= "red")+
    facet_wrap(fc_w)+
    theme_classic()
}


#------------------------
#IMPORTAZIONE DATI
#-------------------------
tidymodels_prefer()
PATH <- "https://raw.githubusercontent.com/aldosolari/DM/master/docs/HomePrices/"
train = read_csv2(paste0(PATH,"home_prices_train.csv"))
test = read_csv2(paste0(PATH,"home_prices_test.csv"))
dataset=train;data_pred=test

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

#-------------------------
#RECIPIENTI PER I MODELLI
#-------------------------

basic_rec = recipe(price ~.,training) %>%
  step_mutate(age=as.integer(format(as.Date(date_sold), "%Y"))-pmax(yr_built, year_renovated))%>%
  step_mutate(floors=ifelse(floors==1,"One","More"))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms==33, 3)) %>%
  step_mutate(bathrooms=ifelse(bathrooms%%1>0.5, round(bathrooms), floor(bathrooms)))%>%
  step_mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)) %>%
  step_mutate(garden=abs(sqft_lot-sqft_living))%>%
  step_mutate(bedrooms=replace(bedrooms, bedrooms>=8, 8)) %>%
  #step_mutate(diff_year=yr_built-year_renovated) %>%
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


#-----------------------
#CREAZIONE MODELLI
#-----------------------


lm_spec <- linear_reg() %>% set_engine("lm")

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


#------------------------
#WORKFLOW
#------------------------


lm_wflow <- 
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(splines_rec)


xg_wflow <- 
  workflow() %>% 
  add_model(xg_model) %>%
  add_recipe(alberi_rec) 

#-------------------------------
#FIT DEI MODELLI
#------------------------------

lm_fit <- lm_wflow %>% fit(data = training)

xg_fit <- xg_wflow %>% 
  fit(data = training)
#-------------------------
#PREVISIONI
#---------------------------

pred_lm=predict(lm_fit,test)

pred_xg=predict(xg_fit,test)

#-------------------------
#ENSEMBLE DEI MODELLI
#-------------------------

finals_prev=pred_lm$.pred*0.24+pred_xg$.pred*0.76

toc()
#write(finals_prev,"826018_previsioni.txt",ncolumns = 1)

#NOTA: IL WARNING ESCE IN QUANTO NUMERO PARAMETRI MODELLO LINEARE ELEVATO 
