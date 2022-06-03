library(ggplot2)


#Function---------------

ggplot_two_variable=function(dati,x,y){
  ggplot(dati) +
    geom_point(aes(x=x, y=10^(y), color=price)) + 
    geom_smooth(aes(x=x, y=10^(y)),col= "red")
}

gg_two_plus_wrap=function(dati,x,y,fc_w){
  ggplot(dati) +
    geom_point(aes(x=x, y=(y),col=fc_w),pch=19,show.legend = F) + 
    geom_smooth(method = lm, formula = y ~ x,aes(x=x, y=(y)),col= "red")+
    facet_wrap(fc_w)+
    theme_classic()
}

ris=function(rf_fit,train,test){
  pred=predict(rf_fit,train %>% select(-price))
  tr=Metrics::mae(actual=train$price,pred$.pred)
  pred_test=predict(rf_fit,test %>% select(-price))
  ts=Metrics::mae(actual=test$price,pred_test$.pred)
  return(c(tr,ts))
}

#------------

tidymodels_prefer()

set.seed(123)

data_split = initial_split(train, prop = 0.80,strata = price)

train = training(data_split);test =  testing(data_split)


#modello a casu su tutto
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(price ~ ., data = train)

pred=predict(lm_form_fit,test[,-2])
Metrics::mae(actual=test$price,pred$.pred)

Metrics::mape(actual=test$price,pred$.pred)
Metrics::rmse(actual=10^(test$price),10^(pred$.pred))

#--------------.

#nessun valore mancante
sum(is.na(train))

#Risposta distribuita normalmente, non si notano problemi
{win.graph()
  par(mfrow=c(2,1))
  hist(train$price,main="Logaritmo del train$price")
  hist(10^(train$price),main="Prezzi Reali")
}

#Analisi spaziale
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

#Variabile Condition
train$condition=as.factor(train$condition)
levels(train$condition)=c("average","fair","good","poor", "very_good")
tapply(train$price, train$condition, mean)

#Variabile bedrooms
train$bedrooms=as.factor(train$bedrooms)

tapply(train$price, train$bedrooms, mean)
summary(as.factor(train$bedrooms))

#sembra un po assurdo che ci siano case con 33 camere da letto
(val=which(train$bedrooms==33))

(as.data.frame(train[val,]))
#è presente una sola casa con 33 stanze, indaghiamo meglio questo valore

##Binning sulla variabile per capire quale valore attendermi su bedrooms
sqft_living_binning<-cut(train$sqft_living, c(0,1100,1400,1700,13540), right=FALSE,labels = c(seq(1,4)))
aggregate(as.numeric(bedrooms) ~ as.numeric(bathrooms)+sqft_living_binning+as.numeric(floors), FUN=median,data=train)

#mi aspetto 4 stanze non 33----> Il train$price se ci fossero 4 stanze sarebbe piu o meno spiegabile?
v=aggregate(10^(price) ~ as.numeric(bathrooms)+sqft_living_binning[-val]+as.numeric(floors), FUN=median,data=train[-val,])
cbind(v[18:21,],10^train[val,"price"])
#differenza di train$price troppo elevata, potrebbe essere un errore oppure si trova in una zona diversa dalle altre?

range=train[ifelse(train$sqft_living>1500 & train$sqft_living<1700, TRUE, F),]
which(range$bedrooms==33)

g1 <- subset(range, bedrooms == "33")


##Prezzi nella zona?
train[val,c("longitude","lattitude")]
range_2=range[ifelse((range$longitude > -122.34 & range$longitude < -122.32) & 
                       (range$lattitude > 47.65 & range$lattitude < 47.72), TRUE, F),]

mean(range_2$price)
train[val,"price"] ###Si ci può stare!! Prezzi simili procedo con una reimputazione


#Imputo il valore corretto
train$bedrooms[val]=3


#Stanze con 0 bedrooms??

(val=which(train$bedrooms==0))
View(train[val,])

#Variabile bathrooms


tapply(train$price, train$bathrooms, mean)
summary(as.factor(train$bathrooms))

#Variabile floors
tapply(train$price, train$floors, median)
summary(as.factor(train$floors))

#Variabile view

tapply(train$price, train$view, mean) #correlazione positiva evidente

#Waterfront
train$waterfront=as.factor(train$waterfront)
tapply(train$price, train$waterfront, mean) #evidente differenza


##analizzo quello dopo anche sulla base di waterfront!
{win.graph()
  par(mfrow=c(2,1))
  plot(train$sqft_living,10^train$price)
  plot(train$nn_sqft_living,10^train$price)
}

#Condition
tapply(train$price, train$condition, median)
summary(as.factor(train$condition))

#Osservazione importante variabil:

equal_sum=ifelse(train$sqft_above+train$sqft_basement==train$sqft_living,T,F)
sum(equal_sum==F) #tutti uguali


#Trasformazione variabili?

#Indago variabili

value=sapply(c("sqft_living","sqft_lot","nn_sqft_lot","nn_sqft_living"), function(x) which(colnames(train)==x))

all_numVar=as.data.frame(train[,value])

colnames <- dimnames(all_numVar)[[2]]
for (i in 1:4) {
  {win.graph()
    par(mfrow=c(2,1))
    hist(all_numVar[,i], main=colnames[i], probability=TRUE, col="gray", border="white")
    d <- density(all_numVar[,i])
    hist(log(all_numVar[,i]), main=colnames[i], probability=TRUE, col="gray", border="white")
    d <- density(log(all_numVar[,i]))
    lines(d, col="red")
  }
}


#---------------------
#Grafici

#yr_bulit---> nessun andamento particolare
ggplot_two_variable(train,yr_built,price)

#date_sold--> niente
ggplot_two_variable(train,date_sold,price)

#Sqft living ### interesssante
ggplot_two_variable(train,sqft_living,price)

#fb interessante
ggplot_two_variable(train,fb,price)

#nn interessante
ggplot_two_variable(train,nn_sqft_living,price)


view(train[which(10^(train$price)>6*10^6 & train$nn_sqft_living>8.2),]) #case particolari
#nessun evidenza di problemi, magari case particolari

gg_two_plus_wrap(train,sqft_living,price,condition)
gg_two_plus_wrap(train,sqft_living,price,waterfront)

#Price waterfront
{win.graph()
  gg_two_plus_wrap(train,sqft_living,10^price,waterfront)
}

#analisi boxplot
summary(as.factor(train$bedrooms))
{win.graph()
  boxplot(price ~ as.factor(bathrooms),data = train %>%
            mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)))
}

{win.graph()
  boxplot(price ~ as.factor(floors),data = train)
}

my.data=bake(basic_rec_lm %>% prep(),new_data = train)
my.data=as.data.frame(my.data)

win.graph()
my.data%>%
  select(price,bedrooms)%>%
  ggplot(aes(y = price, fill = bedrooms, x = bedrooms)) +
  theme_bw() +
  geom_boxplot(show.legend = F) +
  # order of colors is determined by the order of levels of xy$setup
  scale_fill_brewer(palette=1) 
