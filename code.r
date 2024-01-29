


#################################################
###########      1st Stage      #################
#################################################
library(dlnm); library(mvmeta); library(splines); library(tsModel); library(mgcv);library(data.table)

#load time-series data of warm season
mcc_example.data<-as.data.table(read.csv("xxxx/MCC_exampledata.csv"))

#load data on locations' information 
mcc_example.info<-as.data.table(read.csv("xxxx/MCC_exampleinfo.csv"))

lth_city<-unique(mcc_example.data$locations)

for(i in 1:length(lth_city))
{

  city_data<-mcc_example.data[locations%in%lth_city[i]]

  df_yr<-round(length(unique(city_data$year))/10)
  
  cb.hw<-crossbasis(city_data[,hw],lag=10,argvar=list(fun="lin"),arglag=list(fun="ns",knots=logknots(10,2)),group = city_data$year)
  
  model<-gam(death~ns(yday,df=4):factor(year)+ns(date,df=df_yr)+dow+cb.hw,family=quasipoisson,data=city_data,na.action="na.exclude")
  
  reduced<-crossreduce(cb.hw,model,type="overall")
  mcc_example.info[locations%in%lth_city[i],":="(coef=coef(reduced),vcov=vcov(reduced))]
}

#################################################
###########      2nd Stage      #################
#################################################
mvall <- mvmeta(coef~continent+avgtmean+rangetmean+gdp.capita+kgclzone,
                vcov,mcc_example.info,control=list(showiter=T),method="reml")

#################################################
###########      3rd Stage      #################
#################################################
#1. load time-series heatwave data for grid cells for a specific decade
load("xxxx/pixel1990_1999hw.rdata")

#2. load data on pixels' information
load("xxxx/pixel1990_1999info.rdata")

ids<-pixel1990_1999info$gID

for(r in 1:length(ids))
{
  id_name<-as.character(ids[r])

  #1. predict the association
  pixel_point_chara<-pixel1990_1999info[gID==ids[r]]
  mv_p<-predict(mvall,newdata = pixel_point_chara,vcov = T)
  
  #2 extract the coef and vcov
  coef.pixel<-mv_p$fit
  vcov.pixel<-mv_p$vcov
  
  #3 save predicted association 
  pixel1990_1999info[gID==ids[r],":="(coef=coef.pixel,vcov=vcov.pixel)]
}


