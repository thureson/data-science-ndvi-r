rm(list=ls())
#data imports
setwd("C:/Users/Theresa/Documents/Introduction_to_DS/MiniProject/Delivery/Code/rawData")
ndvi_data=read.csv("NDVI_Nuuksio.csv")
monthly_climate_data=read.csv("monthly_weather_Nuuksio.csv")
climate_data=read.csv("daily_weather_Nuuksio.csv")


#########################################################################
#processing date format##################################################
#########################################################################

##NDVI ###################################################################
id=seq(1:nrow(ndvi_data))
ndvi_data=cbind(id,ndvi_data)

month=substring(ndvi_data$system.time_start,1,3)
m=match(month,month.abb)  #find numeric aquivalent for abbreviated month names
month=sprintf("%02d",m)   #add leading 0

x=gsub(",","",ndvi_data$system.time_start) #deleting comma after day
y=strsplit(x,split=" ") #split into day, month and year
y2=unlist(y)

#indices of day: it is every 3rd element starting with position 2
s=seq(from=2,to=449,by=3) 
day=y2[s]
day=formatC(as.numeric(day), width=2, flag="0")

#indices of year: it is every 3rd element starting with position 3
s=seq(from=3,to=450,by=3)
year=y2[s]
t=paste(year,month,day,sep="-")
as.Date(t) #change into R date fromat

ndvi_df=data.frame(ndvi_data,t)



##Daily Climate ##################################################################
attach(climate_data)

#combine day, year,month to 1 String
t=paste(climate_data$Year,climate_data$m,climate_data$d,sep="-") 
t=as.Date(t)
climate_df=data.frame(t,climate_data)
detach(climate_data)
rm(m)

summary(climate_data)
#replace negative and missing values for snow depth ################################
a1=climate_df$Snow.depth..cm.<0
sum(as.numeric(a1),na.rm = T)
b=(is.na(climate_df$Snow.depth..cm.)==T)
sum(as.numeric(b))
mis.snow=subset(dfX,(a1|b))
table(mis.snow$m)

# set missing and negative values for snow to zero 
#when there is more than 2°C snow turns into water
#remaining possibility for snow: remaining snow layer from previous days. but when it's not a winter
#mont ant the average temperature is above  10°C we can assume there is no snow.
a2=climate_df$Air.temperature..degC.>10
a3=!(climate_df$m %in% c(11,12,1,2,3))
climate_df$snow=ifelse((a1|b)&(a2|a3),0,climate_df$Snow.depth..cm)
neg_snow=subset(climate_df,climate_df$snow<0|is.na(climate_df$snow)==T)
summary(neg_snow)
table(neg_snow$m)

###########################################################################
#combining datasets    ####################################################
##########################################################################


climate_df$t=as.Date(climate_df$t)
ndvi_df$t=as.Date(ndvi_df$t)
merged=merge(x=ndvi_df,y=climate_df,by="t",all.x=T)
df=merge(x=merged,y=monthly_climate_data,by=c("Year","m"),all.x = T)

#climate data is only available since June 2014 so the observations form January to May 2014 are neglected
df=subset(df,id>=33) 
detach(climate_df)
dfX=data.frame(df$t,df$Year, as.factor(df$m), df$NDVI,df$Precipitation.amount..mm.,df$snow,df$Air.temperature..degC.,
              df$Maximum.temperature..degC.,df$Minimum.temperature..degC.,
              df$Monthly.precipitation.amount..mm.,df$Monthly.mean.temperature..degC.)
colnames(dfX)=c("t", "year", "m", "NDVI","precipitation","snow","average.temp", "max.temp", "min.temp", "monthly_precipitation","monthly_average.temp")
# dfX=data.frame(df$t,df$Year, as.factor(df$m), df$Snow.depth..cm.,df$Maximum.temperature..degC.,df$Minimum.temperature..degC., df$Monthly.precipitation.amount..mm.,df$Monthly.mean.temperature..degC.)
# colnames(dfX)=c("t", "year", "m", "NDVI","snow", "max.temp", "min.temp", "monthly_precipitation","monthly_average.temp")
is.factor(dfX$m)



###########################################################################
#estimate missing values    ###############################################
###########################################################################

#plot missing values##############################################################
par(mfrow=c(1,1))
plot(df$t,df$Air.temperature..degC.,type="o",pch=20,col="brown1",ylab="average temperature",xlab="time")
abline(h=0,lty="dotted",col="grey")
plot(dfX$t,dfX$precipitation,type="o",pch=20,col="cyan4",ylab="precipitation (mm)",xlab="time")
abline(h=0,lty="dotted",col="grey")
plot(dfX$t,dfX$snow.depth,type="o",pch=20,col="darkgoldenrod1",ylab="snow depth (cm)",xlab="time")


### plot correlations
plot(dfX,pch=20,col="cornflowerblue")
summary(dfX)

#### estimate missing values ###

#temperature #####################################################################
# boxplot(dfX$average.temp~dfX$m)
summary(dfX$average.temp)
dfX$average.temp=ifelse(is.na(dfX$average.temp),(dfX$max.temp+dfX$min.temp)/2,dfX$average.temp)
summary(dfX$average.temp)

#snow depth #####################################################################
#model snow from month and year
df=data.frame(dfX$year,dfX$m,dfX$NDVI,dfX$precipitation,dfX$snow,dfX$average.temp)
colnames(df)=c("year","month","NDVI","precipitation","snow","average.temp")
plot(df,pch=20,col="cornflowerblue")
plot(df$average.temp,df$snow.depth,pch=20) #nonlinear relationships from snow to

mis.snow2=subset(dfX,dfX$snow<0|is.na(dfX$snow)==T)
table(mis.snow2$m)

#estimate missing snow by average of previous and next observation
for(i in 2:(nrow(dfX)-1)){
  if(dfX[i,]$snow<0|is.na(dfX[i,]$snow)==T){
    dfX[i,]$snow=(dfX[i-1,]$snow+dfX[i+1,]$snow)/2
  }
}


#replace implausible values for precipitation by average of this month and year
neg_rain=subset(dfX,dfX$precipitation<0)
dfX$rain=ifelse(dfX$precipitation<0,dfX$monthly_precipitation/30,dfX$precipitation)



#compare monthly and daily precipitation and temperature############################
par(mfrow=c(1,1))
plot(dfX$t,dfX$average.temp,type="o",pch=20,col="grey",ylab="temperature",xlab="time",
     main="Average temperature in °C")
abline(h=0,lty="dotted",col="grey")
lines(dfX$t,dfX$monthly_average.temp,type="l",pch=20,col="brown1",lty="dotted")
legend("bottomleft",col=c("grey","brown"),lty = c("solid","dotted"),
       legend=c("daily average","monthly average"),bty="n",cex=0.9)

plot(dfX$t,dfX$precipitation,type="o",pch=20,col="grey",ylab="precipitation amount",xlab="time",
     main="Precipitation amount in mm")
abline(h=0,lty="dotted",col="grey")
lines(dfX$t,dfX$monthly_precipitation/30,type="l",pch=20,col="cyan4",lty="dotted",lwd=1.5)
legend("topleft",col=c("grey","cyan4"),lty = c("solid","dotted"),
       legend=c("daily","monthly average"),bty="n",cex=0.9,lwd=2)






#################################################################################
# Time Series Models  ###########################################################
#################################################################################

# transform NDVI data to monthly data ##########################################
#aggregate mean over month an year
library(lubridate)
d=day(dfX$t)
average_NDVI_df=aggregate( NDVI ~ m + year , dfX , mean )


ms=c(seq(7,12),rep(seq(1,12),3),seq(1,9))
ys=c(rep("2014",6),rep(c("2015","2016","2017"),times=c(12,12,12)),rep("2018",9))
id=seq(1:length(ms))
ms=data.frame("id"=id,"year"=ys,"m"=as.factor(ms))
#ms=data.frame("year"=ys,"m"=as.factor(ms))
str(ms$year)
str(average_NDVI_df$year)
average_NDVI_df$year=as.factor(average_NDVI_df$year)
average_NDVI_df=merge(ms,average_NDVI_df,by=c("m","year"),all.x = T,sort=F)
df<- average_NDVI_df[order(average_NDVI_df$id),]


#estimate missing NDVIs by average of previous and next month
for(i in 2:(nrow(df)-1)){
  if(is.na(df[i,]$NDVI)){
    df[i,]$NDVI=(df[i-1,]$NDVI+df[i+1,]$NDVI)/2
  }
}
#################################################################################
#creating a NDVI time series #########################################################
#table(as.factor(dfX$year)) #get frequency
ts.ndvi=ts(df$NDVI,start=c(2014, 7), end=c(2018, 9), frequency=12)
Time=time(ts.ndvi)
plot.ts(ts.ndvi,col="cornflowerblue",ylab="NDVI",type="o",pch=20)


#descriptive statistics #########################################################

#This will print the cycle across years.
ndviCycle=cycle(ts.ndvi)


#This will aggregate the cycles and display a year on year trend.
#aggregate Splits the data into subsets, computes summary statistics for each, 
#and returns the result in a convenient form.
y=aggregate(ts.ndvi,FUN=mean)
y
plot(y)

#Boxplot across months to see seasonal effect
boxplot(ts.ndvi~ndviCycle,xlab="month",ylab="NDVI")
#there seems to be big differences in the means and quite high
#NDVI in Dezember. This may be due to the large variance caused by the wide
#fluctuations in those months that can be seen in the earth Enhgine picture.
#It differs from the harmonic model in Earth Engine and the seasonal trend  below
#which shows quite smoot transitions from one month to the other.

# Seasonal decomposition
fit <- stl(ts.ndvi, s.window="period")
plot(fit,col="cornflowerblue")
#The plot above shows the original time series (top),
#the estimatedtrend component (second from top),
#the estimated seasonal component (third from top),
#and the estimated irregular component (bottom).

#################################################################################
#fitting a linear model #########################################################
plot.ts(ts.ndvi,col="cornflowerblue",ylab="NDVI",main="Linear TS Model",type="o",
        pch=20)
m.linear=lm(ts.ndvi~time(ts.ndvi))
ts.fitted.linear=ts(fitted(m.linear))
summary(m.linear)
abline(m.linear,col="brown1")
legend("topleft",legend=c("true","linear fit"),col=c("cornflowerblue","brown1"),
       pch=20,bty="n")
#################################################################################

#harmonic model##################################################################
library(TSA)
har.=harmonic(ts.ndvi,1)   # harmonic components
har2=cbind(har.,time(ts.ndvi)) # add linear component
model4=lm(ts.ndvi~har2)
summary(model4)

par(mfrow=c(1,1))
plot(ts(fitted(model4),start=c(2014, 7), end=c(2018, 9), freq=12),xlab="t", ylim=range(c(fitted(model4),ts.ndvi)),
     ylab='β2 cos(2πωt) + β3 sin(2πωt) ',type='l', col="brown1", pch=20,main="Harmonic function")

plot(ts(fitted(model4),start=c(2014, 7), end=c(2018, 9), freq=12),
     ylab='NDVI',type='o', col="brown1",lty="dotted", pch=20,
     ylim=range(c(fitted(model4),ts.ndvi)),main="Harmonic model")
lines(as.numeric(Time),ts.ndvi,pch=20,type="o",col="cornflowerblue")
legend("topleft",col=c("cornflowerblue","brown1"),pch=20,legend=c("true","fitted"),
       bty = "n", cex=0.9,pt.cex = 0.9)


#using trainset and testset##############################################
t_split=which(df$year=="2017"&df$m=="7")
trainset<-df[1:t_split,]
testset<-df[(t_split):nrow(df),]

ts.train=ts(df$NDVI,start=start(ts.ndvi), end=c(2017, 7), frequency=12)

har.=harmonic(ts.train,1)
har2=cbind(har.,time(ts.train)) # add linear component
model.train=lm(ts.train~har2)
summary(model.train)

ts.test=ts(fitted(model.train),start=end(ts.train), end=c(2018, 9), freq=12)
fitted(model.train)

##plot model and prediction
plot(ts.ndvi,pch=20,type="o",col="cornflowerblue",ylab='NDVI',main="Harmonic model Cross Validated")
lines(as.numeric(Time[1:t_split]),ts(fitted(model.train)),pch=20,type="o", col="chartreuse4")
lines(as.numeric(Time[(t_split):nrow(df)]),ts.test,type='o', col="brown1",lty="dotted",pch=20)
legend("topleft",col=c("cornflowerblue","chartreuse4","brown1"),pch=20,legend=c("true","trained model","predicted"),
       bty = "n", cex=0.8,pt.cex = 0.9)

#only prediction
plot(ts.test,
     ylab='NDVI',type='o', col="brown1",lty="dotted", pch=20,
     ylim=range(c(fitted(model4),ts.ndvi)),main="Harmonic model Cross Validated")
lines(as.numeric(Time[(t_split):nrow(df)]),ts.ndvi[(t_split):nrow(df)],pch=20,type="o",
      col="cornflowerblue")
legend("topleft",col=c("cornflowerblue","brown1"),pch=20,legend=c("true","fitted"),
       bty = "n", cex=0.9,pt.cex = 0.9)
time(ts.test)

###detrended##########################################################
har.=harmonic(ts.train,1)
#har2=cbind(har.,time(ts.train)) # add linear component
yhat.train=ts.fitted.linear[1:t_split]
model.train.h.detrended=lm((ts.train-yhat.train)~har.)
summary(fitted(model.train.h.detrended))

ts.test.h.detrended=ts(fitted(model.train.h.detrended),start=end(ts.train), end=c(2018, 9), 
                       freq=12)

yhat.test=ts.fitted.linear[t_split:nrow(df)]
##plot model and prediction
plot(ts.ndvi,pch=20,type="o",col="grey",ylab='NDVI',main="Harmonic model Cross Validated")
lines(as.numeric(Time[1:t_split]),ts(fitted(model.train.h.detrended)+yhat.train),pch=20,type="o",
      col="brown1")
lines(as.numeric(Time[(t_split):nrow(df)]),ts.test.h.detrended+ts.fitted.linear[t_split:nrow(df)],type='l', 
      col="brown1",lty="dotted",lwd=2)
legend("topleft",col=c("grey","brown1","brown1"),pch=c(20,20,NA),lty=c(NA,NA,"dotted"),
       legend=c("true","trained model","predicted"),
       bty = "n", cex=0.8,pt.cex = 0.9)

## comparison detrended vs not dentreded ##################################################
par(mfrow=c(1,1))
plot(ts.ndvi,type="l",col="grey",ylab='NDVI')
lines(as.numeric(Time[1:t_split]),ts(fitted(model.train.h.detrended)+yhat.train),pch=20,type="o",
      col="brown1",lwd=1.5)
lines(as.numeric(Time[(t_split):nrow(df)]),ts.test.h.detrended+ts.fitted.linear[t_split:nrow(df)],
      type='l', col="brown1",lty="dotted",lwd=2)
lines(as.numeric(Time[1:t_split]),ts(fitted(model.train)),pch=20,type="o", col="chartreuse",lwd=1.5)
lines(as.numeric(Time[(t_split):nrow(df)]),ts.test,type='l', col="chartreuse",lty="dotted",lwd=2)
legend("topleft",col=c("grey",NA,"brown1","chartreuse"),pch=c(NA,NA,20,20),
       lty=c("solid",NA,NA,NA),
       legend=c("true","solid line:training, dotted line: testing","detrended","not detrended"),
       bty = "n", cex=0.8,pt.cex = 0.9)


rmse.harmonic.cross<-sqrt(mean((testset$NDVI-ts.test)^2))
rmse.harmonic.cross.detrended<-sqrt(mean((testset$NDVI-(ts.test.h.detrended+ts.fitted.linear[t_split:nrow(df)]))^2))
c(rmse.harmonic.cross,rmse.harmonic.cross.detrended)



plot(testset$NDVI,ts.test,pch=20,col="cornflowerblue",ylim=c(min(testset$NDVI),max(testset$NDVI)),
     xlim=c(min(testset$NDVI),max(testset$NDVI)))
abline(a=0,b=1,lty="dotted",col="grey")



#################################################################################
#SVM Regression on monthly values  ##############################################
#################################################################################

monthly_climate=cbind(monthly_climate_data,"year"=as.factor(monthly_climate_data$Year))
str(df$year)
average_df=merge(df,monthly_climate,by=c("m","year"),all.x = T,sort=F)
average_df<- average_df[order(df$id),]

for(i in 2:(nrow(climate_df)-1)){
  if(climate_df[i,]$snow<0|is.na(climate_df[i,]$snow)==T){
    k=1
    while(climate_df[i-k,]$snow<0|is.na(climate_df[i-k,]$snow)==T){
      k=k+1 #find valid predecessor
    }
    j=1
    while(climate_df[i+j,]$snow<0|is.na(climate_df[i+j,]$snow)==T){
      j=j+1 #find valid successor 
    }
    climate_df[i,]$snow=(climate_df[i-k,]$snow+climate_df[i+j,]$snow)/2
  }
}

snow_df=aggregate( snow ~ m + Year , climate_df , mean )
colnames(snow_df)=c("m","year","snow")
summary(snow_df)
str(average_df)
average_df$year=as.numeric(as.character(average_df$year))
average_df_all=merge(average_df,snow_df,by=c("m","year"),all.x = T,sort=F)
df<- average_df_all[order(average_df_all$id),]


#estimate missing snow by average of previous and next month
for(i in 2:(nrow(df)-1)){
  is.na(df[i,]$snow)
  if(is.na(df[i,]$snow)){
    df[i,]$snow=(df[i-1,]$snow+df[i+1,]$snow)/2
  }
}



library(e1071)
set.seed(1708)

daten=data.frame(df$NDVI, df$m,df$year, df$Monthly.precipitation.amount..mm.,df$Monthly.mean.temperature..degC., df$snow)
daten=na.omit(daten)

colnames(daten)=c("NDVI","m","year","monthly.precipitation", "average.temp","snow")
str(daten)
daten$year=as.numeric(as.character(daten$year))

svm.model_all<- svm(NDVI~.,data=daten, type="eps-regression")
summary(svm.model_all)
predict.y.all <- predict(svm.model_all,daten)
t=as.numeric(time(ts.ndvi))
par(mfrow=c(1,1))
plot(t,df$NDVI,type="o",pch=20,col="cornflowerblue")
lines(t[1:length(Time)],predict.y.all,type="o",pch=20,col="red")
rmse.svm.all<-sqrt(mean((daten$NDVI-predict.y.all)^2))

##with cost=10###############################################
tunedModel2=svm(NDVI~.,data=daten, type="eps-regression",cost=10)
predict.y.tuned2 <- predict(tunedModel2,daten)
rmse.cost<-sqrt(mean((daten$NDVI-predict.y.tuned2)^2))

#together with harmonic model
plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI',main="Harmonic model vs SVM Regression")
lines(t[1:length(Time)],predict.y.all,type="o",pch=20,col="chartreuse4")
lines(t,ts(fitted(model4)),pch=20,type="o", col="brown1")
legend("topleft",col=c("grey","chartreuse4","brown1","chartreuse4","brown1"),pch=c(NA,20,20),
       legend=c("true","svm regression","harmonic model"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid",NA,NA))
ts.fitted.all=ts(fitted(model4))
rmse.harmonic=sqrt(mean((df$NDVI-ts.fitted.all)^2))
c(rmse.svm.all,rmse.harmonic)




#as a comparison to time series: regression with only month
x=data.frame(daten$m,daten$NDVI)
colnames(x)=c("m","NDVI")
svm.model_month<- svm(NDVI~.,data=x, type="eps-regression")
summary(svm.model_month)
predict.y.date <- predict(svm.model_month,x)
t=as.numeric(Time)
rmse.svm.date<-sqrt(mean((x$NDVI-predict.y.date)^2))



plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI')
lines(t[1:length(Time)],predict.y.all,type="o",pch=20,col="chartreuse4")
lines(t[1:length(Time)],predict.y.date,type="o",pch=20,col="cornflowerblue")
lines(t[1:length(Time)],tunedModel2$fitted,type="o",pch=20,col="brown1")
legend("topleft",col=c("grey","chartreuse4","cornflowerblue","brown1"),pch=c(NA,20,20,20),
       legend=c("true","svm (all variables)","svm (month only)","svm (cost=10)"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid",NA,NA,NA))




#as a comparison to time series: regression with only climate & year (to memorize trend which 
#is assumed to be climate related in Nuuksio)
x.climate=daten
x.climate$m=NULL
svm.model_climate<- svm(NDVI~.,data=x.climate, type="eps-regression")
predict.y.climate <- predict(svm.model_climate,x.climate)
t=as.numeric(Time)
rmse.svm.climate<-sqrt(mean((x.climate$NDVI-predict.y.climate)^2))
rmse.svm.climate

# tuneResult=tune(svm, NDVI~., data=x.climate,ranges=list(epsilon=seq(0,1,0.1), cost =2^c(0:8)))
# #Print optimum value of parameters
# print(tuneResult)
# svm.model_climate= tuneResult$best.model
# predict.y.climate <- predict(svm.model_climate,x.climate)
# rmse.svm.climate<-sqrt(mean((x.climate$NDVI-predict.y.climate)^2))
# rmse.svm.climate

# apparently setting cost=10 works better than tuning
svm.model_climate<- svm(NDVI~.,data=x.climate, type="eps-regression",cost=10)
predict.y.climate <- predict(svm.model_climate,x.climate)
rmse.svm.climate<-sqrt(mean((x.climate$NDVI-predict.y.climate)^2))
rmse.svm.climate



plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI')
lines(t[1:length(Time)],predict.y.all,type="o",pch=20,col="chartreuse4")
lines(t[1:length(Time)],predict.y.climate,type="o",pch=20,col="cornflowerblue")
lines(t[1:length(Time)],tunedModel2$fitted,type="o",pch=20,col="brown1")
legend("topleft",col=c("grey","chartreuse4","cornflowerblue","brown1"),pch=c(NA,20,20,20),
       legend=c("true","svm (all variables)","svm (climate only, cost=10)","svm (cost=10)"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid",NA,NA,NA))


#together with harmonic model
plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI')
lines(t[1:length(Time)],predict.y.all,type="o",pch=20,col="chartreuse4")
lines(t[1:length(Time)],predict.y.date,type="o",pch=20,col="cornflowerblue")
lines(t,ts(fitted(model4)),pch=20,type="o", col="brown1")
lines(t[1:length(Time)],predict.y.climate,type="o",pch=20,col="darkgoldenrod1")
abline(m.linear,col="olivedrab1")
legend("topleft",col=c("grey","olivedrab1","brown1","chartreuse4","cornflowerblue","darkgoldenrod1"),
       pch=c(NA,NA,20,20,20,20),
       legend=c("true","linear ts","harmonic ts","svm (all variables)","svm (month only)","svm (climate only,cost=10)"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid","solid",NA,NA,NA,NA))

##############################################################################################
#export ###################################################################################

models.df=cbind(as.numeric(Time),daten,round(predict.y.all,digits=6) ,
                round(predict.y.date,digits=6),
                round(predict.y.climate,digits=6),
                round(ts.fitted.all[1:length(ts.fitted.all)],digits=6),
                round(ts.fitted.linear[1:length(ts.fitted.all)],digits=6))
colnames(models.df)=c("time",colnames(daten),"SVMfit_all","SVMfit_month","SVMfit_climate(cost=10)","TSfit_harmonic","TSfit_linear")
models.df$m=NULL
models.df$year=NULL
setwd("C:/Users/Theresa/Documents/Introduction_to_DS/MiniProject/time_series_analysis/results")
write.csv(models.df,file="models.csv",row.names = F)

##############################################################################################
#using trainset and testset  #################################################################
t_split_svm=37
trainset<-daten[1:t_split_svm,]
testset<-daten[(t_split_svm):nrow(daten),]

svm.model<- svm(NDVI~.,data=trainset, type="eps-regression")
summary(svm.model)
predict.y <- predict(svm.model,testset)
rmse.svm.cross<-sqrt(mean((testset$NDVI-predict.y)^2))


#tune
tuneResult=tune(svm, NDVI~., data=trainset,ranges=list(epsilon=seq(0,1,0.1), cost =2^c(0:8)))

#Print optimum value of parameters
print(tuneResult)

#Plot the perfrormance of SVM Regression model
plot(tuneResult)

#new model with tuned parameters
tunedModel= tuneResult$best.model
predict.y.tuned <- predict(tunedModel,testset)
rmse.tuned.cross<-sqrt(mean((testset$NDVI-predict.y.tuned)^2))
rmse.tuned.cross

tunedModel2.cross=svm(NDVI~.,data=trainset, type="eps-regression",cost=10)
predict.y.tuned2.cross <- predict(tunedModel2.cross,testset)
rmse.tuned2.cross<-sqrt(mean((testset$NDVI-predict.y.tuned2.cross)^2))
c(rmse.tuned.cross,rmse.tuned2.cross)


t_test=Time[(t_split_svm):nrow(daten)]
t_train=Time[1:t_split_svm]
plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI')
lines(t_train,svm.model$fitted,type="o",pch=20,col="chartreuse4")
lines(t_test,predict.y,type="l",pch=20,col="chartreuse4",lty="dotted",lwd=2)
lines(t_train,tunedModel$fitted,type="o",pch=20,col="cornflowerblue")
lines(t_test,predict.y.tuned,type="l",pch=20,col="cornflowerblue",lty="dotted",lwd=2)
lines(t_train,tunedModel2.cross$fitted,type="o",pch=20,col="brown1")
lines(t_test,predict.y.tuned2.cross,type="l",pch=20,col="brown1",lty="dotted",lwd=2)
legend("topleft",col=c("grey",NA,"chartreuse4","cornflowerblue","brown1"),
       pch=c(NA,NA,20,20,20), ncol=2,
       legend=c("true","solid line:training, dotted line: testing","svm default", "svm tuned (cost=2, epsilon =0.1)","svm cost=10"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid",NA,NA,NA,NA))

#seems like the default values work a bit better so we will keep those and only increase
#the cost parameter
#################################################################################

#as a comparison to time series: regression with only month#########################
#-> what if there was no change in climate just the seasonal periods?
dates=data.frame(daten$m,daten$NDVI)
colnames(dates)=c("m","NDVI")
dates.train=dates[1:t_split_svm,]
dates.test<-daten[(t_split_svm):nrow(daten),]

svm.model.dates.cross<- svm(NDVI~.,data=dates.train, type="eps-regression")
predict.y.date.cross <- predict(svm.model.dates.cross,dates.test)
rmse.svm.date.cross<-sqrt(mean((dates.test$NDVI-predict.y.date.cross)^2))


t_test=Time[(t_split_svm):nrow(daten)]
t_train=Time[1:t_split_svm]
#plot(t_test,dfX$NDVI[(t_split_svm):nrow(daten)],type="o",pch=20,col="cornflowerblue")
#lines(t_test,predict.y,type="o",pch=20,col="brown1",lty="dotted")
plot(testset$NDVI,predict.y,pch=20,col="cornflowerblue",ylim=c(min(testset$NDVI),max(testset$NDVI)),
     xlim=c(min(testset$NDVI),max(testset$NDVI)))
abline(a=0,b=1,lty="dotted",col="grey")
#even though the rmse is quite small the SVM seems not the best choice but the extreme values differ from the model



#as a comparison to time series: regression with climate & year only###############
climates=daten
climates$m=NULL
climates.train=climates[1:t_split_svm,]
climates.test<-climates[(t_split_svm):nrow(climates),]

svm.model.climates.cross<- svm(NDVI~.,data=climates.train, type="eps-regression")
tuneResult=tune(svm, NDVI~., data=climates.train,ranges=list(epsilon=seq(0,1,0.1), cost =2^c(0:8)))
print(tuneResult)
svm.model.climates.cross= tuneResult$best.model

predict.y.climate.cross <- predict(svm.model.climates.cross,climates.test)
rmse.svm.climate.cross<-sqrt(mean((climates.test$NDVI-predict.y.climate.cross)^2))
rmse.svm.climate.cross





#together with harmonic model ##################################################################
plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI',main="Harmonic model vs SVM Regression")
lines(t_train,svm.model$fitted,type="o",pch=20,col="chartreuse4")
lines(t_test,predict.y,type="l",pch=20,col="chartreuse4",lty="dotted",lwd=2)

lines(as.numeric(Time[1:t_split]),ts(fitted(model.train.h.detrended)+yhat.train),pch=20,type="o", col="brown1")
lines(as.numeric(Time[(t_split):nrow(df)]),ts.test.h.detrended+ts.fitted.linear[t_split:nrow(df)],type='l', 
      col="brown1",lty="dotted",lwd=2)

legend("topleft",col=c("grey","chartreuse4","brown1","chartreuse4","brown1"),pch=c(NA,20,20,NA,NA),
       legend=c("true","svm regression","harmonic model","predicted svm","predicted harmonic"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid",NA,NA,"dotted","dotted"))

c(rmse.svm.cross,rmse.harmonic.cross.detrended)


##################################################################################################
#together with month & climate only models ##################################################################
plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI')
lines(t_train,svm.model$fitted,type="o",pch=20,col="chartreuse4")
lines(t_test,predict.y,type="l",pch=20,col="chartreuse4",lty="dotted",lwd=2)

lines(as.numeric(Time[1:t_split]),ts(fitted(model.train.h.detrended)+yhat.train),pch=20,type="o",
      col="brown1",lwd=1.5)
lines(as.numeric(Time[(t_split):nrow(df)]),ts.test.h.detrended+ts.fitted.linear[t_split:nrow(df)],
      type='l', col="brown1",lty="dotted",lwd=2)

lines(t_test,predict.y.date.cross,type="l",pch=20,col="cornflowerblue",lty="dotted",lwd=2)
lines(t_train,svm.model.dates.cross$fitted,type="o",pch=20,col="cornflowerblue")

lines(t_test,predict.y.climate.cross,type="l",pch=20,col="darkgoldenrod1",lty="dotted",lwd=2)
lines(t_train,svm.model.climates.cross$fitted,type="o",pch=20,col="darkgoldenrod1")

legend("topleft",col=c("grey",NA,"brown1","chartreuse4","cornflowerblue","darkgoldenrod1"),
       pch=c(NA,NA,20,20,20,20), ncol=2,
       legend=c("true","solid line:training, dotted line: testing","harmonic ts", 
                "svm (all)","svm (month)","svm (climate, cost=10)"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid",NA,NA,NA,NA,NA))
c(rmse.svm.cross,rmse.harmonic.cross,rmse.svm.date.cross,rmse.tuned2.cross)


## export cross validation ################################################################
# models2.df=cbind(as.numeric(Time),daten,
#                 round(c(ts(fitted(model.train.h.detrended)+yhat.train),rep(0,nrow(testset)-1)),digits=6) ,
#                 round(c(ts.test.h.detrended+ts.fitted.linear[t_split:nrow(df)],rep(0,nrow(trainset)-1)),digits=6) ,
#                 
#                 round(c(svm.model.dates.cross$fitted,rep(0,nrow(testset)-1)),digits=6) ,
#                 round(c(predict.y.date.cross,rep(0,nrow(trainset)-1)),digits=6) ,
#                 
#                 round(c(svm.model.climates.cross$fitted,rep(0,nrow(testset)-1)),digits=6) ,
#                 round(c(predict.y.climate.cross,rep(0,nrow(trainset)-1)),digits=6) ,
#                 
#                 round(ts.fitted.linear[1:length(ts.fitted.all)],digits=6))

ts.har.cross.all=as.numeric(ts.test.h.detrended+ts.fitted.linear[(t_split):nrow(df)])
length(ts.har.cross.all)
models2.df=cbind(as.numeric(Time),daten,
                 round(c(svm.model.dates.cross$fitted,predict.y.date.cross[2:nrow(testset)]),digits=6) ,
                 
                 round(c(svm.model.climates.cross$fitted,predict.y.climate.cross[2:nrow(testset)]),digits=6) ,
                 
                 round(c(ts(fitted(model.train.h.detrended)+yhat.train),
                         ts.har.cross.all[2:nrow(testset)]),
                       digits=6) ,
                 
                 round(ts.fitted.linear[1:length(ts.fitted.all)],digits=6))

colnames(models2.df)=c("time",colnames(daten),"SVMfit_month","SVMfit_climate","TSfit_harmonic","TSfit_linear")
models.df$m=NULL
models.df$year=NULL
setwd("C:/Users/Theresa/Documents/Introduction_to_DS/MiniProject/time_series_analysis/results")
write.csv(models2.df,file="modelsCross.csv",row.names = F)



###without cost=10############################################################
plot(ts.ndvi,pch=20,type="l",col="grey",ylab='NDVI')
lines(t_train,svm.model$fitted,type="o",pch=20,col="chartreuse4")
lines(t_test,predict.y,type="l",pch=20,col="chartreuse4",lty="dotted",lwd=2)

lines(as.numeric(Time[1:t_split]),ts(fitted(model.train.h.detrended)+yhat.train),pch=20,type="o",
      col="brown1",lwd=1.5)
lines(as.numeric(Time[(t_split):nrow(df)]),ts.test.h.detrended+ts.fitted.linear[t_split:nrow(df)],
      type='l', col="brown1",lty="dotted",lwd=2)

lines(t_test,predict.y.date.cross,type="l",pch=20,col="cornflowerblue",lty="dotted",lwd=2)
lines(t_train,svm.model.dates.cross$fitted,type="o",pch=20,col="cornflowerblue")

legend("topleft",col=c("grey",NA,"brown1","chartreuse4","cornflowerblue"),
       pch=c(NA,NA,20,20,20), ncol=2,
       legend=c("true","solid line:training, dotted line: testing","harmonic ts", "svm (all)","svm (month)"),
       bty = "n", cex=0.8,pt.cex = 0.8,lty=c("solid",NA,NA,NA,NA))
c(rmse.svm.cross,rmse.harmonic.cross,rmse.svm.date.cross)




##Calculate parameters of the SVR model##################

#Find value of W
W = t(svm.model_all$coefs) %*% svm.model_all$SV
W
W = t(svm.model_month$coefs) %*% svm.model_month$SV
W


#Find value of b
b = svm.model_all$rho


###export rmse##############################################################
rmses=matrix(c(rmse.harmonic,rmse.svm.all,rmse.svm.date,rmse.svm.climate,
               rmse.harmonic.cross.detrended,rmse.svm.cross,rmse.svm.date.cross,rmse.svm.climate.cross),nrow=2,byrow=T)
row.names(rmses)=c("training error","test error")
colnames(rmses)=c("TSfit_harmonic","SVMfit_all","SVMfit_month","SVMfit_climate(cost=10)")

library(xlsx) 
write.xlsx2(rmses,"rmses.xlsx")

getwd()
library(xtable)
print(xtable(rmses, type = "latex",digits = 4), file = "rmses.tex")

################################################################################
#################################################################################


#compare plots of climate and NDVI to understand where model fails
par(mfrow=c(2,2))
#par(mfrow=c(1,1))
#plot(dfX$t,dfX$NDVI,type="o",pch=20,col="cornflowerblue",ylab="NDVI",xlab="time")
plot(dfX$t,dfX$monthly_average.temp,type="o",pch=20,col="brown1",ylab="average temperature",xlab="time")
abline(h=0,lty="dotted",col="grey")
plot(dfX$t,dfX$monthly_precipitation,type="o",pch=20,col="cyan4",ylab="precipitation (mm)",xlab="time")
plot(dfX$t,dfX$snow,type="o",pch=20,col="darkgoldenrod3",ylab="snow depth (cm)",xlab="time")

#it seems the reason the NDVI was quite high around 2018 us that the temperature was
#also quite high around then as well as the rain. But compared to 2015 the temperature
#in the summer before this winter was higher, so a relatively high temperature in the 
#summer and a not so low temperature in the winter seem to be a good combination for 
#the NDVI not dropping so low in winter.


##look for explanations of deviations in common time series
df_mean=data.frame("NDVI"=df$NDVI,"precipitation"=df$Monthly.precipitation.amount..mm.,
              "temperature"= df$Monthly.mean.temperature..degC.,"snow.depth"=df$snow)


ts.all=ts(df_mean,start=c(2014, 7), end=c(2018, 9), frequency=12)
t=time(ts.all)
par(mfrow=c(1,1))
#plot(ts.all,plot.type = "s", col=c("cornflowerblue","cyan4","brown1","darkgoldenrod1"))
plot(ts.all,col="cornflowerblue",main="")

ts.temp=ts(df_mean$temperature, start=c(2014, 7), end=c(2018, 9), frequency=12)
ndviCycle=cycle(ts.temp)
#This will aggregate the cycles and display a year on year trend.
#aggregate Splits the data into subsets, computes summary statistics for each, 
#and returns the result in a convenient form.
y=aggregate(ts.temp,FUN=mean)
y
plot(y)


fit <- stl(ts.temp, s.window="period")
plot(fit,col="cornflowerblue",main="temperature decomposition")

ts.rain=ts(df_mean$precipitation, start=c(2014, 7), end=c(2018, 9), frequency=12)
fit <- stl(ts.rain, s.window="period")
plot(fit,col="cornflowerblue",main="precipitation decomposition")



### analyze strongest model deviations #######################################################
df1=average_df_all
df1$Year=NULL
df1$Time=NULL
df1$Time.zone=NULL
df1$id=NULL
df1$d=NULL
colnames(df1)=c("month","year","NDVI", "precipitation amount (mm)","mean temperature (°C)",
                "snow depth (cm)")
subset(df1,df1$month=="12")

setwd("C:/Users/Theresa/Documents/Introduction_to_DS/MiniProject/time_series_analysis/results")
dfJul=subset(df1,df1$month=="7")
print(xtable(dfJul, type = "latex",digits=c(0,0,0,2,2,2,2)),include.rownames=FALSE, file = "df_jul.tex")


setwd("C:/Users/Theresa/Documents/Introduction_to_DS/MiniProject/time_series_analysis/results")
dfApr=subset(df1,df1$month=="4")
print(xtable(dfApr, type = "latex",digits=c(0,0,0,2,2,2,2)),include.rownames=FALSE, file = "df_apr.tex")




# #################################################################################
# #SVM Regression on daily   values  ##############################################
# #################################################################################
# library(e1071)
# set.seed(1708)
# daten=data.frame(dfX$t,dfX$year,dfX$m,dfX$NDVI,dfX$monthly_average.temp, dfX$max.temp,dfX$min.temp, dfX$monthly_precipitation,dfX$snow)
# daten=na.omit(daten)
# t=daten$dfX.t
# daten$dfX.t=NULL
# colnames(daten)=c("year","m","NDVI","average.temp","max.temp","min.temp", "rain","snow")
# svm.model_all<- svm(NDVI~.,data=daten, type="eps-regression")
# summary(svm.model_all)
# predict.y <- predict(svm.model_all,daten)
# plot(dfX$t,dfX$NDVI,type="o",pch=20,col="cornflowerblue")
# lines(t,predict.y,type="o",pch=20,col="red")
# 
# #using trainset and testset
# t_split=96 # use observations from year 2018-2019 for testing
# trainset<-daten[1:t_split,]
# testset<-daten[(t_split+1):nrow(daten),]
# 
# svm.model<- svm(NDVI~.,data=trainset, type="eps-regression")
# summary(svm.model)
# predict.y <- predict(svm.model,testset)
# rmse.svm.daily<-sqrt(mean((testset$NDVI-predict.y)^2))
# rmse.svm.daily
# 
# 
# t_test=dfX$t[(t_split+1):nrow(daten)]
# plot(t_test,dfX$NDVI[(t_split+1):nrow(daten)],type="o",pch=20,col="cornflowerblue")
# lines(t_test,predict.y,type="o",pch=20,col="red")
# plot(testset$NDVI,predict.y,pch=20,col="cornflowerblue",ylim=c(min(testset$NDVI),max(testset$NDVI)),
#      xlim=c(min(testset$NDVI),max(testset$NDVI)))
# abline(a=0,b=1,lty="dotted",col="grey")
# #even though the rmse is quite small the SVM seems not the best choice but the extreme values differ from the model
# 
# 
# ##Calculate parameters of the SVR model##################
# 
# #Find value of W
# W = t(svm.model_all$coefs) %*% svm.model_all$SV
# W
# 
# #Find value of b
# b = svm.model_all$rho
# 
# 
# 
# #using trainset and testset##########################################################
# t_split=96 # use observations from year 2018-2019 for testing
# trainset<-daten[1:t_split,]
# testset<-daten[(t_split+1):nrow(daten),]
# 
# svm.model<- svm(NDVI~.,data=trainset, type="eps-regression")
# summary(svm.model)
# predict.y <- predict(svm.model,testset)
# rmse.svm.daily<-sqrt(mean((testset$NDVI-predict.y)^2))
# rmse.svm.daily
# 
# #tune#############################################################################
# tuneResult=tune(svm, NDVI~., data=trainset,ranges=list(epsilon=seq(0,1,0.1), cost = 2^(2:9)))
# 
# #Print optimum value of parameters
# print(tuneResult)
# 
# #Plot the perfrormance of SVM Regression model
# plot(tuneResult)
# 
# #new model with tuned parameters
# tunedModel= tuneResult$best.model
# predict.y.tuned <- predict(tunedModel,testset)
# rmse_tuned<-sqrt(mean((testset$NDVI-predict.y.tuned)^2))
# rmse_tuned
# #seems like the default values work a bit better
# 
# par(mfrow=c(2,2))
# t_test=dfX$t[(t_split+1):nrow(daten)]
# plot(t_test,dfX$NDVI[(t_split+1):nrow(daten)],type="o",pch=20,col="cornflowerblue")
# lines(t_test,predict.y,type="o",pch=20,col="red")
# plot(testset$NDVI,predict.y.tuned,pch=20,col="cornflowerblue",ylim=c(min(testset$NDVI),max(testset$NDVI)),
#      xlim=c(min(testset$NDVI),max(testset$NDVI)))
# abline(a=0,b=1,lty="dotted",col="grey")
# #################################################################################
# 
# 
# t_test=dfX$t[(t_split+1):nrow(daten)]
# plot(dfX$t[(t_split+1):nrow(daten)],dfX$NDVI[(t_split+1):nrow(daten)],type="o",pch=20,col="cornflowerblue")
# lines(t_test,predict.y,type="o",pch=20,col="red")
# plot(testset$NDVI,predict.y,pch=20,col="cornflowerblue",ylim=c(min(testset$NDVI),max(testset$NDVI)),
#      xlim=c(min(testset$NDVI),max(testset$NDVI)))
# abline(a=0,b=1,lty="dotted",col="grey")
# 
# 
# 
# ##Calculate parameters of the SVR model
# 
# #Find value of W
# W = t(modelsvm$coefs) %*% modelsvm$SV
# 
# #Find value of b
# b = modelsvm$rho
# 
# ##seems like regression model and harmonic model based on monthly values work much better
# ###########################################################################################
# 
# 
# ###########################################################################
# ## Linear Model of NDVI ###################################################
# ###########################################################################
# is.factor(dfX$m)
# lm.ndvi=lm(NDVI~year+m+average.temp+snow+rain,data=dfX)
# summary(lm.ndvi)
# 
# m1=lm(NDVI~m,data=dfX)
# s.linear=summary(m1)
# c(s.linear$r.squared,s.linear$adj.r.squared)
# ##seems like there is almost no increase of the R^2 if we add other variables
# #tan month so linear model might not be a good choice
# #consider non-linear relationship
# #only poor results with linear model because of correlation of the independent variables