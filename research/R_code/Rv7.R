setwd("C:/Users/408574/Dropbox/Gas Price Attitudes/Data")
data1<-read.csv ("data_adjusted_real.csv")
library(ggplot2)
library(zoo)
library(plotly)

data1$date<-as.Date(as.character(data1$DCO_RECEIPT_DATE),"%Y %m %d") #convert received date to date format

data1$dd<-(format(data1$date,format = "%Y%m" ))
data2<-subset(data1,VIN_RV_FUEL_TYPE=="Gas")


zm<-aggregate(FUEL_PRICE ~ dd, data2, mean) #fuel price (mean)
zm$date<-as.yearmon(zm$dd,"%Y%m")


zm$FUEL_PRICE_ch_1m<-NA
zm$FUEL_PRICE_per_1m<-NA

for (i in 1:108){
  zm$FUEL_PRICE_ch_1m[i+1]<-zm$FUEL_PRICE[i+1]-zm$FUEL_PRICE[i]
  zm$FUEL_PRICE_per_1m[i+1]<- (zm$FUEL_PRICE[i+1]-zm$FUEL_PRICE[i])*100/zm$FUEL_PRICE[i]
}


zm$FUEL_PRICE_ch_2m<-NA
zm$FUEL_PRICE_per_2m<-NA

for (i in 1:107){
  zm$FUEL_PRICE_ch_2m[i+2]<-zm$FUEL_PRICE[i+2]-zm$FUEL_PRICE[i]
  zm$FUEL_PRICE_per_2m[i+2]<- (zm$FUEL_PRICE[i+2]-zm$FUEL_PRICE[i])*100/zm$FUEL_PRICE[i]
}

zm$FUEL_PRICE_ch_3m<-NA
zm$FUEL_PRICE_per_3m<-NA

for (i in 1:106){
  zm$FUEL_PRICE_ch_3m[i+3]<-zm$FUEL_PRICE[i+3]-zm$FUEL_PRICE[i]
  zm$FUEL_PRICE_per_3m[i+3]<- (zm$FUEL_PRICE[i+3]-zm$FUEL_PRICE[i])*100/zm$FUEL_PRICE[i]
}

zm$FUEL_PRICE_ch_4m<-NA
zm$FUEL_PRICE_per_4m<-NA

for (i in 1:105){
  zm$FUEL_PRICE_ch_4m[i+4]<-zm$FUEL_PRICE[i+4]-zm$FUEL_PRICE[i]
  zm$FUEL_PRICE_per_4m[i+4]<- (zm$FUEL_PRICE[i+4]-zm$FUEL_PRICE[i])*100/zm$FUEL_PRICE[i]
}

zm$FUEL_PRICE_ch_5m<-NA
zm$FUEL_PRICE_per_5m<-NA

for (i in 1:104){
  zm$FUEL_PRICE_ch_5m[i+5]<-zm$FUEL_PRICE[i+5]-zm$FUEL_PRICE[i]
  zm$FUEL_PRICE_per_5m[i+5]<- (zm$FUEL_PRICE[i+5]-zm$FUEL_PRICE[i])*100/zm$FUEL_PRICE[i]
}

zm$FUEL_PRICE_ch_6m<-NA
zm$FUEL_PRICE_per_6m<-NA

for (i in 1:103){
  zm$FUEL_PRICE_ch_6m[i+6]<-zm$FUEL_PRICE[i+6]-zm$FUEL_PRICE[i]
  zm$FUEL_PRICE_per_6m[i+6]<- (zm$FUEL_PRICE[i+6]-zm$FUEL_PRICE[i])*100/zm$FUEL_PRICE[i]
}

write.csv(zm,"Fuel Changes Over Time_real.csv")

hist(zm$FUEL_PRICE,xlab="Average Montly Fuel Price", main="Frequency of average monthly fuel price",col="grey")





rising_thr <- 10
falling_thr <- -10

stable_upper <- 10
stable_lower <- -10

low_thr <- 313
high_thr <- 313

zm$cat<-NA
for (i in 1:109){
zm$cat[i]<-ifelse(zm$FUEL_PRICE_ch_1m [i] <= falling_thr,"falling",zm$cat[i])
zm$cat[i]<-ifelse(rising_thr <= zm$FUEL_PRICE_ch_1m[i]  ,"rising",zm$cat[i])

zm$cat[i]<-ifelse(zm$FUEL_PRICE [i] <= low_thr && stable_lower <=zm$FUEL_PRICE_ch_1m[i] && zm$FUEL_PRICE_ch_1m[i] <= stable_upper, "low",zm$cat[i])
zm$cat[i]<-ifelse(high_thr <= zm$FUEL_PRICE[i] && stable_lower <=zm$FUEL_PRICE_ch_1m[i] && zm$FUEL_PRICE_ch_1m[i]<= stable_upper, "high",zm$cat[i])

}

write.csv(zm,"Categories_real_thr_360.csv")


data2$cat<-NA
for (i in 1:108){
  data2$cat<-ifelse(data2$dd==zm$dd[i],zm$cat[i],data2$cat)
}


write.csv(data2,"data_categories_mdiff_real.csv")



data2$diff_1m<-NA
data2$diff_2m<-NA
data2$diff_3m<-NA
data2$diff_4m<-NA
data2$diff_5m<-NA
data2$diff_6m<-NA


for (i in 1:108){
  data2$diff_1m<-ifelse(data2$dd==zm$dd[i],zm$FUEL_PRICE_ch_1m[i],data2$diff_1m)
  data2$diff_2m<-ifelse(data2$dd==zm$dd[i],zm$FUEL_PRICE_ch_2m[i],data2$diff_2m)
  data2$diff_3m<-ifelse(data2$dd==zm$dd[i],zm$FUEL_PRICE_ch_3m[i],data2$diff_3m)
  data2$diff_4m<-ifelse(data2$dd==zm$dd[i],zm$FUEL_PRICE_ch_4m[i],data2$diff_4m)
  data2$diff_5m<-ifelse(data2$dd==zm$dd[i],zm$FUEL_PRICE_ch_5m[i],data2$diff_5m)
  data2$diff_6m<-ifelse(data2$dd==zm$dd[i],zm$FUEL_PRICE_ch_6m[i],data2$diff_6m)
  
}



par(mfrow=c(2,4))

#income
MM<-subset(data2, select=c(DEMO_INCOME,cat))

MM$DEMO_INCOME1=0
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="Prefer not to answer" ,19,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$15,000 or less" | MM$DEMO_INCOME=="$15,001-$20,000" | MM$DEMO_INCOME=="21",1,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$20,001-$25,000" ,2,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$25,001-$30,000" ,3,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$30,001-$35,000" ,4,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$35,001-$40,000" ,5,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$40,001-$45,000" ,6,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$45,001-$50,000" ,7,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$50,001-$55,000" ,8,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$55,001-$65,000" ,9,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$65,001-$75,000" ,10,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$75,001-$85,000" ,11,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$85,001-$100,000" ,12,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$100,001-$125,000" ,13,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$125,001-$150,000" ,14,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$150,001-$200,000" ,15,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$200,001-$300,000" ,16,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$300,001-$400,000" ,17,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="Over $400,000" | MM$DEMO_INCOME=="22" |MM$DEMO_INCOME=="23",18,MM$DEMO_INCOME1)


par(mar= c(8,4.8,4.1,2.1))
t<-table(MM$cat,MM$DEMO_INCOME1)
t<-t[c(4,1,2,3),-1]


barplot(prop.table(t,margin=1) ,las = 2,cex.names = 0.85,col=c("ivory3","lightgrey","azure3","lavenderblush3"),beside= TRUE,cex.axis = 0.8,axis.lty = 3,font=2,names.arg = c("","","","","","","","","","","","","","","","","","",""),ylab="Density",ylim=c(0,0.2))
mtext("Income distribution among buyers in each period", side=3, line=1,at=(55),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,-0.001), fill=c("ivory3","lightgrey","azure3","lavenderblush3"),legend=c("rising (n= 346378, no response= 46571)","falling (n= 254427, no response= 34647)","high (n= 304200 , no response= 50019)","low (n= 433988, no response= 80389)"),bty="n",cex=1.2, pt.cex=1.2)


end_point = 94        #Angle the reasons text
text(seq(3.1,end_point,by=5.0),par("usr")[3],adj=c(1.1,1.1),labels=c("$20,000 or less","$20,001-$25,000","$25,001-$30,000","$30,001-$35,000","$35,001-$40,000","$40,001-$45,000","$45,001-$50,000","$50,001-$55,000","$55,001-$65,000","$65,001-$75,000","$75,001-$85,000","$85,001-$100,000","$100,001-$125,000","$125,001-$150,000","$150,001-$200,000","$200,001-$300,000","$300,001-$400,000","Over $400,000","Prefer not to answer"),srt=45,xpd=TRUE,cex=1.2,font=2)




#Age


dfalling<-subset(data2,select=c(DEMO_AGE1),cat=="falling")
drising<-subset(data2,select=c(DEMO_AGE1),cat=="rising")
dlow<-subset(data2,select=c(DEMO_AGE1),cat=="low")
dhigh<-subset(data2,select=c(DEMO_AGE1),cat=="high")

plot(density(na.omit(dfalling$DEMO_AGE1)), col = "seagreen",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))
par(new=TRUE)
plot(density(na.omit(drising$DEMO_AGE1)), col = "maroon3",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))
par(new=TRUE)
plot(density(na.omit(dlow$DEMO_AGE1)), col = "orange",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))
par(new=TRUE)
plot(density(na.omit(dhigh$DEMO_AGE1)), col = "lightblue",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))


mtext("Age distribution among buyers for each period", side=1, line=3,at=(50),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,-0.001), fill=c("maroon3","seagreen","lightblue","orange"),legend=c("rising (n= 346378 , no response= 51656)","falling (n= 254427, no response= 39050)","high (n= 304200, no response= 55594)","low (n=433988, no response= 82899)"),bty="n",cex=1,pt.cex = 0.5)


#Education
par(mar= c(12,4.8,4.1,2.1))

MM<-subset(data2, select=c(DEMO_EDUCATION,cat))

MM$DEMO_EDUCATION1=3
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="",0,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Grade school" | MM$DEMO_EDUCATION=="Some high school" | MM$DEMO_EDUCATION=="High school graduate",1,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Trade school graduate" | MM$DEMO_EDUCATION=="Some trade school" ,2,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Graduate with bachelor's degree"  ,4,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Some post-graduate study" |  MM$DEMO_EDUCATION=="Post-graduate degree",5,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Other"  ,6,MM$DEMO_EDUCATION1)

t1<-table(MM$cat,MM$DEMO_EDUCATION1)
t1<-t1[c(4,1,2,3),-1]



barplot(prop.table(t1,margin=1) ,cex.names = 0.85,col=c("lightgrey","lightblue","lightcyan1","mistyrose"),beside= TRUE,cex.axis = 0.8,axis.lty = 3,font=2,names.arg = c("","","","","",""),ylim=c(0,0.35))
mtext("Education level among buyers for each period", side=3, line=1,at=(18),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,0.01), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising (n= 346378, no response= 53316)","falling (n= 254427, no response= 39511)","high (n= 304200, no response= 55939)","low (n= 433988 , no response= 87033)"),bty="n",cex=1,pt.cex=0.8)


end_point = 30      #Angle the text
text(seq(3.5,end_point,by=5),par("usr")[3],adj=c(1.1,1.1),labels=c("High School and below","Trade School studies","Undergrad studies","Graduate with Bachelor's","Post graduate studies","Other"),srt=45,xpd=TRUE,cex=1,font=4)


#Location

t2<-table(data2$cat,data2$DEMO_LOCATION)
t2<-t2[c(4,1,2,3),c(2,4,5,3)]


barplot(prop.table(t2,margin=1) ,cex.names = 0.85,col=c("lightgrey","lightblue","lightcyan1","mistyrose"),beside= TRUE,cex.axis = 0.8,ylim=c(0,0.7),axis.lty = 3,font=2)
mtext("Location type among buyers for each period", side=3, line=1,at=(12),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,0.01), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising (n= 346378, no response= 33582)","falling (n= 254427, no response= 27268)","high (n=304200, no response= 40981)","low (n= 433988, no response= 64277)"),bty="n",cex=1,pt.cex=0.8)

#Occupation
par(mar= c(12,4.8,4.1,2.1))
t4<-table(data2$cat,data2$DEMO_OCCUPATION)
t4<-t4[c(4,1,2,3),-1]


barplot(prop.table(t4,margin=1) ,cex.names = 0.85,col=c("lightgrey","lightblue","lightcyan1","mistyrose"),beside= TRUE,cex.axis = 0.8,ylim=c(0,0.3),axis.lty = 3,font=2,names.arg = c("","","","","","","","","","","","","","","","","","","",""),ylab="Density")
mtext("Occupation among buyers for each period", side=3, line=1,at=(60),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,0.01), fill=c("lightgrey","lightblue","lightcyan1","mistyrose","seashell"),legend=c("rising (n= 346378, no response= 41269)","falling (n= 254427, no response= 32036)","high (n=304200, no response= 46790)","low (n= 433988, no response= 72814)"),bty="n",cex=1,pt.cex=1)


end_point = 103    #Angle the reasons text
text(seq(3.5,end_point,by=5),par("usr")[3],adj=c(1.1,1.1),labels=colnames(t4),srt=45,xpd=TRUE,cex=1,font=4)

#gender
par(mar= c(8,4,4.1,2.1))
t5<-table(data2$cat,data2$DEMO_GENDER1)
t5<-t5[c(4,1,2,3),-1]

barplot(prop.table(t5,margin=1),col = c("lightgrey","lightblue","lightcyan1","mistyrose"),beside=TRUE,font=2 ,lwd=3,main="",ylab="Fraction of total")

mtext("Gender distribution among buyers for each period", side=3, line=1,at=(6),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,-0.001), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising (n= 346378, no response= 45374)","falling (n= 254427, no response= 35867)","high (n=304200, no response= 50596)","low (n= 433988, no response= 78127)"),bty="n",cex=1,pt.cex = 1)

#marital status
t6<-table(data2$cat,data2$DEMO_MARITAL)
t6<-t6[c(4,1,2,3),c(3,4,2)]

barplot(prop.table(t6,margin=1),col = c("lightgrey","lightblue","lightcyan1","mistyrose"),beside=TRUE,font=2 ,lwd=3,main="",ylab="Fraction of total")

mtext("Marital status among buyers for each period", side=3, line=1,at=(9),font=2,cex = 1.2)
legend("topright",inset=c(0.01,-0.001), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising (n= 346378, no response= 30055)","falling (n= 254427, no response= 24996)","high (n=304200, no response= 38017)","low (n= 433988, no response= 59099)"),bty="n",cex=1,pt.cex = 1)

#--------different threshold

par(mfrow=c(1,1))

#income
MM<-subset(data2, select=c(DEMO_INCOME,cat))

MM$DEMO_INCOME1=0
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="Prefer not to answer" ,19,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$15,000 or less" | MM$DEMO_INCOME=="$15,001-$20,000" | MM$DEMO_INCOME=="21",1,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$20,001-$25,000" ,2,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$25,001-$30,000" ,3,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$30,001-$35,000" ,4,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$35,001-$40,000" ,5,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$40,001-$45,000" ,6,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$45,001-$50,000" ,7,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$50,001-$55,000" ,8,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$55,001-$65,000" ,9,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$65,001-$75,000" ,10,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$75,001-$85,000" ,11,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$85,001-$100,000" ,12,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$100,001-$125,000" ,13,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$125,001-$150,000" ,14,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$150,001-$200,000" ,15,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$200,001-$300,000" ,16,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="$300,001-$400,000" ,17,MM$DEMO_INCOME1)
MM$DEMO_INCOME1=ifelse(MM$DEMO_INCOME=="Over $400,000" | MM$DEMO_INCOME=="22" |MM$DEMO_INCOME=="23",18,MM$DEMO_INCOME1)


par(mar= c(8,4.8,4.1,2.1))
t<-table(MM$cat,MM$DEMO_INCOME1)
t<-t[c(4,1,2,3),-1]


barplot(prop.table(t,margin=1) ,las = 2,cex.names = 0.85,col=c("ivory3","lightgrey","azure3","lavenderblush3"),beside= TRUE,cex.axis = 0.8,axis.lty = 3,font=2,names.arg = c("","","","","","","","","","","","","","","","","","",""),ylab="Density",ylim=c(0,0.2))
mtext("Income distribution", side=3, line=1,at=(55),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,-0.001), fill=c("ivory3","lightgrey","azure3","lavenderblush3"),legend=c("rising","falling","high","low"),bty="n",cex=1.2, pt.cex=1.2)


end_point = 94        #Angle the reasons text
text(seq(3.1,end_point,by=5.0),par("usr")[3],adj=c(1.1,1.1),labels=c("$20,000 or less","$20,001-$25,000","$25,001-$30,000","$30,001-$35,000","$35,001-$40,000","$40,001-$45,000","$45,001-$50,000","$50,001-$55,000","$55,001-$65,000","$65,001-$75,000","$75,001-$85,000","$85,001-$100,000","$100,001-$125,000","$125,001-$150,000","$150,001-$200,000","$200,001-$300,000","$300,001-$400,000","Over $400,000","Prefer not to answer"),srt=45,xpd=TRUE,cex=1.2,font=2)



#------------Demographics by categories-------------------
#Age


dfalling<-subset(data2,select=c(DEMO_AGE1),cat=="falling")
drising<-subset(data2,select=c(DEMO_AGE1),cat=="rising")
dlow<-subset(data2,select=c(DEMO_AGE1),cat=="low")
dhigh<-subset(data2,select=c(DEMO_AGE1),cat=="high")

plot(density(na.omit(dfalling$DEMO_AGE1)), col = "seagreen",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))
par(new=TRUE)
plot(density(na.omit(drising$DEMO_AGE1)), col = "maroon3",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))
par(new=TRUE)
plot(density(na.omit(dlow$DEMO_AGE1)), col = "orange",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))
par(new=TRUE)
plot(density(na.omit(dhigh$DEMO_AGE1)), col = "lightblue",lwd=3,main="",xlab="",xlim=c(0,100),ylim=c(0,0.035))


mtext("Age distribution", side=1, line=3,at=(50),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,-0.001), fill=c("maroon3","seagreen","lightblue","orange"),legend=c("rising","falling","high","low"),bty="n",cex=1,pt.cex = 0.5)


#Education
par(mar= c(12,4.8,4.1,2.1))

MM<-subset(data2, select=c(DEMO_EDUCATION,cat))

MM$DEMO_EDUCATION1=3
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="",0,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Grade school" | MM$DEMO_EDUCATION=="Some high school" | MM$DEMO_EDUCATION=="High school graduate",1,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Trade school graduate" | MM$DEMO_EDUCATION=="Some trade school" ,2,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Graduate with bachelor's degree"  ,4,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Some post-graduate study" |  MM$DEMO_EDUCATION=="Post-graduate degree",5,MM$DEMO_EDUCATION1)
MM$DEMO_EDUCATION1=ifelse(MM$DEMO_EDUCATION=="Other"  ,6,MM$DEMO_EDUCATION1)

t1<-table(MM$cat,MM$DEMO_EDUCATION1)
t1<-t1[c(4,1,2,3),-1]



barplot(prop.table(t1,margin=1) ,cex.names = 0.85,col=c("lightgrey","lightblue","lightcyan1","mistyrose"),beside= TRUE,cex.axis = 0.8,axis.lty = 3,font=2,names.arg = c("","","","","",""),ylim=c(0,0.35))
mtext("Education level", side=3, line=1,at=(18),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,0.01), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising","falling","high","low"),bty="n",cex=1,pt.cex=0.8)


end_point = 30      #Angle the text
text(seq(3.5,end_point,by=5),par("usr")[3],adj=c(1.1,1.1),labels=c("High School and below","Trade School studies","Undergrad studies","Graduate with Bachelor's","Post graduate studies","Other"),srt=45,xpd=TRUE,cex=1,font=4)


#Location

t2<-table(data2$cat,data2$DEMO_LOCATION)
t2<-t2[c(4,1,2,3),c(2,4,5,3)]


barplot(prop.table(t2,margin=1) ,cex.names = 0.85,col=c("lightgrey","lightblue","lightcyan1","mistyrose"),beside= TRUE,cex.axis = 0.8,ylim=c(0,0.7),axis.lty = 3,font=2)
mtext("Location type", side=3, line=1,at=(12),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,0.01), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising","falling","high","low"),bty="n",cex=1,pt.cex=0.8)

#Occupation
par(mar= c(12,4.8,4.1,2.1))
t4<-table(data2$cat,data2$DEMO_OCCUPATION)
t4<-t4[c(4,1,2,3),-1]


barplot(prop.table(t4,margin=1) ,cex.names = 0.85,col=c("lightgrey","lightblue","lightcyan1","mistyrose"),beside= TRUE,cex.axis = 0.8,ylim=c(0,0.3),axis.lty = 3,font=2,names.arg = c("","","","","","","","","","","","","","","","","","","",""),ylab="Density")
mtext("Occupation", side=3, line=1,at=(60),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,0.01), fill=c("lightgrey","lightblue","lightcyan1","mistyrose","seashell"),legend=c("rising","falling","high","low"),bty="n",cex=1,pt.cex=1)


end_point = 103    #Angle the reasons text
text(seq(3.5,end_point,by=5),par("usr")[3],adj=c(1.1,1.1),labels=colnames(t4),srt=45,xpd=TRUE,cex=1,font=4)

#gender
par(mar= c(8,4,4.1,2.1))
t5<-table(data2$cat,data2$DEMO_GENDER1)
t5<-t5[c(4,1,2,3),-1]

barplot(prop.table(t5,margin=1),col = c("lightgrey","lightblue","lightcyan1","mistyrose"),beside=TRUE,font=2 ,lwd=3,main="",ylab="Fraction of total")

mtext("Gender distribution", side=3, line=1,at=(6),font=2,cex = 1.2)
legend("topleft",inset=c(0.01,-0.001), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising","falling","high","low"),bty="n",cex=1,pt.cex = 1)

#marital status
t6<-table(data2$cat,data2$DEMO_MARITAL)
t6<-t6[c(4,1,2,3),c(3,4,2)]

barplot(prop.table(t6,margin=1),col = c("lightgrey","lightblue","lightcyan1","mistyrose"),beside=TRUE,font=2 ,lwd=3,main="",ylab="Fraction of total")

mtext("Marital status", side=3, line=1,at=(9),font=2,cex = 1.2)
legend("topright",inset=c(0.01,-0.001), fill=c("lightgrey","lightblue","lightcyan1","mistyrose"),legend=c("rising","falling","high","low"),bty="n",cex=1,pt.cex = 1)




#-----------MNPS-----------------
data2$cat<-NA
for (i in 1:108){
  data2$cat<-ifelse(data2$dd==zm$dd[i],zm$cat[i],data2$cat)
}

library(twang)
data(data2)
set.seed(1)

data2<-na.omit(data2)
mnp.data2<-mnps(as.factor(cat)~DEMO_AGE1+DEMO_INCOME+DEMO_GENDER1+DEMO_EDUCATION,data=data2,estimand="ATE",verbose=FALSE,stop.method = c("es.mean","ks.mean"),n.trees=3000)
