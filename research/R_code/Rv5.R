setwd("C:/Users/408574/Dropbox/Gas Price Attitudes/Data")
data1<-read.csv ("data_adjusted_real.csv")
library(ggplot2)
library(zoo)
library(plotly)

#----------Plotting time difference between purchasing and sending back survey--------------------------

data1$date<-as.Date(paste(data1$ADMARK_RV_BYR,data1$ADMARK_RV_BMON,"15"),format="%Y%m%d") #convert purchasing date to date format
data1$dateR<-as.Date(as.character(data1$DCO_RECEIPT_DATE),"%Y %m %d") #convert received date to date format

z<-data1$dateR-data1$date  # vector of time difference for each respondent

hist(as.numeric(z),breaks=100,main="time difference between purchasing a vehicling     
     and sending back survey",xlab="time difference (day)")                  #histogram of time difference

plot_ly(x=as.numeric(z),type="histogram")

#-----------Plot average purchaser income over time-----------------------------------------------------

MM<-data1


MM$date<-as.Date(as.character(MM$DCO_RECEIPT_DATE),"%Y %m %d")
MM$dd<-(format(MM$date,format = "%Y%m" ))


MM$DEMO_INCOME1=0
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

MM1<-subset(MM,select=c(dd,DEMO_INCOME1),DEMO_INCOME1 != 0)

incomeav<-aggregate(DEMO_INCOME1 ~ dd, MM1, mean)
incomeav$date<-as.yearmon(incomeav$dd,"%Y%m")


b<-ggplot(data=incomeav,aes(x=date, y=DEMO_INCOME1),axis=FALSE) 
v<- b +geom_path(lineend="butt",linejoin="round",linemitre=1)+geom_line(aes(color="brown"),lwd=1.5)+theme(axis.text.x=element_text(size=15))+labs(x="Date",y="Income")+ggtitle("Average income over time")
x<-v+scale_y_continuous(breaks = 1:18,labels=c("$20,000 or less","$20,001-$25,000","25,001-$30,000","30,001-$35,000","35,001-$40,000","40,001-$45,000","$45,001-$50,000","50,001-$55,000","$55,001-$65,000","$65,001-$75,000","75,001-$85,000","$85,001-$100,000","$100,001-$125,000","$125,001-$150,000","$150,001-$200,000","$200,001-$300,000","$300,001-$400,000","Over $400,000"))




#--------Plot average age of purchaser over time----------------------------------------------------------

MM$date<-as.Date(as.character(MM$DCO_RECEIPT_DATE),"%Y %m %d")
data1$dd<-(format(data1$dateR,format = "%Y%m" ))

ageav<-aggregate(DEMO_AGE1 ~ dd, data1, mean)
ageav$date<-as.yearmon(ageav$dd,"%Y%m")

b<-ggplot(data=ageav,aes(x=date, y=DEMO_AGE1),axis=FALSE) 
v<- b +geom_path(lineend="butt",linejoin="round",linemitre=1)+geom_line(color="brown",lwd=1.5)+theme(axis.text.x=element_text(size=15),plot.margin=unit(c(1,2,1,4),"cm"))+labs(x="Date",y="Age (years)")+ggtitle("Average age over time")


#------- Plot fuel price change compare to previous month------------------------------------

data1$dd<-(format(data1$dateR,format = "%Y%m" ))
data2<-subset(data1,VIN_RV_FUEL_TYPE=="Gas")


zm<-aggregate(FUEL_PRICE_REAL ~ dd, data2, mean) #fuel price (mean)
zm$date<-as.yearmon(zm$dd,"%Y%m")


zm$diff<-NA
zm$per<-NA

for (i in 1:108){
  zm$diff[i+1]<-zm$FUEL_PRICE_REAL[i+1]-zm$FUEL_PRICE_REAL[i]
  zm$per[i+1]<- (zm$FUEL_PRICE_REAL[i+1]-zm$FUEL_PRICE_REAL[i])*100/zm$FUEL_PRICE_REAL[i]
}

#plot
b<-ggplot(data=zm,aes(x=date, y=diff),axis=FALSE) 
v<- b +geom_path(lineend="butt",linejoin="round",linemitre=1)+theme(axis.text.x=element_text(size=15),plot.margin=unit(c(1,2,1,1),"cm"))+labs(x="Date",y="Fuel Price (cents)")+ggtitle("Fuel Price Difference (ICE Purchasers)")

b<-ggplot(data=zm,aes(x=date, y=per),axis=FALSE) 
v<- b +geom_path(lineend="butt",linejoin="round",linemitre=1)+theme(axis.text.x=element_text(size=15),plot.margin=unit(c(1,2,1,1),"cm"))+labs(x="Date",y="Fuel Price (percentage)")+ggtitle("% Fuel Price Difference (ICE Purchasers)")


hist(zm$per,main= "Number of months for observed price change",xlab="observed price change percentage",breaks = 20,col="steelblue4")
write.csv(zm,"Price Change Info.csv")

hist(zm$diff,main= "Number of months for observed price change",xlab="observed price change",breaks = 20,col="steelblue4")



data2$cat<-NA
data2$diff<-NA

for (i in 2:108){
  data2$cat<-ifelse(data2$dd==zm$dd[i],zm$per[i],data2$cat)
}

for (i in 2:108){
  data2$diff<-ifelse(data2$dd==zm$dd[i],zm$diff[i],data2$diff)
}


hist(data2$cat,xlim=c(-30,15),xlab= "% of Fuel price change vs previous month", main="Share of each priod of price change of respondents",col="steelblue2" )
hist(data2$diff,xlab= "Fuel price change vs previous month", main="Share of each priod of price change of respondents",col="steelblue2" ,ylim=c(0,250000),xlim=c(-80,50))

#-----Summary Stat of fuel price over time--------------

SumStat<-aggregate(FUEL_PRICE ~ dd, data2, mean)
StatSD<-aggregate(FUEL_PRICE ~ dd, data2, sd)
StatMedian<-aggregate(FUEL_PRICE ~ dd, data2, median)

SumStat<-cbind(SumStat,StatSD$FUEL_PRICE,StatMedian$FUEL_PRICE,zm$date)
colnames(SumStat)<-c("dd","Mean","SD","Median","Date")
write.csv(SumStat,"Summary Stat Fuel Price over time.csv")

SumStatdiff<-summary(zm$diff)


#--------------Unemployment Rate-------------
setwd("C:/Users/408574/Dropbox/Gas Price Attitudes")
SR<-read.csv ("SR.csv")

SR$Date1<-as.Date(SR$Date,format="%m/ %d/ %Y")

plot(SR$Date1,SR$X,type="l",xlab="Date",ylab="Unemployment Rate",main="Unemployment rate over time",lwd=2)

#--------------GDP Plot--------

setwd("C:/Users/408574/Dropbox/Gas Price Attitudes")
GDP<-read.csv ("GDP.csv")
GDP$X<-as.yearqtr(GDP$X,format="%Y-%m")
plot(GDP$X,GDP$GDP.Now,type="l",xlab="Date",ylab="GDP",main="GDP percent change from preceding period",lwd=2)

