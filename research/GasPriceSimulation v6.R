# For David # setwd("~/Dropbox (MIT)/Research - Shared/Gas Price Attitudes/Data") 
# For James # setwd("~/Dropbox (MIT)/Gas Price Attitudes/Data")
library(lubridate)
library(stargazer)
library(plyr)
# Load data
gas_price_data <- read.csv("data_categories_real.csv")

count(gas_price_data, 'VIN_RV_FUEL_TYPE')

# PRE-PROCESSING OF DATA
#-----------------------
gas_price_data$DEMO_EDUCATION1 = 0
gas_price_data$DEMO_EDUCATION1=ifelse(gas_price_data$DEMO_EDUCATION=="Grade school" | gas_price_data$DEMO_EDUCATION=="Some high school", "a.Less than High school",gas_price_data$DEMO_EDUCATION1)
gas_price_data$DEMO_EDUCATION1=ifelse(gas_price_data$DEMO_EDUCATION=="High school graduate" , "b.High school graduate",gas_price_data$DEMO_EDUCATION1)
gas_price_data$DEMO_EDUCATION1=ifelse(gas_price_data$DEMO_EDUCATION=="Some community college" | gas_price_data$DEMO_EDUCATION=="Some university" | gas_price_data$DEMO_EDUCATION=="Some trade school", "c.Some college, no degree",gas_price_data$DEMO_EDUCATION1)
gas_price_data$DEMO_EDUCATION1=ifelse(gas_price_data$DEMO_EDUCATION=="Graduate with two-year degree" | gas_price_data$DEMO_EDUCATION=="Trade school graduate" , "d.Associate degree",gas_price_data$DEMO_EDUCATION1)
gas_price_data$DEMO_EDUCATION1=ifelse(gas_price_data$DEMO_EDUCATION=="Graduate with bachelor's degree" | gas_price_data$DEMO_EDUCATION=="Some post-graduate study" , "e.Bachelor's degree",gas_price_data$DEMO_EDUCATION1)
gas_price_data$DEMO_EDUCATION1=ifelse(gas_price_data$DEMO_EDUCATION=="Post-graduate degree" , "f.Post-graduate degree",gas_price_data$DEMO_EDUCATION1)
gas_price_data$DEMO_EDUCATION1 <- as.factor(gas_price_data$DEMO_EDUCATION1)
gas_price_data <- within(gas_price_data, DEMO_EDUCATION1 <- relevel(DEMO_EDUCATION1, ref = "0"))

gas_price_data&VIN_RV_BODY1 = gas_price_data&VIN_RV_BODY



# Subset dataset to only people who purchased a Gasoline vehicle
gas_price_data <- subset(gas_price_data, VIN_RV_FUEL_TYPE == "Gas")
gas_price_data$GPM_COMBINED = 1 / gas_price_data$MPG_COMBINED
gas_price_data$RV_ESTMILES1 <- gas_price_data$RV_ESTMILES / 10000
gas_price_data$FIN_PRICE_EDITED1 <- gas_price_data$FIN_PRICE_EDITED / 10000

# Add state names
gas_price_data$State_ = ifelse(gas_price_data$ADMARK_STATE == 1, "AL",
                                   ifelse(gas_price_data$ADMARK_STATE == 2, "AZ",
                                          ifelse(gas_price_data$ADMARK_STATE == 3, "AR",
                                                 ifelse(gas_price_data$ADMARK_STATE == 4, "CA",
                                                        ifelse(gas_price_data$ADMARK_STATE == 5, "CO",
                                                               ifelse(gas_price_data$ADMARK_STATE == 6, "CT",
                                                                      ifelse(gas_price_data$ADMARK_STATE == 7, "DC",
                                                                             ifelse(gas_price_data$ADMARK_STATE == 8, "DE",
                                                                                    ifelse(gas_price_data$ADMARK_STATE == 9, "FL",
                                                                                           ifelse(gas_price_data$ADMARK_STATE == 10, "GA",
                                                                                                  ifelse(gas_price_data$ADMARK_STATE == 11, "ID",
                                                                                                         ifelse(gas_price_data$ADMARK_STATE == 12, "IL",
                                                                                                                ifelse(gas_price_data$ADMARK_STATE == 13, "IN",
                                                                                                                       ifelse(gas_price_data$ADMARK_STATE == 14, "IA",
                                                                                                                              ifelse(gas_price_data$ADMARK_STATE == 15, "KS",
                                                                                                                                     ifelse(gas_price_data$ADMARK_STATE == 16, "KY",
                                                                                                                                            ifelse(gas_price_data$ADMARK_STATE == 17, "LA",
                                                                                                                                                   ifelse(gas_price_data$ADMARK_STATE == 18, "ME",
                                                                                                                                                          ifelse(gas_price_data$ADMARK_STATE == 19, "MD",
                                                                                                                                                                 ifelse(gas_price_data$ADMARK_STATE == 20, "MA",
                                                                                                                                                                        ifelse(gas_price_data$ADMARK_STATE == 21, "MI",
                                                                                                                                                                               ifelse(gas_price_data$ADMARK_STATE == 22, "MN",
                                                                                                                                                                                      ifelse(gas_price_data$ADMARK_STATE == 23, "MS",
                                                                                                                                                                                             ifelse(gas_price_data$ADMARK_STATE == 24, "MO",
                                                                                                                                                                                                    ifelse(gas_price_data$ADMARK_STATE == 25, "MT",
                                                                                                                                                                                                           ifelse(gas_price_data$ADMARK_STATE == 26, "NE",
                                                                                                                                                                                                                  ifelse(gas_price_data$ADMARK_STATE == 27, "NV",
                                                                                                                                                                                                                         ifelse(gas_price_data$ADMARK_STATE == 28, "NH",
                                                                                                                                                                                                                                ifelse(gas_price_data$ADMARK_STATE == 29, "NJ",
                                                                                                                                                                                                                                       ifelse(gas_price_data$ADMARK_STATE == 30, "NM",
                                                                                                                                                                                                                                              ifelse(gas_price_data$ADMARK_STATE == 31, "NY",NA)))))))))))))))))))))))))))))))
gas_price_data$State_ =         ifelse(gas_price_data$ADMARK_STATE == 32, "NC",
                                           ifelse(gas_price_data$ADMARK_STATE == 33, "ND",
                                                  ifelse(gas_price_data$ADMARK_STATE == 34, "OH",
                                                         ifelse(gas_price_data$ADMARK_STATE == 35, "OK",
                                                                ifelse(gas_price_data$ADMARK_STATE == 36, "OR",
                                                                       ifelse(gas_price_data$ADMARK_STATE == 37, "PA", 
                                                                              ifelse(gas_price_data$ADMARK_STATE == 38, "RI",
                                                                                     ifelse(gas_price_data$ADMARK_STATE == 39, "SC",
                                                                                            ifelse(gas_price_data$ADMARK_STATE == 40, "SD",
                                                                                                   ifelse(gas_price_data$ADMARK_STATE == 41, "TN",
                                                                                                          ifelse(gas_price_data$ADMARK_STATE == 42, "TX",
                                                                                                                 ifelse(gas_price_data$ADMARK_STATE == 43, "UR",
                                                                                                                        ifelse(gas_price_data$ADMARK_STATE == 44, "VT",
                                                                                                                               ifelse(gas_price_data$ADMARK_STATE == 45, "VA",
                                                                                                                                      ifelse(gas_price_data$ADMARK_STATE == 46, "WA",
                                                                                                                                             ifelse(gas_price_data$ADMARK_STATE == 47, "WV",
                                                                                                                                                    ifelse(gas_price_data$ADMARK_STATE == 48, "WI",
                                                                                                                                                           ifelse(gas_price_data$ADMARK_STATE == 49, "WY",
                                                                                                                                                                  ifelse(gas_price_data$ADMARK_STATE == 50, "AK",
                                                                                                                                                                         ifelse(gas_price_data$ADMARK_STATE == 51, "HI", gas_price_data$State_ ))))))))))))))))))))


gas_price_data$State_ <- as.factor(gas_price_data$State_)
gas_price_data <- within(gas_price_data, State_ <- relevel(State_, ref = "IL"))

gas_price_data$DEMO_GENDER2=0
gas_price_data$DEMO_GENDER2=ifelse(gas_price_data$DEMO_GENDER1=="Male" ,"Male",gas_price_data$DEMO_GENDER2)
gas_price_data$DEMO_GENDER2 <- as.factor(gas_price_data$DEMO_GENDER2)
gas_price_data <- within(gas_price_data, DEMO_GENDER2 <- relevel(DEMO_GENDER2, ref = "0"))

gas_price_data$DEMO_INCOME1=0
gas_price_data$DEMO_INCOME1=ifelse(gas_price_data$DEMO_INCOME=="Prefer not to answer" ,"f.PrefNoAnswer",gas_price_data$DEMO_INCOME1)
gas_price_data$DEMO_INCOME1=ifelse(gas_price_data$DEMO_INCOME=="$15,000 or less" | gas_price_data$DEMO_INCOME=="$15,001-$20,000" | gas_price_data$DEMO_INCOME=="$20,001-$25,000" | gas_price_data$DEMO_INCOME=="21","a.Under25k",gas_price_data$DEMO_INCOME1)
gas_price_data$DEMO_INCOME1=ifelse(gas_price_data$DEMO_INCOME=="$25,001-$30,000" | gas_price_data$DEMO_INCOME=="$30,001-$35,000" | gas_price_data$DEMO_INCOME=="$35,001-$40,000" | gas_price_data$DEMO_INCOME=="$40,001-$45,000" | gas_price_data$DEMO_INCOME=="$45,001-$50,000" | gas_price_data$DEMO_INCOME=="$50,001-$55,000" ,"b.25k_55k",gas_price_data$DEMO_INCOME1)
gas_price_data$DEMO_INCOME1=ifelse(gas_price_data$DEMO_INCOME=="$55,001-$65,000" | gas_price_data$DEMO_INCOME=="$65,001-$75,000" | gas_price_data$DEMO_INCOME=="$75,001-$85,000" | gas_price_data$DEMO_INCOME=="$85,001-$100,000" ,"c.55k_100k",gas_price_data$DEMO_INCOME1)
gas_price_data$DEMO_INCOME1=ifelse(gas_price_data$DEMO_INCOME=="$100,001-$125,000" | gas_price_data$DEMO_INCOME=="$125,001-$150,000" | gas_price_data$DEMO_INCOME=="$150,001-$200,000","d.100k_200k",gas_price_data$DEMO_INCOME1)
gas_price_data$DEMO_INCOME1=ifelse(gas_price_data$DEMO_INCOME=="$200,001-$300,000" | gas_price_data$DEMO_INCOME=="$300,001-$400,000" | gas_price_data$DEMO_INCOME=="Over $400,000" | gas_price_data$DEMO_INCOME=="22" |gas_price_data$DEMO_INCOME=="23","e.Over200k",gas_price_data$DEMO_INCOME1)
gas_price_data$DEMO_INCOME1 <- as.factor(gas_price_data$DEMO_INCOME1)
gas_price_data <- within(gas_price_data, DEMO_INCOME1 <- relevel(DEMO_INCOME1, ref = "0"))

gas_price_data$DEMO_LOCATION1=0
gas_price_data$DEMO_LOCATION1=ifelse(gas_price_data$DEMO_LOCATION=="Farming area" ,"a.Farming area",gas_price_data$DEMO_LOCATION1)
gas_price_data$DEMO_LOCATION1=ifelse(gas_price_data$DEMO_LOCATION=="Metropolitan city" ,"d.Metropolitan city",gas_price_data$DEMO_LOCATION1)
gas_price_data$DEMO_LOCATION1=ifelse(gas_price_data$DEMO_LOCATION=="Small town or rural city" ,"b.Small town or rural city",gas_price_data$DEMO_LOCATION1)
gas_price_data$DEMO_LOCATION1=ifelse(gas_price_data$DEMO_LOCATION=="Suburban community of a larger city" ,"c.Suburban community of a larger city",gas_price_data$DEMO_LOCATION1)
gas_price_data$DEMO_LOCATION1 <- as.factor(gas_price_data$DEMO_LOCATION1)
gas_price_data <- within(gas_price_data, DEMO_LOCATION1 <- relevel(DEMO_LOCATION1, ref = "0"))

gas_price_data$DEMO_MARITAL1=0
gas_price_data$DEMO_MARITAL1=ifelse(gas_price_data$DEMO_MARITAL=="Divorced, widowed, separated" ,"b.Divorced, widowed, separated",gas_price_data$DEMO_MARITAL1)
gas_price_data$DEMO_MARITAL1=ifelse(gas_price_data$DEMO_MARITAL=="Married" ,"a.Married",gas_price_data$DEMO_MARITAL1)
gas_price_data$DEMO_MARITAL1=ifelse(gas_price_data$DEMO_MARITAL=="Single, never married" ,"c.Single, never married",gas_price_data$DEMO_MARITAL1)
gas_price_data$DEMO_MARITAL1 <- as.factor(gas_price_data$DEMO_MARITAL1)
gas_price_data <- within(gas_price_data, DEMO_MARITAL1 <- relevel(DEMO_MARITAL1, ref = "0"))


# SUMMARY STATISTICS
#-------------------
# Number of responses by state
table(gas_price_data$State_)
# Number of responses by year / month
table(gas_price_data$dd) 

count(gas_price_data, 'DEMO_EDUCATION')
count(gas_price_data, 'DEMO_EDUCATION1')
count(gas_price_data, 'DEMO_LOCATION')
count(gas_price_data, 'DEMO_GENDER1')
count(gas_price_data, 'DEMO_INCOME')
count(gas_price_data, 'DEMO_INCOME1')
count(gas_price_data, 'DEMO_MARITAL1')
count(gas_price_data, 'VIN_RV_BODY')


# ALIGNMENT OF GAS PRICES
#------------------------

# Convert DCO_RECEIPT_DATE to R date format
gas_price_data$SurveyReceiptDate <- as.Date(formatC(gas_price_data$DCO_RECEIPT_DATE, 8, 0, "d", 0), "%Y%m%d")

gas_price_data$PurchaseDate <- as.Date(paste(gas_price_data$ADMARK_RV_BYR, gas_price_data$ADMARK_RV_BMON, 1, sep = "-"), format = "%Y-%m-%d")

# Extract survey receipt date Month and Year
gas_price_data$SurveyReceiptMonth <- format(gas_price_data$SurveyReceiptDate,format="%m")
gas_price_data$SurveyReceiptYear <- format(gas_price_data$SurveyReceiptDate,format="%Y")

# Calculate the National Average Gas Price in the month that the survey was received
NatAvgPriceAtSurvey <- aggregate( FUEL_PRICE_REAL ~ SurveyReceiptMonth + SurveyReceiptYear, gas_price_data , mean )
colnames(NatAvgPriceAtSurvey)[3] <- "NatAvgPriceAtSurvey"

# Calculate the State Average Gas Price in the month that the survey was received
StateAvgPriceAtSurvey <- aggregate( FUEL_PRICE_REAL ~ SurveyReceiptMonth + SurveyReceiptYear + ADMARK_STATE, gas_price_data , mean )
colnames(StateAvgPriceAtSurvey)[4] <- "StateAvgPriceAtSurvey"

# Merge National and State Averages back into main gas_price_data table
gas_price_data <- merge(gas_price_data, NatAvgPriceAtSurvey)
gas_price_data <- merge(gas_price_data, StateAvgPriceAtSurvey)

AvgPriceData <- gas_price_data[,c("SurveyReceiptMonth","SurveyReceiptYear","ADMARK_STATE", "NatAvgPriceAtSurvey","StateAvgPriceAtSurvey")]
colnames(AvgPriceData)[1] <- "ADMARK_RV_BMON"
colnames(AvgPriceData)[2] <- "ADMARK_RV_BYR"
colnames(AvgPriceData)[4] <- "NatAvgPriceAtPurchase"
colnames(AvgPriceData)[5] <- "StateAvgPriceAtPurchase"
AvgPriceData$ADMARK_RV_BYR <- strtoi(AvgPriceData$ADMARK_RV_BYR)
AvgPriceData$ADMARK_RV_BMON <- strtoi(AvgPriceData$ADMARK_RV_BMON)
AvgPriceData$ADMARK_RV_BYR <- as.character(AvgPriceData$ADMARK_RV_BYR)
AvgPriceData$ADMARK_RV_BMON <- as.character(AvgPriceData$ADMARK_RV_BMON)
AvgPriceData$ADMARK_STATE <- as.character(AvgPriceData$ADMARK_STATE)

gas_price_data$ADMARK_RV_BYR <- as.character(gas_price_data$ADMARK_RV_BYR)
gas_price_data$ADMARK_RV_BMON <- as.character(gas_price_data$ADMARK_RV_BMON)
gas_price_data$ADMARK_STATE <- as.character(gas_price_data$ADMARK_STATE)

AvgPriceData <- unique(AvgPriceData)

# We lose 200k observations here - why is this?
gas_price_data <- merge(gas_price_data, AvgPriceData, all.x = TRUE)

rm(AvgPriceData)
rm(NatAvgPriceAtSurvey)

# Calculate lagged prices at the point of survey

StateAvgPriceAtSurvey$SurveyDate <- as.Date(paste(StateAvgPriceAtSurvey$SurveyReceiptYear, StateAvgPriceAtSurvey$SurveyReceiptMonth, 1, sep = "-"), format = "%Y-%m-%d")

StateAvgPriceAtSurveyCopy <- StateAvgPriceAtSurvey

StateAvgPriceAtSurvey$SurveyDate1Monthback <- StateAvgPriceAtSurvey$SurveyDate %m-% months(1)
StateAvgPriceAtSurvey$SurveyDate2Monthback <- StateAvgPriceAtSurvey$SurveyDate %m-% months(2)
StateAvgPriceAtSurvey$SurveyDate3Monthback <- StateAvgPriceAtSurvey$SurveyDate %m-% months(3)
StateAvgPriceAtSurvey$SurveyDate4Monthback <- StateAvgPriceAtSurvey$SurveyDate %m-% months(4)
StateAvgPriceAtSurvey$SurveyDate5Monthback <- StateAvgPriceAtSurvey$SurveyDate %m-% months(5)
StateAvgPriceAtSurvey$SurveyDate6Monthback <- StateAvgPriceAtSurvey$SurveyDate %m-% months(6)


colnames(StateAvgPriceAtSurveyCopy)[4] <- "StateAvgPriceSurveyDate1Monthback"
colnames(StateAvgPriceAtSurveyCopy)[5] <- "SurveyDate1Monthback"
StateAvgPriceAtSurveyCopy$SurveyReceiptYear <- NULL
StateAvgPriceAtSurveyCopy$SurveyReceiptMonth <- NULL

Temp <- merge(StateAvgPriceAtSurvey, StateAvgPriceAtSurveyCopy, by=c('ADMARK_STATE', 'SurveyDate1Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtSurveyCopy)[2] <- "StateAvgPriceSurveyDate2Monthback"
colnames(StateAvgPriceAtSurveyCopy)[3] <- "SurveyDate2Monthback"

Temp2 <- merge(Temp, StateAvgPriceAtSurveyCopy, by=c('ADMARK_STATE', 'SurveyDate2Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtSurveyCopy)[2] <- "StateAvgPriceSurveyDate3Monthback"
colnames(StateAvgPriceAtSurveyCopy)[3] <- "SurveyDate3Monthback"

Temp3 <- merge(Temp2, StateAvgPriceAtSurveyCopy, by=c('ADMARK_STATE', 'SurveyDate3Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtSurveyCopy)[2] <- "StateAvgPriceSurveyDate4Monthback"
colnames(StateAvgPriceAtSurveyCopy)[3] <- "SurveyDate4Monthback"

Temp4 <- merge(Temp3, StateAvgPriceAtSurveyCopy, by=c('ADMARK_STATE', 'SurveyDate4Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtSurveyCopy)[2] <- "StateAvgPriceSurveyDate5Monthback"
colnames(StateAvgPriceAtSurveyCopy)[3] <- "SurveyDate5Monthback"

Temp5 <- merge(Temp4, StateAvgPriceAtSurveyCopy, by=c('ADMARK_STATE', 'SurveyDate5Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtSurveyCopy)[2] <- "StateAvgPriceSurveyDate6Monthback"
colnames(StateAvgPriceAtSurveyCopy)[3] <- "SurveyDate6Monthback"

Temp6 <- merge(Temp5, StateAvgPriceAtSurveyCopy, by=c('ADMARK_STATE', 'SurveyDate6Monthback'), all.x=TRUE)

#Temp3 <- Temp3[ , order(names(Temp3))]

Temp6 <- Temp6[ , order(names(Temp6))]

Temp6$SurveyReceiptYear <- NULL
Temp6$SurveyReceiptMonth <- NULL
Temp6$StateAvgPriceAtSurvey <- NULL

gas_price_data$SurveyDate <- as.Date(paste(gas_price_data$SurveyReceiptYear, gas_price_data$SurveyReceiptMonth, 1, sep = "-"), format = "%Y-%m-%d")
gas_price_data <- merge(gas_price_data, Temp6, by=c('ADMARK_STATE', 'SurveyDate'), all.x=TRUE)

rm(StateAvgPriceAtSurvey)
rm(StateAvgPriceAtSurveyCopy)
rm(Temp)
rm(Temp2)
rm(Temp3)
rm(Temp4)
rm(Temp5)
rm(Temp6)


# Calculate lagged prices at the point of purchase

StateAvgPriceAtPurchase <- gas_price_data[,c("ADMARK_RV_BYR","ADMARK_RV_BMON","ADMARK_STATE", "StateAvgPriceAtPurchase")]
StateAvgPriceAtPurchase <- unique(StateAvgPriceAtPurchase)
StateAvgPriceAtPurchase$PurchaseDate <- as.Date(paste(StateAvgPriceAtPurchase$ADMARK_RV_BYR, StateAvgPriceAtPurchase$ADMARK_RV_BMON, 1, sep = "-"), format = "%Y-%m-%d")

StateAvgPriceAtPurchaseCopy <- StateAvgPriceAtPurchase

StateAvgPriceAtPurchase$PurchaseDate1Monthback <- StateAvgPriceAtPurchase$PurchaseDate %m-% months(1)
StateAvgPriceAtPurchase$PurchaseDate2Monthback <- StateAvgPriceAtPurchase$PurchaseDate %m-% months(2)
StateAvgPriceAtPurchase$PurchaseDate3Monthback <- StateAvgPriceAtPurchase$PurchaseDate %m-% months(3)
StateAvgPriceAtPurchase$PurchaseDate4Monthback <- StateAvgPriceAtPurchase$PurchaseDate %m-% months(4)
StateAvgPriceAtPurchase$PurchaseDate5Monthback <- StateAvgPriceAtPurchase$PurchaseDate %m-% months(5)
StateAvgPriceAtPurchase$PurchaseDate6Monthback <- StateAvgPriceAtPurchase$PurchaseDate %m-% months(6)

colnames(StateAvgPriceAtPurchaseCopy)[4] <- "StateAvgPricePurchaseDate1Monthback"
colnames(StateAvgPriceAtPurchaseCopy)[5] <- "PurchaseDate1Monthback"
StateAvgPriceAtPurchaseCopy$ADMARK_RV_BYR <- NULL
StateAvgPriceAtPurchaseCopy$ADMARK_RV_BMON <- NULL

Temp <- merge(StateAvgPriceAtPurchase, StateAvgPriceAtPurchaseCopy, by=c('ADMARK_STATE', 'PurchaseDate1Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtPurchaseCopy)[2] <- "StateAvgPricePurchaseDate2Monthback"
colnames(StateAvgPriceAtPurchaseCopy)[3] <- "PurchaseDate2Monthback"

Temp2 <- merge(Temp, StateAvgPriceAtPurchaseCopy, by=c('ADMARK_STATE', 'PurchaseDate2Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtPurchaseCopy)[2] <- "StateAvgPricePurchaseDate3Monthback"
colnames(StateAvgPriceAtPurchaseCopy)[3] <- "PurchaseDate3Monthback"

Temp3 <- merge(Temp2, StateAvgPriceAtPurchaseCopy, by=c('ADMARK_STATE', 'PurchaseDate3Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtPurchaseCopy)[2] <- "StateAvgPricePurchaseDate4Monthback"
colnames(StateAvgPriceAtPurchaseCopy)[3] <- "PurchaseDate4Monthback"

Temp4 <- merge(Temp3, StateAvgPriceAtPurchaseCopy, by=c('ADMARK_STATE', 'PurchaseDate4Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtPurchaseCopy)[2] <- "StateAvgPricePurchaseDate5Monthback"
colnames(StateAvgPriceAtPurchaseCopy)[3] <- "PurchaseDate5Monthback"

Temp5 <- merge(Temp4, StateAvgPriceAtPurchaseCopy, by=c('ADMARK_STATE', 'PurchaseDate5Monthback'), all.x=TRUE)

colnames(StateAvgPriceAtPurchaseCopy)[2] <- "StateAvgPricePurchaseDate6Monthback"
colnames(StateAvgPriceAtPurchaseCopy)[3] <- "PurchaseDate6Monthback"

Temp6 <- merge(Temp5, StateAvgPriceAtPurchaseCopy, by=c('ADMARK_STATE', 'PurchaseDate6Monthback'), all.x=TRUE)

# Temp3 <- Temp3[ , order(names(Temp3))]
Temp6 <- Temp6[ , order(names(Temp6))]

Temp6$ADMARK_RV_BYR <- NULL
Temp6$ADMARK_RV_BMON <- NULL
Temp6$StateAvgPriceAtPurchase <- NULL

gas_price_data <- merge(gas_price_data, Temp6, by=c('ADMARK_STATE', 'PurchaseDate'), all.x=TRUE)

rm(StateAvgPriceAtPurchase)
rm(StateAvgPriceAtPurchaseCopy)
rm(Temp)
rm(Temp2)
rm(Temp3)
rm(Temp4)
rm(Temp5)
rm(Temp6)

# Generate test sample to check that the dates and average prices are being manipulated correctly

TestSample <- gas_price_data[,c("ADMARK_STATE", "PurchaseDate", "PurchaseDate1Monthback", "SurveyDate", "SurveyDate1Monthback", "FUEL_PRICE", "StateAvgPriceAtPurchase", "StateAvgPricePurchaseDate1Monthback", "StateAvgPriceAtSurvey", "StateAvgPriceSurveyDate1Monthback")]

# Calculate price differences at the point of survey (use to estimate how respondents' expensive prices are formed in the first stage)

gas_price_data$Diff1MonthSurvey <- gas_price_data$StateAvgPriceAtSurvey - gas_price_data$StateAvgPriceSurveyDate1Monthback
gas_price_data$Diff2MonthSurvey <- gas_price_data$StateAvgPriceSurveyDate1Monthback - gas_price_data$StateAvgPriceSurveyDate2Monthback
gas_price_data$Diff3MonthSurvey <- gas_price_data$StateAvgPriceSurveyDate2Monthback - gas_price_data$StateAvgPriceSurveyDate3Monthback
gas_price_data$Diff4MonthSurvey <- gas_price_data$StateAvgPriceSurveyDate3Monthback - gas_price_data$StateAvgPriceSurveyDate4Monthback
gas_price_data$Diff5MonthSurvey <- gas_price_data$StateAvgPriceSurveyDate4Monthback - gas_price_data$StateAvgPriceSurveyDate5Monthback
gas_price_data$Diff6MonthSurvey <- gas_price_data$StateAvgPriceSurveyDate5Monthback - gas_price_data$StateAvgPriceSurveyDate6Monthback

gas_price_data$Diff1MonthPurchase <- gas_price_data$StateAvgPriceAtSurvey - gas_price_data$StateAvgPricePurchaseDate1Monthback
gas_price_data$Diff2MonthPurchase <- gas_price_data$StateAvgPricePurchaseDate1Monthback - gas_price_data$StateAvgPricePurchaseDate2Monthback
gas_price_data$Diff3MonthPurchase <- gas_price_data$StateAvgPricePurchaseDate2Monthback - gas_price_data$StateAvgPricePurchaseDate3Monthback
gas_price_data$Diff4MonthPurchase <- gas_price_data$StateAvgPricePurchaseDate3Monthback - gas_price_data$StateAvgPricePurchaseDate4Monthback
gas_price_data$Diff5MonthPurchase <- gas_price_data$StateAvgPricePurchaseDate4Monthback - gas_price_data$StateAvgPricePurchaseDate5Monthback
gas_price_data$Diff6MonthPurchase <- gas_price_data$StateAvgPricePurchaseDate5Monthback - gas_price_data$StateAvgPricePurchaseDate6Monthback


gas_price_data$Diff1MonthSurveyIncrease <- ifelse(gas_price_data$Diff1MonthSurvey >= 0 , gas_price_data$Diff1MonthSurvey, 0)
gas_price_data$Diff1MonthSurveyDecrease <- ifelse(gas_price_data$Diff1MonthSurvey < 0 , gas_price_data$Diff1MonthSurvey, 0)
gas_price_data$Diff2MonthSurveyIncrease <- ifelse(gas_price_data$Diff2MonthSurvey >= 0 , gas_price_data$Diff2MonthSurvey, 0)
gas_price_data$Diff2MonthSurveyDecrease <- ifelse(gas_price_data$Diff2MonthSurvey < 0 , gas_price_data$Diff2MonthSurvey, 0)
gas_price_data$Diff3MonthSurveyIncrease <- ifelse(gas_price_data$Diff3MonthSurvey >= 0 , gas_price_data$Diff3MonthSurvey, 0)
gas_price_data$Diff3MonthSurveyDecrease <- ifelse(gas_price_data$Diff3MonthSurvey < 0 , gas_price_data$Diff3MonthSurvey, 0)
gas_price_data$Diff4MonthSurveyIncrease <- ifelse(gas_price_data$Diff4MonthSurvey >= 0 , gas_price_data$Diff4MonthSurvey, 0)
gas_price_data$Diff4MonthSurveyDecrease <- ifelse(gas_price_data$Diff4MonthSurvey < 0 , gas_price_data$Diff4MonthSurvey, 0)
gas_price_data$Diff5MonthSurveyIncrease <- ifelse(gas_price_data$Diff5MonthSurvey >= 0 , gas_price_data$Diff5MonthSurvey, 0)
gas_price_data$Diff5MonthSurveyDecrease <- ifelse(gas_price_data$Diff5MonthSurvey < 0 , gas_price_data$Diff5MonthSurvey, 0)
gas_price_data$Diff6MonthSurveyIncrease <- ifelse(gas_price_data$Diff6MonthSurvey >= 0 , gas_price_data$Diff6MonthSurvey, 0)
gas_price_data$Diff6MonthSurveyDecrease <- ifelse(gas_price_data$Diff6MonthSurvey < 0 , gas_price_data$Diff6MonthSurvey, 0)

gas_price_data$Diff1MonthPurchaseIncrease <- ifelse(gas_price_data$Diff1MonthPurchase >= 0 , gas_price_data$Diff1MonthPurchase, 0)
gas_price_data$Diff1MonthPurchaseDecrease <- ifelse(gas_price_data$Diff1MonthPurchase < 0 , gas_price_data$Diff1MonthPurchase, 0)
gas_price_data$Diff2MonthPurchaseIncrease <- ifelse(gas_price_data$Diff2MonthPurchase >= 0 , gas_price_data$Diff2MonthPurchase, 0)
gas_price_data$Diff2MonthPurchaseDecrease <- ifelse(gas_price_data$Diff2MonthPurchase < 0 , gas_price_data$Diff2MonthPurchase, 0)
gas_price_data$Diff3MonthPurchaseIncrease <- ifelse(gas_price_data$Diff3MonthPurchase >= 0 , gas_price_data$Diff3MonthPurchase, 0)
gas_price_data$Diff3MonthPurchaseDecrease <- ifelse(gas_price_data$Diff3MonthPurchase < 0 , gas_price_data$Diff3MonthPurchase, 0)
gas_price_data$Diff4MonthPurchaseIncrease <- ifelse(gas_price_data$Diff4MonthPurchase >= 0 , gas_price_data$Diff4MonthPurchase, 0)
gas_price_data$Diff4MonthPurchaseDecrease <- ifelse(gas_price_data$Diff4MonthPurchase < 0 , gas_price_data$Diff4MonthPurchase, 0)
gas_price_data$Diff5MonthPurchaseIncrease <- ifelse(gas_price_data$Diff5MonthPurchase >= 0 , gas_price_data$Diff5MonthPurchase, 0)
gas_price_data$Diff5MonthPurchaseDecrease <- ifelse(gas_price_data$Diff5MonthPurchase < 0 , gas_price_data$Diff5MonthPurchase, 0)
gas_price_data$Diff6MonthPurchaseIncrease <- ifelse(gas_price_data$Diff6MonthPurchase >= 0 , gas_price_data$Diff6MonthPurchase, 0)
gas_price_data$Diff6MonthPurchaseDecrease <- ifelse(gas_price_data$Diff6MonthPurchase < 0 , gas_price_data$Diff6MonthPurchase, 0)



# HOW ARE CONSUMER PERCEPTIONS OF GAS PRICES FORMED?
#---------------------------------------------------

test_reg1 <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL, data = gas_price_data)
summary(test_reg1)

test_reg_sym3 <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurvey + Diff2MonthSurvey + Diff3MonthSurvey, data = gas_price_data)
summary(test_reg_sym3)



test_reg_sym6 <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurvey + Diff2MonthSurvey + Diff3MonthSurvey + Diff4MonthSurvey + Diff5MonthSurvey + Diff6MonthSurvey, data = gas_price_data)
summary(test_reg_sym6)



test_reg_asym3 <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurveyIncrease + Diff2MonthSurveyIncrease + Diff3MonthSurveyIncrease + Diff1MonthSurveyDecrease + Diff2MonthSurveyDecrease + Diff3MonthSurveyDecrease, data = gas_price_data)
summary(test_reg_asym3)

test_reg_asym3b <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurveyIncrease + Diff2MonthSurveyIncrease + Diff3MonthSurveyIncrease + Diff1MonthSurveyDecrease + Diff2MonthSurveyDecrease + Diff3MonthSurveyDecrease + DEMO_INCOME1, data = gas_price_data)
summary(test_reg_asym3b)

test_reg_asym3c <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurveyIncrease + Diff2MonthSurveyIncrease + Diff3MonthSurveyIncrease + Diff1MonthSurveyDecrease + Diff2MonthSurveyDecrease + Diff3MonthSurveyDecrease + DEMO_INCOME1 + BLD_RV_CAR_TRUCK_GRP, data = gas_price_data)
summary(test_reg_asym3c)


test_reg_asym6 <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurveyIncrease + Diff2MonthSurveyIncrease + Diff3MonthSurveyIncrease + Diff4MonthSurveyIncrease + Diff5MonthSurveyIncrease + Diff6MonthSurveyIncrease + Diff1MonthSurveyDecrease + Diff2MonthSurveyDecrease + Diff3MonthSurveyDecrease + Diff4MonthSurveyDecrease + Diff5MonthSurveyDecrease + Diff6MonthSurveyDecrease, data = gas_price_data)
summary(test_reg_asym6)

test_reg_asym6b <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurveyIncrease + Diff2MonthSurveyIncrease + Diff3MonthSurveyIncrease + Diff4MonthSurveyIncrease + Diff5MonthSurveyIncrease + Diff6MonthSurveyIncrease + Diff1MonthSurveyDecrease + Diff2MonthSurveyDecrease + Diff3MonthSurveyDecrease + Diff4MonthSurveyDecrease + Diff5MonthSurveyDecrease + Diff6MonthSurveyDecrease + BLD_RV_CAR_TRUCK_GRP, data = gas_price_data)
summary(test_reg_asym6b)



results_table_sym <- stargazer(test_reg1, test_reg_sym3, test_reg_sym6, type = "html", single.row = TRUE)
write(results_table_sym, "results_sym.html")

results_table_asym <- stargazer(test_reg_asym3d, test_reg_asym6d, type = "html", single.row = TRUE)
write(results_table_asym, "results_asym.html")


test_reg_sym3d <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurvey + Diff2MonthSurvey + Diff3MonthSurvey + DEMO_AGE1 + DEMO_EDUCATION1 + DEMO_GENDER2 + DEMO_INCOME1 + DEMO_LOCATION1 + DEMO_MARITAL1 + BLD_RV_CAR_TRUCK_GRP + MPG_COMBINED + RV_ESTMILES1 + State_, data = gas_price_data)
summary(test_reg_sym3d)

test_reg_sym6d <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurvey + Diff2MonthSurvey + Diff3MonthSurvey + Diff4MonthSurvey + Diff5MonthSurvey + Diff6MonthSurvey + DEMO_AGE1 + DEMO_EDUCATION1 + DEMO_GENDER2 + DEMO_INCOME1 + DEMO_LOCATION1 + DEMO_MARITAL1 + BLD_RV_CAR_TRUCK_GRP + MPG_COMBINED + RV_ESTMILES1 + State_, data = gas_price_data)
summary(test_reg_sym6d)

test_reg_asym3d <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurveyIncrease + Diff2MonthSurveyIncrease + Diff3MonthSurveyIncrease + Diff1MonthSurveyDecrease + Diff2MonthSurveyDecrease + Diff3MonthSurveyDecrease + DEMO_AGE1 + DEMO_EDUCATION1 + DEMO_GENDER2 + DEMO_INCOME1 + DEMO_LOCATION1 + DEMO_MARITAL1 + BLD_RV_CAR_TRUCK_GRP + MPG_COMBINED + RV_ESTMILES1 + State_, data = gas_price_data)
summary(test_reg_asym3d)

test_reg_asym6d <- lm(formula = FUEL_EXPENSIVE_REAL ~ FUEL_PRICE_REAL + Diff1MonthSurveyIncrease + Diff2MonthSurveyIncrease + Diff3MonthSurveyIncrease + Diff4MonthSurveyIncrease + Diff5MonthSurveyIncrease + Diff6MonthSurveyIncrease + Diff1MonthSurveyDecrease + Diff2MonthSurveyDecrease + Diff3MonthSurveyDecrease + Diff4MonthSurveyDecrease + Diff5MonthSurveyDecrease + Diff6MonthSurveyDecrease + DEMO_AGE1 + DEMO_EDUCATION1 + DEMO_GENDER2 + DEMO_INCOME1 + DEMO_LOCATION1 + DEMO_MARITAL1 + BLD_RV_CAR_TRUCK_GRP + MPG_COMBINED + RV_ESTMILES1 + State_, data = gas_price_data)
summary(test_reg_asym6d)

results_table_ISDC <- stargazer(test_reg_sym3d, test_reg_sym6d, test_reg_asym3d, test_reg_asym6d, type = "html", single.row = TRUE)
write(results_table_ISDC, "results_ISDC.html")

# SIMULATION OF THE RESPONDENT's EXPENSIVE PRICE AT THE TIME OF PURCHASE
#-----------------------------------------------------------------------

gas_price_data$EstExpensiveAtPurchase_sym3 = 171.714600 + 
  (0.580412 * gas_price_data$StateAvgPriceAtPurchase) + 
  (-0.229096 * gas_price_data$Diff1MonthPurchaseIncrease) + 
  (-0.056882 * gas_price_data$Diff2MonthPurchaseIncrease) + 
  (0.110402 * gas_price_data$Diff3MonthPurchaseIncrease)

gas_price_data$EstExpensiveAtPurchase_asym3b = 160.8998 +
  (.5638 * gas_price_data$StateAvgPriceAtPurchase) + 
  (-0.0727 * gas_price_data$Diff1MonthPurchaseIncrease) + 
  (-0.0191 * gas_price_data$Diff2MonthPurchaseIncrease) + 
  (.0853 * gas_price_data$Diff3MonthPurchaseIncrease) + 
  (-0.2906 * gas_price_data$Diff1MonthPurchaseDecrease) + 
  (-0.2074 * gas_price_data$Diff2MonthPurchaseDecrease) + 
  (-0.2787 * gas_price_data$Diff3MonthPurchaseDecrease) + 
  (13.7600 * ifelse(gas_price_data$DEMO_INCOME1 == "100k_200k",1,0)) +
  (1.6327 * ifelse(gas_price_data$DEMO_INCOME1 == "125k_55k",1,0)) +
  (5.5673 * ifelse(gas_price_data$DEMO_INCOME1 == "55k_100k",1,0)) +
  (32.7524 * ifelse(gas_price_data$DEMO_INCOME1 == "Over200k",1,0)) +
  (13.7314 * ifelse(gas_price_data$DEMO_INCOME1 == "PrefNoAnswer",1,0)) +
  (0.8222 * ifelse(gas_price_data$DEMO_INCOME1 == "Under25k",1,0))

gas_price_data$EstExpensiveAtPurchase_asym3c = 160.8998 +
  (.5638 * gas_price_data$StateAvgPriceAtPurchase) + 
  (-0.0727 * gas_price_data$Diff1MonthPurchaseIncrease) + 
  (-0.0191 * gas_price_data$Diff2MonthPurchaseIncrease) + 
  (.0853 * gas_price_data$Diff3MonthPurchaseIncrease) + 
  (-0.2906 * gas_price_data$Diff1MonthPurchaseDecrease) + 
  (-0.2074 * gas_price_data$Diff2MonthPurchaseDecrease) + 
  (-0.2787 * gas_price_data$Diff3MonthPurchaseDecrease) + 
  (13.7600 * ifelse(gas_price_data$DEMO_INCOME1 == "100k_200k",1,0)) +
  (1.6327 * ifelse(gas_price_data$DEMO_INCOME1 == "125k_55k",1,0)) +
  (5.5673 * ifelse(gas_price_data$DEMO_INCOME1 == "55k_100k",1,0)) +
  (32.7524 * ifelse(gas_price_data$DEMO_INCOME1 == "Over200k",1,0)) +
  (13.7314 * ifelse(gas_price_data$DEMO_INCOME1 == "PrefNoAnswer",1,0)) +
  (0.8222 * ifelse(gas_price_data$DEMO_INCOME1 == "Under25k",1,0)) +
  (-8.4305 * ifelse(gas_price_data$BLD_RV_CAR_TRUCK_GRP == "Total Truck",1,0))

gas_price_data$EstExpensiveAtPurchase_asym6 = 157.883989 + 
  (0.607738 * gas_price_data$StateAvgPriceAtPurchase) + 
  (-0.133331 * gas_price_data$Diff1MonthPurchaseIncrease) + 
  (-0.056882 * gas_price_data$Diff2MonthPurchaseIncrease) + 
  (0.110402 * gas_price_data$Diff3MonthPurchaseIncrease) + 
  (0.070342 * gas_price_data$Diff4MonthPurchaseIncrease) + 
  (-0.047893 * gas_price_data$Diff5MonthPurchaseIncrease) + 
  (-0.141760 * gas_price_data$Diff6MonthPurchaseIncrease) + 
  (-0.351994 * gas_price_data$Diff1MonthPurchaseDecrease) + 
  (-0.225911 * gas_price_data$Diff2MonthPurchaseDecrease) + 
  (-0.225340 * gas_price_data$Diff3MonthPurchaseDecrease) + 
  (-0.177538 * gas_price_data$Diff4MonthPurchaseDecrease) + 
  (-0.095090 * gas_price_data$Diff5MonthPurchaseDecrease) + 
  (-0.106373 * gas_price_data$Diff6MonthPurchaseDecrease)


#Simluation below does not work?
#gas_price_data$EstExpensiveAtPurchasePurchase = 140.843072 + (0.666845 * gas_price_data$StateAvgPriceAtPurchase) + (0.004797 * gas_price_data$Diff1MonthPurchaseIncrease) + (0.051766 * gas_price_data$Diff2MonthPurchaseIncrease) + (-0.015262 * gas_price_data$Diff3MonthPurchaseIncrease) + (-0.124171 * gas_price_data$Diff4MonthPurchaseIncrease) + (-0.062406 * gas_price_data$Diff5MonthPurchaseIncrease) + (-0.024566 * gas_price_data$Diff6MonthPurchaseIncrease) + (-0.370649 * gas_price_data$Diff1MonthPurchaseDecrease) + (-0.099832 * gas_price_data$Diff2MonthPurchaseDecrease) + (-0.122369 * gas_price_data$Diff3MonthPurchaseDecrease) + (-0.095560 * gas_price_data$Diff4MonthPurchaseDecrease) + (-0.151895 * gas_price_data$Diff5MonthPurchaseDecrease) + (-0.129922 * gas_price_data$Diff6MonthPurchaseDecrease)

# Initial indicator of gas price 'pain'

gas_price_data$Comfort_asym3b = gas_price_data$EstExpensiveAtPurchase_asym3b - gas_price_data$StateAvgPriceAtPurchase
gas_price_data$Comfort_asym3c = gas_price_data$EstExpensiveAtPurchase_asym3c - gas_price_data$StateAvgPriceAtPurchase
gas_price_data$Comfort_asym6 = gas_price_data$EstExpensiveAtPurchase_asym6 - gas_price_data$StateAvgPriceAtPurchase



cor(gas_price_data$Comfort_asym3c, gas_price_data$StateAvgPriceAtPurchase,use="complete.obs")


# FINAL REGRESSIONS - WHAT IS THE EFFECT OF GAS PRICE PAIN ON VEHICLE PURCHASES?
#-------------------------------------------------------------------------------

fe_test <- lm(formula = MPG_COMBINED ~ StateAvgPriceAtPurchase, data = gas_price_data)
summary(fe_test)

fe_test <- lm(formula = MPG_COMBINED ~ FIN_PRICE_EDITED + StateAvgPriceAtPurchase + Comfort_asym3c, data = gas_price_data)
summary(fe_test)




fe_test_asym3b <- lm(formula = MPG_COMBINED ~ FIN_PRICE_EDITED + StateAvgPriceAtPurchase + Comfort_asym3b + DEMO_INCOME1 + BLD_RV_CAR_TRUCK_GRP, data = gas_price_data)
summary(fe_test_asym3b)

fe_test_asym3c <- lm(formula = MPG_COMBINED ~ FIN_PRICE_EDITED + StateAvgPriceAtPurchase + Comfort_asym3c + DEMO_INCOME1 + BLD_RV_CAR_TRUCK_GRP, data = gas_price_data)
summary(fe_test_asym3c)

fe_test_asym6 <- lm(formula = MPG_COMBINED ~ FIN_PRICE_EDITED + StateAvgPriceAtPurchase + Comfort_asym6 + DEMO_INCOME1 + BLD_RV_CAR_TRUCK_GRP, data = gas_price_data)
summary(fe_test_asym6)



