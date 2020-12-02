
library(dplyr)
library(readxl)
library(cellranger)
library(here)
library(ggplot2)
library(scales)
library(tidyverse)
library(plyr)
library(stats)
library(readxl)
library(patchwork)


setwd("../")

#Data LEE


#Mai
sheet_names <- excel_sheets(here::here("Suivi_LEE_pilote_Carbiosep.xlsx"))
data_LEE_1<-as.data.frame(NULL)
data_LEE_2<-as.data.frame(NULL)
#mai <- NULL
# TSHmai <- as.data.frame(NULL)
# for (i in 1:2)
# {
name1 <-sheet_names[2]
name2 <-sheet_names[1]
# name2<-paste("sheet_",name)
data1 <-as.data.frame(read_excel(here::here("Suivi_LEE_pilote_Carbiosep.xlsx"),col_names =TRUE,skip = 2,sheet=2))
data2 <-as.data.frame(read_excel(here::here("Suivi_LEE_pilote_Carbiosep.xlsx"),col_names =TRUE,skip = 2,sheet=1))
# data$sheet<-name1
data_LEE_2 <- assign(name1,data1)
data_LEE_1 <- assign(name2,data2)
# TSH<-data[6,11:13]
# TSHmai<-rbind(TSHmai,TSH)
# mai<- rbind(mai,data2) 
# rm(name,name2,data,data2,TSH)
# }
names_data_LEE_1<-c("Date",	"Days",	"Day",	"TSH (h)",	"Dilution",	"Debit (ml/min)",	"pH range",	"NLR kg/m3/jour",	"MES (g/L)",	"MVS (g/L)",	"1"	,"NH4 (mgN/L)",	"LQNH4",	"NO2 (mgN/L)",	"LQNO2",	"NO3 (mgN/L)",	"LQNO3",	"NTK (mgN/L)",	"LQNTK"	,"Inc NTK",	"NTK (mg/L)2",	"NTK/NH4",	"NH4/NTK",	"Nomenclature",	"Alcalinity (mgCaCO3/L)","NH4","NO2",	"NO3",	"2",	"NH4 (mgN/L) s"	,"LQNH4s",	"NO2 (mgN/L) s",	"LQNO2s",	"NO3 (mgN/L) s",	"LQNO3s",	"NTKs",	"LQNTKs",	"Inc NTKs",	"N Total",	"BILAN MATIERE",	"Nomenclatures",	"Alcalinite s","NH4s","NO2s",	"NO3s",	"somme mg N/L", "somme1", "somme2", "somme3")
names_data_LEE_2<-c("Date",	"Days",	"TSH (d)",	"Dilution",	"Debit (ml/min)",	"pH range",	"NLR kg/m3/jour",	"1"	,"NH4 (mgN/L)",	"LQNH4",	"NO2 (mgN/L)",	"LQNO2",	"NO3 (mgN/L)",	"LQNO3",	"NTK (mgN/L)",	"LQNTK"	,"Inc NTK",	"NTotal (mg/L)",	"NTK/NH4",	"NH4/NTK",	"Nomenclature",	"Alcalinity (mgCaCO3/L)","NH4","NO2",	"NO3",	"Alcalinité TA","Nomenclatures",	"NH4 (mgN/L) s"	,"LQNH4s",	"NO2 (mgN/L) s",	"LQNO2s",	"NO3 (mgN/L) s",	"LQNO3s",	"NTKs",	"LQNTKs",	"Inc NTKs",	"N Total s",	"BILAN MATIERE",	"Nomenclatures",	"Alcalinite s TAC","NH4s","NO2s",	"NO3s",	"somme mg N/L", "somme1", "somme2", "somme3","%inhibition")
names(data_LEE_1)<-names_data_LEE_1
names(data_LEE_2)<-names_data_LEE_2


# mai$dateok <- strftime(mai$date,format="%Y-%m-%d")
# mai$time <- format(as.POSIXct(mai$hours),format="%H:%M:%S")
# mai <- mai[,-7:-12]
# row.has.na <- apply(mai,1,function(x){any(is.na(x))})
# mai <-mai[!row.has.na,]
# mai[,3]<-lapply(mai[,3], as.double)
# mai[,5:6]<-lapply(mai[,5:6], as.double)
# #mai[,9]<-lapply(mai[,9], as.Date)
# 
# dime<-paste(mai$time,mai$dateok)
# mai$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
# mai$Seconds<-as.difftime(mai$time,format="%H:%M:%S")
# 
# 
# rm(sheet_names,row.has.na,dime)
# 
# 
# 
# 
# #Data
# pHmai<-mai %>% select(pH,time,dateok)
# O2mai<-mai %>% select(O2,time,dateok)
# Condmai<-mai %>% select(cond,time,dateok)
# Tempmai<-mai %>% select(temperature,time,dateok)
# 
# 
# 
# #pHjuin<-juin %>% select(pH,time,dateok)
# 
# #Graphs
# 
# #pH
# pH_mai<-ggplot(mai, aes(hours,pH,group=1)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
# pH_juin<-ggplot(juin, aes(hours,pH,group=2)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
# pH_juillet<-ggplot(juillet, aes(hours,pH,group=3)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
# pH_aout<-ggplot(aout, aes(hours,pH,group=4)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
# pH_septembre<-ggplot(septembre, aes(hours,pH,group=5)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
# pH_octobre<-ggplot(octobre, aes(hours,pH,group=6)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
# 

#%-------------------------------------------------------------





#CARBIOSEP Pilot Data
#setwd("E:/Carbiosep/Pilote/Donnees R")
#First campaign data

#December
sheet_names <- excel_sheets(here::here("Suivi Decembre TSH.xlsm"))
decembre<-as.data.frame(NULL)
#Decembre <- NULL
TSHdecembre <- as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Decembre TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHdecembre<-rbind(TSHdecembre,TSH)
  decembre<- rbind(decembre,data2) 
  rm(name,name2,data,data2,TSH)
}
names(decembre)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
decembre$dateok <- strftime(decembre$date,format="%Y-%m-%d")
decembre$time <- format(as.POSIXct(decembre$hours),format="%H:%M:%S")
decembre <- decembre[,-7:-12]
row.has.na <- apply(decembre,1,function(x){any(is.na(x))})
decembre <-decembre[!row.has.na,]
decembre[,3]<-lapply(decembre[,3], as.double)
decembre[,5:6]<-lapply(decembre[,5:6], as.double)
#decembre[,9]<-lapply(decembre[,9], as.Date)

dime<-paste(decembre$time,decembre$dateok)
decembre$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
decembre$Seconds<-as.difftime(decembre$time,format="%H:%M:%S")
decembre$Seconds<-as.numeric(decembre$Seconds)


rm(sheet_names,row.has.na,dime)


#Janvier
sheet_names <- excel_sheets(here::here("Suivi Janvier TSH.xlsm"))
janvier<-as.data.frame(NULL)
#Janvier <- NULL
TSHjanvier <- as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Janvier TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHjanvier<-rbind(TSHjanvier,TSH)
  janvier<- rbind(janvier,data2) 
  rm(name,name2,data,data2,TSH)
}
names(janvier)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
janvier$dateok <- strftime(janvier$date,format="%Y-%m-%d")
janvier$time <- format(as.POSIXct(janvier$hours),format="%H:%M:%S")
janvier <- janvier[,-7:-12]
row.has.na <- apply(janvier,1,function(x){any(is.na(x))})
janvier <-janvier[!row.has.na,]
janvier[,3]<-lapply(janvier[,3], as.double)
janvier[,5:6]<-lapply(janvier[,5:6], as.double)
#janvier[,9]<-lapply(janvier[,9], as.Date)

dime<-paste(janvier$time,janvier$dateok)
janvier$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
janvier$Seconds<-as.difftime(janvier$time,format="%H:%M:%S")
janvier$Seconds<-as.numeric(janvier$Seconds)


rm(sheet_names,row.has.na,dime)


#Fevrier
sheet_names <- excel_sheets(here::here("Suivi Fevrier TSH.xlsm"))
fevrier<-as.data.frame(NULL)
#Fevrier <- NULL
TSHfevrier <- as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Fevrier TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHfevrier<-rbind(TSHfevrier,TSH)
  fevrier<- rbind(fevrier,data2) 
  rm(name,name2,data,data2,TSH)
}
names(fevrier)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
fevrier$dateok <- strftime(fevrier$date,format="%Y-%m-%d")
fevrier$time <- format(as.POSIXct(fevrier$hours),format="%H:%M:%S")
fevrier <- fevrier[,-7:-12]
row.has.na <- apply(fevrier,1,function(x){any(is.na(x))})
fevrier <-fevrier[!row.has.na,]
fevrier[,3]<-lapply(fevrier[,3], as.double)
fevrier[,5:6]<-lapply(fevrier[,5:6], as.double)
#fevrier[,9]<-lapply(fevrier[,9], as.Date)

dime<-paste(fevrier$time,fevrier$dateok)
fevrier$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
fevrier$Seconds<-as.difftime(fevrier$time,format="%H:%M:%S")
fevrier$Seconds<-as.numeric(fevrier$Seconds)


rm(sheet_names,row.has.na,dime)


#Mars
sheet_names <- excel_sheets(here::here("Suivi Mars TSH.xlsm"))
mars<-as.data.frame(NULL)
#Mars <- NULL
TSHmars <- as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Mars TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHmars<-rbind(TSHmars,TSH)
  mars<- rbind(mars,data2) 
  rm(name,name2,data,data2,TSH)
}
names(mars)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
mars$dateok <- strftime(mars$date,format="%Y-%m-%d")
mars$time <- format(as.POSIXct(mars$hours),format="%H:%M:%S")
mars <- mars[,-7:-12]
row.has.na <- apply(mars,1,function(x){any(is.na(x))})
mars <-mars[!row.has.na,]
mars[,3]<-lapply(mars[,3], as.double)
mars[,5:6]<-lapply(mars[,5:6], as.double)
#mars[,9]<-lapply(mars[,9], as.Date)

dime<-paste(mars$time,mars$dateok)
mars$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
mars$Seconds<-as.difftime(mars$time,format="%H:%M:%S")
mars$Seconds<-as.numeric(mars$Seconds)


rm(sheet_names,row.has.na,dime)

#Avril
sheet_names <- excel_sheets(here::here("Suivi Avril TSH.xlsm"))
avril<-as.data.frame(NULL)
#Avril <- NULL
TSHavril <- as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Avril TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHavril<-rbind(TSHavril,TSH)
  avril<- rbind(avril,data2) 
  rm(name,name2,data,data2,TSH)
}
names(avril)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
avril$dateok <- strftime(avril$date,format="%Y-%m-%d")
avril$time <- format(as.POSIXct(avril$hours),format="%H:%M:%S")
avril <- avril[,-7:-12]
row.has.na <- apply(avril,1,function(x){any(is.na(x))})
avril <-avril[!row.has.na,]
avril[,3]<-lapply(avril[,3], as.double)
avril[,5:6]<-lapply(avril[,5:6], as.double)
#avril[,9]<-lapply(avril[,9], as.Date)

dime<-paste(avril$time,avril$dateok)
avril$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
avril$Seconds<-as.difftime(avril$time,format="%H:%M:%S")
avril$Seconds<-as.numeric(avril$Seconds)


rm(sheet_names,row.has.na,dime)




#Second experimental campaign data
#Mai
sheet_names <- excel_sheets(here::here("Suivi Mai TSH.xlsm"))
mai<-as.data.frame(NULL)
#mai <- NULL
TSHmai <- as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Mai TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHmai<-rbind(TSHmai,TSH)
  mai<- rbind(mai,data2) 
  rm(name,name2,data,data2,TSH)
}
names(mai)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
mai$dateok <- strftime(mai$date,format="%Y-%m-%d")
mai$time <- format(as.POSIXct(mai$hours),format="%H:%M:%S")
mai <- mai[,-7:-12]
row.has.na <- apply(mai,1,function(x){any(is.na(x))})
mai <-mai[!row.has.na,]
mai[,3]<-lapply(mai[,3], as.double)
mai[,5:6]<-lapply(mai[,5:6], as.double)
#mai[,9]<-lapply(mai[,9], as.Date)

dime<-paste(mai$time,mai$dateok)
mai$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
mai$Seconds<-as.difftime(mai$time,format="%H:%M:%S")
mai$Seconds<-as.numeric(mai$Seconds)
mai<-mai[-c(1:42628),]

rm(sheet_names,row.has.na,dime)

#Juin
sheet_names <- excel_sheets(here::here("Suivi Juin TSH.xlsm"))
juin <- as.data.frame(NULL)
TSHjuin<-as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Juin TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHjuin<-rbind(TSHjuin,TSH)
  juin<- rbind(juin,data2) 
  rm(name,name2,data,data2,TSH)
}
names(juin)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
juin$dateok <- strftime(juin$date,format="%Y-%m-%d")
juin$time <- format(as.POSIXct(juin$hours),format="%H:%M:%S")
juin <- juin[,-7:-12]
row.has.na <- apply(juin,1,function(x){any(is.na(x))})
juin <-juin[!row.has.na,]
juin[,3]<-lapply(juin[,3], as.double)
juin[,5:6]<-lapply(juin[,5:6], as.double)
#juin[,9]<-lapply(juin[,9], as.Date.date)

dime<-paste(juin$time,juin$dateok)
juin$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
juin$Seconds<-as.difftime(juin$time,format="%H:%M:%S")
juin$Seconds<-as.numeric(juin$Seconds)

rm(sheet_names,row.has.na,dime)


#Juillet
sheet_names <- excel_sheets(here::here("Suivi Juillet TSH.xlsm"))
juillet <- as.data.frame(NULL)
TSHjuillet<-as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Juillet TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHjuillet<-rbind(TSHjuillet,TSH)
  juillet<- rbind(juillet,data2) 
  rm(name,name2,data,data2,TSH)
}
names(juillet)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
juillet$dateok <- strftime(juillet$date,format="%Y-%m-%d")
juillet$time <- format(as.POSIXct(juillet$hours),format="%H:%M:%S")
juillet <- juillet[,-7:-12]
row.has.na <- apply(juillet,1,function(x){any(is.na(x))})
juillet <-juillet[!row.has.na,]
juillet[,3]<-lapply(juillet[,3], as.double)
juillet[,5:6]<-lapply(juillet[,5:6], as.double)
#juillet[,9]<-lapply(juillet[,9], as.Date.date)

dime<-paste(juillet$time,juillet$dateok)
juillet$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
juillet$Seconds<-as.difftime(juillet$time,format="%H:%M:%S")
juillet$Seconds<-as.numeric(juillet$Seconds)

rm(sheet_names,row.has.na,dime)

#Aout
sheet_names <- excel_sheets(here::here("Suivi Aout TSH.xlsm"))
aout <- as.data.frame(NULL)
TSHaout<-as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Aout TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHaout<-rbind(TSHaout,TSH)
  aout<- rbind(aout,data2) 
  rm(name,name2,data,data2,TSH)
}
names(aout)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
aout$dateok <- strftime(aout$date,format="%Y-%m-%d")
aout$time <- format(as.POSIXct(aout$hours),format="%H:%M:%S")
aout <- aout[,-7:-12]
row.has.na <- apply(aout,1,function(x){any(is.na(x))})
aout <-aout[!row.has.na,]
aout[,3]<-lapply(aout[,3], as.double)
aout[,5:6]<-lapply(aout[,5:6], as.double)
#aout[,9]<-lapply(aout[,9], as.Date)

dime<-paste(aout$time,aout$dateok)
aout$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
aout$Seconds<-as.difftime(aout$time,format="%H:%M:%S")
aout$Seconds<-as.numeric(aout$Seconds)

rm(sheet_names,row.has.na,dime)



#Septembre
sheet_names <- excel_sheets(here::here("Suivi Septembre TSH.xlsm"))
septembre <- as.data.frame(NULL)
TSHseptembre<-as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Septembre TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHseptembre<-rbind(TSHseptembre,TSH)
  septembre<- rbind(septembre,data2) 
  rm(name,name2,data,data2,TSH)
}
names(septembre)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
septembre$dateok <- strftime(septembre$date,format="%Y-%m-%d")
septembre$time <- format(as.POSIXct(septembre$hours),format="%H:%M:%S")
septembre <- septembre[,-7:-12]
row.has.na <- apply(septembre,1,function(x){any(is.na(x))})
septembre <-septembre[!row.has.na,]
septembre[,3]<-lapply(septembre[,3], as.double)
septembre[,5:6]<-lapply(septembre[,5:6], as.double)
#septembre[,9]<-lapply(septembre[,9], as.Date)

dime<-paste(septembre$time,septembre$dateok)
septembre$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
septembre$Seconds<-as.difftime(septembre$time,format="%H:%M:%S")
septembre$Seconds<-as.numeric(septembre$Seconds)

rm(sheet_names,row.has.na,dime)

#Octobre
sheet_names <- excel_sheets(here::here("Suivi Octobre TSH.xlsm"))
octobre <- as.data.frame(NULL)
TSHoctobre<-as.data.frame(NULL)
for (i in 1:length(sheet_names))
{
  name <-sheet_names[i]
  name2<-paste0("sheet_",name)
  data <-read_excel(here::here("Suivi Octobre TSH.xlsm"),col_names =FALSE,skip = 1,sheet=i)
  data$sheet<-name
  data2 <- assign(name,data)
  TSH<-cbind(data[6,11:13],data[2,1])
  TSHoctobre<-rbind(TSHoctobre,TSH)
  octobre<- rbind(octobre,data2) 
  rm(name,name2,data,data2,TSH)
}
names(octobre)<-c("date","hours","O2","pH","cond","temperature","7","8","9","10","11","12","13")
octobre$dateok <- strftime(octobre$date,format="%Y-%m-%d")
octobre$time <- format(as.POSIXct(octobre$hours),format="%H:%M:%S")
octobre <- octobre[,-7:-12]
row.has.na <- apply(octobre,1,function(x){any(is.na(x))})
octobre <-octobre[!row.has.na,]
octobre[,3]<-lapply(octobre[,3], as.double)
octobre[,5:6]<-lapply(octobre[,5:6], as.double)
#octobre[,9]<-lapply(octobre[,9], as.Date)

dime<-paste(octobre$time,octobre$dateok)
octobre$dateTime <- strptime(as.character(dime),format="%H:%M:%S %Y-%m-%d")
octobre$Seconds<-as.difftime(octobre$time,format="%H:%M:%S")
octobre$Seconds<-as.numeric(octobre$Seconds)


rm(sheet_names,row.has.na,dime)


#Data
pHmai<-mai %>% select(pH,time,dateok)
O2mai<-mai %>% select(O2,time,dateok)
Condmai<-mai %>% select(cond,time,dateok)
Tempmai<-mai %>% select(temperature,time,dateok)

pHjuin<-juin %>% select(pH,time,dateok)
O2juin<-juin %>% select(O2,time,dateok)
Condjuin<-juin %>% select(cond,time,dateok)
Tempjuin<-juin %>% select(temperature,time,dateok)

pHjuillet<-juillet %>% select(pH,time,dateok)
O2juillet<-juillet %>% select(O2,time,dateok)
Condjuillet<-juillet %>% select(cond,time,dateok)
Tempjuillet<-juillet %>% select(temperature,time,dateok)

pHaout<-aout %>% select(pH,time,dateok)
O2aout<-aout %>% select(O2,time,dateok)
Condaout<-aout %>% select(cond,time,dateok)
Tempaout<-aout %>% select(temperature,time,dateok)

pHseptembre<-septembre %>% select(pH,time,dateok)
O2septembre<-septembre %>% select(O2,time,dateok)
Condseptembre<-septembre %>% select(cond,time,dateok)
Tempseptembre<-septembre %>% select(temperature,time,dateok)

pHoctobre<-octobre %>% select(pH,time,dateok)
O2octobre<-octobre %>% select(O2,time,dateok)
Condoctobre<-octobre %>% select(cond,time,dateok)
Tempoctobre<-octobre %>% select(temperature,time,dateok)


#pHjuin<-juin %>% select(pH,time,dateok)

#Graphs

#pH
pH_mai<-ggplot(mai, aes(hours,pH,group=1)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
pH_juin<-ggplot(juin, aes(hours,pH,group=2)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
pH_juillet<-ggplot(juillet, aes(hours,pH,group=3)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
pH_aout<-ggplot(aout, aes(hours,pH,group=4)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
pH_septembre<-ggplot(septembre, aes(hours,pH,group=5)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
pH_octobre<-ggplot(octobre, aes(hours,pH,group=6)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))


#O2
O2_mai<-ggplot(mai, aes(hours,O2,group=1)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
O2_juin<-ggplot(juin, aes(hours,O2,group=2)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
O2_juillet<-ggplot(juillet, aes(hours,O2,group=3)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
O2_aout<-ggplot(aout, aes(hours,O2,group=4)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
O2_septembre<-ggplot(septembre, aes(hours,O2,group=5)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
O2_octobre<-ggplot(octobre, aes(hours,O2,group=6)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))



#Conductivity
Cond_mai<-ggplot(mai, aes(hours,cond,group=1)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Cond_juin<-ggplot(juin, aes(hours,cond,group=2)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Cond_juillet<-ggplot(juillet, aes(hours,cond,group=3)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Cond_aout<-ggplot(aout, aes(hours,cond,group=4)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Cond_septembre<-ggplot(septembre, aes(hours,cond,group=5)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Cond_octobre<-ggplot(octobre, aes(hours,cond,group=6)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))



#Temperature
Temp_mai<-ggplot(mai, aes(hours,temperature,group=1)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Temp_juin<-ggplot(juin, aes(hours,temperature,group=2)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Temp_juillet<-ggplot(juillet, aes(hours,temperature,group=3)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Temp_aout<-ggplot(aout, aes(hours,temperature,group=4)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Temp_septembre<-ggplot(septembre, aes(hours,temperature,group=5)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))
Temp_octobre<-ggplot(octobre, aes(hours,temperature,group=6)) + facet_wrap(~ dateok,scales="free_y") + geom_line()+scale_x_datetime(breaks = date_breaks("12 hour"), minor_breaks=date_breaks("60 min"), labels=date_format("%H:%M:%S"))













#pH_mai<-qplot(time, pH, data = mai,geom="line") + facet_wrap(~ dateok) + ylim(5.5,7)
# plot(pH_mai)
# plot(pH_juin)
# plot(pH_juillet)
# plot(pH_aout,)
# plot(pH_septembre)
# plot(pH_octobre)



#O2
# O2_mai<-ggplot(mai, aes(time,O2,group=1)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# O2_juin<-ggplot(juin, aes(time,O2,group=2)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# O2_juillet<-ggplot(juillet, aes(time,O2,group=3)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# O2_aout<-ggplot(aout, aes(time,O2,group=4)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# O2_septembre<-ggplot(septembre, aes(time,O2,group=5)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# O2_octobre<-ggplot(octobre, aes(time,O2,group=6)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
#O2_mai<-qplot(time, O2, data = mai,geom="line") + facet_wrap(~ dateok)+ylim(4,8)
# plot(O2_mai)
# plot(O2_juin)
# plot(O2_juillet)
# plot(O2_aout,)
# plot(O2_septembre)
# plot(O2_octobre)

#Conductivity
# cond_mai<-ggplot(mai, aes(time,cond,group=1)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# cond_juin<-ggplot(juin, aes(time,cond,group=2)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# cond_juillet<-ggplot(juillet, aes(time,cond,group=3)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# cond_aout<-ggplot(aout, aes(time,cond,group=4)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# cond_septembre<-ggplot(septembre, aes(time,cond,group=5)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# cond_octobre<-ggplot(octobre, aes(time,cond,group=6)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# plot(O2_mai)
# plot(O2_juin)
# plot(O2_juillet)
# plot(O2_aout,)
# plot(O2_septembre)
# plot(O2_octobre)

#Temperature
# temp_mai<-ggplot(mai, aes(time,temperature,group=1)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# temp_juin<-ggplot(juin, aes(time,temperature,group=2)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# temp_juillet<-ggplot(juillet, aes(time,temperature,group=3)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# temp_aout<-ggplot(aout, aes(time,temperature,group=4)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# temp_septembre<-ggplot(septembre, aes(time,temperature,group=5)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# temp_octobre<-ggplot(octobre, aes(time,temperature,group=6)) + facet_wrap(~ dateok) + geom_line()+ylim(6,8)
# # plot(O2_mai)
# plot(O2_juin)
# plot(O2_juillet)
# plot(O2_aout,)
# plot(O2_septembre)
# plot(O2_octobre)

#data<-as.data.frame(mai,juin,juillet,aout,septembre,octobre)
# data<-multiplot(pH_mai,pH_juin,pH_juillet,pH_aout,pH_septembre,pH_octobre, O2_mai,O2_juin,
#                  O2_juillet,O2_aout,O2_septembre,O2_octobre,cols=4)

#print(data)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }



#Dynamique pH

library(stats)
library(readxl)


ts_max<-function(signal)
{
  points_max=which(diff(sign(diff(signal)))==-2)+1
  return(points_max)
}
ts_min<-function(signal)
{
  points_min=which(diff(sign(diff(-signal)))==-2)+1
  return(points_min)
}



#Premiere campaigne

#Decembre

pHdecembre<-decembre %>% select(pH,time,dateok)

stl_decembre=stl(ts(pHdecembre$pH, frequency=10), "periodic")

trend_decembre=as.numeric(stl_decembre$time.series[,2])
max_decembre=ts_max(trend_decembre)
min_decembre=ts_min(trend_decembre)
min_decembre<-min_decembre[-1]
plot(trend_decembre, type = 'l')
abline(v=max_decembre, col="red")
abline(v=min_decembre, col="blue")

m<-length(min_decembre)
maxydecembre<-data.frame(NULL)
minydecembre<-data.frame(NULL)

slopepHupdecembre<-data.frame(NULL)



slopepHdowndecembre<-data.frame(NULL)
for(j in 1:m){i <- min_decembre[j]; minydecembre[j,1] <- pHdecembre[i,1]; minydecembre[j,2] <- pHdecembre[i,3]}
for(j in 1:m){h <- max_decembre[j]; maxydecembre[j,1] <- pHdecembre[h,1]; maxydecembre[j,2] <- pHdecembre[h,3]}
for(a in 1:m){slopepHdowndecembre[a,1]<-((minydecembre[(a+1),1]-maxydecembre[a,1])/(min_decembre[a+1]-max_decembre[a]));slopepHdowndecembre$Month<-c('Decembre')}

names(slopepHdowndecembre)[1]<-'Delta'


slopepHupdecembre<-as.data.frame(((maxydecembre[,1]-minydecembre[,1])/(max_decembre-min_decembre)))
ggplot(slopepHupdecembre, aes(rowdecembre,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0,0.04))+theme(axis.text.x = element_text(angle = 45))
names(slopepHupdecembre)[1]<-'Delta'
slopepHupdecembre$Month<-c('Decembre')
slopepHupdecembre$Day<-minydecembre$dateok
slopepHdowndecembre$Day<-maxydecembre$dateok


rowdecembre<-as.numeric(rownames(slopepHdowndecembre))
names(slopepHupdecembre)[1]<-'Delta'

ggplot(slopepHdowndecembre, aes(rowdecembre,Delta))+geom_point()
ggplot(slopepHdowndecembre, aes(rowdecembre,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.03,0))+theme(axis.text.x = element_text(angle = 45))


#Janvier

pHjanvier<-janvier %>% select(pH,time,dateok)
stl_janvier=stl(ts(pHjanvier$pH, frequency=10), "periodic")

trend_janvier=as.numeric(stl_janvier$time.series[,2])
max_janvier=ts_max(trend_janvier)
#max_janvier<-max_janvier[-1]
min_janvier=ts_min(trend_janvier)
plot(trend_janvier, type = 'l')
abline(v=max_janvier, col="red")
abline(v=min_janvier, col="blue")

m<-length(min_janvier)
maxyjanvier<-data.frame(NULL)
minyjanvier<-data.frame(NULL)

slopepHupjanvier<-data.frame(NULL)



slopepHdownjanvier<-data.frame(NULL)
for(j in 1:m){i <- min_janvier[j]; minyjanvier[j,1] <- pHjanvier[i,1]; minyjanvier[j,2] <- pHjanvier[i,3]}
for(j in 1:m){h <- max_janvier[j]; maxyjanvier[j,1] <- pHjanvier[h,1]; maxyjanvier[j,2] <- pHjanvier[h,3]}
for(a in 1:m){slopepHdownjanvier[a,1]<-((minyjanvier[(a+1),1]-maxyjanvier[a,1])/(min_janvier[a+1]-max_janvier[a]));slopepHdownjanvier$Month<-c('Janvier')}

names(slopepHdownjanvier)[1]<-'Delta'


slopepHupjanvier<-as.data.frame(((maxyjanvier[,1]-minyjanvier[,1])/(max_janvier-min_janvier)))

names(slopepHupjanvier)[1]<-'Delta'
slopepHupjanvier$Month<-c('Janvier')
slopepHupjanvier$Day<-minyjanvier$dateok
slopepHdownjanvier$Day<-maxyjanvier$dateok


rowjanvier<-as.numeric(rownames(slopepHdownjanvier))
names(slopepHupjanvier)[1]<-'Delta'

ggplot(slopepHdownjanvier, aes(rowjanvier,Delta))+geom_point()


#Fevrier

pHfevrier<-fevrier %>% select(pH,time,dateok)

stl_fevrier=stl(ts(pHfevrier$pH, frequency=10), "periodic")

trend_fevrier=as.numeric(stl_fevrier$time.series[,2])
max_fevrier=ts_max(trend_fevrier)
#max_fevrier<-max_fevrier[-1]
min_fevrier=ts_min(trend_fevrier)
min_fevrier<-min_fevrier[-1]
plot(trend_fevrier, type = 'l')
abline(v=max_fevrier, col="red")
abline(v=min_fevrier, col="blue")

m<-length(min_fevrier)
maxyfevrier<-data.frame(NULL)
minyfevrier<-data.frame(NULL)

slopepHupfevrier<-data.frame(NULL)



slopepHdownfevrier<-data.frame(NULL)
for(j in 1:m){i <- min_fevrier[j]; minyfevrier[j,1] <- pHfevrier[i,1]; minyfevrier[j,2] <- pHfevrier[i,3]}
for(j in 1:m){h <- max_fevrier[j]; maxyfevrier[j,1] <- pHfevrier[h,1]; maxyfevrier[j,2] <- pHfevrier[h,3]}
for(a in 1:m){slopepHdownfevrier[a,1]<-((minyfevrier[(a+1),1]-maxyfevrier[a,1])/(min_fevrier[a+1]-max_fevrier[a]));slopepHdownfevrier$Month<-c('Fevrier')}

names(slopepHdownfevrier)[1]<-'Delta'


slopepHupfevrier<-as.data.frame(((maxyfevrier[,1]-minyfevrier[,1])/(max_fevrier-min_fevrier)))

names(slopepHupfevrier)[1]<-'Delta'
slopepHupfevrier$Month<-c('Fevrier')
slopepHupfevrier$Day<-minyfevrier$dateok
slopepHdownfevrier$Day<-maxyfevrier$dateok


rowfevrier<-as.numeric(rownames(slopepHdownfevrier))
names(slopepHupfevrier)[1]<-'Delta'

ggplot(slopepHdownfevrier, aes(rowfevrier,Delta))+geom_point()




#Mars

pHmars<-mars %>% select(pH,time,dateok)

stl_mars=stl(ts(pHmars$pH, frequency=10), "periodic")

trend_mars=as.numeric(stl_mars$time.series[,2])
max_mars=ts_max(trend_mars)
min_mars=ts_min(trend_mars)
min_mars<-min_mars[-1]
plot(trend_mars, type = 'l')
abline(v=max_mars, col="red")
abline(v=min_mars, col="blue")

m<-length(min_mars)
maxymars<-data.frame(NULL)
minymars<-data.frame(NULL)

slopepHupmars<-data.frame(NULL)



slopepHdownmars<-data.frame(NULL)
for(j in 1:m){i <- min_mars[j]; minymars[j,1] <- pHmars[i,1]; minymars[j,2] <- pHmars[i,3]}
for(j in 1:m){h <- max_mars[j]; maxymars[j,1] <- pHmars[h,1]; maxymars[j,2] <- pHmars[h,3]}
for(a in 1:m){slopepHdownmars[a,1]<-((minymars[(a+1),1]-maxymars[a,1])/(min_mars[a+1]-max_mars[a]));slopepHdownmars$Month<-c('Mars')}

names(slopepHdownmars)[1]<-'Delta'


slopepHupmars<-as.data.frame(((maxymars[,1]-minymars[,1])/(max_mars-min_mars)))

names(slopepHupmars)[1]<-'Delta'
slopepHupmars$Month<-c('Mars')
slopepHupmars$Day<-minymars$dateok
slopepHdownmars$Day<-maxymars$dateok


rowmars<-as.numeric(rownames(slopepHdownmars))
names(slopepHupmars)[1]<-'Delta'

ggplot(slopepHdownmars, aes(rowmars,Delta))+geom_point()

#Avril

pHavril<-avril %>% select(pH,time,dateok)

stl_avril=stl(ts(pHavril$pH, frequency=5), "periodic")

trend_avril=as.numeric(stl_avril$time.series[,2])
max_avril=ts_max(trend_avril)
max_avril<-max_avril[-1]
min_avril=ts_min(trend_avril)
min_avril<-min_avril[-1]
plot(trend_avril, type = 'l')
abline(v=max_avril, col="red")
abline(v=min_avril, col="blue")

m<-length(min_avril)
maxyavril<-data.frame(NULL)
minyavril<-data.frame(NULL)

slopepHupavril<-data.frame(NULL)



slopepHdownavril<-data.frame(NULL)
for(j in 1:m){i <- min_avril[j]; minyavril[j,1] <- pHavril[i,1]; minyavril[j,2] <- pHavril[i,3]}
for(j in 1:m){h <- max_avril[j]; maxyavril[j,1] <- pHavril[h,1]; maxyavril[j,2] <- pHavril[h,3]}
for(a in 1:m){slopepHdownavril[a,1]<-((minyavril[(a+1),1]-maxyavril[a,1])/(min_avril[a+1]-max_avril[a]));slopepHdownavril$Month<-c('Avril')}

names(slopepHdownavril)[1]<-'Delta'


slopepHupavril<-as.data.frame(((maxyavril[,1]-minyavril[,1])/(max_avril-min_avril)))

names(slopepHupavril)[1]<-'Delta'
slopepHupavril$Month<-c('Avril')
slopepHupavril$Day<-minyavril$dateok
slopepHdownavril$Day<-maxyavril$dateok


rowavril<-as.numeric(rownames(slopepHdownavril))
#names(slopepHupavril)[1]<-'Delta'

ggplot(slopepHdownavril, aes(rowavril,Delta))+geom_point()



#----------------------------------------------------------------------------------------------



#Deuxieme campaigne

#pHmai2<- read.csv(file="Chapters/Data/pHmai.csv", header=TRUE,sep="\t")

#Mai

pHmai<-mai %>% select(pH,time,dateok)

stl_mai=stl(ts(pHmai$pH, frequency=50), "periodic")

trend_mai=as.numeric(stl_mai$time.series[,2])

max_mai=ts_max(trend_mai)
max_mai<-max_mai[-1]
min_mai=ts_min(trend_mai)
#min_mai[186]<-3660
plot(trend_mai, type = 'l')
abline(v=max_mai, col="red")
abline(v=min_mai, col="blue")









m<-length(min_mai)
maxymai<-data.frame(NULL)
minymai<-data.frame(NULL)

slopepHupmai<-data.frame(NULL)


slopepHdownmai<-data.frame(NULL)
for(j in 1:m){i <- min_mai[j]; minymai[j,1] <- pHmai[i,1]; minymai[j,2] <- pHmai[i,3]}
for(j in 1:m){h <- max_mai[j]; maxymai[j,1] <- pHmai[h,1]; maxymai[j,2] <- pHmai[h,3]}
for(a in 1:m){slopepHdownmai[a,1]<-((minymai[(a+1),1]-maxymai[a,1])/(min_mai[a+1]-max_mai[a]));slopepHdownmai$Month<-c('Mai')}

names(slopepHdownmai)[1]<-'Delta'
#slopepHupmai<-((maxymai[,1]-minymai[,1])/(max_mai-min_mai))


slopepHupmai<-as.data.frame(((maxymai[,1]-minymai[,1])/(max_mai-min_mai)))

names(slopepHupmai)[1]<-'Delta'
slopepHupmai$Month<-c('Mai')
slopepHupmai$Day<-minymai$dateok
slopepHdownmai$Day<-maxymai$dateok


rowmai<-as.numeric(rownames(slopepHdownmai))

ggplot(slopepHupmai, aes(rowmai,Delta))+geom_point()

ggplot(slopepHdownmai, aes(rowmai,Delta))+geom_point()

ggplot(slopepHdownmai, aes(rowmai,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()

ggplot(slopepHupmai, aes(rowmai,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()



#Juin

pHjuin<-juin %>% select(pH,time,dateok)

stl_juin=stl(ts(pHjuin$pH, frequency=50), "periodic")

trend_juin=as.numeric(stl_juin$time.series[,2])
max_juin=ts_max(trend_juin)
min_juin=ts_min(trend_juin)
plot(trend_juin, type = 'l')
abline(v=max_juin, col="red")
abline(v=min_juin, col="blue")

m<-length(min_juin)
maxyjuin<-data.frame(NULL)
minyjuin<-data.frame(NULL)

slopepHupjuin<-data.frame(NULL)



slopepHdownjuin<-data.frame(NULL)
for(j in 1:m){i <- min_juin[j]; minyjuin[j,1] <- pHjuin[i,1]; minyjuin[j,2] <- pHjuin[i,3]}
for(j in 1:m){h <- max_juin[j]; maxyjuin[j,1] <- pHjuin[h,1]; maxyjuin[j,2] <- pHjuin[h,3]}
for(a in 1:m){slopepHdownjuin[a,1]<-((minyjuin[(a+1),1]-maxyjuin[a,1])/(min_juin[a+1]-max_juin[a]));slopepHdownjuin$Month<-c('Juin')}

names(slopepHdownjuin)[1]<-'Delta'


slopepHupjuin<-as.data.frame(((maxyjuin[,1]-minyjuin[,1])/(max_juin-min_juin)))

names(slopepHupjuin)[1]<-'Delta'
slopepHupjuin$Month<-c('Juin')
slopepHupjuin$Day<-minyjuin$dateok
slopepHdownjuin$Day<-maxyjuin$dateok


rowjuin<-as.numeric(rownames(slopepHdownjuin))
names(slopepHupjuin)[1]<-'Delta'

ggplot(slopepHdownjuin, aes(rowjuin,Delta))+geom_point()


#Juillet

pHjuillet<-juillet %>% select(pH,time,dateok)

stl_juillet=stl(ts(pHjuillet$pH, frequency=50), "periodic")

trend_juillet=as.numeric(stl_juillet$time.series[,2])
max_juillet=ts_max(trend_juillet)
max_juillet<-max_juillet[-1]
min_juillet=ts_min(trend_juillet)
plot(trend_juillet, type = 'l')
abline(v=max_juillet, col="red")
abline(v=min_juillet, col="blue")

m<-length(min_juillet)
maxyjuillet<-data.frame(NULL)
minyjuillet<-data.frame(NULL)

slopepHupjuillet<-data.frame(NULL)



slopepHdownjuillet<-data.frame(NULL)
for(j in 1:m){i <- min_juillet[j]; minyjuillet[j,1] <- pHjuillet[i,1]; minyjuillet[j,2] <- pHjuillet[i,3]}
for(j in 1:m){h <- max_juillet[j]; maxyjuillet[j,1] <- pHjuillet[h,1]; maxyjuillet[j,2] <- pHjuillet[h,3]}
for(a in 1:m){slopepHdownjuillet[a,1]<-((minyjuillet[(a+1),1]-maxyjuillet[a,1])/(min_juillet[a+1]-max_juillet[a]));slopepHdownjuillet$Month<-c('Juillet')}

names(slopepHdownjuillet)[1]<-'Delta'


slopepHupjuillet<-as.data.frame(((maxyjuillet[,1]-minyjuillet[,1])/(max_juillet-min_juillet)))

names(slopepHupjuillet)[1]<-'Delta'
slopepHupjuillet$Month<-c('Juillet')
slopepHupjuillet$Day<-minyjuillet$dateok
slopepHdownjuillet$Day<-maxyjuillet$dateok


rowjuillet<-as.numeric(rownames(slopepHdownjuillet))
names(slopepHupjuillet)[1]<-'Delta'

ggplot(slopepHdownjuillet, aes(rowjuillet,Delta))+geom_point()


#Aout

pHaout<-aout %>% select(pH,time,dateok)

stl_aout=stl(ts(pHaout$pH, frequency=100), "periodic")

trend_aout=as.numeric(stl_aout$time.series[,2])
max_aout=ts_max(trend_aout)
#max_aout<-max_aout[-1]
min_aout=ts_min(trend_aout)
min_aout<-min_aout[-1]
plot(trend_aout, type = 'l')
abline(v=max_aout, col="red")
abline(v=min_aout, col="blue")

m<-length(min_aout)
maxyaout<-data.frame(NULL)
minyaout<-data.frame(NULL)

slopepHupaout<-data.frame(NULL)



slopepHdownaout<-data.frame(NULL)
for(j in 1:m){i <- min_aout[j]; minyaout[j,1] <- pHaout[i,1]; minyaout[j,2] <- pHaout[i,3]}
for(j in 1:m){h <- max_aout[j]; maxyaout[j,1] <- pHaout[h,1]; maxyaout[j,2] <- pHaout[h,3]}
for(a in 1:m){slopepHdownaout[a,1]<-((minyaout[(a+1),1]-maxyaout[a,1])/(min_aout[a+1]-max_aout[a]));slopepHdownaout$Month<-c('Aout')}

names(slopepHdownaout)[1]<-'Delta'


slopepHupaout<-as.data.frame(((maxyaout[,1]-minyaout[,1])/(max_aout-min_aout)))

names(slopepHupaout)[1]<-'Delta'
slopepHupaout$Month<-c('Aout')
slopepHupaout$Day<-minyaout$dateok
slopepHdownaout$Day<-maxyaout$dateok


rowaout<-as.numeric(rownames(slopepHdownaout))
names(slopepHupaout)[1]<-'Delta'

ggplot(slopepHdownaout, aes(rowaout,Delta))+geom_point()


#Septembre

pHseptembre<-septembre %>% select(pH,time,dateok)

stl_septembre=stl(ts(pHseptembre$pH, frequency=50), "periodic")

trend_septembre=as.numeric(stl_septembre$time.series[,2])
max_septembre=ts_max(trend_septembre)
max_septembre<-max_septembre[-1]
min_septembre=ts_min(trend_septembre)
#min_septembre<-min_septembre[-1]
plot(trend_septembre, type = 'l')
abline(v=max_septembre, col="red")
abline(v=min_septembre, col="blue")

m<-length(min_septembre)
maxyseptembre<-data.frame(NULL)
minyseptembre<-data.frame(NULL)

slopepHupseptembre<-data.frame(NULL)



slopepHdownseptembre<-data.frame(NULL)
for(j in 1:m){i <- min_septembre[j]; minyseptembre[j,1] <- pHseptembre[i,1]; minyseptembre[j,2] <- pHseptembre[i,3]}
for(j in 1:m){h <- max_septembre[j]; maxyseptembre[j,1] <- pHseptembre[h,1]; maxyseptembre[j,2] <- pHseptembre[h,3]}
for(a in 1:m){slopepHdownseptembre[a,1]<-((minyseptembre[(a+1),1]-maxyseptembre[a,1])/(min_septembre[a+1]-max_septembre[a]));slopepHdownseptembre$Month<-c('Septembre')}

names(slopepHdownseptembre)[1]<-'Delta'


slopepHupseptembre<-as.data.frame(((maxyseptembre[,1]-minyseptembre[,1])/(max_septembre-min_septembre)))

names(slopepHupseptembre)[1]<-'Delta'
slopepHupseptembre$Month<-c('Septembre')
slopepHupseptembre$Day<-minyseptembre$dateok
slopepHdownseptembre$Day<-maxyseptembre$dateok


rowseptembre<-as.numeric(rownames(slopepHdownseptembre))
names(slopepHupseptembre)[1]<-'Delta'

ggplot(slopepHdownseptembre, aes(rowseptembre,Delta))+geom_point()


#Octobre

pHoctobre<-octobre %>% select(pH,time,dateok)

stl_octobre=stl(ts(pHoctobre$pH, frequency=50), "periodic")

trend_octobre=as.numeric(stl_octobre$time.series[,2])
max_octobre=ts_max(trend_octobre)
#max_octobre<-max_octobre[-1]
min_octobre=ts_min(trend_octobre)
min_octobre<-min_octobre[-1]
plot(trend_octobre, type = 'l')
abline(v=max_octobre, col="red")
abline(v=min_octobre, col="blue")

m<-length(min_octobre)
maxyoctobre<-data.frame(NULL)
minyoctobre<-data.frame(NULL)

slopepHupoctobre<-data.frame(NULL)



slopepHdownoctobre<-data.frame(NULL)
for(j in 1:m){i <- min_octobre[j]; minyoctobre[j,1] <- pHoctobre[i,1]; minyoctobre[j,2] <- pHoctobre[i,3]}
for(j in 1:m){h <- max_octobre[j]; maxyoctobre[j,1] <- pHoctobre[h,1]; maxyoctobre[j,2] <- pHoctobre[h,3]}
for(a in 1:m){slopepHdownoctobre[a,1]<-((minyoctobre[(a+1),1]-maxyoctobre[a,1])/(min_octobre[a+1]-max_octobre[a]));slopepHdownoctobre$Month<-c('Octobre')}

names(slopepHdownoctobre)[1]<-'Delta'


slopepHupoctobre<-as.data.frame(((maxyoctobre[,1]-minyoctobre[,1])/(max_octobre-min_octobre)))

names(slopepHupoctobre)[1]<-'Delta'
slopepHupoctobre$Month<-c('Octobre')
slopepHupoctobre$Day<-minyoctobre$dateok
slopepHdownoctobre$Day<-maxyoctobre$dateok


rowoctobre<-as.numeric(rownames(slopepHdownoctobre))
names(slopepHupoctobre)[1]<-'Delta'

ggplot(slopepHdownoctobre, aes(rowoctobre,Delta))+geom_point()









#Total delta up and down


#First campaing
slopepHuptotalfirst<-data.frame(NULL)
slopepHupdecembre$Inflection_points<-rowdecembre
slopepHupjanvier$Inflection_points<-rowjanvier
slopepHupfevrier$Inflection_points<-rowfevrier
slopepHupmars$Inflection_points<-rowmars
slopepHupavril$Inflection_points<-rowavril


slopepHuptotalfirst<-rbind(slopepHupavril,slopepHupmars,slopepHupfevrier,slopepHupjanvier,slopepHupdecembre)

neworder1st <- c("Decembre","Janvier","Fevrier","Mars","Avril")
slopepHuptotalfirst2<-arrange(transform(slopepHuptotalfirst,Month=factor(Month,levels=neworder1st)),Month)

ggplot(slopepHuptotalfirst2, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Month,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0,0.04))


#Average per day Up

averageslopepHuptotalfirst2<-aggregate(list(Deltaperday = slopepHuptotalfirst2$Delta), list(Day= slopepHuptotalfirst2$Day), mean)
averageslopepHuptotalfirst2$Day <- as.Date(averageslopepHuptotalfirst2$Day , "%Y-%m-%d")


meandeltaupfirst<-ggplot(averageslopepHuptotalfirst2, aes(Day,Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0.005,0.04))+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2018-12-04','2019-05-03')))
meandeltaupfirstnum<-ggplot(averageslopepHuptotalfirst2, aes(as.numeric(rownames(averageslopepHuptotalfirst2)),Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0.005,0.04))

slopepHdowndecembre$Inflection_points<-rowdecembre
slopepHdownjanvier$Inflection_points<-rowjanvier
slopepHdownfevrier$Inflection_points<-rowfevrier
slopepHdownmars$Inflection_points<-rowmars
slopepHdownavril$Inflection_points<-rowavril
slopepHdowntotalfirst<-rbind(slopepHdownavril,slopepHdownmars,slopepHdownfevrier,slopepHdownjanvier,slopepHdowndecembre)

#neworder1st <- c("Decembre","Janvier","Fevrier","Mars","Avril")
slopepHdowntotalfirst2<-arrange(transform(slopepHdowntotalfirst,Month=factor(Month,levels=neworder1st)),Month)

ggplot(slopepHdowntotalfirst2, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Month,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.05,0))

#Average per day Down

averageslopepHdowntotalfirst2<-aggregate(list(Deltaperday = slopepHdowntotalfirst2$Delta), list(Day= slopepHdowntotalfirst2$Day), mean)
averageslopepHdowntotalfirst2$Day <- as.Date(averageslopepHdowntotalfirst2$Day , "%Y-%m-%d")


meandeltadownfirst<-ggplot(averageslopepHdowntotalfirst2, aes(Day,Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.05,0))+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2018-12-04','2019-05-03')))
meandeltadownfirstnum<-ggplot(averageslopepHdowntotalfirst2, aes(as.numeric(rownames(averageslopepHdowntotalfirst2)),Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.05,0))

#ggplot(slopepHdowntotalfirst, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()




#second campaing
slopepHupmai$Inflection_points<-rowmai
slopepHupjuin$Inflection_points<-rowjuin
slopepHupjuillet$Inflection_points<-rowjuillet
slopepHupaout$Inflection_points<-rowaout
slopepHupseptembre$Inflection_points<-rowseptembre
slopepHupoctobre$Inflection_points<-rowoctobre

slopepHuptotalsecond<-rbind(slopepHupoctobre,slopepHupseptembre,slopepHupaout,slopepHupjuillet,slopepHupjuin,slopepHupmai)

neworder2nd <- c("Mai","Juin","Juillet","Aout","Septembre","Octobre")
slopepHuptotalsecond2<-arrange(transform(slopepHuptotalsecond,Month=factor(Month,levels=neworder2nd)),Month)
ggplot(slopepHuptotalsecond2, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Month,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0,0.01))


#Average per day Up

averageslopepHuptotalsecond2<-aggregate(list(Deltaperday = slopepHuptotalsecond2$Delta), list(Day= slopepHuptotalsecond2$Day), mean)
averageslopepHuptotalsecond2$Day <- as.Date(averageslopepHuptotalsecond2$Day , "%Y-%m-%d")



meandeltaupsecond<-ggplot(averageslopepHuptotalsecond2, aes(Day,Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0,0.005))+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2019-05-15','2019-10-17')))
meandeltaupsecondnum<-ggplot(averageslopepHuptotalsecond2, aes(as.numeric(rownames(averageslopepHuptotalsecond2)),Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0,0.005))


slopepHdownmai$Inflection_points<-rowmai
slopepHdownjuin$Inflection_points<-rowjuin
slopepHdownjuillet$Inflection_points<-rowjuillet
slopepHdownaout$Inflection_points<-rowaout
slopepHdownseptembre$Inflection_points<-rowseptembre
slopepHdownoctobre$Inflection_points<-rowoctobre

slopepHdowntotalsecond<-rbind(slopepHdownoctobre,slopepHdownseptembre,slopepHdownaout,slopepHdownjuillet,slopepHdownjuin,slopepHdownmai)

#neworder2nd <- c("Mai","Juin","Juillet","Aout","Septembre","Octobre")
slopepHdowntotalsecond2<-arrange(transform(slopepHdowntotalsecond,Month=factor(Month,levels=neworder2nd)),Month)
ggplot(slopepHdowntotalsecond2, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Month,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.005,0))


#Average per dayDown

averageslopepHdowntotalsecond2<-aggregate(list(Deltaperday = slopepHdowntotalsecond2$Delta), list(Day= slopepHdowntotalsecond2$Day), mean)
averageslopepHdowntotalsecond2$Day <- as.Date(averageslopepHdowntotalsecond2$Day , "%Y-%m-%d")


meandeltadownsecond<-ggplot(averageslopepHdowntotalsecond2, aes(Day,Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2019-05-15','2019-10-17')))
meandeltadownsecondnum<-ggplot(averageslopepHdowntotalsecond2, aes(as.numeric(rownames(averageslopepHdowntotalsecond2)),Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))


#ggplot(slopepHdowntotalsecond, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()


#TSH et NLR

TSH1st<-rbind(TSHdecembre,TSHjanvier,TSHfevrier,TSHmars,TSHavril)
names(TSH1st)<-c("TSH","Units","Day","Date")
TSH1st$Date<-as.Date(TSH1st$Date)
TSHfirstcampaign<-ggplot(TSH1st,aes(Date,TSH,group=1))+geom_point()+xlab("Days")+ylab("HRT (h)")+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2018-12-04','2019-05-03')))
TSH2nd<-rbind(TSHmai,TSHjuin,TSHjuillet,TSHaout,TSHseptembre,TSHoctobre)
names(TSH2nd)<-c("TSH","Units","Day","Date")
TSH2nd$Date<-as.Date(TSH2nd$Date)
TSHsecondcampaign<-ggplot(TSH2nd,aes(Date,TSH,group=1))+geom_point()+xlab("Days")+ylab("HRT (d)")+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2019-05-15','2019-10-17')))


meandeltadownfirst/TSHfirstcampaign
meandeltadownsecond/TSHsecondcampaign


#NLR
NLRfirstcampaign<-as.data.frame(cbind(data_LEE_1$`NLR kg/m3/jour`,data_LEE_1$Date))
names(NLRfirstcampaign)<-c("NLR kg/m3/jour","Date")
`Data 1st camp`$Date<-as.Date(`Data 1st camp`$Date,"%Y-%m-%d")
NLR1stcampaign<-ggplot(`Data 1st camp`,aes(`Data 1st camp`$Date,`Data 1st camp`$`NLR kg/m3/jour`,group=1))+geom_point()+xlab("Day")+ylab("NLR (kg/m3/day)")+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2018-12-04','2019-05-03')))

NLRsecondcampaign<-as.data.frame(cbind(data_LEE_2$`NLR kg/m3/jour`,data_LEE_2$Date))
names(NLRsecondcampaign)<-c("NLR kg/m3/jour","Date")
`Data 2nd camp`$Date<-as.Date(`Data 2nd camp`$Date,"%Y-%m-%d")
NLR2ndcampaign<-ggplot(`Data 2nd camp`,aes(`Data 2nd camp`$Date,`Data 2nd camp`$`NLR kg/m3/jour`,group=1))+geom_point()+xlab("Day")+ylab("NLR (kg/m3/day)")+scale_x_date(date_breaks = "20 day", labels=date_format("%b-%Y"),limits = as.Date(c('2019-05-15','2019-10-17')))

meandeltadownfirst/NLR1stcampaign
meandeltadownsecond/NLR2ndcampaign

meandeltadownsecond/TSHsecondcampaign
meandeltadownfirst/TSHfirstcampaign/NLR1stcampaign


meandeltaupfirst/meandeltadownfirst/TSHfirstcampaign/NLR1stcampaign


meandeltaupsecond/meandeltadownsecond/TSHsecondcampaign/NLR2ndcampaign

meandeltadownsecond/TSHsecondcampaign/NLR2ndcampaign
meandeltaupfirst/TSHfirstcampaign/NLR1stcampaign
meandeltaupsecond/TSHsecondcampaign/NLR2ndcampaign

meandeltaupfirstnum/meandeltadownfirstnum
meandeltaupsecondnum/meandeltadownsecondnum


#Delta down vs HRT 
#first campaign

TSH1st2<-TSH1st[-c(27:28),]
averageslopepHdowntotalfirst2$HRT<-TSH1st2$TSH
#For each pH set

averageslopepHdowntotalfirst1pHset<-averageslopepHdowntotalfirst2[c(1:97),]
averageslopepHdowntotalfirst2pHset<-averageslopepHdowntotalfirst2[c(98:134),]
averageslopepHdowntotalfirst3pHset<-averageslopepHdowntotalfirst2[c(135:146),]
ggplot(averageslopepHdowntotalfirst2, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (h)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.02,0)) +ggtitle("Total first campaign")

ggplot(averageslopepHdowntotalfirst1pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (h)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.02,0))+scale_x_continuous(limits=c(0,20))+ggtitle("pH set 6.2-6.5")
set1<-ggplot(averageslopepHdowntotalfirst1pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (h)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.02,0))+ggtitle("pH set 6.2-6.5")

ggplot(averageslopepHdowntotalfirst2pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (h)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.02,0))+ggtitle("pH set 6.2-6.4")
set2<-ggplot(averageslopepHdowntotalfirst2pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (h)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.02,0))+ggtitle("pH set 6.2-6.4")

ggplot(averageslopepHdowntotalfirst3pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (h)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.02,0))+ggtitle("pH set 6.2-6.25")
set3<-ggplot(averageslopepHdowntotalfirst3pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (h)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.02,0))+ggtitle("pH set 6.2-6.25")


set1+set2/set3

#second campaign
TSH2nd2<-TSH2nd[-c(1:13),]
TSH2nd2<-TSH2nd2[-c(1),]
averageslopepHdowntotalsecond2$HRT<-TSH2nd2$TSH

#For each pH set
averageslopepHdowntotalsecond1pHset<-averageslopepHdowntotalsecond2[c(1:14),]
averageslopepHdowntotalsecond2pHset<-averageslopepHdowntotalsecond2[c(15:40),]
averageslopepHdowntotalsecond3pHset<-averageslopepHdowntotalsecond2[c(41:63),]
averageslopepHdowntotalsecond4pHset<-averageslopepHdowntotalsecond2[c(64:147),]



ggplot(averageslopepHdowntotalsecond2, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("Total second campaign")


ggplot(averageslopepHdowntotalsecond1pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 6.2-6.25")
set4<-ggplot(averageslopepHdowntotalsecond1pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 6.2-6.25")

ggplot(averageslopepHdowntotalsecond2pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 5.8-5.85")
set5<-ggplot(averageslopepHdowntotalsecond2pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 5.8-5.85")

ggplot(averageslopepHdowntotalsecond3pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 6.2-6.25")
set6<-ggplot(averageslopepHdowntotalsecond3pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 6.2-6.25")

ggplot(averageslopepHdowntotalsecond4pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 7.0-7.05")+scale_x_continuous(limits=c(0,12))
set7<-ggplot(averageslopepHdowntotalsecond4pHset, aes(HRT,Deltaperday,group=1)) + geom_point()+xlab("HRT (d)")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+ggtitle("pH set 7.0-7.05")+scale_x_continuous(limits=c(0,12))

(set4+set5)/(set6+set7)




#%----------------------------------------


# 
# 
# #Dynamique pH
# 
# library(stats)
# library(readxl)
# 
# 
# ts_max<-function(signal)
# {
#   points_max=which(diff(sign(diff(signal)))==-2)+1
#   return(points_max)
# }
# ts_min<-function(signal)
# {
#   points_min=which(diff(sign(diff(-signal)))==-2)+1
#   return(points_min)
# }
# 
# 
# 
# 
# #----------------------------------------------------------------------------------------------
# 
# 
# 
# #Deuxieme campaigne
# 
# #pHmai2<- read.csv(file="Chapters/Data/pHmai.csv", header=TRUE,sep="\t")
# 
# #Mai
# 
# pHmai<- read.csv(file="pHmai.csv",header=TRUE,sep="\t")
# 
# stl_mai=stl(ts(pHmai$pH, frequency=50), "periodic")
# 
# trend_mai=as.numeric(stl_mai$time.series[,2])
# 
# max_mai=ts_max(trend_mai)
# max_mai<-max_mai[-1]
# min_mai=ts_min(trend_mai)
# #min_mai[186]<-3660
# plot(trend_mai, type = 'l')
# abline(v=max_mai, col="red")
# abline(v=min_mai, col="blue")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# m<-length(min_mai)
# maxymai<-data.frame(NULL)
# minymai<-data.frame(NULL)
# 
# slopepHupmai<-data.frame(NULL)
# 
# 
# slopepHdownmai<-data.frame(NULL)
# for(j in 1:m){i <- min_mai[j]; minymai[j,1] <- pHmai[i,1]; minymai[j,2] <- pHmai[i,3]}
# for(j in 1:m){h <- max_mai[j]; maxymai[j,1] <- pHmai[h,1]; maxymai[j,2] <- pHmai[h,3]}
# for(a in 1:m){slopepHdownmai[a,1]<-((minymai[(a+1),1]-maxymai[a,1])/(min_mai[a+1]-max_mai[a]));slopepHdownmai$Month<-c('Mai')}
# 
# names(slopepHdownmai)[1]<-'Delta'
# #slopepHupmai<-((maxymai[,1]-minymai[,1])/(max_mai-min_mai))
# 
# 
# slopepHupmai<-as.data.frame(((maxymai[,1]-minymai[,1])/(max_mai-min_mai)))
# names(slopepHupmai)[1]<-'Delta'
# names(maxymai)<-c("pH","dateok")
# names(minymai)<-c("pH","dateok")
# slopepHupmai$Month<-c('Mai')
# slopepHupmai$Day<-minymai$dateok
# slopepHdownmai$Day<-maxymai$dateok
# 
# 
# rowmai<-as.numeric(rownames(slopepHdownmai))
# 
# ggplot(slopepHupmai, aes(rowmai,Delta))+geom_point()
# 
# ggplot(slopepHdownmai, aes(rowmai,Delta))+geom_point()
# 
# ggplot(slopepHdownmai, aes(rowmai,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()
# 
# ggplot(slopepHupmai, aes(rowmai,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()
# 
# 
# 
# #Juin
# 
# pHjuin<- read.csv(file="pHjuin.csv",header=TRUE,sep="\t")
# 
# stl_juin=stl(ts(pHjuin$pH, frequency=50), "periodic")
# 
# trend_juin=as.numeric(stl_juin$time.series[,2])
# max_juin=ts_max(trend_juin)
# min_juin=ts_min(trend_juin)
# plot(trend_juin, type = 'l')
# abline(v=max_juin, col="red")
# abline(v=min_juin, col="blue")
# 
# m<-length(min_juin)
# maxyjuin<-data.frame(NULL)
# minyjuin<-data.frame(NULL)
# 
# slopepHupjuin<-data.frame(NULL)
# 
# 
# 
# slopepHdownjuin<-data.frame(NULL)
# for(j in 1:m){i <- min_juin[j]; minyjuin[j,1] <- pHjuin[i,1]; minyjuin[j,2] <- pHjuin[i,3]}
# for(j in 1:m){h <- max_juin[j]; maxyjuin[j,1] <- pHjuin[h,1]; maxyjuin[j,2] <- pHjuin[h,3]}
# for(a in 1:m){slopepHdownjuin[a,1]<-((minyjuin[(a+1),1]-maxyjuin[a,1])/(min_juin[a+1]-max_juin[a]));slopepHdownjuin$Month<-c('Juin')}
# 
# names(slopepHdownjuin)[1]<-'Delta'
# 
# 
# slopepHupjuin<-as.data.frame(((maxyjuin[,1]-minyjuin[,1])/(max_juin-min_juin)))
# 
# names(slopepHupjuin)[1]<-'Delta'
# names(maxyjuin)<-c("pH","dateok")
# names(minyjuin)<-c("pH","dateok")
# slopepHupjuin$Month<-c('Juin')
# slopepHupjuin$Day<-minyjuin$dateok
# slopepHdownjuin$Day<-maxyjuin$dateok
# 
# 
# rowjuin<-as.numeric(rownames(slopepHdownjuin))
# names(slopepHupjuin)[1]<-'Delta'
# 
# ggplot(slopepHdownjuin, aes(rowjuin,Delta))+geom_point()
# 
# 
# #Juillet
# 
# pHjuillet<- read.csv(file="pHjuillet.csv",header=TRUE,sep="\t")
# 
# stl_juillet=stl(ts(pHjuillet$pH, frequency=50), "periodic")
# 
# trend_juillet=as.numeric(stl_juillet$time.series[,2])
# max_juillet=ts_max(trend_juillet)
# max_juillet<-max_juillet[-1]
# min_juillet=ts_min(trend_juillet)
# plot(trend_juillet, type = 'l')
# abline(v=max_juillet, col="red")
# abline(v=min_juillet, col="blue")
# 
# m<-length(min_juillet)
# maxyjuillet<-data.frame(NULL)
# minyjuillet<-data.frame(NULL)
# 
# slopepHupjuillet<-data.frame(NULL)
# 
# 
# 
# slopepHdownjuillet<-data.frame(NULL)
# for(j in 1:m){i <- min_juillet[j]; minyjuillet[j,1] <- pHjuillet[i,1]; minyjuillet[j,2] <- pHjuillet[i,3]}
# for(j in 1:m){h <- max_juillet[j]; maxyjuillet[j,1] <- pHjuillet[h,1]; maxyjuillet[j,2] <- pHjuillet[h,3]}
# for(a in 1:m){slopepHdownjuillet[a,1]<-((minyjuillet[(a+1),1]-maxyjuillet[a,1])/(min_juillet[a+1]-max_juillet[a]));slopepHdownjuillet$Month<-c('Juillet')}
# 
# names(slopepHdownjuillet)[1]<-'Delta'
# 
# 
# slopepHupjuillet<-as.data.frame(((maxyjuillet[,1]-minyjuillet[,1])/(max_juillet-min_juillet)))
# 
# names(slopepHupjuillet)[1]<-'Delta'
# names(maxyjuillet)<-c("pH","dateok")
# names(minyjuillet)<-c("pH","dateok")
# slopepHupjuillet$Month<-c('Juillet')
# slopepHupjuillet$Day<-minyjuillet$dateok
# slopepHdownjuillet$Day<-maxyjuillet$dateok
# 
# 
# rowjuillet<-as.numeric(rownames(slopepHdownjuillet))
# names(slopepHupjuillet)[1]<-'Delta'
# 
# ggplot(slopepHdownjuillet, aes(rowjuillet,Delta))+geom_point()
# 
# 
# #Aout
# 
# pHaout<- read.csv(file="pHaout.csv",header=TRUE,sep="\t")
# 
# stl_aout=stl(ts(pHaout$pH, frequency=50), "periodic")
# 
# trend_aout=as.numeric(stl_aout$time.series[,2])
# max_aout=ts_max(trend_aout)
# min_aout=ts_min(trend_aout)
# min_aout<-min_aout[-1]
# plot(trend_aout, type = 'l')
# abline(v=max_aout, col="red")
# abline(v=min_aout, col="blue")
# 
# m<-length(min_aout)
# maxyaout<-data.frame(NULL)
# minyaout<-data.frame(NULL)
# 
# slopepHupaout<-data.frame(NULL)
# 
# 
# 
# slopepHdownaout<-data.frame(NULL)
# for(j in 1:m){i <- min_aout[j]; minyaout[j,1] <- pHaout[i,1]; minyaout[j,2] <- pHaout[i,3]}
# for(j in 1:m){h <- max_aout[j]; maxyaout[j,1] <- pHaout[h,1]; maxyaout[j,2] <- pHaout[h,3]}
# for(a in 1:m){slopepHdownaout[a,1]<-((minyaout[(a+1),1]-maxyaout[a,1])/(min_aout[a+1]-max_aout[a]));slopepHdownaout$Month<-c('Aout')}
# 
# names(slopepHdownaout)[1]<-'Delta'
# 
# 
# slopepHupaout<-as.data.frame(((maxyaout[,1]-minyaout[,1])/(max_aout-min_aout)))
# 
# names(slopepHupaout)[1]<-'Delta'
# names(maxyaout)<-c("pH","dateok")
# names(minyaout)<-c("pH","dateok")
# slopepHupaout$Month<-c('Aout')
# slopepHupaout$Day<-minyaout$dateok
# slopepHdownaout$Day<-maxyaout$dateok
# 
# 
# rowaout<-as.numeric(rownames(slopepHdownaout))
# names(slopepHupaout)[1]<-'Delta'
# 
# ggplot(slopepHdownaout, aes(rowaout,Delta))+geom_point()
# 
# 
# #Septembre
# 
# pHseptembre<- read.csv(file="pHseptembre.csv",header=TRUE,sep="\t")
# 
# stl_septembre=stl(ts(pHseptembre$pH, frequency=50), "periodic")
# 
# trend_septembre=as.numeric(stl_septembre$time.series[,2])
# max_septembre=ts_max(trend_septembre)
# max_septembre<-max_septembre[-1]
# min_septembre=ts_min(trend_septembre)
# plot(trend_septembre, type = 'l')
# abline(v=max_septembre, col="red")
# abline(v=min_septembre, col="blue")
# 
# m<-length(min_septembre)
# maxyseptembre<-data.frame(NULL)
# minyseptembre<-data.frame(NULL)
# 
# slopepHupseptembre<-data.frame(NULL)
# 
# 
# 
# slopepHdownseptembre<-data.frame(NULL)
# for(j in 1:m){i <- min_septembre[j]; minyseptembre[j,1] <- pHseptembre[i,1]; minyseptembre[j,2] <- pHseptembre[i,3]}
# for(j in 1:m){h <- max_septembre[j]; maxyseptembre[j,1] <- pHseptembre[h,1]; maxyseptembre[j,2] <- pHseptembre[h,3]}
# for(a in 1:m){slopepHdownseptembre[a,1]<-((minyseptembre[(a+1),1]-maxyseptembre[a,1])/(min_septembre[a+1]-max_septembre[a]));slopepHdownseptembre$Month<-c('Septembre')}
# 
# names(slopepHdownseptembre)[1]<-'Delta'
# 
# 
# slopepHupseptembre<-as.data.frame(((maxyseptembre[,1]-minyseptembre[,1])/(max_septembre-min_septembre)))
# 
# names(slopepHupseptembre)[1]<-'Delta'
# names(maxyseptembre)<-c("pH","dateok")
# names(minyseptembre)<-c("pH","dateok")
# slopepHupseptembre$Month<-c('Septembre')
# slopepHupseptembre$Day<-minyseptembre$dateok
# slopepHdownseptembre$Day<-maxyseptembre$dateok
# 
# 
# rowseptembre<-as.numeric(rownames(slopepHdownseptembre))
# names(slopepHupseptembre)[1]<-'Delta'
# 
# ggplot(slopepHdownseptembre, aes(rowseptembre,Delta))+geom_point()
# 
# 
# #Octobre
# 
# pHoctobre<- read.csv(file="pHoctobre.csv",header=TRUE,sep="\t")
# 
# stl_octobre=stl(ts(pHoctobre$pH, frequency=50), "periodic")
# 
# trend_octobre=as.numeric(stl_octobre$time.series[,2])
# max_octobre=ts_max(trend_octobre)
# min_octobre=ts_min(trend_octobre)
# min_octobre<-min_octobre[-1]
# plot(trend_octobre, type = 'l')
# abline(v=max_octobre, col="red")
# abline(v=min_octobre, col="blue")
# 
# m<-length(min_octobre)
# maxyoctobre<-data.frame(NULL)
# minyoctobre<-data.frame(NULL)
# 
# slopepHupoctobre<-data.frame(NULL)
# 
# 
# 
# slopepHdownoctobre<-data.frame(NULL)
# for(j in 1:m){i <- min_octobre[j]; minyoctobre[j,1] <- pHoctobre[i,1]; minyoctobre[j,2] <- pHoctobre[i,3]}
# for(j in 1:m){h <- max_octobre[j]; maxyoctobre[j,1] <- pHoctobre[h,1]; maxyoctobre[j,2] <- pHoctobre[h,3]}
# for(a in 1:m){slopepHdownoctobre[a,1]<-((minyoctobre[(a+1),1]-maxyoctobre[a,1])/(min_octobre[a+1]-max_octobre[a]));slopepHdownoctobre$Month<-c('Octobre')}
# 
# names(slopepHdownoctobre)[1]<-'Delta'
# 
# 
# slopepHupoctobre<-as.data.frame(((maxyoctobre[,1]-minyoctobre[,1])/(max_octobre-min_octobre)))
# 
# names(slopepHupoctobre)[1]<-'Delta'
# names(maxyoctobre)<-c("pH","dateok")
# names(minyoctobre)<-c("pH","dateok")
# slopepHupoctobre$Month<-c('Octobre')
# slopepHupoctobre$Day<-minyoctobre$dateok
# slopepHdownoctobre$Day<-maxyoctobre$dateok
# 
# 
# rowoctobre<-as.numeric(rownames(slopepHdownoctobre))
# names(slopepHupoctobre)[1]<-'Delta'
# 
# ggplot(slopepHdownoctobre, aes(rowoctobre,Delta))+geom_point()
# 
# 
# 
# 
# 
# 
# 
# 
# #--------------------------------------------------------------------------------------------------------------
# 
# 
# #Total delta up and down
# 
# 
# 
# #second campaing
# slopepHupmai$Inflection_points<-rowmai
# slopepHupjuin$Inflection_points<-rowjuin
# slopepHupjuillet$Inflection_points<-rowjuillet
# slopepHupaout$Inflection_points<-rowaout
# slopepHupseptembre$Inflection_points<-rowseptembre
# slopepHupoctobre$Inflection_points<-rowoctobre
# 
# slopepHuptotalsecond<-rbind(slopepHupoctobre,slopepHupseptembre,slopepHupaout,slopepHupjuillet,slopepHupjuin,slopepHupmai)
# 
# neworder2nd <- c("Mai","Juin","Juillet","Aout","Septembre","Octobre")
# slopepHuptotalsecond2<-arrange(transform(slopepHuptotalsecond,Month=factor(Month,levels=neworder2nd)),Month)
# ggplot(slopepHuptotalsecond2, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Month,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0,0.01))
# 
# 
# #Average per day Up
# 
# averageslopepHuptotalsecond2<-aggregate(list(Deltaperday = slopepHuptotalsecond2$Delta), list(Day= slopepHuptotalsecond2$Day), mean)
# averageslopepHuptotalsecond2$Day <- as.Date(averageslopepHuptotalsecond2$Day , "%Y-%m-%d")
# 
# 
# 
# meandeltaupsecond<-ggplot(averageslopepHuptotalsecond2, aes(Day,Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Up (units/second)")+scale_y_continuous(limits=c(0,0.01))+scale_x_date(date_breaks = "10 day", labels=date_format("%b-%Y"),limits = as.Date(c('2018-12-04','2019-05-03')))
# 
# 
# slopepHdownmai$Inflection_points<-rowmai
# slopepHdownjuin$Inflection_points<-rowjuin
# slopepHdownjuillet$Inflection_points<-rowjuillet
# slopepHdownaout$Inflection_points<-rowaout
# slopepHdownseptembre$Inflection_points<-rowseptembre
# slopepHdownoctobre$Inflection_points<-rowoctobre
# 
# slopepHdowntotalsecond<-rbind(slopepHdownoctobre,slopepHdownseptembre,slopepHdownaout,slopepHdownjuillet,slopepHdownjuin,slopepHdownmai)
# 
# #neworder2nd <- c("Mai","Juin","Juillet","Aout","Septembre","Octobre")
# slopepHdowntotalsecond2<-arrange(transform(slopepHdowntotalsecond,Month=factor(Month,levels=neworder2nd)),Month)
# ggplot(slopepHdowntotalsecond2, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Month,scales="free_x") + geom_point()+xlab("Batch number")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.005,0))
# 
# 
# #Average per dayDown
# 
# averageslopepHdowntotalsecond2<-aggregate(list(Deltaperday = slopepHdowntotalsecond2$Delta), list(Day= slopepHdowntotalsecond2$Day), mean)
# averageslopepHdowntotalsecond2$Day <- as.Date(averageslopepHdowntotalsecond2$Day , "%Y-%m-%d")
# 
# 
# meandeltadownsecond<-ggplot(averageslopepHdowntotalsecond2, aes(Day,Deltaperday,group=1)) + geom_point()+xlab("Daily average")+ylab("Delta Down (units/second)")+scale_y_continuous(limits=c(-0.003,0))+scale_x_date(date_breaks = "10 day", labels=date_format("%b-%Y"),limits = as.Date(c('2019-05-15','2019-10-17')))
# 
# 
# #ggplot(slopepHdowntotalsecond, aes(Inflection_points,Delta,group=1)) + facet_wrap(~ Day,scales="free_x") + geom_point()
# 
# 
# #TSH et NLR
# 
# TSH2nd<-read.csv(file="TSH2nd.csv", header=TRUE,sep="\t")
# 
# Data_2nd_camp<-read.csv(file="data_2nd_camp.csv", header=FALSE,skip = 1,sep="\t")
# names_data_2nd_camp<-c("Date",	"Days",	"TSH (d)",	"Dilution",	"Debit (ml/min)",	"pH range",	"NLR kg/m3/jour",	"1"	,"NH4 (mgN/L)",	"LQNH4",	"NO2 (mgN/L)",	"LQNO2",	"NO3 (mgN/L)",	"LQNO3",	"NTK (mgN/L)",	"LQNTK"	,"Inc NTK",	"NTotal (mg/L)",	"NTK/NH4",	"NH4/NTK",	"Nomenclature",	"Alcalinity (mgCaCO3/L)","NH4","NO2",	"NO3",	"Alcalinité TA","Nomenclatures",	"NH4 (mgN/L) s"	,"LQNH4s",	"NO2 (mgN/L) s",	"LQNO2s",	"NO3 (mgN/L) s",	"LQNO3s",	"NTKs",	"LQNTKs",	"Inc NTKs",	"N Total s",	"BILAN MATIERE",	"Nomenclatures",	"Alcalinite s TAC","NH4s","NO2s",	"NO3s",	"somme mg N/L", "somme1", "somme2", "somme3","%inhibition")
# names(Data_2nd_camp)<-names_data_2nd_camp
# meandeltadownfirst/TSHfirstcampaign
# meandeltadownsecond/TSHsecondcampaign
# 
# 
# #NLR
# NLRfirstcampaign<-as.data.frame(cbind(data_LEE_1$`NLR kg/m3/jour`,data_LEE_1$Date))
# names(NLRfirstcampaign)<-c(	"NLR kg/m3/jour","Date")
# `Data 1st camp`$Date<-as.Date(`Data 1st camp`$Date,"%Y-%m-%d")
# NLR1stcampaign<-ggplot(`Data 1st camp`,aes(`Data 1st camp`$Date,`Data 1st camp`$`NLR kg/m3/jour`,group=1))+geom_point()+xlab("Day")+ylab("NLR (kg/m3/day)")+scale_x_date(date_breaks = "10 day", labels=date_format("%b-%Y"),limits = as.Date(c('2018-12-04','2019-05-03')))
# 
# NLRsecondcampaign<-as.data.frame(cbind(data_LEE_2$`NLR kg/m3/jour`,data_LEE_2$Date))
# names(NLRsecondcampaign)<-c(	"NLR kg/m3/jour","Date")
# `Data 2nd camp`$Date<-as.Date(`Data 2nd camp`$Date,"%Y-%m-%d")
# NLR2ndcampaign<-ggplot(`Data 2nd camp`,aes(`Data 2nd camp`$Date,`Data 2nd camp`$`NLR kg/m3/jour`,group=1))+geom_point()+xlab("Day")+ylab("NLR (kg/m3/day)")+scale_x_date(date_breaks = "10 day", labels=date_format("%b-%Y"),limits = as.Date(c('2019-05-15','2019-10-17')))
# 
# meandeltadownfirst/NLR1stcampaign
# meandeltadownsecond/NLR2ndcampaign
# 
# 
# meandeltadownfirst/TSHfirstcampaign/NLR1stcampaign
# 
# meandeltadownsecond/TSHsecondcampaign/NLR2ndcampaign




