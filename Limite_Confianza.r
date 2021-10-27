
#2v0 datos ##############################################################
Data<-LecturaXTBForex("EURUSD","1h")
#2v0 datos ##############################################################

#3v0 Indicadores ##############################################################
nPeriodos<-14
nOfset<-1
dsma<-3
#############################################
Data$aux<-(c(NULL,Lag(Data$Close,1)))
Data$RetLog<-log(Data$Close/Data$aux)
log(2)
Data$AcumRLogTotal<-0
Data$AcumRLogB<-0
Data$AcumRLogC<-0
Data$Posicion<-"Pending"
#Definimos el filtro de cruce
Data$smafilter<-SMA(Cl(Data),dsma)
#calculamos el atr
Data$ATR_Data<-as.data.frame(ATR(HLC(Data),nPeriodos))
#caluclamos el up level -
Data$Up_Lev<-(Hi(Data)+Lo(Data))/2-nOfset*Data$ATR_Data$atr
#calculamos el dn level +
Data$Dn_Lev<-(Hi(Data)+Lo(Data))/2+nOfset*Data$ATR_Data$atr
#generamos lagas de las señales anteriores
Data$Up_Lev_1<-Lag(Data$Up_Lev,k = 1)
Data$Dn_Lev_1<-Lag(Data$Dn_Lev,k = 1)
#predefinimos variables que seran modificadas en el loop
#Data$UP_Trend<-Data$Up_Lev
#Data$down_Trend<-Data$Up_Lev
#Data$LimiteConfianza<-Data$Up_Lev
#definimos los niveles de tendencia
Data$UP_Trend<-0
Data$down_Trend<-0
Data$LimiteConfianza<-1
#definimos cuando devemos de iniciar el loop
starting<-nPeriodos+max(nPeriodos,nOfset,dsma)
i<-starting
for (i in c(starting:nrow(Data))) {
  #              up_trend   := close[1] > up_trend[1]   ?             max(          up_lev,           up_trend[1])   : up_lev
  Data$UP_Trend[i]<-ifelse(Data$smafilter[i-1]>Data$UP_Trend[i-1],max(Data$Up_Lev[i],Data$UP_Trend[i-1]),Data$Up_Lev[i])
  #down_trend :=                       close[1] < down_trend[1] ?         min(dn_lev,                     down_trend[1]) : dn_lev
  Data$down_Trend[i]<-ifelse(Data$smafilter[i-1]<Data$down_Trend[i-1],min(Data$Dn_Lev[i],Data$down_Trend[i-1]),Data$Dn_Lev[i])
  # trend :=                      close >            down_trend[1] ? 1:                 close < up_trend[1] ? -1 : nz(trend[1], 1)
  if (as.numeric(Data$smafilter[i])>as.numeric(Data$down_Trend[i-1])) {
    Data$LimiteConfianza[i]<-Data$down_Trend[i-1]
    Data$Posicion[i]<-"Long"
  }else{
    if (as.numeric(Data$smafilter[i])<as.numeric(Data$UP_Trend[i-1])) {
      Data$LimiteConfianza[i]<-Data$UP_Trend[i-1]
      Data$Posicion[i]<-"Short"
    }else{
      Data$LimiteConfianza[i]<-Data$LimiteConfianza[i-1]
      #Data$Posicion[i]<-substr(Data$Posicion[i-1], 1, 1) 
      Data$Posicion[i]<-Data$Posicion[i-1]  
    }
  }
  #Data$AcumRLog$A[i]<-ifelse(Data$Posicion[i]=="L",Data$RetLog[i]+Data$AcumRLog[i-1],-Data$RetLog[i]+Data$AcumRLog[i-1])
  #Data$AcumRLogA[i]<-ifelse(Data$Posicion[i]=="L",Data$RetLog[i]+Data$AcumRLogA[i-1],
  #                           ifelse(Data$Posicion[i]=="S",-Data$RetLog[i]+Data$AcumRLogA[i-1],+Data$AcumRLogA[i-1]))
  #
  #Data$AcumRLogB[i]<-ifelse(Data$Posicion[i]=="L"|Data$Posicion[i]=="Short",Data$RetLog[i]+Data$AcumRLogB[i-1],
  #                           ifelse(Data$Posicion[i]=="S"|Data$Posicion[i]=="Long",-Data$RetLog[i]+Data$AcumRLogB[i-1],+Data$AcumRLogB[i-1]))
  #
  #Data$AcumRLogC[i]<-ifelse(Data$Posicion[i]=="Short",Data$RetLog[i]+Data$AcumRLogC[i-1],
  #                           ifelse(Data$Posicion[i]=="Long",-Data$RetLog[i]+Data$AcumRLogC[i-1],+Data$AcumRLogC[i-1]))
  Data$AcumRLogTotal[i]<-ifelse((Data$Posicion[i]=="Long"&Data$Posicion[i]==Data$Posicion[i-1])|(Data$Posicion[i]=="Short"&Data$Posicion[i-1]=="Long"),Data$RetLog[i]+Data$AcumRLogTotal[i-1],
                             ifelse((Data$Posicion[i]=="Short"&Data$Posicion[i]==Data$Posicion[i-1])|(Data$Posicion[i]=="Long"&Data$Posicion[i-1]=="Short"),-Data$RetLog[i]+Data$AcumRLogTotal[i-1],+Data$AcumRLogTotal[i-1]))
}

write.csv(Data,"D:/DATOS_GESTAMP/jorge_datos/TRADING_QUANTITATIVO/01DiseñoSistema/ESTRATEGIAS/limiteconfi.txt")
Data%>%
  #filter(LimiteConfianza>1.08)%>%
  #filter(Datetime>"2020-09-22 02:00:00")%>%
  #ggplot(aes(x=Datetime,y = LimiteConfianza,colour=Posicion))+geom_point(aes(x=Datetime,y=Close))+geom_line(aes(x=Datetime,y = LimiteConfianza))
  ggplot(aes(x=Datetime,y = AcumRLogTotal,colour=Posicion))+geom_point()


table(Data$Posicion)/nrow(Data)*100

sum(Data$RetLog[Data$Posicion=="L"])
sum(Data$RetLog[Data$Posicion=="S"])
sum(Data$RetLog[Data$Posicion=="Long"])
sum(Data$RetLog[Data$Posicion=="Short"])



