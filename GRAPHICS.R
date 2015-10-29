# GRAPHICS<- function(dirFol,YStart,YEnd){

print("Wait a moment please...")  
rutOrigen=paste0(dirFol,"/PROCESS/03_SERIES_DAILY_With_Holes/")
files <-list.files(rutOrigen,pattern="\\.csv$")
files <-files[grep("_to",files)]

data=lapply(paste0(rutOrigen,"/",files,sep=""),function(x){read.table(x,header=T,sep=",")})
dir.create(paste0(rutOrigen,"GrapSeriesAfterQCfrom"),showWarnings = FALSE)
dir.create(paste0(rutOrigen,"BoxplotAfterQCfrom"),showWarnings = FALSE)

media=function(x){
  x=na.omit(x)
  media=mean(x)
}
sumando=function(x){
  x=na.omit(x)
  if(length(x)>0){
    suma=sum(x)
  }else{suma=NA}
  return(suma)
}

####Para gráficos diarios

for(h in 1:length(files)){
  #head(data)
  data=lapply(paste0(rutOrigen,"/",files,sep=""),function(x){read.table(x,header=T,sep=",")})
  data=data[[h]];data=as.data.frame(data)
  data=filter(data,year %in% c(YStart:YEnd))
  
  month <-  data$month
  day   <-  data$day
  year  <-  data$year
  Dec   <-  data$Dec 
  
  Var=substring(files[h],1,4)#Variable a trabajar
  #Información mensual (Calculo de promedios del mes o acumulados segun el caso, para cada año)
  if(Var=="RAIN"){
    MonthData=aggregate(data,by = list(month,year),sumando)[-c(3:5)]
  }else{MonthData=aggregate(data,by = list(month,year),media)[-c(3:5)]}
  
  MonthData=as.data.frame(MonthData)
  nom=names(data[-c(1:3)])#Nombre de las estaciones
  Est=length(nom)         #cantidad de estaciones
  
  #Para identificar unidades de las variables  
  if(Var=="ESOL"){
    Unit="CCM2"
  }else if(Var=="RAIN"){
    Unit="mm"
  }else if(Var=="TMAX"){
    Unit="°C"
  }else if(Var=="TMIN"){
    Unit="°C"
  }else if(Var=="RHUM"){
    Unit="%"
  }
  
  #Limites de los gráficos a nivel diario
  LInf=min((summary(data[4:(Est+3)]))[1,]);LSup=max((summary(data[4:(Est+3)]))[6,])
  LInf=as.data.frame(LInf)
  ##Esta función es similar a lo que se hace en "Separar por columnas" de Excel
  LInf=LInf %>% separate(LInf,into=c("Calc","Val"),sep="\\:")
  LInf=as.numeric(LInf$Val)
  LSup=as.data.frame(LSup);LSup=LSup %>% separate(LSup,into=c("Calc","Val"),sep="\\:")
  LSup=as.numeric(LSup$Val)
  ymin=LInf;ymax=LSup
  
  #Limites de los gráficos a nivel mensual
  LInf=min((summary(MonthData[3:(Est+2)]))[1,]);LSup=max((summary(MonthData[3:(Est+2)]))[6,])
  LInf=as.data.frame(LInf)
  ##Esta función es similar a lo que se hace en "Separar por columnas" de Excel
  LInf=LInf %>% separate(LInf,into=c("Calc","Val"),sep="\\:")
  LInf=as.numeric(LInf$Val)
  LSup=as.data.frame(LSup);LSup=LSup %>% separate(LSup,into=c("Calc","Val"),sep="\\:")
  LSup=as.numeric(LSup$Val)
  yminMonth=LInf;ymaxMonth=LSup
  
  #Organizacion de fechas para el gráfico diario    
  mesd=data[1];diad=data[2];añod=data[3];mesd=as.numeric(unlist(mesd));diad=as.numeric(unlist(diad));añod=as.numeric(unlist(añod));
  ved=paste0(mesd,"/",diad,"/",añod); datesd=as.Date(ved,"%m/%d/%Y")
  labDatesd <- seq(min(datesd), tail(datesd, 1),by = "months")
  cantGraf=ceiling(length(nom)/4)#Cantidad de gráficos
  minyear=min(data$year);maxyear=max(data$year)
  
  #Organizacion de fechas para el gráfico Mensual
  mes=MonthData[1];dia=rep(1,dim(mes)[1]);año=MonthData[2];mes=as.numeric(unlist(mes));dia=as.numeric(unlist(dia));año=as.numeric(unlist(año));
  ve=paste0(mes,"/",dia,"/",año); dates=as.Date(ve,"%m/%d/%Y")
  labDates <- seq(min(dates), tail(dates, 1),by = "months")
  
  #for daily
  for(j in 1:cantGraf){
    if(j<cantGraf){
      limG <- 4*j 
    }else{limG <- Est }
    png(paste0(rutOrigen,"GrapSeriesAfterQCfrom","/",YStart,"-",YEnd,"_",Var,"_SeriesDaily","_",j,".png"), width = 800, height = 450,res=100)
    par(mfrow=c(2,2),mar=c(1.5,1.9,2.5,0.1),mgp = c(1, 0.5, 0))
    
    stations <- as.data.frame(data[,4:ncol(data)])
    
    for(i in 1:limG){
      plot(datesd,stations[,i],type="l",ylim=c(ymin,ymax),ylab="",xaxt="n",xlab="",xaxs="i", yaxs="i",cex.axis=0.8)
      axis.Date(side=1,datesd,at=labDatesd,format="%b %y",las=1,cex.axis=0.8);title(nom[i],line=0.1,cex.main=0.8)
    }
    mtext(paste0(Var," (",Unit,")"," Daily","\n",minyear," - ",maxyear), side = 3, line = -2, outer = TRUE,cex = 0.8)
    dev.off()
  }
  
  ##for mounth
  
  for(j in 1:cantGraf){
    
    if(j<cantGraf){
      limG <- 4*j 
    }else{limG <- Est }
    png(paste0(rutOrigen,"GrapSeriesAfterQCfrom","/",YStart,"-",YEnd,"_",Var,"_SeriesMounth","_",j,".tif"), width = 800, height = 450,res=100)
    par(mfrow=c(2,2),mar=c(1.5,1.9,2.5,0.1),mgp = c(1, 0.5, 0))  
    
    stations <- as.data.frame(MonthData[,3:ncol(MonthData)])
    
    
    for(i in 1:limG){
      
      plot(dates,stations[,i],type="l",ylim=c(yminMonth,ymaxMonth),ylab="",xaxt="n",xlab="",xaxs="i", yaxs="i",cex.axis=0.8)
      axis.Date(side=1,dates,at=labDates,format="%b %y",las=1,cex.axis=0.8);title(nom[i],line=0.1,cex.main=0.8)
    }
    mtext(paste0(Var," (",Unit,")", " Monthly","\n",minyear," - ",maxyear), side = 3, line = -2, outer = TRUE,cex = 0.8)
    dev.off()
    
  }
  
}
###Despues de este for puede salir repetidamente "ERROR : undefined columns selected"
###Simplemente indica que los par() de 4 espacios (2x2), no todos quedaron totalmente llenos
###Aparecerá un error por espacio (de grafico) que no se usó

##Boxplot decadal y mensual
for(h in 1:length(files)){
  
  Var=substring(files[h],1,4)#Variable a trabajar
  if(Var=="ESOL"){
    Unit="CCM2"
  }else if(Var=="RAIN"){
    Unit="mm"
  }else if(Var=="TMAX"){
    Unit="°C"
  }else if(Var=="TMIN"){
    Unit="°C"
  }else if(Var=="RHUM"){
    Unit="%"
  }
  
  data=lapply(paste0(rutOrigen,"/",files,sep=""),function(x){read.table(x,header=T,sep=",")})
  data=data[[h]];data=as.data.frame(data)
  data=filter(data, year %in% c(YStart:YEnd))
  
  minyear=min(data$year);maxyear=max(data$year)
  #Vector que asignara un valor de 1 a 3 si se esta entre los 10 primeros dias, 
  #entre el 11 y el 20 o del 21 en adelante respectivamente
  Dec=rep(0,dim(data)[1])
  
  for(d in 1:dim(data)[1]){
    if(data[d,2]>=1&data[d,2]<=10){
      Dec[d]=1}else if(data[d,2]>=11&data[d,2]<=20){
        Dec[d]=2}else {
          Dec[d]=3  
        }
  }
  #Insertando decadas en la base de datos  
  A=data[,c(1:3)];colum=dim(data)[2]; B=(data[c(4:colum)])
  data=cbind(A,Dec,B)
  est=names(data)[-c(1:4)]
  
  LInf=min((summary(data[5:(length(est)+4)]))[1,]);LSup=max((summary(data[5:(length(est)+4)]))[6,])
  LInf=as.data.frame(LInf)
  ##Esta función es similar a lo que se hace en "Separar por columnas" de Excel
  LInf=LInf %>% separate(LInf,into=c("Calc","Val"),sep="\\:")
  LInf=as.numeric(LInf$Val)
  LSup=as.data.frame(LSup);LSup=LSup %>% separate(LSup,into=c("Calc","Val"),sep="\\:")
  LSup=as.numeric(LSup$Val)
  ymin=LInf;ymax=LSup
  
  #Gráficos decadales
  
  for(t in 1:length(est)){
    
    graMatriz <- data
    graMatriz <- graMatriz[-which(is.na(data[,(4+t)])==TRUE),]
    
    png(paste0(rutOrigen,"BoxplotAfterQCfrom","/",YStart,"-",YEnd,"_",Var,"_DataDecadal","_",t,".png"), width = 800, height = 450,res=100)  
    gr<-ggplot(graMatriz, aes(factor(month),get(est[t])))+geom_boxplot(aes(fill=factor(Dec)),outlier.colour = "red")+
      xlab("Mes")+ylab(est[t])+ggtitle(paste(Var,"(",Unit,")",minyear,"-",maxyear))+ scale_fill_discrete(name="Decada \ndel mes")+coord_cartesian(ylim = c(ymin, ymax)) 
    print(gr)
    dev.off()    
    
    #Gráficos Mensuales
    
    png(paste0(rutOrigen,"BoxplotAfterQCfrom","/",YStart,"-",YEnd,"_",Var,"_DataMonth","_",t,".png"), width = 800, height = 450,res=100)  
    gr<-ggplot(graMatriz, aes(factor(month),get(est[t])))+geom_boxplot(outlier.colour = "red")+xlab("Mes")+
      ylab(est[t])+ggtitle(paste(Var,"(",Unit,")",minyear,"-",maxyear))+ scale_fill_discrete(name="Mes")+coord_cartesian(ylim = c(ymin, ymax)) 
    print(gr)
    dev.off()    
  }
  
}
print("Please check inside 03_SERIES_DAILY_With_Holes, the GrapSeriesAfterQCfrom and BoxplotAfterQCfrom folders")
rm(A,año,añod,B,cantGraf,colum,d,data,dates,datesd,day,Dec,dia,diad,est,Est,files,gr,graMatriz,h,i,j,labDates,labDatesd,limG,LInf,LSup,maxyear,media,mes,mesd,minyear,month,MonthData,nom,rutOrigen,stations,sumando,t,Unit,Var,ve,ved,year,ymax,ymaxMonth,ymin,yminMonth)


#)


# }