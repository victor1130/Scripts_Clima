library(ggplot2)
library(lubridate)
library(dplyr)
library(plyr) 
library(scales)
library(plotrix)
library(gridExtra)
library(reshape2)

dirFol    <- "//dapadfs/workspace_cluster_8/AEPS/ARGENTINA_2015/CLIMA/PROCESAMIENTO_CLIMA/WITHOUT_SBRI/"
setwd(dirFol)

rut=paste0(dirFol,"PROCESS/04_SERIES_DAILY_OK/")
dir.create(paste0(rut,"/GRAPS_COMPARE2"),showWarnings=F)
DESTINO=paste0(rut,"GRAPS_COMPARE2/")

files=list.files(rut,pattern = "_FUS.txt",full.names = T)
nam=list.files(rut,pattern = "_FUS.txt")
names=substring(nam,1,nchar(nam)-8)
Allfiles=lapply(files, function(x){read.table(x,header = T)})
names(Allfiles)=names

##Between year min= Ymin and year max=Ymax
YStart=2006;YEnd=2014
monthStart=9; monthEnd=6 # Mes de inicio y fin de temporada respectivamente
############################################################
######Generacion de graficos generales comparativos#########
vecYears=seq(YStart,YEnd)
lab=0

for(j in 1:length(vecYears))
  lab[j]=paste0(substring(vecYears[j],3,4),"/",substring(vecYears[j+1],3,4))

lab=lab[-length(lab)]

##Lineas para filtar la informacion por el periodo de interes desde el inicio de temporada hasta final de la misma
monthYear=1:12 #meses en el year
VecStart=monthYear[monthYear>=monthStart];  VecEnd=monthYear[monthYear<=monthEnd]

texto=paste0(month.name[c(VecStart,VecEnd)][1],"-",month.name[c(VecStart,VecEnd)][length(month.name[c(VecStart,VecEnd)])])
dd=format(seq(as.Date("1983-09-01"),as.Date("1984-07-01"),length.out = 30),"%m/%d")

SerieOk=list()
SerieOk2=list()
for(i in 1:length(names)){
  
  filmin=paste0(YStart,"-",monthStart,"-01");  filmax=paste0(YEnd,"-",(monthEnd+1),"-01")
    
  Allfiles[[i]]$Dates=as.Date(Allfiles[[i]]$Dates)
  VAR=substring(names[i],nchar(names[i])-3,nchar(names[i]))
  #filtro year de interes
  filtro1=Allfiles[[i]]$Dates>=as.Date(filmin) &Allfiles[[i]]$Dates<=as.Date(filmax)
  SerieOk[[i]]=Allfiles[[i]][filtro1,]

  filtro2=months(SerieOk[[i]]$Date) %in% month.name[c(VecStart,VecEnd)]#desde 1ero de septiembre hasta 1ero de julio
  SerieOk[[i]]=SerieOk[[i]][filtro2,]
  
  #Leyendo fechas
  SerieOk[[i]]$Dates=as.Date(SerieOk[[i]]$Dates)
  SerieOk[[i]]$Year <- year(SerieOk[[i]]$Dates)#Creando vector de años
  SerieOk[[i]]$Month<- month(SerieOk[[i]]$Dates)
  SerieOk[[i]]$Day= as.numeric(format(SerieOk[[i]]$Dates,"%j"))
  SerieOk[[i]]$DayCal=day(SerieOk[[i]]$Dates)#dia calendario, dia del mes
  SerieOk[[i]]$Season=NA
  
  pos=which(SerieOk[[i]]$Month==monthStart&SerieOk[[i]]$DayCal==1)#posiciones de inicio de temporada, de aqui se puede suponer las posiciones de fin tambien
  # pero para la ultima agrego una siguiente temporada ficticia,solo para poder
  pos=c(pos,dim(SerieOk[[i]])[1]+1)
  cant=length(pos) #definira la cantidad de curvas
  
  season=1:(cant-1)# como agregue una posicion ficticia de inicio de temporada debo quitarla de la cantidad real de temporadas -1
  SerieOk[[i]]$Season=rep(season,diff(pos))
  SerieOk[[i]]$Seq=sequence(diff(pos))
  SerieOk[[i]]$Seq2=as.Date(SerieOk[[i]]$Day, origin=as.Date("1990-01-01"))
  
  SerieOk2[[i]]=SerieOk[[i]]
  
  #Acumulados desde inicio hasta final de temporada
  if(VAR=="ESOL"|VAR=="RAIN"){
    acum=aggregate(SerieOk[[i]]$Value,list(SerieOk[[i]]$Season),cumsum)[-1]
    SerieOk[[i]]$Value=c(unlist(acum))
  
    stationName=substring(names[i],1,nchar(names[i])-5)
  
  png(paste0(DESTINO,names[i],".png"), width = 800, height = 450,res=100)
  gr=ggplot(SerieOk[[i]],aes(Seq,Value))+geom_line( aes(colour = as.factor(Season)) )+xlab("Days of season")+ylab(VAR)+
    scale_colour_discrete(name="Season",labels=lab)+ggtitle(paste0(stationName,"\nSeason_",texto))+
    scale_x_discrete(limit = seq(min(SerieOk[[i]]$Seq),max(SerieOk[[i]]$Seq),10),labels = c(dd))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme_bw()
  
  print(gr)
  dev.off() 
  }
  
  if(VAR=="TMAX"|VAR=="TMIN"|VAR=="RHUM"){
    prom=aggregate(SerieOk[[i]]$Value,list(SerieOk[[i]]$Season),mean)[-1]
    #SerieOk[[i]]$Value=c(unlist(prom))
    SerieOk[[i]]=prom
    SerieOk[[i]]$Season=lab
  }
  
}

names(SerieOk)=names
names(SerieOk2)=names

StaNames=unique(substring(names,1,nchar(names)-5))
filESyRN=grep("ESOL|RAIN",names)
#Para obtener los acumulados maximos de estas dos variables por season
for(k in 1:length(filESyRN)){
acumS=aggregate(SerieOk[[filESyRN[k]]]$Value,list(SerieOk[[filESyRN[k]]]$Season),max)[-1]
SerieOk[[filESyRN[k]]]=acumS
SerieOk[[filESyRN[k]]]$Season=lab
}

filTMX=grep("TMAX",names);filTMN=grep("TMIN",names);filRMH=grep("RHUM",names)
stationName=unique(substring(names,1,nchar(names)-5))
StaNames=unique(substring(names,1,nchar(names)-5))
for(h in 1:length(filTMX)){

    TX=SerieOk2[[filTMX[h]]]$Value
    TN=SerieOk2[[filTMN[h]]]$Value
    DR=TX-TN
    JOINT=data.frame(SerieOk2[[filTMX[h]]]$Dates,SerieOk2[[filTMX[h]]]$Season,
                     SerieOk2[[filTMX[h]]]$Seq,SerieOk2[[filTMX[h]]]$DayCal,SerieOk2[[filTMX[h]]]$Month,SerieOk2[[filTMX[h]]]$Year,DR)
    colnames(JOINT)=c("Dates","Season","Seq","DayCal","Month","Year","DR") 
    JOINT$dec=NA#decada del mes
    

    y=lapply(JOINT$DayCal, function(x){
      if(x<=10){y=1
      }else if(x>10&x<=20){y=2
      }else{y=3}
    })  
    y=as.vector(unlist(y))
    
    JOINT$dec=y
  
  joint3=aggregate(JOINT,by = list(JOINT$dec,JOINT$Month,JOINT$Year),mean) 
  
  pos=which(joint3$Month==monthStart&joint3$DayCal==5.5)#posiciones de inicio de temporada, de aqui se puede suponer las posiciones de fin tambien
  # pero para la ultima agrego una siguiente temporada ficticia,solo para poder
  pos=c(pos,dim(joint3)[1]+1)
  joint3$Seq2= sequence(diff(pos))
  
  
  #png(paste0(DESTINO,stationName[h],"DR.png"), width = 800, height = 450,res=100)
  gr=ggplot(joint3,aes(Seq2,DR))+geom_line( aes(colour = as.factor(Season)) )+xlab("Decade of season")+ylab("DR")+
                       scale_colour_discrete(name="Season",labels=lab)+ggtitle(paste0(stationName[h],"\nSeason_",texto))+
                       scale_x_discrete(limit = seq(min(joint3$Seq2),max(joint3$Seq2),1),labels = c(dd))+
                       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                       theme_bw()
  ggsave(paste0(DESTINO,stationName[h],"_Dec_Season_DR.png"),plot = gr,width =22 ,height = 15,units = "cm")

}        
                     
StaNames=unique(substring(names,1,nchar(names)-5))

# coord_radar <- function (theta = "x", start = 0, direction = 1) 
# {
#   theta <- match.arg(theta, c("x", "y"))
#   r <- if (theta == "x") 
#     "y"
#   else "x"
#   ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
#           direction = sign(direction),
#           is_linear = function(coord) TRUE)
# }

for(i in 1:length(StaNames)){
posdes=grep(StaNames[i],names)#Posiciones de una misma estacion en los archivos desagregados

vari=substring(names[posdes],nchar(names[posdes])-3,nchar(names[posdes]))
rot=c("Season",vari)#rotulos para dataframe


joint=do.call(cbind,SerieOk[posdes])#uniendo todos los archivos de una misma estacion
del=grep("Season",names(joint))#borrando todas las columnas de seasion de esta union
jointx=joint[-del]
seasonx=joint[del[1]]

JOINT=data.frame(SerieOk[[6]]$Season,SerieOk[[6]]$x,SerieOk[[7]]$x,SerieOk[[8]]$x,SerieOk[[9]]$x,SerieOk[[10]]$x)

JOINT=data.frame(seasonx,jointx)
colnames(JOINT)=rot
   
   JOINT=JOINT[-c(2,3)]
   JOINT$DR=as.numeric(as.character(JOINT$TMAX))-as.numeric(as.character(JOINT$TMIN))
   JOINT$TAVG=(as.numeric(as.character(JOINT$TMAX))-as.numeric(as.character(JOINT$TMIN)))/2
   
  JOINT2 <- reshape2::melt(JOINT,id.vars = "Season")

      # gr2=ggplot(JOINT2, aes(x = variable, y = value)) +
      #     geom_polygon(aes(group = Season, color = Season), fill = NA, size = 1, show.legend = F) +
      #     geom_line(aes(group = Season, color = Season), size = 1,show.legend = T)+# + #legendas
      #     theme(strip.text.x = element_text(size = rel(0.8)),
      #           axis.text.x = element_text(size = rel(0.8)),
      #           axis.ticks.y = element_blank(),
      #           axis.text.y = element_blank() ) +
      #     xlab("") + ylab("") +
      #     guides(color = guide_legend(ncol=1)) +
      #     ggtitle(paste0("Radar Chart for season, ",StaNames[i]))+
      #     coord_radar()
    
      
      varss=unique(JOINT2$variable)
      q=list()
      for(x in 1:length(varss)){
        
        if(x==1){
          xlab="Season"
        }else if(x>=2){xlab=""}
        
        if(varss[x]=="RHUM"){
        und="%"}else {und="°C"}
       
        da=subset(JOINT2,variable==varss[x])
          ymin=mean(da$value)/2
          ymax=max(da$value)
          minm=min(da$value)
        
      q[[x]]= ggplot(subset(JOINT2,variable==varss[x]), aes(x= Season,y = value))+
        geom_bar(stat="identity",width = 0.9,fill="skyblue")+
        facet_wrap(~variable)+
        coord_flip(ylim=c(minm,ymax))+
        xlab(xlab)+
        ylab(und)+
        theme_bw()
      
      }
      
      
      gr3=grid.arrange(q[[1]],q[[2]],q[[3]],q[[4]],q[[5]],nrow = 1,top=paste0("Comparative for season, ",StaNames[i]))
      
      ggsave(paste0(DESTINO,StaNames[i],"_Var_Season.png"),plot = gr3,width =15 ,height = 5,dpi = 300)
  }
  
  # ggplot(JOINT2, aes(x = variable, y = value)) +
  #   geom_polygon(aes(group = Season, color = Season), fill = NA, size = 2) +
  #   facet_wrap(~ Season) +
  #   theme(axis.ticks.x = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.ticks.y = element_blank(),
  #         axis.text.y = element_blank()) +
  #   xlab("") + ylab("") +
  #   #guides(color = "none") +
  #   coord_radar()

  





















Data2=SerieOk[[9]]

Data2=aggregate(Data2,by=list(Data2$DayCal,Data2$Month),mean)

#png(paste0(DESTINO,names[i],".png"), width = 800, height = 450,res=100)
polar.plot(lengths = Data2$x,polar.pos = seq(1,350,by=350/304),rp.type="l",clockwise=TRUE,
           grid.col="gray",start=90,labels=c("Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1","Mar 1","Apr 1","May 1","Jun 1"),
           label.pos =  seq(1,350,by=350/10), main=paste0("Mercedes TMAX Multi-Season"),boxed.radial=F,radial.lim=c(15,40))# ,radial.lim=c(50,80))

#dev.off() 
############################################################
#Funcion para generar sumas acumuladas dentro de un vector.
# depende de la longitud promedio del evento longEv
longEv=140
acumEvent<-function(x){
  size=length(x)
  long=longEv
  cant=floor(size/long)
  sumas=0
  
  for(i in 1:(size-long+1)){
    sumas[i]=sum(x[(i):(long+i-1)])
  }
  sumas=c(rep(0,(long-1)),sumas)
  return(sumas)
}
#Funcion para buscar el valor maximo y la fecha en que ocurrio
maxEsol<-function(x){
  #x=Acum[[i]]
  maxi=max(x[[3]])
  date=as.Date(x[which.max(x[[3]]),1])
  
  result=data.frame(date,maxi)
  return(result)
}
maxRain<-function(x){
  maxi=max(x[[2]])
  date=as.Date(x[which.max(x[[2]]),1])
  
  result=data.frame(date,maxi)
  return(result)
}

Acum=list()     #tiene la series de acumulados
TopEsol=list()  # contiene los momentos del valor maximo de la variable por año
TopRain=list()
col=j=0
for(i in 1:length(Allfiles)){
  
  #Filtro de tiempo
  Allfiles[[i]]= filter(Allfiles[[i]],as.Date(Date)>=paste0(Ymin,"-01-01")& as.Date(Date)<=paste0(Ymax,"-12-30"))
  
  #tail(Allfiles[[i]])
  col[i]=dim (Allfiles[[i]])[2]
  if(col[i]<=4){   #implica que la estacion tiene pocas variables registradas
    Acum[[i]]=data.frame(Allfiles[[i]]$Date,apply(Allfiles[[i]][c(2)],MARGIN = 2,FUN=acumEvent))
    colnames(Acum[[i]])=c("Date","Rain")  
  }else{
    Acum[[i]]=data.frame(Allfiles[[i]]$Date,apply(Allfiles[[i]][,c(2,9)],MARGIN = 2,FUN=acumEvent))
    colnames(Acum[[i]])=c("Date","Rain","Esol")
  }
  
  Acum[[i]]$Date=as.Date((Acum[[i]]$Date))
  
  if(col[i]>4){
    Top=lapply(split(Acum[[i]],cut(Acum[[i]]$Date, "1 year")), function(x) maxEsol(x))
    TopEsol[[i]]=do.call(rbind,Top)
    colnames(TopEsol[[i]])=c("Date","EsolMax")
    
  }
  TopR=lapply(split(Acum[[i]],cut(Acum[[i]]$Date, "1 year")), function(x) maxRain(x))
  TopRain[[i]]=do.call(rbind,TopR)
  colnames(TopRain[[i]])=c("Date","RainMax")
  
}
names(TopEsol)=names(TopRain)=names(Acum)=names

topEsolcsv = do.call("rbind",lapply(TopEsol, as.data.frame))
write.csv(topEsolcsv,paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/TOPESOL.csv"))

topRaincsv = do.call("rbind",lapply(TopRain, as.data.frame))
write.csv(topRaincsv,paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/TOPRAIN.csv"))

#Los siguientes graficos permitiran comparar los acumulados máximos posibles en una periodo de cosecha preestablecido con las fechas de siembre y cosecha registradas

zona=as.character(unique(Eventos2$Influencia))

Even=list()
for(i in 1:length(zona)){
  Even[[i]]=filter(Eventos2,Influencia==zona[i])
}
names(Even)=zona

#Lluvia
graf=ggplot(Acum$CCUATIA,aes(Date,Rain))+geom_line()+scale_x_date(labels = date_format("%m/%y"),date_breaks ="4 month")+theme_bw()

graf=graf+geom_vline(xintercept= as.numeric(Even$MELO$Fecha_Cosecha),linetype = 4,colour = "red")+
  geom_vline(xintercept= as.numeric(Even$MELO$Fecha_Siembra),linetype = 4,colour = "blue")+
  ggtitle("Lluvias acumuladas en periodos de 140 días \n CUATIA")+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text=element_text(size=6), 
                                                                      axis.title=element_text(size=10,face="bold"),plot.title=element_text(size=12))

for(i in 1:dim(TopRain$CCUATIA)[1]){
  graf = graf + annotate("rect", xmin=((TopRain$CCUATIA$Date[i]))-139, xmax=((TopRain$CCUATIA$Date[i])), ymin = -Inf, ymax=Inf, alpha=.3, fill="green")
}
ggsave(paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/RAIN_ACUM_EVENT_CUATIA.png"),plot=graf,width=18, height=8,units = "cm",dpi = 600)
#Esol

graf2=ggplot(Acum$MERCEDES,aes(Date,Esol))+geom_line()+scale_x_date(labels = date_format("%m/%y"),date_breaks ="4 month")+theme_bw()+
  geom_vline(xintercept= as.numeric(Even$MELO$Fecha_Cosecha),linetype = 4,colour = "red")+
  geom_vline(xintercept= as.numeric(Even$MELO$Fecha_Siembra),linetype = 4,colour = "blue")+
  ggtitle("Energía solar acumulada en periodos de 140 días\n Mercedes")+
  coord_cartesian(ylim=c(32000,80000))+theme(axis.text.x = element_text(angle = 90, hjust = 1),
                                             axis.text=element_text(size=6), axis.title=element_text(size=10,face="bold"),plot.title=element_text(size=12))

for(i in 1:dim(TopEsol$MERCEDES)[1]){
    graf2 = graf2 + annotate("rect", xmin=((TopEsol$MERCEDES$Date[i]))-139, xmax=((TopEsol$MERCEDES$Date[i])), ymin = -Inf, ymax=Inf, alpha=.3, fill="green")
}
ggsave(paste0(dirFol,"/PROCESS/04_SERIES_DAILY_OK/ESOL_ACUM_EVENT_MERCEDES.png"),plot=graf2,width=18, height=8,units = "cm",dpi = 600)
