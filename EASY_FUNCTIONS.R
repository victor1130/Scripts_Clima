#############################################################################################
#This script contains methodologies for quality control for hourly and daily climatic series
#
#This script is free: you can redistribute it and/or modify
#Autor: VICTOR HUGO PATIÑO BRAVO
#v.h.patino@cgiar.org
#Marzo, 2016
#Version V.03.16
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

# PACKAGES
if(require(ggplot2)==FALSE){install.packages("ggplot2")} 
if(require(rpart)==FALSE){ install.packages("rpart")}
if(require(RMAWGEN)==FALSE){install.packages("RMAWGEN") } 
#if(require(chron)==FALSE){install.packages("chron")}
if(require(randomForest)==FALSE){install.packages("randomForest")}
if(require(utils)==FALSE){install.packages("utils")}
if(require(stringr)==FALSE){install.packages("stringr")}
if(require(tcltk)==FALSE){install.packages("tcltk")}
if(require(sirad)==FALSE){install.packages("sirad")}
if(require(tidyr)==FALSE){install.packages("tidyr")}
if(require(dplyr)==FALSE){install.packages("dplyr")}
if(require(base64enc)==FALSE){install.packages("base64enc")}
if(require(rCharts)==FALSE){install.packages("rCharts")}
if(require(gridExtra)==FALSE){install.packages("gridExtra")}

# Work directory  :: #dirFol    <- "C:/Users/nameUser/Desktop/workspace/"
dirFol    <- "C:/Users/vhpatino/Desktop/Arandu/"
#dirFol<-"C:/Users/vhpatino/Desktop/data_test/ambalema/"
setwd(dirFol)

GRAPHICS  <- function(){source(paste0(dirFol,"/GRAPHICS.R"))}
# Years of analisys

# Para colocar años a procesar... 4 digitos
YStart    <-  2013#Star Year for analisys
YEnd      <-  2016#End  Year for analisys

                                  ########  ########  ######   #### ##    ## 
                                  ##     ## ##       ##    ##   ##  ###   ## 
                                  ##     ## ##       ##         ##  ####  ## 
                                  ########  ######   ##   ####  ##  ## ## ## 
                                  ##     ## ##       ##    ##   ##  ##  #### 
                                  ##     ## ##       ##    ##   ##  ##   ### 
                                  ########  ########  ######   #### ##    ## 
                        ########  ########   #######   ######  ########  ######   ######  
                        ##     ## ##     ## ##     ## ##    ## ##       ##    ## ##    ## 
                        ##     ## ##     ## ##     ## ##       ##       ##       ##       
                        ########  ########  ##     ## ##       ######    ######   ######  
                        ##        ##   ##   ##     ## ##       ##             ##       ## 
                        ##        ##    ##  ##     ## ##    ## ##       ##    ## ##    ## 
                        ##        ##     ##  #######   ######  ########  ######   ######  

# Create folders
FOLDERS(dirFol)
        ########################################################################
        #WARNING!!!! :You need put files on SERIES_ORIGINAL folder for continue#
        ########################################################################

# Quality Control station hourly
# Use Dup=1 inside QCHOURLY to view duplicateds. Default Dup=NULL
QCHOURLY(dirFol)

# Convert
CONVERT(dirFol)

# "Mix" of the same station in differents files with two temporal scales (hourly and daily). 
MIX(dirFol)

# Quality Control station daily
QCDAILY(dirFol)

# Inputs
INPUTS(dirFol)

# Descriptive Graphs
# num=? --> variable to work
# 1:"ESOL" 2:"RAIN" 3:"RHUM" 4:"TMAX" 5:"TMIN"
PLOTSERIES(dirFol,num=5)
# debo corregir que sólo salgan los años de interes

#Graphics after QC
GRAPHICS()### revisar estos graficos..algunos ya no son tan utiles

#Summary for variable

#revisar el grafico de lines, debe tener discontinuidad en donde no hay datos
SUMMARY(dirFol,"TMAX",YStart,YEnd)
SUMMARY(dirFol,"TMIN",YStart,YEnd)
SUMMARY(dirFol,"RAIN",YStart,YEnd)
SUMMARY(dirFol,"ESOL",YStart,YEnd)
SUMMARY(dirFol,"RHUM",YStart,YEnd)

#Generate Data: Precipitation, Temperature max and min.

      #########################################################################
      #     WARNING!!!! :You need check that the files TMAX_to, TMIN_to and   #
      #          RAIN_to contain the same stations before continue            #
      #########################################################################

#You can use DontUse vector for exclude positions of the stations that you don't will use
#eg: 
DontUse=c(1,4,5)
GENERATOR_T_R(dirFol,YStart,YEnd,DontUse = DontUse)
#length(DontUse)
#Relative humidity and Solar energy
GEN_RHUM(dirFol)
GEN_ESOL(dirFol)

#Text files of ESOL and Final Graphics
END_GRAPS(dirFol)

##############END