#############################################################################################
#This script contains methodologies for quality control for hourly and daily climatic series
#
#This script is free: you can redistribute it and/or modify
#Autor: VICTOR HUGO PATIÑO BRAVO
#v.h.patino@cgiar.org
#Agosto, 2015
#Version V.02.15
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

#PACKAGES
library(chron)
library(stringr)
library(tcltk)
library(sirad)
library(randomForest)
library(rpart)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RMAWGEN)

#Work directory
dirFol    <- "C:/Users/vhpatino/Desktop/FranciscoYopal/"
setwd(dirFol)

GRAPHICS  <- function(){source(paste0(dirFol,"/GRAPHICS.R"))}
#Years of analisys
YStart    <- 2008 #Star Year for analisys
YEnd      <- 2014 #End  Year for analisys

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



#Create folders
FOLDERS(dirFol)
        ########################################################################
        #WARNING!!!! :You need put files on SERIES_ORIGINAL folder for continue#
        ########################################################################

#Quality Control station hourly
QCHORLY(dirFol)

#Convert
CONVERT(dirFol)

#Mix
MIX(dirFol)#Debo corregir...si no hay datos horarios y diarios esta funcion no tiene sentido

#Quality Control station daily
QCDAILY(dirFol)

#Inputs
INPUTS(dirFol)

#Graphics after QC
GRAPHICS()

#Summary for variable

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
#eg: DontUse=c(2,4,9)
#DontUse=c(4,7,11,13:16,19,25,26:30)
GENERATOR_T_R(dirFol,YStart,YEnd,DontUse=c(1:3))
#length(DontUse)
#Relative humidity and Solar energy
GEN_RHUM(dirFol)
GEN_ESOL(dirFol)

#Text files of ESOL and Final Graphics
END_GRAPS(dirFol)


##############END