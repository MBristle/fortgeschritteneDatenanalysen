#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)!=0){
  print(class(args))
}

# load necessary library files
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(readr)
p_load(dplyr)
source("getBRMModel.R")

physio<-read_rds("physio.rds")

names<-c(1:12)
formulas<-list("Value~1",
               
               "Value~1+(1|Spielfeld)",
               "Value~1+(1|BiopacSubject)",

               
               "Value~1+Factor1 + (1|BiopacSubject)",
               "Value~1+Factor2 + (1|BiopacSubject)",
               
               "Value~1+Factor2+ betweenCond + (1|BiopacSubject)",
               "Value~1+Factor2 * betweenCond + (1|BiopacSubject)",
               
               "Value~1+Factor2 * betweenCond+ Time  + (1|BiopacSubject)",
               "Value~1+Factor2 * betweenCond* Time  + (1|BiopacSubject)",
               
               "Value~1+Factor1+Factor2 * betweenCond + Time + (1|BiopacSubject)",
               "Value~1+Factor1*Factor2 + betweenCond + Time + (1|BiopacSubject)",
               "Value~1+Factor1*Factor2 * betweenCond + Time + (1|BiopacSubject)"
)
measurement<-list("RRi")

models<-lapply(measurement,function(x,y){within(y,measurement<-x)},y= data_frame(formulas,names)) %>% bind_rows()
models <- split(models, seq(nrow(models)))

est_model<-lapply(models[args],function(x,y){getBRMModel(dataframe = x,data=y,path = "models/")},y=physio)



