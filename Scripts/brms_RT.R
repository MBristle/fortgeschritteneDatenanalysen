#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)!=0){
  print(class(args))
  
}

# load necessary library files
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(dplyr)
p_load(readr)
source("getBRMModel.R")

behaviorData<-read_rds("behavior.rds")


names<-list("m0","m1a","m1b", 
            "m2","m2a", "m2b","m2c",
            "m3a","m3b"
)
formulas<-list(   "VasSlide.RT~1",
                  
                  "VasSlide.RT~1+(1|Name)", 
                  "VasSlide.RT~1+(1|Spielfeld)",
                  
                  "VasSlide.RT~1+Factor1+Factor2 + (1|Name)",
                  "VasSlide.RT~1+Factor1*Factor2 + (1|Name)",
                
		              "VasSlide.RT~1+Factor1*Factor2 + betweenCond+(1|Name)",
                  "VasSlide.RT~1+Factor1*Factor2 * betweenCond + (1|Name)",
		
		              "VasSlide.RT~1+Factor1+Factor2 + betweenCond+(1|Name)",
                  "VasSlide.RT~1+Factor1+Factor2 * betweenCond + (1|Name)"
)
models<-data_frame(formulas,names)
models$measurement<-"RT"
models <- split(models, seq(nrow(models)))

est_model<-lapply(models[args],function(x,y){getBRMModel(dataframe = x,data = y,control = list(adapt_delta = 0.95),family = weibull(),path = "models/")},y=behaviorData)
