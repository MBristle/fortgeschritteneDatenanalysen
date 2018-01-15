#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)!=0){
  print(class(as.numeric(args)))
  
}

# load necessary library files
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(readr)
p_load(lme4)
p_load(dplyr)
p_load(brms)
p_load(parallel)

options (mc.cores=parallel::detectCores ()) # Run on multiple cores
behaviorData<-read_rds("behavior.rds")

getBRMModel <- function(dataframe=NULL,form=NULL,measurement=NULL,data=NULL,name=NULL,path="",family = NULL, prior=NULL,autocor=NULL) {
  options (mc.cores=parallel::detectCores ()) # Run on multiple cores
  if (!is.null( dataframe)){
    form=as.formula(eval(parse(text =dataframe$formulas)))
    name=as.character(dataframe$names)
    measurement=dataframe$measurement
  }
  assertthat::not_empty(form)
  assertthat::not_empty(data)
  assertthat::not_empty(name)
  assertthat::not_empty(measurement)
  dir.create(file.path( path), showWarnings = F)
  tryCatch(
    {
      data<- filter(data,Measurement==measurement)
    },
    error=function(cond) {
      message(cond)
    }
  )
  
  
  
  out <- tryCatch(
    {
      read_rds(paste0(path,measurement,"_",name,".rds"))
    },
    error=function(cond) {
      message(cond)
      cat(paste("\n\nFile does not seem to exist: ", paste0(path, name), "\nnew Model will be calculated and saved with the specified name. \nGo grab a cup of coffee  and do 10 Push-ups:)\n\n"))
      
      model<-brm(form,data = data,prior=prior,family = family, autocor=autocor)
      #model<-lmer(formula = form,data = data)
      saveRDS(model,paste0(path,measurement,"_",name,".rds"))
      return(model)
    }
  )    
  return(out)
}


names<-list("m0",
                     "m1a",
                     "m1b",
                     
                     "m2",
                     "m2a",
                     "m2b",
                     "m2c"
)
formulas<-list("VasSlide.VAS~1",
                        
                        "VasSlide.VAS~1+(1|Name)", 
                        "VasSlide.VAS~1+(1|Spielfeld)",
                        
                        "VasSlide.VAS~1+Factor1+Factor2 + (1|Name)+ (1|Spielfeld)",
                        "VasSlide.VAS~1+Factor1*Factor2 + (1|Name)+ (1|Spielfeld)",
                        "VasSlide.VAS~1+Factor1*Factor2 + betweenCond + (1|Name)+ (1|Spielfeld)",
                        "VasSlide.VAS~1+Factor1*Factor2 * betweenCond + (1|Name)+ (1|Spielfeld)"
)

models<-data_frame(formulas,names)
models$measurement<-"VAS"
models <- split(models, seq(nrow(models)))

est_model<-lapply(models[args],function(x,y){getBRMModel(dataframe = x,data = y,path = "models/")},y=behaviorData)

