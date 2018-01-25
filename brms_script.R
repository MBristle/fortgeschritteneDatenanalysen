
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
p_load(tableone)
p_load(dplyr)
p_load(ggplot2)
p_load(merTools)
p_load(brms)
p_load(shinystan)
p_load(parallel)


options (mc.cores=parallel::detectCores ()) # Run on multiple cores
#physio<-na.omit(filter(PhysioData_1,(grepl("Stimulus",Target))&(Time!=-2)))
physio<-read_rds("physio.rds")

getBRMModel <- function(dataframe=NULL,form=NULL,measurement=NULL,data=NULL,name=NULL,path="",prior=NULL,autocor=NULL) {
  options (mc.cores=parallel::detectCores ()) # Run on multiple cores
  if (!require("pacman")) install.packages("pacman")
  library(pacman)
  p_load(readr)
  p_load(lme4)
  p_load(brms)
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
  data_f<- filter(data,measurement==data$Measurement)
  
  
  out <- tryCatch(
    {
      

        error("JUST errored")
        read_rds(paste0(path,measurement,"_",name,".rds"))
    },
    error=function(cond) {
      message(cond)
      cat(paste("\n\nFile does not seem to exist: ", paste0(path, name), "\nnew Model will be calculated and saved with the specified name. \nGo grab a cup of coffee  and do 10 Push-ups:)\n\n"))
      
      model<-brm(form,data = data_f,prior=prior,autocor=autocor)
      # model<-lmer(form,data = data_f)
      cat(paste("Finished model: ",as.character(form) ))
      saveRDS(model,paste0(path,measurement,"_",name,".rds"))
      return(model)
    }
)
  return(out)
  }
  
  
  names<-list(1:14
  )
  formulas<-list("Value~1",
                 
                 "Value~1+(1|BiopacSubject)",
                 "Value~1+(1|Spielfeld)",
                 
                 "Value~1+Factor1 + (1|BiopacSubject)",
                 "Value~1+Factor2 + (1|BiopacSubject)",
                 
                 "Value~1+Factor1+Factor2 + (1|BiopacSubject)",
                 "Value~1+Factor1*Factor2 + (1|BiopacSubject)",
                 
                 "Value~1+Factor1+ betweenCond + (1|BiopacSubject)",
                 "Value~1+Factor1 * betweenCond + (1|BiopacSubject)",
                 
                 "Value~1+Factor2+ betweenCond + (1|BiopacSubject)",
                 "Value~1+Factor2 * betweenCond + (1|BiopacSubject)",
                 
                 "Value~1+Factor1*Factor2 + betweenCond + (1|BiopacSubject)",
                 "Value~1+Factor1*Factor2 * betweenCond + (1|BiopacSubject)",
                 
                 "Value~1+Factor1*Factor2 * betweenCond + Time + (1|BiopacSubject)"
  )
  
  
  measurement<-list("Phight",
                    "QT",
                    "Rhight",
                    "RRi",
                    "ST"
  )
  models<-lapply(measurement,function(x,y){within(y,measurement<-x)},y= data_frame(formulas,names)) %>% bind_rows()
  models <- split(models, seq(nrow(models)))
  # Calculate the number of cores
  #no_cores <- detectCores() - 1
  # Initiate cluster
  #cl <- makeCluster(no_cores)
  #clusterExport(cl, list("getBRMModel","read_rds"))
  #est_model<-parLapply(cl,models,function(x,y){getBRMModel(dataframe = x,data=y,path = "models/")},y=physio)
  est_model<-lapply(models[args],function(x,y){getBRMModel(dataframe = x,data=y,path = "models/")},y=physio)
  
  #stopCluster(cl)
  