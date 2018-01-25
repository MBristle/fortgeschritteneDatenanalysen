getBRMModel <- function(dataframe=NULL,form=NULL,measurement=NULL,data=NULL,name=NULL,path="",family = student() ,control=NULL, prior=NULL,autocor=NULL) {
  options (mc.cores=parallel::detectCores ()) # Run on multiple cores
  # load necessary library files
  if (!require("pacman")) install.packages("pacman")
  library(pacman)
  p_load(readr)
  p_load(dplyr)
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
      cat(paste("\n\nFile does not exist: ", paste0(path, name), "\nnew Model will be calculated and saved with the specified name. \nGo grab a cup of coffee  and do 10 Push-ups:)\n\n"))
      
      model<-brm(form,data = data,prior=prior,family = family,control=control, autocor=autocor)
      saveRDS(model,paste0(path,measurement,"_",name,".rds"))
      return(model)
    }
  )    
  return(out)
}