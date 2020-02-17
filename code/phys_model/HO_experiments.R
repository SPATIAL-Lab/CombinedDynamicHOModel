
############################
###### HO EXPERIMENTS ###### 
############################


## load required libraries
library(deSolve)
library(doParallel)
library(foreach)
library(gdata)
library(MASS)

## set wd
setwd("/Users/Sarah/Desktop/CombinedDynamicHOModel/github/phys_model/")


## read in parameter and output tables for all birds from AB model   
ifile = "../../input_output/B1/B1.csv" 
ifileParms = "../../input_output/B1/B1Parms.csv"
Angry_Birds = read.csv(ifile, header=T, sep=",") 
Angry_BirdsParms = read.csv(ifileParms, header=T, sep=",") 
Angry_Birds = Angry_Birds[,-1]
Angry_BirdsParms = Angry_BirdsParms[,-1] 

## read in HO data for environmental water, prey water and prey in RBC
source("HO_data.R")

## read in output from SS phys/food model run for TL 2; assumes initial values for state variables (d2H,d18O res pool and body water) equal to SS values 
ssBird = read.csv("/Users/Sarah/Desktop/CombinedDynamicHOModel/large_data/ssBird.csv")

## read in phys/food (HO) model parameters
exp = "E2C"  
HungerIncreaseNight_all = Angry_BirdsParms$HungerIncreaseNight 
HungerIncreaseDay_all = Angry_BirdsParms$HungerIncreaseDay  
source("HO_parameters.R")

## read in phys/food model constants
source("HO_constants.R")

## read in phys/food model functions
source("HO_functions.R") 
 
## set filename for output table for individual birds from phys/food model   
froot = "../../input_output/B1/B1_HO_"  


## retrive n of reps
Tot.reps = max(Angry_Birds$Rep)


## run phys/food model 
for (i in 1:Tot.reps) { 
  Jim = Angry_Birds[Angry_Birds$Rep == i,]
  HungerIncreaseNight = Angry_BirdsParms$HungerIncreaseNight[Angry_BirdsParms$Rep == i]
  HungerIncreaseDay = Angry_BirdsParms$HungerIncreaseDay[Angry_BirdsParms$Rep == i]
  out = HOdriver(Jim)
  out.df = as.data.frame(out)
  
  ## convert d2H,d18O body water to keratin
  # H isotope composition of follicle water
  out.df$RHfollw = Pbw * delta.to.R(out.df$d2Hbw, RstandardH) + (1 - Pbw) * delta.to.R(out.df$d2Hf, RstandardH)
  out.df$d2Hfollw = R.to.delta(out.df$RHfollw, RstandardH)
  
  # H isotope composition of keratin
  out.df$RHker = PfH * delta.to.R(out.df$d2Hres, RstandardH) + (1 - PfH) * alphaHprot * out.df$RHfollw 
  out.df$d2Hker = R.to.delta(out.df$RHker, RstandardH)
  
  # O isotope composition of keratin; equals that of the protein component of the food store as no other source of O is added at the site of keratin formation  
  out.df$d18Oker = out.df$d18Oresprot

  write.csv(out.df, paste0(froot,i,".csv")) 
}


