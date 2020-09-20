
############################
### AB MODEL EXPERIMENTS ### 
############################


## load required libraries
library(raster) 
library(plyr)


## set wd to the master project directory (i.e., the directory that contains the 'code' and 'data' folders)


## create 'output' folder and ex subfolders where AB model output will be saved
dir.create("output", showWarnings = FALSE)

dir.create("output/B1_rl", showWarnings = FALSE)
dir.create("output/B2A_rl", showWarnings = FALSE)
dir.create("output/B2B_rl", showWarnings = FALSE)
dir.create("output/B2C_rl", showWarnings = FALSE)
dir.create("output/E1_rl", showWarnings = FALSE)
dir.create("output/E2A_rl", showWarnings = FALSE)
dir.create("output/E2B_rl", showWarnings = FALSE)
dir.create("output/E2C_rl", showWarnings = FALSE)
dir.create("output/E1B1_rl", showWarnings = FALSE)

## read in model constants
source("code/AB_model/AB_constants.R")   

## read in model function
source("code/AB_model/AB_function.R") 

for(ex in c("B1_rl", "B2A_rl", "B2B_rl", "B2C_rl", "E1_rl", "E2A_rl", "E2B_rl", 
             "E2C_rl", "E1B1_rl")){
  
  froot = ex  
  
  ## restrict start location to the center of habitat area file 
  Habitat.df = as.data.frame(Habitat, xy=TRUE)
  colnames(Habitat.df) = c("Lon", "Lat", "Habitat")
  Habitat.df = Habitat.df[complete.cases(Habitat.df),]
  
  LonWStart = as.integer(ncol(Habitat)/4)   
  LonEStart = ncol(Habitat) - as.integer(ncol(Habitat)/4)
  LatSStart = as.integer(nrow(Habitat)/4) 
  LatNStart = nrow(Habitat) - as.integer(nrow(Habitat)/4)
  
  Start = Habitat.df[(Habitat.df$Lon >= LonWStart & Habitat.df$Lon <= LonEStart & Habitat.df$Lat >= LatSStart & Habitat.df$Lat <= LatNStart),]
  
  
  
  ## set start location - random locations
  Rep = NULL
  Index = NULL
  Lon = NULL
  Lat = NULL
  HomeLocHab = NULL
  
  for (i in 1:Reps) {
  
  Rep[i] = i
  Index[i] = sample(rownames(Start),1)
  Lon[i] = Start$Lon[rownames(Start) == Index[i]]
  Lat[i] = Start$Lat[rownames(Start) == Index[i]]
  HomeLocHab[i] = Start$Habitat[rownames(Start) == Index[i]]
  
  }
  
  Birds.df = data.frame("Rep"=Rep, "Lon"=Lon, "Lat"=Lat, "HomeLocHab"=HomeLocHab)
  
  
  ## set parameter values, depending on ex name
  if (ex == "E1_rl") {
  	# Parameter set for E1
  	ThirstIncreaseNight = rep(0.1, Reps)
  	ThirstIncreaseDay = rep(0.2, Reps) 
  	HungerIncreaseNight = rep(1, Reps)
  	HungerIncreaseDay = rep(5, Reps)
  	AvEatSuccRate = rep(0.1, Reps)
  	WeightType1 = rep(1000, Reps)
  	WeightType2 = rep(100, Reps)
  	WeightType4 = rep(1, Reps)
  	Search.dist = rep(30, Reps)
  	HomeLocHab = Birds.df$HomeLocHab
  } else {
  	if (ex == "E2A_rl") {
  		# Parameter set for E2A 
  		ThirstIncreaseNight = rep(0.1*1.5, Reps)
  		ThirstIncreaseDay = rep(0.2*1.5, Reps) 
  		HungerIncreaseNight = rep(1*1.5, Reps)
  		HungerIncreaseDay = rep(5*1.5, Reps)
  		AvEatSuccRate = rep(0.1, Reps)
  		WeightType1 = rep(1000, Reps)
  		WeightType2 = rep(100, Reps)
  		WeightType4 = rep(1, Reps)
  		Search.dist = rep(30, Reps)
  		HomeLocHab = Birds.df$HomeLocHab
  	} else {
  		if (ex == "E2B_rl") {
  			# Parameter set for E2B
  			ThirstIncreaseNight = rep(0.1, Reps)
  			ThirstIncreaseDay = rep(0.2, Reps) 
  			HungerIncreaseNight = rep(1, Reps)
  			HungerIncreaseDay = rep(5, Reps)
  			AvEatSuccRate = rep(0.1*2, Reps)
  			WeightType1 = rep(1000*2, Reps)
  			WeightType2 = rep(100*2, Reps)
  			WeightType4 = rep(1*2, Reps)
  			Search.dist = rep(30, Reps)
  			HomeLocHab = Birds.df$HomeLocHab
  		} else {
  			if (ex == "E2C_rl") {
  				# Parameter set for E2C
  				ThirstIncreaseNight = rep(0.1, Reps)
  				ThirstIncreaseDay = rep(0.2, Reps) 
  				HungerIncreaseNight = rep(1, Reps)
  				HungerIncreaseDay = rep(5, Reps)
  				AvEatSuccRate = rep(0.1, Reps)
  				WeightType1 = rep(1000, Reps)
  				WeightType2 = rep(100, Reps)
  				WeightType4 = rep(1, Reps)
  				Search.dist = rep(30*2, Reps)
  				HomeLocHab = Birds.df$HomeLocHab
  			} else {
  				if (ex == "B1_rl") {
  					# Parameter set for B1
  					mred = runif(Reps, 0.9, 1.1)
  					mgreen = runif(Reps, 0.9, 1.1)
  					mblue = runif(Reps, 0.9, 1.1)
  					ThirstIncreaseNight = 0.1*mred
  					ThirstIncreaseDay = 0.2*mred 
  					HungerIncreaseNight = 1*mred
  					HungerIncreaseDay = 5*mred
  					AvEatSuccRate = 0.1*mgreen
  					WeightType1 = 1000*mgreen
  					WeightType2 = 100*mgreen
  					WeightType4 = rep(1,Reps)
  					Search.dist = 30*mblue
  					HomeLocHab = Birds.df$HomeLocHab
  				} else {
  					if (ex == "B2A_rl") {
  						# Parameter set for B2A
  						mred = runif(Reps, 0.9, 1.1)
  						mgreen = runif(Reps, 0.9, 1.1)
  						mblue = runif(Reps, 0.9, 1.1)
  						ThirstIncreaseNight = 0.1*1.5*mred
  						ThirstIncreaseDay = 0.2*1.5*mred 
  						HungerIncreaseNight = 1*1.5*mred
  						HungerIncreaseDay = 5*1.5*mred
  						AvEatSuccRate = 0.1*mgreen
  						WeightType1 = 1000*mgreen
  						WeightType2 = 100*mgreen
  						WeightType4 = rep(1,Reps)
  						Search.dist = 30*mblue
  						HomeLocHab = Birds.df$HomeLocHab
  					} else {
  						if (ex == "B2B_rl") {
  							# Parameter set for B2B
  							mred = runif(Reps, 0.9, 1.1)
  							mgreen = runif(Reps, 0.9, 1.1)
  							mblue = runif(Reps, 0.9, 1.1)
  							ThirstIncreaseNight = 0.1*mred
  							ThirstIncreaseDay = 0.2*mred 
  							HungerIncreaseNight = 1*mred
  							HungerIncreaseDay = 5*mred
  							AvEatSuccRate = 0.1*2*mgreen
  							WeightType1 = 1000*2*mgreen
  							WeightType2 = 100*2*mgreen
  							WeightType4 = rep(1,Reps)
  							Search.dist = 30*mblue
  							HomeLocHab = Birds.df$HomeLocHab
  						} else {
  							if (ex == "B2C_rl") {
  								# Parameter set for B2C
  								mred = runif(Reps, 0.9, 1.1)
  								mgreen = runif(Reps, 0.9, 1.1)
  								mblue = runif(Reps, 0.9, 1.1)
  								ThirstIncreaseNight = 0.1*mred
  								ThirstIncreaseDay = 0.2*mred 
  								HungerIncreaseNight = 1*mred
  								HungerIncreaseDay = 5*mred
  								AvEatSuccRate = 0.1*mgreen
  								WeightType1 = 1000*mgreen
  								WeightType2 = 100*mgreen
  								WeightType4 = rep(1,Reps)
  								Search.dist = 30*2*mblue
  								HomeLocHab = Birds.df$HomeLocHab
  							} else {
  								if (ex == "E1B1_rl") {
  									# Parameter set for E1B1
  									mred = runif(Reps, 0.9, 1.1)
  									mgreen = runif(Reps, 0.9, 1.1)
  									mblue = runif(Reps, 0.9, 1.1)
  									ThirstIncreaseNight = 0.1*mred
  									ThirstIncreaseDay = 0.2*mred 
  									HungerIncreaseNight = 1*mred
  									HungerIncreaseDay = 5*mred
  									AvEatSuccRate = 0.1*mgreen
  									WeightType1 = 1000*mgreen
  									WeightType2 = 100*mgreen
  									WeightType4 = rep(1,Reps)
  									Search.dist = 30*mblue
  									HomeLocHab = Birds.df$HomeLocHab
  								}
  							}
  						}
  					}
  				}
  			}
  		}
  	}
  }
  
  ## create and write df with parameter values and home location habitats
  BirdParms = data.frame("Rep"=c(1:Reps), "ThirstIncreaseNight"=ThirstIncreaseNight, "ThirstIncreaseDay"=ThirstIncreaseDay, "HungerIncreaseNight"=HungerIncreaseNight, "HungerIncreaseDay"=HungerIncreaseDay, 
  "AvEatSuccRate"=AvEatSuccRate, "WeightType1"=WeightType1, "WeightType2"=WeightType2, "WeightType4"=WeightType4, "Search.dist"=Search.dist, "HomeLocHab"=HomeLocHab)
  
  write.csv(BirdParms, paste0("output/",ex,"/",froot,"Parms.csv"))
  
  
  ## run model and write df with model results for all reps 
  start.t = proc.time()
  Bird = birds(Reps, locs = "r")
  print(proc.time() - start.t)
  
  write.csv(Bird, paste0("output/",ex,"/",froot,".csv"))
  print(paste("Experiment", ex, "done")) 
}
