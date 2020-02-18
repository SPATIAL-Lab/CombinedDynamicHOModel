
############################
### AB MODEL EXPERIMENTS ### 
############################


## load required libraries
library(raster) 
library(plyr)


## set wd to the master project directory (i.e., the directory that contains the 'code' and 'data' folders)


## set exp name
exp = "B1" 
froot = "B1"  


## read in model constants
source("code/AB_model/AB_constants.R")  


## restrict start location to the center of habitat area file 
Habitat.df = as.data.frame(Habitat, xy=TRUE)
colnames(Habitat.df) = c("Lon", "Lat", "Habitat")
Habitat.df = Habitat.df[complete.cases(Habitat.df),]

LonWStart = as.integer(ncol(Habitat)/4)   
LonEStart = ncol(Habitat) - as.integer(ncol(Habitat)/4)
LatSStart = as.integer(nrow(Habitat)/4) 
LatNStart = nrow(Habitat) - as.integer(nrow(Habitat)/4)

Start = Habitat.df[(Habitat.df$Lon >= LonWStart & Habitat.df$Lon <= LonEStart & Habitat.df$Lat >= LatSStart & Habitat.df$Lat <= LatNStart),]


## read in model function
source("code/AB_model/AB_function.R") 


## set start location - equal number of birds nesting in each habitat 
for (i in 1:Reps) {
	
		Birds.RIP = sort(sample(c(1:Reps),35))
		Birds.MEAD = sort(sample(c(1:Reps)[! c(1:Reps) %in% Birds.RIP],35))
		Birds.SLP = sort(c(1:Reps)[! c(1:Reps) %in% c(Birds.RIP,Birds.MEAD)])
			
}

Birds.RIP.df = data.frame("Rep"=Birds.RIP, "HomeLocHab"=rep(1,length(Birds.RIP)))
Birds.MEAD.df = data.frame("Rep"=Birds.MEAD, "HomeLocHab"=rep(2,length(Birds.MEAD)))
Birds.SLP.df = data.frame("Rep"=Birds.SLP, "HomeLocHab"=rep(3,length(Birds.SLP)))

Birds.df = rbind(Birds.RIP.df, Birds.MEAD.df, Birds.SLP.df) 

Birds.df = Birds.df[order(Birds.df$Rep),]


## set parameter values, depending on exp name
if (exp == "E1") {
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
	if (exp == "E2A") {
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
		if (exp == "E2B") {
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
			if (exp == "E2C") {
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
				if (exp == "B1") {
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
					if (exp == "B2A") {
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
						if (exp == "B2B") {
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
							if (exp == "B2C") {
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
								if (exp == "E1B1") {
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

write.csv(BirdParms, paste0("output/",exp,"/",froot,"Parms.csv"))


## run model and write df with model results for all reps 
start.t = proc.time()
Bird = birds(Reps)
print(proc.time() - start.t)

write.csv(Bird, paste0("output/",exp,"/",froot,".csv"))


