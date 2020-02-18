
############################
######### HO TABLES ######## 
############################


## load required libraries
library(gdata)
library(raster)
library(RColorBrewer)
library(sp)
library(plyr)


## set wd to the master project directory (i.e., the directory that contains the 'code' and 'data' folders)


## create 'output/tables' folder where summary tables will be saved
dir.create("output", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE)


### First, create table for all reps from each exp: e.g., B1

## read in AB model output; a single data table for all reps
Angry_Birds = read.csv("output/B1/B1.csv", header=T, sep=",")
Angry_BirdsParms = read.csv("output/B1/B1Parms.csv", header=T, sep=",") 

## read in phys/food model output; a data table for each rep 
froot = "output/B1/B1_HO_" 

## create a single data table for all reps
Reps = c(1:max(Angry_Birds$Rep))
dlist = list()
for (i in Reps) {
	d = read.csv(paste0(froot, i, ".csv"))
	d$Rep = i
	dlist[[i]] = d
}
d_all = do.call(rbind, dlist)


## add day n to AB and phys/food model outputs 
Day.no_each_bird = rep(c(1:60),each=24) # for each rep (for d dataset)
Day.no = rep(Day.no_each_bird, length(Reps)) # for all reps (for Angry_Birds and d_all dataset)
   
# (all inds.; 60 d simulation)   
Angry_Birds$Day.no = Day.no
d_all$Day.no = Day.no


## identify and remove dead reps
Dead = unique(d_all$Rep[d_all$O2 < 0])

# (- dead inds.; 60 d simulation) 
Angry_Birds2 = Angry_Birds[! Angry_Birds$Rep %in% Dead, ]
Reps2 = Reps[! Reps %in% Dead]
d_all2 = d_all[! d_all$Rep %in% Dead, ]


## remove first 30 d of simulation

# (all inds.; 30 d simulation)
Angry_Birds30 = Angry_Birds[Angry_Birds$Day.no > 30, ]
d_all30 = d_all[d_all$Day.no > 30, ]

# (- dead inds.; 30 d simulation)
Angry_Birds302 = Angry_Birds30[! Angry_Birds30$Rep %in% Dead, ]
d_all302 = d_all30[! d_all30$Rep %in% Dead, ]



## calc ind. avg. and SD (all inds.; 30 d simulation)
output = matrix(ncol=87,nrow=length(Reps))
for (i in Reps) {
	output[i,1] = i
	d = read.csv(paste0(froot, i, ".csv"))
	d$Day.no = Day.no_each_bird
	d30 = d[d$Day.no > 30, ]
	output[i,2] = mean(d30$O2) 
	output[i,3] = mean(d30$H2)
	output[i,4] = mean(d30$d2Hres)
	output[i,5] = mean(d30$d18Ores)
	output[i,6] = mean(d30$d18Oresprot)
	output[i,7] = mean(d30$d2Hbw)
	output[i,8] = mean(d30$d18Obw)
	output[i,9] = mean(d30$d2Hker)
	output[i,10] = mean(d30$d18Oker)
	output[i,11] = sd(d30$O2) 
	output[i,12] = sd(d30$H2)
	output[i,13] = sd(d30$d2Hres)
	output[i,14] = sd(d30$d18Ores)
	output[i,15] = sd(d30$d18Oresprot)
	output[i,16] = sd(d30$d2Hbw)
	output[i,17] = sd(d30$d18Obw)
	output[i,18] = sd(d30$d2Hker)
	output[i,19] = sd(d30$d18Oker)
	
	output[i,20] = mean(d30$d2Hew)
	output[i,21] = mean(d30$d2Hinsw)
	output[i,22] = mean(d30$d2Hins)
	output[i,23] = mean(d30$d2Hinscarb)
	output[i,24] = mean(d30$d2Hinsfat)
	output[i,25] = mean(d30$d2Hinsprot)
	output[i,26] = mean(d30$d18Oew)
	output[i,27] = mean(d30$d18Oinsw)
	output[i,28] = mean(d30$d18Oins)
	output[i,29] = mean(d30$d18Oinscarb)
	output[i,30] = mean(d30$d18Oinsfat)
	output[i,31] = mean(d30$d18Oinsprot)
	output[i,32] = sd(d30$d2Hew)
	output[i,33] = sd(d30$d2Hinsw)
	output[i,34] = sd(d30$d2Hins)
	output[i,35] = sd(d30$d2Hinscarb)
	output[i,36] = sd(d30$d2Hinsfat)
	output[i,37] = sd(d30$d2Hinsprot)
	output[i,38] = sd(d30$d18Oew)
	output[i,39] = sd(d30$d18Oinsw)
	output[i,40] = sd(d30$d18Oins)
	output[i,41] = sd(d30$d18Oinscarb)
	output[i,42] = sd(d30$d18Oinsfat)
	output[i,43] = sd(d30$d18Oinsprot)
	
	output[i,44] = mean(d30$FMR)
	output[i,45] = mean(d30$O2in)
	output[i,46] = mean(d30$O2out)
	output[i,47] = mean(d30$FfH)
	output[i,48] = mean(d30$Ffw)
	output[i,49] = mean(d30$Fdw)
	output[i,50] = mean(d30$Fvw)
	output[i,51] = mean(d30$FwasteH)
	output[i,52] = mean(d30$Flw)
	output[i,53] = mean(d30$FfO)
	output[i,54] = mean(d30$FO2)
	output[i,55] = mean(d30$FwasteO)
	output[i,56] = mean(d30$FCO2)
	output[i,57] = sd(d30$FMR)
	output[i,58] = sd(d30$O2in)
	output[i,59] = sd(d30$O2out)
	output[i,60] = sd(d30$FfH)
	output[i,61] = sd(d30$Ffw)
	output[i,62] = sd(d30$Fdw)
	output[i,63] = sd(d30$Fvw)
	output[i,64] = sd(d30$FwasteH)
	output[i,65] = sd(d30$Flw)
	output[i,66] = sd(d30$FfO)
	output[i,67] = sd(d30$FO2)
	output[i,68] = sd(d30$FwasteO)
	output[i,69] = sd(d30$FCO2)
	
	output[i,70] = mean(d30$d2Hf)
	output[i,71] = mean(d30$d2Hfw)
	output[i,72] = mean(d30$d2Hdw)
	output[i,73] = mean(d30$d18Of)
	output[i,74] = mean(d30$d18Ofw)
	output[i,75] = mean(d30$d18Odw)
	output[i,76] = mean(d30$d18Oo2)
	output[i,77] = sd(d30$d2Hf)
	output[i,78] = sd(d30$d2Hfw)
	output[i,79] = sd(d30$d2Hdw)
	output[i,80] = sd(d30$d18Of)
	output[i,81] = sd(d30$d18Ofw)
	output[i,82] = sd(d30$d18Odw)
	output[i,83] = sd(d30$d18Oo2)
	
	# HO isotopic inputs from prey and prey water weighted by n of preys eaten
	output[i,84] = mean((d30$d2Hf*d30$Eat.no))/mean(d30$Eat.no)
	output[i,85] = mean((d30$d2Hfw*d30$Eat.no))/mean(d30$Eat.no)
	output[i,86] = mean((d30$d18Of*d30$Eat.no))/mean(d30$Eat.no)
	output[i,87] = mean((d30$d18Ofw*d30$Eat.no))/mean(d30$Eat.no)
		
}	
meansInd = as.data.frame(output)
names(meansInd) = c("Rep","O2_avg","H2_avg","d2Hres_avg","d18Ores_avg","d18Oresprot_avg","d2Hbw_avg","d18Obw_avg","d2Hker_avg","d18Oker_avg",
					"O2_sd","H2_sd","d2Hres_sd","d18Ores_sd","d18Oresprot_sd","d2Hbw_sd","d18Obw_sd","d2Hker_sd","d18Oker_sd", 
					"d2Hew_avg","d2Hinsw_avg","d2Hins_avg","d2Hinscarb_avg","d2Hinsfat_avg","d2Hinsprot_avg", 
					"d18Oew_avg","d18Oinsw_avg","d18Oins_avg","d18Oinscarb_avg","d18Oinsfat_avg","d18Oinsprot_avg",
					"d2Hew_sd","d2Hinsw_sd","d2Hins_sd","d2Hinscarb_sd","d2Hinsfat_sd","d2Hinsprot_sd", 
					"d18Oew_sd","d18Oinsw_sd","d18Oins_sd","d18Oinscarb_sd","d18Oinsfat_sd","d18Oinsprot_sd",
					"FMR_avg","O2in_avg","O2out_avg","FfH_avg","Ffw_avg","Fdw_avg","Fvw_avg","FwasteH_avg","Flw_avg",
					"FfO_avg","FO2_avg","FwasteO_avg","FCO2_avg",
					"FMR_sd","O2in_sd","O2out_sd","FfH_sd","Ffw_sd","Fdw_sd","Fvw_sd","FwasteH_sd","Flw_sd","FfO_sd","FO2_sd",
					"FwasteO_sd","FCO2_sd",
					"d2Hf_avg","d2Hfw_avg","d2Hdw_avg","d18Of_avg","d18Ofw_avg","d18Odw_avg","d18OO2_avg",
					"d2Hf_sd","d2Hfw_sd","d2Hdw_sd","d18Of_sd","d18Ofw_sd","d18Odw_sd","d18OO2_sd",
					"d2Hf_avgw","d2Hfw_avgw","d18Of_avgw","d18Ofw_avgw"
					)


## retrive home location habitat for each rep
HomeLoc = ddply(Angry_Birds, "Rep", head, 1)
HomeLocHab = HomeLoc$Habitat


## calc distance home-closest riparian cell for each rep 
Dist = 0 
for (i in Reps) {

Habitat = raster("data/Habitat2.gri")		
extent = extent(1-0.5, ncol(Habitat)+0.5, 1-0.5, nrow(Habitat)+0.5)
extent(Habitat) = extent
 
Habitat.df = as.data.frame(Habitat, xy=TRUE)
Habitat.df = Habitat.df[complete.cases(Habitat.df),]
names(Habitat.df) = c("Lon","Lat","Hab")

HomeLon = Angry_Birds$Lon[Angry_Birds$Rep==i][1]
HomeLat = Angry_Birds$Lat[Angry_Birds$Rep==i][1]

# Data frame of lon, lat and hab for riparian cells  
HabitatRIP = Habitat.df[Habitat.df$Hab == 1,]

# Dist between each (riparian) cell lon and home lon, and between each (riparian) cell lat and home lat - and actual dist  
HabitatRIP$LonDiff = abs(HabitatRIP$Lon - HomeLon)
HabitatRIP$LatDiff = abs(HabitatRIP$Lat - HomeLat)
HabitatRIP$Dist = sqrt(HabitatRIP$LonDiff^2 + HabitatRIP$LatDiff^2)
HabitatRIP = HabitatRIP[with(HabitatRIP, order(HabitatRIP$Dist)),]
Dist[i] = HabitatRIP$Dist[1]
} 


## calc prop of time spent in each habitat for each rep 
output = matrix(ncol=4,nrow=length(Reps))
for (i in Reps) {
	output[i,1] = i
	output[i,2] = length(Angry_Birds30$Rep[Angry_Birds30$Rep==i & Angry_Birds30$Habitat==1])/nrow(Angry_Birds30[Angry_Birds30$Rep==i,])
	output[i,3] = length(Angry_Birds30$Rep[Angry_Birds30$Rep==i & Angry_Birds30$Habitat==2])/nrow(Angry_Birds30[Angry_Birds30$Rep==i,])
	output[i,4] = length(Angry_Birds30$Rep[Angry_Birds30$Rep==i & Angry_Birds30$Habitat==3])/nrow(Angry_Birds30[Angry_Birds30$Rep==i,])
}
timeHab = as.data.frame(output)
names(timeHab) = c("Rep","timeRIP","timeMEAD","timeSLP")
timeRIP = timeHab$timeRIP
timeMEAD = timeHab$timeMEAD
timeSLP = timeHab$timeSLP


## calc n of drinking / feeding events for each rep
output = matrix(ncol=4,nrow=length(Reps)) 
for (i in Reps) {
	output[i,1] = i
	output[i,2] = sum(Angry_Birds30$DrinkYN[Angry_Birds30$Rep == i])
	output[i,3] = sum(Angry_Birds30$EatYN[Angry_Birds30$Rep == i])
	output[i,4] = sum(Angry_Birds30$Eat.no[Angry_Birds30$Rep == i])
}
drinkEatHab = as.data.frame(output)
names(drinkEatHab) = c("Rep","DrinkYN","EatYN","Eat.no") 
DrinkYN = drinkEatHab$DrinkYN
EatYN = drinkEatHab$EatYN
Eat.no = drinkEatHab$Eat.no 


## retrive parameter values 
ThirstIncreaseNight = Angry_BirdsParms$ThirstIncreaseNight
ThirstIncreaseDay = Angry_BirdsParms$ThirstIncreaseDay
HungerIncreaseNight = Angry_BirdsParms$HungerIncreaseNight
HungerIncreaseDay = Angry_BirdsParms$HungerIncreaseDay
AvEatSuccRate = Angry_BirdsParms$AvEatSuccRate
WeightType1 = Angry_BirdsParms$WeightType1
WeightType2 = Angry_BirdsParms$WeightType2
WeightType4 = Angry_BirdsParms$WeightType4
Search.dist = Angry_BirdsParms$Search.dist 


## add parameter values to table 
meansInd$HomeLocHab = HomeLocHab
meansInd$Dist = Dist
meansInd$timeRIP = timeHab$timeRIP
meansInd$timeMEAD = timeHab$timeMEAD
meansInd$timeSLP = timeHab$timeSLP
meansInd$DrinkYN = drinkEatHab$DrinkYN
meansInd$EatYN = drinkEatHab$EatYN
meansInd$Eat.no = drinkEatHab$Eat.no 

meansInd$ThirstIncreaseNight = ThirstIncreaseNight
meansInd$ThirstIncreaseDay = ThirstIncreaseDay
meansInd$HungerIncreaseNight = HungerIncreaseNight
meansInd$HungerIncreaseDay = HungerIncreaseDay
meansInd$AvEatSuccRate = AvEatSuccRate
meansInd$WeightType1 = WeightType1
meansInd$WeightType2 = WeightType2
meansInd$WeightType4 = WeightType4
meansInd$Search.dist = Search.dist


## remove dead inds from ind. avg. table
meansInd2 = meansInd[! meansInd$Rep %in% Dead, ]
meansInd = meansInd2



## calc useful metrics to data analysis 

## body water turnover = ind. avg. total body water pool size / ind. avg. input or output flux; in this case, should divide by output flux as Fdw is fixed whenever drinking occurs, whereas Flw is adjusted
meansInd$Turn = meansInd$H2_avg / (meansInd$Fvw_avg + meansInd$FwasteH_avg + meansInd$Flw_avg)


## calc avg input d2H,d18O, weighted by the relative proporions of each flux
meansInd$pFfH = meansInd$FfH_avg / (meansInd$FfH_avg + meansInd$Ffw_avg + meansInd$Fdw_avg)
meansInd$pFfw = meansInd$Ffw_avg / (meansInd$FfH_avg + meansInd$Ffw_avg + meansInd$Fdw_avg)
meansInd$pFdw = meansInd$Fdw_avg / (meansInd$FfH_avg + meansInd$Ffw_avg + meansInd$Fdw_avg)
meansInd$pFfO = meansInd$FfO_avg / (meansInd$FfO_avg + meansInd$Ffw_avg + meansInd$Fdw_avg + meansInd$FO2_avg) 
meansInd$pFO2 = meansInd$FO2_avg / (meansInd$FfO_avg + meansInd$Ffw_avg + meansInd$Fdw_avg + meansInd$FO2_avg)

meansInd$d2Hin = meansInd$d2Hf_avgw*meansInd$pFfH + meansInd$d2Hfw_avgw*meansInd$pFfw + meansInd$d2Hdw_avg*meansInd$pFdw
meansInd$d18Oin = meansInd$d18Of_avgw*meansInd$pFfO + meansInd$d18Ofw_avgw*meansInd$pFfw + meansInd$d18Odw_avg*meansInd$pFdw + meansInd$d18OO2_avg*alphaOatm_abs*meansInd$pFO2 # HO isotopic inputs from prey and prey water weighted by no of insects eaten are used 


## write ind. avg. table 
write.csv(meansInd, file="output/tables/meansInd.B1.csv")



### Then, for data analysis, create tables for all reps from: 1- all B exps, 2- all E exps, 3- E1B1 exp

meansInd.B1 = read.csv("output/tables/meansInd.B1.csv", header=T, sep=",")
meansInd.B2A = read.csv("output/tables/meansInd.B2A.csv", header=T, sep=",")
meansInd.B2B = read.csv("output/tables/meansInd.B2B.csv", header=T, sep=",")
meansInd.B2C = read.csv("output/tables/meansInd.B2C.csv", header=T, sep=",")

meansInd.B1$exp = rep("B1", length(meansInd.B1$Rep))
meansInd.B2A$exp = rep("B2A", length(meansInd.B2A$Rep))
meansInd.B2B$exp = rep("B2B", length(meansInd.B2B$Rep))
meansInd.B2C$exp = rep("B2C", length(meansInd.B2C$Rep))

meansInd = rbind(meansInd.B1, meansInd.B2A, meansInd.B2B, meansInd.B2C)
meansInd = meansInd[,-1]

write.csv(meansInd, file="output/tables/meansInd.B.csv")


meansInd.E1 = read.csv("output/tables/meansInd.E1.csv", header=T, sep=",")
meansInd.E2A = read.csv("output/tables/meansInd.E2A.csv", header=T, sep=",")
meansInd.E2B = read.csv("output/tables/meansInd.E2B.csv", header=T, sep=",")
meansInd.E2C = read.csv("output/tables/meansInd.E2C.csv", header=T, sep=",")

meansInd.E1$exp = rep("E1", length(meansInd.E1$Rep))
meansInd.E2A$exp = rep("E2A", length(meansInd.E2A$Rep))
meansInd.E2B$exp = rep("E2B", length(meansInd.E2B$Rep))
meansInd.E2C$exp = rep("E2C", length(meansInd.E2C$Rep))

meansInd = rbind(meansInd.E1, meansInd.E2A, meansInd.E2B, meansInd.E2C)
meansInd = meansInd[,-1]

write.csv(meansInd, file="output/tables/meansInd.E.csv")


meansInd.E1B1 = read.csv("output/tables/meansInd.E1B1.csv", header=T, sep=",")

meansInd.E1B1$exp = rep("E1B1", length(meansInd.E1B1$Rep))

meansInd = meansInd.E1B1
meansInd = meansInd[,-1]

write.csv(meansInd, file="output/tables/meansInd.E1B1.csv")

 
 