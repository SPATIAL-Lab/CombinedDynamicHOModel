
############################
#### AB MODEL CONSTANTS #### 
############################


## n of reps
Reps = 105 

## n of days of simulation for each rep
Days = 60

## hour n at the start of simulation 
Hour.no.i = 1

## count at the start of simulation 
count = 0 

## hour n that are night and day
Night = c(1,2,3,4,5,19,20,21,22,23,24) 
Day = c(6:18)

## read in habitat, water and food area files
Habitat = raster("data/Habitat2.gri")  
Water = raster("data/Water.gri")
FoodM = raster("data/FoodM.gri")

Habitat = readAll(Habitat)  
Water = readAll(Water)  
FoodM = readAll(FoodM) 

extent = extent(1-0.5, ncol(Habitat)+0.5, 1-0.5, nrow(Habitat)+0.5)
extent(Habitat) = extent
extent(Water) = extent
extent(FoodM) = extent


