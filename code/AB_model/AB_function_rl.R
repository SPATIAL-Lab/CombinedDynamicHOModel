
############################
#### AB MODEL FUNCTION ##### 
############################

birds = function(Reps) {
	
	## calculate tot n of hours for each rep 
	Hours = Days * 24
		
	## set up table for storing the data; nrows = count of timesteps across days and reps; ncols = n of variables 
	vars = c("Rep", "Hour.no", "BS", "Lon", "Lat", "Thirst", "Hunger", "WaterYN", "Ins.no", "Habitat", "MoveYN", "DrinkYN", "EatYN", "Eat.no", "count")
	Bird = matrix(NA, nrow=Reps * Hours, ncol=length(vars))
	Bird = as.data.frame(Bird)
	colnames(Bird) = vars
	
	## set up plot window
	col.palette = (terrain.colors(12,1))
	habitat.col = c(col.palette[1],col.palette[5],col.palette[9])
	plot(Habitat, col=habitat.col, xlab="Lon", ylab="Lat")
	

	for (i in 1:Reps) {
		
		bird.t= proc.time()
		
		## refresh the food raster; food-scape at the start of simulation is the same for each rep
		Food = FoodM
		
		## start simulation at 1 am; hour.no at the start of simulation is the same for each rep
		Hour.no = Hour.no.i
		
		## set start (home) location - random locations
		Loc = c(Lon[i], Lat[i]) 
		HomeLoc = Loc
		
		## set water and food thresholds
		Water_thresh = runif(1, min=-0.5,max=0.2)
		Food_thresh = round(runif(1, min=-10,max=10),digits=0)
		

		for (t in 1:Hours) { 
			
		## start count of timesteps across days and reps 
    		count = count+1
    		
    		## stop hour n from going beyond 24
    		if (Hour.no > 24) {Hour.no = 1}	
    		
    		## retrieve behavioral state (day or night) based on hour n 
    		if (Hour.no %in% Day) {BS = "D"} else BS = "N"
    		
    		## sense conditions at current location
    		xy = cbind(Loc[1], Loc[2])
    		Habitat.t = extract(Habitat, xy)
    		Water.t = extract(Water, xy)
    		Ins.t = extract(Food, xy)
    		
    		
    		### 1- ARE YOU GOING TO MOVE ? ###
      
      		## If it is night, and you are not home, you are very likely to move (Prob.move=0.99); if you are home, you do not move (Prob.move=0.00)
      		if (BS == "N") {
      			if (Loc[1] != HomeLoc[1] || Loc[2] != HomeLoc[2]) {Prob.move = 0.99} else Prob.move = 0	
      		}
      		
      		## If it is day, and you are thirsty but no water is available, OR you are hungry but not enough food is available, OR both, you are likely to move (Prob.move=0.90); if you are thirsty and water is available, OR you are hungry and enough food is available, OR both, you are not likely to move (Prob.move=0.01). If you are neither thirsty nor hungry, and you are not home, you might move (Prob.move=0.50); if you are home, you do not move (Prob.move=0.00)
      		if (BS == "D") {
      			if (Water.t < Water_thresh || Ins.t * AvEatSuccRate[i] < Food_thresh) {Prob.move = 0.9} else Prob.move = 0.01
      			if (Water_thresh <= 0 & Food_thresh <= 0) {
      			if (Loc[1] != HomeLoc[1] || Loc[2] != HomeLoc[2]) {Prob.move = 0.5} else Prob.move = 0
      		}
      		}
      		
      		MoveYN = rbinom(1,1,Prob.move)
      		
    		
    		### WHERE ARE YOU GOING TO MOVE ?
    		
    		## If you move, 
    		if (MoveYN == 1) {
    			
    			## And it is night, you go home
    			if (BS == "N") {
    				Lon.new = HomeLoc[1]
    				Lat.new = HomeLoc[2] 
    			}
    			
    			## And it is day, you sense conditions within search area and decide where to move
    			if (BS == "D") { 
    				
    				# Define search area: square around your current location with each side = 2*search distance; cannot go beyond the extent of habitat area file; does not include cells with no LiDAR data  
    				LonWSearch = max(Loc[1]-Search.dist[i], 1)
    				LonESearch = min(Loc[1]+Search.dist[i], ncol(Habitat))
    				LatSSearch = max(Loc[2]-Search.dist[i], 1)
    				LatNSearch = min(Loc[2]+Search.dist[i], nrow(Habitat)) 
    
    				Search.area = extent(LonWSearch-0.5, LonESearch+0.5, LatSSearch-0.5, LatNSearch+0.5)

    				Search.area.habitat = crop(Habitat, Search.area)
    				Search.area.water = crop(Water, Search.area)
    				Search.area.insects = crop(Food, Search.area)
    
    				Search.area.habitat.df = as.data.frame(Search.area.habitat, xy=TRUE)
    				Search.area.water.df = as.data.frame(Search.area.water, xy=TRUE)
    				Search.area.insects.df = as.data.frame(Search.area.insects, xy=TRUE)
      
    				Search = Search.area.habitat.df
    				colnames(Search) = c("Lon", "Lat", "Habitat")
    				Search$Water = Search.area.water.df$layer
    				Search$Ins = Search.area.insects.df$layer
      
    				Search = Search[complete.cases(Search),] 
					
					## 1) If you are neither thirsty nor hungry, you have a 20% chance to go home, and a 80% chance to go to any cell within search area (random exploratory movement)
					if (Water_thresh <= 0 & Food_thresh <= 0) {
						
						p = runif(1, min=0, max=1) # Generate a random number from a uniform distribution with min=0 and max=1
						if (p <= 0.2) { # If this number is <= 0.2, you go home
            			Lon.new = HomeLoc[1]
            			Lat.new = HomeLoc[2]
            			} else { # If this number is > 0.2, you go to a random cell within search area
            			Index = sample(rownames(Search),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
            			}
            			
					}
					
					## 2) If you are thirsty but not hungry, you are 1000 times more likely to move to a cell that has water than to a cell that does not have water
					if (Water_thresh > 0 & Food_thresh <= 0) {
						
					# Assign a score of 1 to cells that meet criteria (optimal, i.e., have water), and a score of 4 to cells that do not meet criteria (poor, i.e., do not have water)
        				Search$Score = ifelse(Search$Water == 1, 1, 4)
        	
        				# Multiply the numbers of different cell types by their weights (based on likelihood ratio, i.e., 1000:1) 
        				# A bird is 1000 times more likely to go to an optimal cell than to a poor cell
        				p1 = WeightType1[i] * sum(Search$Score == 1)
        				p2 = WeightType2[i] * sum(Search$Score == 2) # This is 0 as there are no suboptimal cells, in this case
        				p4 = WeightType4[i] * sum(Search$Score == 4)
        	
        				psum = p1 + p2 + p4 # Calculate cumulative weight
          
          				p1 = p1 / psum # Rescale for each cell type
         				p2 = p2 / psum # This is 0 as there are no suboptimal cells, in this case
          				p4 = p4 / psum
          
          				p = runif(1, min=0, max=1)
          	
          				if (p <= p1) { # If p is within the range of probs for type 1 cells
            			Index = sample(rownames(Search[Search$Score == 1,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
            			} else if (p <= p1 + p2) { # If p is within the range of probs for type 2 cells
            			Index = sample(rownames(Search[Search$Score == 2,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
            			} else { # If p is within the range of probs for type 4 cells
            			Index = sample(rownames(Search[Search$Score == 4,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
            			}
            			
					}
					
					## 3) If you are hungry but not thirsty, you are 1000 times more likely to move to a cell that has enough food than to a cell that does not have enough food
					if (Water_thresh <= 0 & Food_thresh > 0) {
						
						# Assign a score of 1 to cells that meet criteria (optimal, i.e., have enough food), and a score of 4 to cells that do not meet criteria (poor, i.e., do not have enough food)
          				Search$Score = ifelse(Search$Ins * AvEatSuccRate[i] >= Food_thresh, 1, 4)
          
        					# Multiply the numbers of different cell types by their weights (based on likelihood ratio, i.e., 1000:1 ?) 
        					# A bird is 1000 times more likely to go to an optimal cell than to a poor cell
          				p1 = WeightType1[i] * sum(Search$Score == 1)
          				p2 = WeightType2[i] * sum(Search$Score == 2) # This is 0 as there are no suboptimal cells, in this case
          				p4 = WeightType4[i] * sum(Search$Score == 4)
          
          				psum = p1 + p2 + p4 # Calculate cumulative weight
          
          				p1 = p1 / psum # Rescale for each cell type
          				p2 = p2 / psum # This is 0 as there are no suboptimal cells, in this case
          				p4 = p4 / psum
          
          				p = runif(1, min=0, max=1)
            
          				if (p <= p1) { # If p is within the range of probs for type 1 cells
            			Index = sample(rownames(Search[Search$Score == 1,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
          				} else if (p <= p1 + p2) { # If p is within the range of probs for type 2 cells
            			Index = sample(rownames(Search[Search$Score == 2,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
          				} else { # If p is within the range of probs for type 4 cells
            			Index = sample(rownames(Search[Search$Score == 4,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
          				}
						
					}
					
					## 4) If you are both thirsty and hungry, you are 1000 times more likely to move to a cell that has both water and enough food than to a cell that has neither water nor enough food; you are 100 times more likley to move to a cell that has either water or enough food than to a cell that has neither water nor enough food
					if (Water_thresh > 0 & Food_thresh > 0) {
						
						# Assign a score of 1 to cells that meet criteria (optimal, i.e., have both water and enough food), a score of 2 to cells that partially meet criteria (suboptimal, i.e., have either water or enough food), and a score of 4 to cells that do not meet criteria (poor, i.e., have neither water nor enough food)
          				Search$Score = ifelse(Search$Water==1 & Search$Ins * AvEatSuccRate[i] >= Food_thresh, 1,
          				ifelse(Search$Water==1 & Search$Ins * AvEatSuccRate[i] < Food_thresh, 2,
          				ifelse(Search$Water==0 & Search$Ins * AvEatSuccRate[i] >= Food_thresh, 2, 4)))
          
        					# Multiply the numbers of different cell types by their weights (based on likelihood ratio, i.e., 1000:100:1) 
        					# A bird is 1000 times more likely to go to an optimal cell than to a poor cell
        					# A bird is 100 times more likely to go to a suboptimal cell than to a poor cell
          				p1 = WeightType1[i] * sum(Search$Score == 1)
          				p2 = WeightType2[i] * sum(Search$Score == 2)
          				p4 = WeightType4[i] * sum(Search$Score == 4)
          
          				psum = p1 + p2 + p4 # Calculate cumulative weight
          
          				p1 = p1 / psum # Rescale for each cell type
          				p2 = p2 / psum
          				p4 = p4 / psum
          
          				p = runif(1, min=0, max=1)
            
          				if (p <= p1) { # If p is within the range of probs for type 1 cells
            			Index = sample(rownames(Search[Search$Score == 1,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
          				} else if (p <= p1 + p2) { # If p is within the range of probs for type 2 cells
            			Index = sample(rownames(Search[Search$Score == 2,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
          				} else { # If p is within the range of probs for type 4 cells
            			Index = sample(rownames(Search[Search$Score == 4,]),1)
            			Lon.new = Search$Lon[rownames(Search) == Index] 
            			Lat.new = Search$Lat[rownames(Search) == Index]
          				}
						
					}
					
    			}

    		}
    		
    		## If you do not move, you stay there 
    		if (MoveYN == 0) {
    			Lon.new = Loc[1] 
      			Lat.new = Loc[2] 
    		} 
    		
    		
    		# Update current location to new location (i.e., location where you have just moved)
    		Loc = c(Lon.new, Lat.new)
    	
    		# Sense conditions at new location (and decide whether to drink / whether and how much to eat) 
    		xy = cbind(Loc[1], Loc[2])
    
    		Habitat.t = extract(Habitat, xy)
    		Water.t = extract(Water, xy)
    		Ins.t = extract(Food, xy)
    		
    		
    		### 2- ARE YOU GOING TO DRINK ?
    		
    		# If it is night, and water is available, and you are thirsty, you are unlikely to drink (Prob.drink=0.10); if you are not thirsty, you are even more unlikely to drink (Prob.drink=0.01). If water is not available, you are not going to drink (Prob.drink=0.00). If you do not drink, your water threshold increases by thirst increase rate at night 
    		if (BS == "N") {
    			if (Water.t == 1) {
    			if (Water_thresh > 0) {Prob.drink = 0.1} else Prob.drink = 0.01
    			} else Prob.drink = 0
    			DrinkYN = rbinom(1, 1, Prob.drink)
    			if (DrinkYN == 0) {Water_thresh = Water_thresh + ThirstIncreaseNight[i]}
    		} 
    		
    		# If it is day, and water is available, and you are thirsty, you are going to drink (Prob.drink=1.00); if you are not thirsty, you might drink (Prob.drink=0.50). If water is not available, you are not going to drink (Prob.drink=0.00). If you do not drink, your water threshold increases by thirst increase rate during the day 
    		if (BS == "D") { 
    			if (Water.t == 1) {
    			if (Water_thresh > 0) {Prob.drink = 1} else Prob.drink = 0.5
    			} else Prob.drink = 0
    			DrinkYN = rbinom(1, 1, Prob.drink)
    			if (DrinkYN == 0) {Water_thresh = Water_thresh + ThirstIncreaseDay[i]}
    		}
    		
    		# For both day and night: if you drink, your water threshold resets to -1
    		if (DrinkYN == 1) {Water_thresh = -1}
    		
    		
    		### 3- ARE YOU GOING TO EAT AND HOW MUCH ?
    		
    		# If it is night, and some food is available, and you are hungry, you are unlikely to eat (Prob.eat=0.10); if you are not hungry, you are even more unlikely to eat (Prob.eat=0.01). If food is not available, you are not going to eat (Prob.eat=0.00). If you do not eat, your food threshold increases by hunger increase rate at night  
    		if (BS == "N") {
    			if (Ins.t > 0) {
    			if (Food_thresh > 0) {Prob.eat = 0.1} else Prob.eat = 0.01
    			} else Prob.eat = 0
    			EatYN = rbinom(1, 1, Prob.eat)
    			if (EatYN == 0) {
    			Eat.no = 0
    			Food_thresh = Food_thresh + HungerIncreaseNight[i]
    		}
    		}
    		
    		## If it is day, and some food is available, and you are hungry, you are going to eat (Prob.eat=1.00); if you are not hungry, you might eat (Prob.eat=0.25). If food is not available, you are not going to eat (Prob.eat=0.00). If you do not eat, your food threshold increases by hunger increase rate during the day 
    		if (BS == "D") {
    			if (Ins.t > 0) {
    			if (Food_thresh > 0) {Prob.eat = 1} else Prob.eat = 0.25 # Prob to eat if it is day, some food is available and you are not hungry: 25%
    			} else Prob.eat = 0
    			EatYN = rbinom(1, 1, Prob.eat)
    			if (EatYN == 0) {
    			Eat.no = 0
    			Food_thresh = Food_thresh + HungerIncreaseDay[i]
    		}	
    		} 
    		
    		# For both day and night: if you eat, the number of insects eaten is given by insect density at current location * eating success rate, and can be max 20. Eating success rate is a random draw from a normal distrubution with mean=0.20 and SD=0.05, and is min 0.01. Food threshold 1- decreases by the number of insects eaten; 2- increases by energy use during that 1 hour timestep (HungerIncrease during the day or at night, as appropriate). Food-scape is depleted by the number of insects eaten
			if (EatYN == 1) {
    		Eat.success = max(rnorm(1, AvEatSuccRate[i], 0.05), 0.01)
    		Eat.no = min(Eat.success * Ins.t, 20) # Max possible n of insects eaten per day: 20
    		cell = cellFromXY(Food, cbind(Loc[1],Loc[2]))  
    		Food[cell] = extract(Food, cell) - Eat.no

    		if (BS == "N") {
    		Food_thresh = Food_thresh - Eat.no + HungerIncreaseNight[i] # Change in food threshold accounts for 1- n insects eaten, 2- energy use during that 1 hour timestep
    		} 
    		if (BS == "D") {
    		Food_thresh = Food_thresh - Eat.no + HungerIncreaseDay[i]	
    		}

    		}
    		
    		
    		## save data for current location in table 
   			Bird[count,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)] = c(i, Hour.no, BS, Loc[1], Loc[2], Water_thresh, Food_thresh, Water.t, Ins.t, Habitat.t, MoveYN, DrinkYN, EatYN, Eat.no, count)
    
    		## plot current location colored by rep ID 
    		cols.r = rainbow(Reps)	
    		cols = cols.r[i]
    		points(Bird$Lon[count], Bird$Lat[count], pch=21, cex=1, lwd=0.1, bg=cols, col="black", type='b')
    		
    		## set hour n to the next timestep 
    		Hour.no = Hour.no + 1
			
		} 
	
	print(paste("Bird", i, "done"))
	print(proc.time() - bird.t)	
	}
	
	return(Bird) 
	}

	
