
############################
####### HO CONSTANTS ####### 
############################

## used by HO driver and/or HO model functions


# Molecular weight of water [g mol^-1]
MwH2O = 18

# rH for each macronutrient: rH = mol H2 produced / mol O2 consumed [mol H2 mol O2^-1]
# Note that H2 produced = H in macronutrient / 2
rHcarb = 6 / 6
rHprot = if (waste_type == "urea") {7 / 6} else {28 / 21}
rHfat = 16 / 23

# Ms for each macronutrient [g mol O2^-1]
Mscarb = 180 / 6
Msprot = if (waste_type == "urea") {178 / 6} else {178 / 21}
Msfat = 256 / 23

# rH for waste: rHwaste = mol H2 in waste / mol O2 consumed [mol H2 mol O2^-1]
rHwaste = if (waste_type == "urea") {2 / 6} else {4 / 21}

# rO for each macronutrient: ro = O produced / O2 consumed [mol O mol O2^-1]
# Note that O produced = O in macronutrient 
rOcarb = 6 / 6 
rOprot = if (waste_type =="urea") {4 / 6} else {16 / 21}
rOfat = 2 / 23

# rO for waste: rOwaste = mol O in waste / mol O2 consumed [mol O mol O2^-1]
rOwaste = if (waste_type == "urea") {1 / 6} else {6 / 21}


#### (3) CALCULATIONS from (1) and (2) #### 

# Average rH for combined macronutrients [mol H2 mol O2^-1]
rH = rHcarb*Pcarb + rHprot*Pprot + rHfat*Pfat

# Average Ms for combined macronutrients [g mol O2^-1]
Ms = Mscarb*Pcarb + Msprot*Pprot + Msfat*Pfat

# Average rO for combined macronutrients [mol O mol O2^-1]
rO = rOcarb*Pcarb + rOprot*Pprot + rOfat*Pfat

# Average respiratory quotient for combined macronutrients [mol CO2 mol O2^-1]
Rq = Rqcarb*Pcarb + Rqprot*Pprot + Rqfat*Pfat


