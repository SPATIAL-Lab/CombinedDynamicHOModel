
############################
####### HO PARAMETERS ###### 
############################

## used by phys/food model (HO) functions


# Functions to convert delta values into R values and vice versa # constants but needed here to convert d18Oo2 into ROo2 
RstandardH = 0.00015575
RstandardO = 0.0020052
delta.to.R = function(delta, Rstandard) {
  R.value = ((1000 + delta)/1000) * Rstandard
  return(R.value)
}
R.to.delta = function(Rsample, Rstandard) {
  delta.value = (Rsample/Rstandard - 1) * 1000
  return(delta.value)
} 

# Waste type; can be "urea" or "uric acid"
waste_type = "uric acid"

# Proportions of macronutrients in the diet 
Pcarb = 0.57
Pprot = 0.33
Pfat = 1 - Pcarb - Pprot

# Body mass [g]
M = 40	

# Body temperature [degrees C]
bTc = 37
# Body temperature [K]
bTk = bTc + 273.15 # calc from 1&2 but needed here to calc BMR 

# Proportion of food mass that is in liquid water form 
Pw = 0.6

# Constants for the calculation of the basal metabolic rate [W = J s^-1]
# Normalization constant for endotherms [W (g^(3 / 4))^-1]
b0 = 2.94 * 10^8
# Elevation coefficient 
elc = 0.71
# Activation energy [eV]
E = 0.63
# Boltzmann's constant [eV K^-1]
k = 8.62 * 10^-5 # constant but needed here to calc BMR


# Constants for the conversion of the basal metabolic rate into field metabolic rate [mol O2 d^-1]
# Mean ratio of field metabolic rate to basal metabolic rate
FMR_BMR = 2.91

# Constants for the calculation of the flux of vapor water out [mol H2O d^-1]
# Coefficients for the allometric relationship between body mass [g] and evaporative water loss [mol H2O d^-1] - fit to Altman & Dittmer (1968) and Crawford & Lasiewski (1986) data (datafile: EWL_mammals_birds_Altman_Crawford.csv)
aEWL = 0.009
bEWL = 0.80

# Amount of energy provided by each prey [(mol O2 d^-1) / (Prey d^-1)] = [mol O2 Prey^-1]
# O2 per prey adjusted to FMR multiplied by 2
BMR = b0 * M^elc * exp(-E/(k*bTk)) # Basal metabolic rate based as a function of body size and temperature [W = J s^-1] # calc from 1&2 but needed here to calc O2 per prey

## constants but needed here to calc CF ##
Rqcarb = 6 / 6
Rqprot = if (waste_type == "urea") {5 / 6} else {14 / 21} 
Rqfat = 16 / 23
Entcarb = 467.1
Entprot = 432.0 
Entfat = 436.5
CFcarb = Rqcarb / Entcarb
CFprot = Rqprot / Entprot
CFfat = Rqfat / Entfat

CF = CFcarb*Pcarb + CFprot*Pprot + CFfat*Pfat # calc from 1&2 but needed here to calc BMR_O2

BMR_O2 = BMR * (60*60*24/1000) * CF # Basal metabolic rate expressed as [mol O2 d^-1] # calc from 1&2 but needed here to calc O2 per prey 
FMR = (FMR_BMR * BMR_O2) # Field metabolic rate [mol O2 d^-1] # calc from 1&2 but needed here to calc O2 per prey 
FMR = FMR *2 #multiply by 2 to rescale to data for songbirds


# calcs needed here to calc O2 per prey 
FMR_hour = FMR / 24
Day = c(6:18)
Night = c(1,2,3,4,5,19,20,21,22,23,24) 
FMRavg = FMR_hour*length(Day) + (FMR_hour/5)*length(Night)
TargetPrey.no_day = 5*length(Day) + 1*length(Night)

O2_per_prey = FMRavg / TargetPrey.no_day

# Proportion of body mass that is fat (reserves)
p_reserves = 0.15

# Amount of energy provided by each fat (triacylglycerol) molecule (reserve unit) [kJ g^-1]
energy_per_reserve_unit = 38

# Proportion of body mass that is water representing minimum preferred threshold
pTBW = 0.68 

# H isotope fractionation associated with evaporative water loss
alphaHbw_vw = 0.937

# O isotope composition of atmospheric O2 [per mil]
d18Oo2 = 23.5
ROo2 = delta.to.R(d18Oo2, RstandardO)

# O isotope fractionation associated with the absorption of O2 in the lungs
alphaOatm_abs = 0.992
      
# O isotope fractionation associated with evaporative water loss  
alphaObw_vw = 0.981 # value used in both Schoeller et al. and Kohn papers

# O isotope fractionation associated with the exhalation of CO2
alphaObw_CO2 = 1.038

# Proportions of carbohydrate and protein in a defatted prey sample in the lab  
PLEpcarb = 0.50
PLEpprot = 0.50

# H isotopic offset between dietary (prey) carbohydrate and protein 
offHpcarb_pprot = 40

# H isotopic offset between dietary (prey) protein and lipids 
offHpprot_pfat = 53.42365

# O isotopic offset between dietary (prey) carbohydrate and protein 
offOpcarb_pprot = 8.063528

# O isotopic offset between dietary (prey) protein and lipids 
offOpprot_pfat = 6

# Proportion of keratin H routed from dietary protein 
PfH = 0.60

# H isotope fractionation associated with the synthesis of keratin protein
alphaHprot = 1.002

# Proportion of keratin O routed from dietary protein 
PfO = 0.19

# Proportion of follicle water derived from body water
Pbw = 0.81

# O isotope fractionation associated with carbonyl O-water interaction [per mil] - Tuned after accounting for O routing
epsOc_w = 10.8
alphaOc_w = (epsOc_w + 1000) / 1000

# Proportion of gut water derived from body water - Tuned after accounting for O routing
g1 = 0.56 

# Proportion of gut water derived from drinking water - Tuned after accounting for O routing
g2 = 0.09


