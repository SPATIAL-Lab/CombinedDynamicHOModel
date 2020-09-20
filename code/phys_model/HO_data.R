
############################
########## HO DATA ######### 
############################


## read in HO data for environmental substrates in RBC

library(openxlsx)

## environmental water
HOdata_ew = read.csv("data/env_water-data.csv")

d18Oew.RIP = HOdata_ew$d18O
d2Hew.RIP = HOdata_ew$d2H


## prey water
HOdata_w = read.csv("data/water-data.csv")
HOdata_pw = HOdata_w[which(HOdata_w$Substrate_type == "P Consumer"),]

d2Hpw.RIP = HOdata_pw$d2H[which(HOdata_pw$Habitat == "Riparian")]
d2Hpw.MEAD = HOdata_pw$d2H[which(HOdata_pw$Habitat == "Meadow")]
d2Hpw.SLP = HOdata_pw$d2H[which(HOdata_pw$Habitat == "Slope")]

d18Opw.RIP = HOdata_pw$d18O[which(HOdata_pw$Habitat == "Riparian")]
d18Opw.MEAD = HOdata_pw$d18O[which(HOdata_pw$Habitat == "Meadow")]
d18Opw.SLP = HOdata_pw$d18O[which(HOdata_pw$Habitat == "Slope")]


## prey
HOdata_p = read.xlsx("data/HO isotope data_substrates_2.xlsx", sheet =4)

d2Hp.MEAD = HOdata_p$corrd2H_avg
d18Op.MEAD = HOdata_p$corrd18O_avg


avgd2Hoff_SM = 34.12
avgd2Hoff_MR = 8.84
avgd18Ooff_SM = 3.02
avgd18Ooff_MR = 4.45

