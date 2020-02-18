
############################
####### FEATHER DATA ####### 
############################

library(gdata)
library(plyr)


## set wd to the master project directory (i.e., the directory that contains the 'code' and 'data' folders)


## rescale fether HO isotope data for spotted towhee to traditional Wassenaar stds based on 'true' values for DS,ORX determined across labs  

HOdata_birds = read.xls("data/feather_raw_data_test.xlsx", sheet = 2, header = T) # this version includes year 
HOdata_birds = HOdata_birds[,-4] 


d2Htrue_std1 = -196.1 #mean d2H value for DS across labs 
d2Htrue_std2 = -48.3 #mean d2H value for ORX across labs
d2Htrue_qc = -116.0 #mean d2H value for POW across labs

d18Otrue_std1 = 4.93 #mean d18O value for DS at Texas A&M
d18Otrue_std2 = 23.45 #mean d18O value for ORX at Texas A&M
d18Otrue_qc = 11.19 #mean d18O value for POW at Texas A&M


raw_stds = read.xls("data/raw_stds.xlsx", sheet = 1, header = T)

d2Hraw_std1 = mean(raw_stds$d2H[raw_stds$stdID=="DS"], na.rm=TRUE) #mean d2H value for all of our DS runs 
d2Hraw_std2 = mean(raw_stds$d2H[raw_stds$stdID=="ORX"], na.rm=TRUE) #mean d2H value for all of our ORX runs
d2Hraw_qc = mean(raw_stds$d2H[raw_stds$stdID=="POW"], na.rm=TRUE) #mean d2H value for all of our POW runs

d2Hraw_std1_sd = sd(raw_stds$d2H[raw_stds$stdID=="DS"], na.rm=TRUE) #SDs for Kragten error prop.
d2Hraw_std2_sd = sd(raw_stds$d2H[raw_stds$stdID=="ORX"], na.rm=TRUE)
d2Hraw_qc_sd = sd(raw_stds$d2H[raw_stds$stdID=="POW"], na.rm=TRUE)

d18Oraw_std1 = mean(raw_stds$d18O[raw_stds$stdID=="DS"], na.rm=TRUE) #mean d18O value for all of our DS runs 
d18Oraw_std2 = mean(raw_stds$d18O[raw_stds$stdID=="ORX"], na.rm=TRUE) #mean d18O value for all of our ORX runs
d18Oraw_qc = mean(raw_stds$d18O[raw_stds$stdID=="POW"], na.rm=TRUE) #mean d18O value for all of our POW runs 


d2Hraw_sample = HOdata_birds$d2H_avg
d18Oraw_sample = HOdata_birds$d18O_avg


d2Htrue_sample = d2Htrue_std1 + ( (d2Hraw_sample - d2Hraw_std1) * ((d2Htrue_std1 - d2Htrue_std2)/(d2Hraw_std1 - d2Hraw_std2))  )
d18Otrue_sample = d18Otrue_std1 + ( (d18Oraw_sample - d18Oraw_std1) * ((d18Otrue_std1 - d18Otrue_std2)/(d18Oraw_std1 - d18Oraw_std2))  )


HOdata_birds$d2H_avg_corr = d2Htrue_sample
HOdata_birds$d18O_avg_corr = d18Otrue_sample

HOdata_birds = HOdata_birds[,-c(2,3)]

write.csv(HOdata_birds, "output/tables/Fdata.csv")

