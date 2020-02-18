
##### Magozzi and Bowen (2020) #####

Code for the combined dynamic model by Magozzi et al. In review to predict local among-individual HO isotopic variance.
Data files needed to run the scripts can be found at: https://www.dropbox.com/sh/z57sa9ezengplpf/AADKI2F46K1fM-ksjLyL5DxEa?dl=0

Users should download the content of the github repository to their local machine and place in a directory. They should then download the data folder into the same directory that has the 'code' folder. Prior to run the scripts they should create the 'ouput' and 'figures' folders (and subfolders) where intermediate & final outputs and figures will be saved, respectively.    

Simulated data used in the paper can be found at: https://www.dropbox.com/sh/u1chiwsr8hvurxb/AAD-ovOZhdHYHeBKnntbZOZma?dl=0 



#### code ####


### AB_model ###

AB_experiments.R, AB_experiments_rl.R: code to run exps with the AB model; the former assumes equal number of birds nesting at each location, the latter random nesting locations

AB_constants.R: AB model constants. It is sourced by AB_experiments.R, AB_experiments_rl.R 

AB_function.R, AB_function_rl.R: AB model function; the former assumes equal number of birds nesting at each location, the latter random locations. They are sourced by AB_experiments.R, AB_experiments_rl.R, respectively


### phys_model ### 

HO_experiments.R, HO_experiments_rl.R: code to run the phys/food model; the former assumes equal number of birds nesting at each location, the latter random locations

HO_parameters.R: phys/food model parameters. It is sourced by HO_experiments.R, HO_experiments_rl.R

HO_constants.R: phys/food model parameters. It is sourced by HO_experiments.R, HO_experiments_rl.R

HO_data.R: code to read in HO isotope data for streamwater, prey water and prey biomass used by the phys/food model. It is sourced by HO_experiments.R, HO_experiments_rl.R 

HO_functions.R, HO_functions_rl.R: phys/food model functions; the former assumes equal number of birds nesting at each location, the latter random locations. They are sourced by HO_experiments.R, HO_experiments_rl.R, respectively

HO_tables.R, HO_tables_rl.HO: code to create summary tables for each exp, B, E and E1B1 exps for both tests with equal number of birds nesting in each habitat type, and for B, E, E1B1 and feather HO data from hatch-year Spotted Towhee for the test with random location


### data_analysis ###

Fdata.R: code to recalibrate feather HO isotope data to traditional standards

data_analysis.R: code to produce figures



#### data ####

Folder with external input (anything not created using the scripts)

utahcounty: shape file of Utah counties 

DEM.tif: digital elevation model

RBCclass_na.grd: spatial distribution of vegetation type class within Red Butte Canyon (RBC) 

Habitat2.grd, FoodM.grd, Water.grd: area files of habitat type class, prey density and water presence in RBC

streamCoords.csv, substrateCoords.csv, netCoords.csv: coordinates of streamwater and prey samples and mist-net locations in RBC 

env_water-data.csv, water-data.csv, HO isotope data_substrates_2.xlsx: HO isotope data for streamwater, prey water and prey biomass 

feather_raw_data_test.xlsx: raw HO data for feathers from hatch-year Spotted Towhees in RBC

raw_stds.xlsx: HO data for lab standards



#### output ####

Folder with intermediate & final ouput (anything created using the scripts)


### B1, B2A, ..., E1, E2A, ..., E1B1 ### 


Each folder contain output from AB model for all reps (e.g., B1Parms.csv, B1.csv) and phys model for each rep (e.g., B1_HO_1.csv, ..., B1_HO_n.csv)) for the test with equal number of birds nesting in each habitat type

### B1_rl, B2A_rl, ..., E1_rl, E2A_rl, ..., E1B1_rl ### 

Each folder contain output from AB model for all reps (e.g., B1_rlParms.csv, B1_rl.csv) and phys model for each rep (e.g., B1_rl_HO_1.csv, ..., B1_rl_HO_n.csv)) for the test with random locations


### tables ###

Fdata.csv: feather HO data recalibrated to traditional standards using the script "code/data_analysis/Fdata.R"

meansIndB1.csv, meansIndB2A.csv, ..., meansIndE1.csv, meansIndE2A.csv, meansIndE1B1.csv: summary tables with individual average variable values for all reps from each exp with equal number of birds nesting in each habitat type

meansIndB.csv, meansIndE.csv, meansIndE1B1.csv: summary tables with individual average variable values for all reps from all B, E, and E1B1 exps with equal number of birds nesting in each habitat type


meansIndB1_rl.csv, meansIndB2A_rl.csv, ..., meansIndE1_rl.csv, meansIndE2A_rl.csv, meansIndE1B1_rl.csv: summary tables with individual average variable values for all reps from each exp with random locations

meansIndB_rl.csv, meansIndE_rl.csv, meansIndE1B1_rl.csv: summary tables with individual average variable values for all reps from all B, E, and E1B1 exps with random locations

meansInd.data_rl.csv: summary table with individual average variable values for all reps from all B, E, and E1B1 exps with random locations and recalibrated feather HO data for hatch-year Spotted Towhees in RBC



#### figures ####

Folder with figures in the paper (pdf and tiff)
 
 
 