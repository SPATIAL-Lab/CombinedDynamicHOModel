
##### Magozzi and Bowen (2020)



### large_data: external input

utahcounty: shape file of Utah counties 

DEM.tif: digital elevation model

RBCclass_na.grd: spatial distribution of veg type class within RBC 

Habitat2.grd, FoodM.grd, Water.grd: area files of habitat type class, prey density and water presence in RBC

streamCoords.csv, substrateCoords.csv, netCoords.csv: coordinates of streamwater and prey samples and mist-net locations in RBC 

env_water-data.csv, water-data.csv, HO isotope data_substrates_2.xlsx: HO isotope data for streamwater, prey water and prey biomass 

feather_raw_data_test.xlsx: raw HO data for hatch-year spotted towhee feathers 

raw_stds.xlsx: HO data for lab standards

Fdata.csv: feather data recalibrated to traditional standards



### input_output: intermediate product

## exp folders: e.g., B1; each contains output from AB model: e.g., B1.csv and B1Parms.csv; and output from phys model: e.g., B1_HO_1, B1_HO_2, …, B1_HO_105

## tables; summary tables for each exp, all B and all E exps, E1B1 exps, and all exps and towhee data in the case of random locations



### figures: contains figures in the paper saved as pdfs and tiffs 



### github 

## AB_model

AB_experiments.R, AB_experiments_rl.R: code to run exps with the AB model; the former assumes equal number of birds nesting at each location, the latter random locations

AB_constants.R: AB model constants

AB_function.R, AB_model_rl.R: AB model function; the former assumes equal number of birds nesting at each location, the latter random locations 

## phys_model

HO_experiments.R, HO_experiments_rl.R: code to run the phys/food model; the former assumes equal number of birds nesting at each location, the latter random locations

HO_parameters.R: phys/food model parameters

HO_constants.R: phys/food model parameters

HO_data.R: HO isotope data for streamwater, prey water and prey biomass to be used by the phys/food model 

HO_functions.R, HO_functions_rl.R: phys/food model functions; the former assumes equal number of birds nesting at each location, the latter random locations 

HO_tables.R, HO_tables_rl.HO: code to create summary tables for each exp, all B and all E exps, E1B1 exps, and all exps and towhee data in the case of random locations


## data_analysis

Fdata.R: code to recalibrate feather HO isotope data to traditional standards

data_analysis.R: code to make the figures  


 