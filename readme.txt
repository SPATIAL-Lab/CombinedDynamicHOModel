
##### Magozzi and Bowen (2020) #####

Code for the combined dynamic model by Magozzi et al. (in review) to predict local among-individual HO isotopic variance.
Data files needed to run the scripts can be found at: https://www.dropbox.com/sh/z57sa9ezengplpf/AADKI2F46K1fM-ksjLyL5DxEa?dl=0 or in the Zenodo archive for this project: https://doi.org/10.5281/zenodo.3673949

Users should download the content of the github repository to their local machine and place in a directory. They should then download the data folder into the same directory that has the 'code' folder.


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

