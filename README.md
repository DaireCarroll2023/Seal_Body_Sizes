# Seal_Body_Sizes
Data and R sctipts for measuring harbour seal body sizes and estimating mass based on .shp files containing outlines. Associated with the manusctipt currently titles "Approaching a population level assessment of body size in pinnipeds using drones, an early warning of environmental degradation."

1_Seal_Volume_Function.R: A function for the estimation of length, width, and ellipsoid volume of harbour seals from georeferenced polygons representing individual outlines
2_Polygon_Process.R: This script uses the curved_length_vol function (1_Seal_Volume_Function.R) to process a folder full of .shp file subfolders containing georeferenced polygons representing individual outlines and outputs a .csv with estimates of length, width, and ellipsoid volume for each individual.
3_Calibration.R: This script processes and calibrates summarized harbor seal measurements based on reference to known individuals

Shp_Files.zip: Folder containing .shp file subfolders containing georeferenced polygons representing individual outlines of harbour seals for 2022 and 2021. 2021 data is published in Infantes et al. 2022 (https://doi.org/10.3389/fevo.2022.905309).

CSV_Files: Folder containing data files

  Known_Seals.csv: True measurments of length and mass for known seals with derived estimates of 'true' width and volume. Drone based estimates of length, width, simple and   complex volume for the same individuals, information on pose.
 
  measurments.csv: Drone based estimates of length, width, simple and   complex volume for all individuals.
  
  Pup_growth.csv: Data on pup mass by age from Harding et al. 2005 (https://doi.org/10.1111/j.0269-8463.2005.00945.x).
  
  Slottsskogen_Data.csv: True and drone based measurments for individual captive harbour seals taken on two seperate occasions.
  
  Summarised_Weights.csv: True measurments of length, girth, and mass for harbour seals.


