#Daire Carroll, Gothenburg University, 2023, carrolldaire@gmail.com
#2: Approaching a population level assessment of body size in pinnipeds using drones, an early warning of environmental degradation.
#a pipeline based on the curved_length_vol function (1_Seal_Volume_Function.R) to process a folder full of shape file subfolders

########################################

my.dir = paste("C:/Users/daire/Desktop/mosaics/Shp_Files - Copy") #here paste your file location eg: ~/Desktop/Drones/All_Shape_files

setwd(my.dir)
files = dir()

Overwrite = FALSE		#should the .shp file be overwritten with new file with polygon dimensions appended
Save_CSV = TRUE		#should a .csv file be saved in the directory containing dimension and lat/lon data
plt1 = FALSE #Do you want to see each polt? Slows the process significantly, reccomend viewing a subset

processed_data = matrix(ncol = 8)

for(i in 1:length(files)){ 
  
  folder = files[i]
  setwd(paste(my.dir,"/",folder,sep = ""))
  
  print(folder)
  
  files2 = dir()
  
  for(k in 1:length(files2 )){
    if(endsWith(files2[k],".shp")==TRUE){
      shape =  files2[k]
    }
  }
  
  print(shape)
  
  seals = st_read(shape)
  seals = st_transform(seals,23032)
  
  seals_dimensions = matrix(ncol = 7, nrow = length(st_geometry(seals)))
  seals_dimensions[,1] = c(1:length(st_geometry(seals)))
  
  for(j in 1:length(st_geometry(seals))){
    pol1 = seals[j,]
    lens = curved_length_vol(pol1, plt = plt1)
    #lens[1] = 0.81*lens[1] + 0.16 #apply transformation
    #lens[2] = 0.91*lens[1] + 0.06 #apply transformation  
    seals_dimensions[j,2:ncol(seals_dimensions)] = lens
  }

  joined = cbind(seals,seals_dimensions[,1],seals_dimensions[,2],seals_dimensions[,3],seals_dimensions[,4])
  
  names(joined)[names(joined) == "seals_dimensions...1."] = "Length_A"
  names(joined)[names(joined) == "seals_dimensions...2."] = "Width_A"
  names(joined)[names(joined) == "seals_dimensions...3."] = "Cylender_vol"
  names(joined)[names(joined) == "seals_dimensions...4."] = "Elipsoid_vol"

  if(Overwrite == TRUE){
    st_write(joined, shape, delete_layer = TRUE)
  }
  
  seals_dimensions = cbind(seals_dimensions,rep(folder,length(seals_dimensions[,1])))

  setwd(my.dir)

  processed_data = rbind(processed_data, seals_dimensions)
   
}

if(Save_CSV == TRUE){
  write.csv(processed_data, "measurments.csv")
}
