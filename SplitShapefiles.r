###split shapefiles with multiple watersheds
#install.packages("sf")
require("sf")

#set path where shapefiles are located, have to have .shp, .dbf, .prj, and .shx files in the folder where the .shp file is
#add new path for new group of shapefiles
path<-"/Users/keirajohnson/Box Sync/Keira_Johnson/Shapefiles/ClusteredShp"

path2<-"/Users/keirajohnson/Box Sync/Keira_Johnson/Shapefiles"

path3<-"/Users/keirajohnson/Downloads/Export"

path4<-"/Users/keirajohnson/Box Sync/Keira_Johnson/Shapefiles/SwedishGovCatchments"

path5<-"/Users/sidneybush/Desktop/AUS_Catchments"

#list all files ending in .shp
#change path
files<-list.files(path = path5, pattern = ".shp")

#select site, only if there are multiple .shp files in your "files" list
site<-sf::st_read(file.path(path5))

#change column NodeID to reflect NodeID in site df
site<-site[complete.cases(site$NodeID),]

#get unique watersheds within each site shapefile, change the NodeID after the"$" to whatever your site file says
watersheds<-unique(site$NodeID)

#this is to remove umlauts and rings
# watersheds_noUmlaut<-stringi::stri_replace_all_fixed(
#   watersheds, 
#   c("ä", "ö", "ü", "Ä", "Ö", "Ü", "å", "Å"), 
#   c("a", "o", "u", "A", "O", "U", "a", "A"), 
#   vectorize_all = FALSE
# )

#change beginning of paste to reflect the prefix of the file
watersheds_NodeID<-paste0("AUS_", watersheds)

#this is where the files will write to
setwd("/Users/sidneybush/Desktop/AUS_Catchments/shapefiles")

#download as ESRI shapefile
#this will write shp, .dbf, .prj, and .shx files
#append = FALSE will overwrite any existing files with the same NodeID
#NodeID is set to the NodeID of the site in the unqiue NodeID column
for (i in 1:length(watersheds)) {
  one_site <- site[site$NodeID == watersheds[i],]
  sf::st_write(one_site, dsn="/Users/sidneybush/Desktop/AUS_Catchments/shapefiles", layer=watersheds_NodeID[i], driver="ESRI Shapefile",
           append = FALSE)
}
