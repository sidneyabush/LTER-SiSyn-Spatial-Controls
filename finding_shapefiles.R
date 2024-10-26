# Clear environment
rm(list = ls())

# Match streams with missing slopes to reference table with shapefile name

ref_table_link <- "https://docs.google.com/spreadsheets/d/11t9YYTzN_T12VAQhHuY5TpVjGS50ymNmKznJK4rKTIU/edit#gid=357814834"
ref_table_folder = drive_get(as_id(ref_table_link))
ref_table <- drive_download(ref_table_folder$drive_resource, overwrite = T)

ref_table <- readxl::read_xlsx("Site_Reference_Table.xlsx")
ref_table$Stream_ID <-paste0(ref_table$LTER, "__", ref_table$Stream_Name)
shapefile_match <-ref_table[,c("Shapefile_Name", "Stream_ID")]

all_sites <- read.csv("Final_Sites.csv")

# Merge the mapped lithologies with drivers_df by the "major_rock" column
all_sites_shapefile_names <- merge(all_sites, shapefile_match, by = "Stream_ID", all.x = TRUE) %>%
  select(Shapefile_Name)  

write.csv(all_sites_shapefile_names, "all_shapefiles.csv", row.names = FALSE)

basin_slopes <- read.csv("streams_with_na_slope.csv")

missing_basin_slopes_shapefile_names <- merge(basin_slopes, shapefile_match, by = "Stream_ID", all.x = TRUE) %>%
  select(Shapefile_Name) 

write.csv(missing_basin_slopes_shapefile_names, "na_slope_shapefiles.csv", row.names = FALSE)



