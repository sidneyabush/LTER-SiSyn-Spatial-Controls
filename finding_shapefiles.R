# Clear environment
rm(list = ls())

librarian::shelf(dplyr, googledrive, stringr)

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


# Read the CSV lists of file names
slope_file_list <- read.csv("na_slope_shapefiles.csv", stringsAsFactors = FALSE)
all_file_list <- read.csv("all_shapefiles.csv", stringsAsFactors = FALSE)

# Define local destination folders
slope_folder <- "basin_slope_shapefiles"
all_folder <- "all_shapefiles"

# Ensure folders exist locally; create them if they don't
dir.create(slope_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(all_folder, showWarnings = FALSE, recursive = TRUE)

# Google Drive folder ID
folder_id <- as_id("1TLEFKLWUpTKwxKiZv9nqhdwccflPuF9g")
files_in_folder <- drive_ls(folder_id)

# Remove file extensions from Google Drive file names for comparison
files_in_folder <- files_in_folder %>%
  mutate(base_name = str_remove(name, "\\.[^.]*$"))  # Remove the extension

# Filter and download files in the slope list
slope_files <- files_in_folder %>%
  filter(base_name %in% slope_file_list$Shapefile_Name)

for (i in 1:nrow(slope_files)) {
  drive_download(as_id(slope_files$id[i]), 
                 path = file.path(slope_folder, slope_files$name[i]), 
                 overwrite = TRUE)
}

# Filter and download files in the all list
all_files <- files_in_folder %>%
  filter(base_name %in% all_file_list$Shapefile_Name)

for (i in 1:nrow(all_files)) {
  drive_download(as_id(all_files$id[i]), 
                 path = file.path(all_folder, all_files$name[i]), 
                 overwrite = TRUE)
}