library(sf)
library(jsonlite)
library(ePiE)

setwd("/Users/osx/Documents/GitHub/ePiE/Inputs/2026_08_01/plotLakes_webapp")
if (!dir.exists("HydroLAKES_polys_v10.gdb")) {
  unzip("HydroLAKES_polys_v10.gdb.zip")
}
lakes = st_read("HydroLAKES_polys_v10.gdb") %>% st_transform(4326)
basins = ePiE::LoadEuropeanBasins()
unique_basin_ids = unique(basins$hl$basin_id) 
dir.create("subsetted_lakes")

basin_id = unique_basin_ids[1]
for (basin_id in unique_basin_ids) {
  basin = basins$hl[basins$hl$basin_id == basin_id, ]
  lakes_subset = lakes[lakes$Hylak_id %in% basin$Hylak_id, ]
  if (nrow(lakes_subset) > 0) {
    output_file = paste0("subsetted_lakes/lakes_", basin_id, ".geojson")
    st_write(lakes_subset, output_file, driver = "GeoJSON", delete_dsn = TRUE)
    cat("Saved subset for basin_id:", basin_id, "to", output_file, "\n")
  } else {
    cat("No lakes found for basin_id:", basin_id, "\n")
  }
}

# geojson to js variables
geojson_files = list.files("subsetted_lakes", pattern = "\\.geojson$", full.names = TRUE)
for (i in seq_along(geojson_files)) {
  file = geojson_files[i]
  lines = readLines(file)
  lines = c("export const LakeData  = ",lines,";")
  writeLines(lines, gsub("\\.geojson$", ".js", file))
  file.remove(file)
}