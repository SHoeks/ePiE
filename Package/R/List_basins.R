

List_basins <- function(folder_all_basins){
 # basins<-list.files(paste0("../Basin data"),pattern = "^[B]")
  # basins<-list.files(paste0("/vol/milkunarc/roldenka/iPiE_archive/EEF/Basin data"),pattern = "^[B]")
  basins<-list.files(folder_all_basins,pattern = "^[B]")
  return(data.frame(Basin_number=seq(1,length(substr(basins, 7, 100))),Basin_IDs=substr(basins, 7, 100)))
}
