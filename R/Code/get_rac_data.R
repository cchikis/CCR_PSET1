get_rac_data = function (state_name, year, JT_type) 
{
  dl_file <- paste0(state_name, "_rac_S000_JT", JT_type, "_", year, ".csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", 
    state_name, "/rac/", dl_file), dl_file)
  R.utils::gunzip(dl_file)
  temp <- data.table::fread(paste0(state_name, "_rac_S000_JT", JT_type, "_", 
    year, ".csv"))
  rm(dl_file)
  file.remove(paste0(state_name, "_rac_S000_JT", JT_type, "_", year, 
    ".csv"))
  temp <- subset(temp, select = -c(CS01, CS02))
  colnames(temp) <- c("Census_Block_Code", "Total_Jobs", "Jobs_Under29", 
    "Jobs_30to54", "Jobs_Over55", "Low_Income_Jobs", "Middle_Income_Jobs", 
    "High_Income_Jobs", "Agr_For_Fish_Hunt", "Mine_Quar_Oil_Gas", 
    "Utilities", "Construction", "Manufacturing", "Whole_Trade", 
    "Retail_Trade", "Transport_Warehouse", "Information", 
    "Finance_Insurance", "Real_Estate", "Prof_Scie_Tech", 
    "Management", "Waste_Admin", "Education", "Health_Social_Care", 
    "Arts_Recr_Enter", "Accom_Food", "Other_Services", "Public_Admin", 
    "Jobs_White", "Jobs_Black_AfroAmer", "Jobs_Amer_Indi_Native", 
    "Jobs_Asian", "Jobs_Hawaii", "Jobs_2_Race", "Jobs_Not_Hispanic", 
    "Jobs_Hispanic", "Jobs_Low_Education", "Jobs_School_Ed", 
    "Jobs_College", "Jobs_Degree", "Data_Date")
  temp[, ] <- lapply(temp[, ], as.character)
  temp$Census_Block_Code <- stringr::str_pad(temp$Census_Block_Code, 
    width = 15, side = "left", pad = 0)
  temp <- transform(temp, StateID = substr(Census_Block_Code, 
    1, 2), CountyID = substr(Census_Block_Code, 3, 5), TractID = substr(Census_Block_Code, 
    6, 11), BlockGroupID = substr(Census_Block_Code, 12, 
    12), CensusBlockID = substr(Census_Block_Code, 13, 15), 
    Census_Block_Code = Census_Block_Code)
  temp <- temp[, c(1, 42:46, 2:41)]
  dl_file_lookup <- paste0(state_name, "_xwalk.csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", 
    state_name, "/", dl_file_lookup), dl_file_lookup)
  R.utils::gunzip(dl_file_lookup)
  temp_lookup <- data.table::fread(paste0(state_name, "_xwalk.csv"))
  file.remove(paste0(state_name, "_xwalk.csv"))
  temp_lookup <- temp_lookup[, c("tabblk2010", "st", "stname", 
    "cty", "ctyname", "trct", "trctname", "bgrp", "bgrpname")]
  temp_lookup[, ] <- lapply(temp_lookup[, ], as.character)
  temp_lookup$tabblk2010 <- stringr::str_pad(temp_lookup$tabblk2010, 
    width = 15, side = "left", pad = 0)
  temp_lookup <- transform(temp_lookup, StateID = substr(tabblk2010, 
    1, 2), CountyID = substr(tabblk2010, 3, 5), TractID = substr(tabblk2010, 
    6, 11), BlockGroupID = substr(tabblk2010, 12, 12), CensusBlockID = substr(tabblk2010, 
    13, 15))
  temp_lookup <- temp_lookup[, c("tabblk2010", "StateID", 
    "stname", "CountyID", "ctyname", "TractID", "trctname", 
    "BlockGroupID", "bgrpname")]
  colnames(temp_lookup) <- c("Census_Block_Code", "StateID", 
    "StateName", "CountyID", "CountyName", "TractID", "TractName", 
    "BlockGroupID", "BlockGroupName")
  df <- merge(temp, temp_lookup, by = "Census_Block_Code", 
    all.x = TRUE)
  df <- subset(df, select = -c(StateID.y, CountyID.y, TractID.y, 
    BlockGroupID.y))
  df <- df[, c(1:2, 47, 3, 48, 4, 49, 5, 50, 6:46)]
  colnames(df)[1:10] <- c("Census_Block_Code", "StateID", 
    "StateName", "CountyID", "CountyName", "TractID", "TractName", 
    "BlockGroupID", "BlockGroupName", "CensusBlockID")
  cols <- df[, 11:50]
  cols <- dplyr::mutate_all(cols, as.numeric)
  geog <- df[, 1:10]
  db <- cbind(geog, cols)
  db$StateAbb <- state_name
  db <- db[, c(1:3, 51, 4:50)]
  return(db)
}
