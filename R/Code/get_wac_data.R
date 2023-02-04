get_wac_data = function (state_name, year, JT_type) 
{
  dl_file <- paste0(state_name, "_wac_S000_JT", JT_type, "_", year, ".csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", 
    state_name, "/wac/", dl_file), dl_file)
  R.utils::gunzip(dl_file)
  temp <- data.table::fread(paste0(state_name, "_wac_S000_JT", JT_type, "_", 
    year, ".csv"))
  rm(dl_file)
  file.remove(paste0(state_name, "_wac_S000_JT", JT_type, "_", year, 
    ".csv"))
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
    "Jobs_College", "Jobs_Degree", "Male_Jobs", "Female_Jobs", 
    "Firm_Age_0_1", "Firm_Age_2_3", "Firm_Age_4_5", "Firm_Age_6_10", 
    "Firm_Age_11_Plus", "Firm_Size_0_19", "Firm_Size_20-49", 
    "Firm_Size_50_249", "Firm_Size_250_499", "Firm_Size_500_Plus", 
    "Data_Date")
  temp[, ] <- lapply(temp[, ], as.character)
  temp$Census_Block_Code <- stringr::str_pad(temp$Census_Block_Code, 
    width = 15, side = "left", pad = 0)
  temp <- transform(temp, StateID = substr(Census_Block_Code, 
    1, 2), CountyID = substr(Census_Block_Code, 3, 5), TractID = substr(Census_Block_Code, 
    6, 11), BlockGroupID = substr(Census_Block_Code, 12, 
    12), CensusBlockID = substr(Census_Block_Code, 13, 15), 
    Census_Block_Code = Census_Block_Code)
  temp <- temp[, c(1, 54:58, 2:53)]
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
  temp_merge <- merge(temp, temp_lookup, by = "Census_Block_Code", 
    all.x = TRUE)
  temp_merge <- temp_merge[, c(1:2, 60, 3, 62, 4, 64, 5, 66, 
    6:58)]
  colnames(temp_merge)[2:9] <- c("StateID", "StateName", "CountyID", 
    "CountyName", "TractID", "TractName", "BlockGroupID", 
    "BlockGroupName")
  temp_merge$StateAbb <- state_name
  temp_merge <- temp_merge[, c(1:3, 63, 4:62)]
  cols <- temp_merge[, 12:63]
  cols <- dplyr::mutate_all(cols, as.numeric)
  geog <- temp_merge[, 1:11]
  db <- cbind(geog, cols)
  return(db)
}
