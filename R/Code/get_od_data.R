get_od_data <- function(state_name, year, main = T, JT_type = "01") {

  # Stage One - Getting OD Data from LODES
  if(main == T) {
    dl_file <- paste0(state_name, "_od_main_JT", JT_type, "_", year, ".csv.gz")
    download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/od/", dl_file), dl_file)
    R.utils::gunzip(dl_file)
    temp <- data.table::fread(paste0(state_name, "_od_main_JT", JT_type, "_", year, ".csv"))
    rm(dl_file)
    file.remove(paste0(state_name, "_od_main_JT", JT_type, "_", year, ".csv"))
  } else {
    dl_file <- paste0(state_name, "_od_aux_JT", JT_type, "_", year, ".csv.gz")
    download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/od/", dl_file), dl_file)
    R.utils::gunzip(dl_file)
    temp <- data.table::fread(paste0(state_name, "_od_aux_JT", JT_type, "_", year, ".csv"))
    rm(dl_file)
    file.remove(paste0(state_name, "_od_aux_JT", JT_type, "_", year, ".csv"))
  }

  # Stage Two - Cleaning the OD data
  temp <- temp[, c("w_geocode", "h_geocode", "S000", paste0("SA0", 1:3), paste0("SE0", 1:3), paste0("SI0", 1:3))]
  temp$w_geocode <- stringr::str_pad(temp$w_geocode, width = 15, side = 'left', pad = 0)
  temp$h_geocode <- stringr::str_pad(temp$h_geocode, width = 15, side = 'left', pad = 0)
  colnames(temp) <- c("Workplace_Census_Block_Code", "Residence_Census_Block_Code", "Total_Job_Flows",
                      "No_29younger", "No_30_54", "No_55older", "No_1250", "No_1251_3333", "No_3333", "No_Goods",
                      "No_Trade_Transport_Util", "No_Services")
  temp <- transform(temp, W_StateID = substr(Workplace_Census_Block_Code, 1, 2), W_CountyID = substr(Workplace_Census_Block_Code, 3, 5),
                    W_TractID = substr(Workplace_Census_Block_Code, 6, 11), W_BlockGroupID = substr(Workplace_Census_Block_Code, 12, 12),
                    W_CensusBlockID = substr(Workplace_Census_Block_Code, 13, 15), Workplace_Census_Block_Code = Workplace_Census_Block_Code)
  temp <- transform(temp, R_StateID = substr(Residence_Census_Block_Code, 1, 2), R_CountyID = substr(Residence_Census_Block_Code, 3, 5),
                    R_TractID = substr(Residence_Census_Block_Code, 6, 11), R_BlockGroupID = substr(Residence_Census_Block_Code, 12, 12),
                    R_CensusBlockID = substr(Residence_Census_Block_Code, 13, 15), Residence_Census_Block_Code = Residence_Census_Block_Code)
  temp <- temp[, c("Workplace_Census_Block_Code", "W_StateID", "W_CountyID", "W_TractID", "W_BlockGroupID", "W_CensusBlockID", "Total_Job_Flows",
                   "No_29younger", "No_30_54", "No_55older", "No_1250", "No_1251_3333", "No_3333", "No_Goods",
                   "No_Trade_Transport_Util", "No_Services", 
                   "Residence_Census_Block_Code", "R_StateID", "R_CountyID", "R_TractID", "R_BlockGroupID", "R_CensusBlockID")]

  # Stage Three - Downloading the Lookup
  dl_file_lookup <- paste0(state_name, "_xwalk.csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/", dl_file_lookup), dl_file_lookup)
  R.utils::gunzip(dl_file_lookup)
  temp_lookup <- data.table::fread(paste0(state_name, "_xwalk.csv"))
  file.remove(paste0(state_name, "_xwalk.csv"))

  # Stage Four - Cleaning the Lookup
  temp_lookup <- temp_lookup[, c("tabblk2010", "st", "stname", "cty", "ctyname", "trct", "trctname", "bgrp","bgrpname")]
  temp_lookup$tabblk2010 <- stringr::str_pad(temp_lookup$tabblk2010, width = 15, side = 'left', pad = 0)
  temp_lookup <- transform(temp_lookup, StateID = substr(tabblk2010, 1, 2), CountyID = substr(tabblk2010, 3, 5),
                           TractID = substr(tabblk2010, 6, 11), BlockGroupID = substr(tabblk2010, 12, 12),
                           CensusBlockID = substr(tabblk2010, 13, 15))
  temp_lookup <- temp_lookup[, c("tabblk2010", "StateID", "stname", "CountyID", "ctyname",
                                 "TractID", "trctname", "BlockGroupID", "bgrpname")]
  colnames(temp_lookup) <- c("Census_Block_Code", "StateID", "StateName", "CountyID", "CountyName",
                             "TractID", "TractName", "BlockGroupID", "BlockGroupName")

  # Stage Five - Joining the Lookup to the OD Data
  temp[, ] <- lapply(temp[, ], as.character)
  temp_lookup[, ] <- lapply(temp_lookup[, ], as.character)
  merge1 <- merge(temp, temp_lookup, by.x = c("Workplace_Census_Block_Code", "W_StateID", "W_CountyID", "W_TractID", "W_BlockGroupID"),
                  by.y = c("Census_Block_Code", "StateID", "CountyID", "TractID", "BlockGroupID"), all.x = TRUE)
  colnames(merge1)[(ncol(merge1)-3):ncol(merge1)] <- c("W_StateName", "W_CountyName", "W_TractName", "W_BlockGroupName")
  merge2 <- merge(merge1, temp_lookup, by.x = c("Residence_Census_Block_Code", "R_StateID", "R_CountyID", "R_TractID", "R_BlockGroupID"),
                  by.y = c("Census_Block_Code", "StateID", "CountyID", "TractID", "BlockGroupID"), all.x = TRUE)
  colnames(merge2)[(ncol(merge2)-3):ncol(merge2)] <- c("R_StateName", "R_CountyName", "R_TractName", "R_BlockGroupName")
  merge2$W_StateAbb <- state_name
  df <- merge2[, c("Workplace_Census_Block_Code", "W_StateID", "W_StateName","W_StateAbb","W_CountyID", "W_CountyName",
                   "W_TractID", "W_TractName", "W_BlockGroupID", "W_BlockGroupName", "W_CensusBlockID",
                   "Residence_Census_Block_Code", "R_StateID", "R_StateName", "R_CountyID", "R_CountyName",
                   "R_TractID", "R_TractName", "R_BlockGroupID", "R_BlockGroupName", "R_CensusBlockID", "Total_Job_Flows", 
                   "No_29younger", "No_30_54", "No_55older", "No_1250", "No_1251_3333", "No_3333", "No_Goods",
                   "No_Trade_Transport_Util", "No_Services")]
  # df = merge2
  df$Total_Job_Flows <- as.numeric(as.character(df$Total_Job_Flows))
  df$No_29younger = as.numeric(as.character(df$No_29younger))
  df$No_30_54 = as.numeric(as.character(df$No_30_54))
  df$No_55older = as.numeric(as.character(df$No_55older))
  df$No_1250 = as.numeric(as.character(df$No_1250))
  df$No_1251_3333 = as.numeric(as.character(df$No_1251_3333))
  df$No_3333 = as.numeric(as.character(df$No_3333))
  df$No_Goods = as.numeric(as.character(df$No_Goods))
  df$No_Trade_Transport_Util = as.numeric(as.character(df$No_Trade_Transport_Util))
  df$No_Services = as.numeric(as.character(df$No_Services))

  return(df)

}