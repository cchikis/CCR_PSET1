#################################################################
# Filename: modelinversion.R
# Author: Cheng, Chikis, Rizzotti
# Date: 01/23/2023
# Note(s):
#################################################################
rm(list = ls(all.names = TRUE))
gc()

# You might need to change this
# setwd("")

packages = c("RColorBrewer", "tictoc", "nleqslv", "DescTools", "latex2exp", "fixest",
             "Rfast", "sf", "quantmod", "readxl", "cowplot", "tigris", "tidylodes",
             "lubridate", "tidyverse")

new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new.packages) > 0) install.packages(new_packages)


library(RColorBrewer)
library(tictoc)
library(nleqslv)
library(DescTools)
library(latex2exp)
library(fixest)
library(Rfast)
library(sf)
library(quantmod)
library(readxl)
library(cowplot)
library(tigris)
library(tidylodes)
library(lubridate)
library(tidyverse)

# Fix for tidy lodes
source("Code/get_od_data.R")
source("Code/get_wac_data.R")
source("Code/get_rac_data.R")


# Should I try solving the model?
solve_model_bool = FALSE

# Zoning data
chicago_zoning = st_read("Data/zoning_2016_01.shp")

code_cw = tibble(ZONE_TYPE = 1:12,
                 ZONE_TYPE_DESC = c("Business", "Commercial",
                                    "Manufacturing", "Residential",
                                    "Planned Development",
                                    "Planned Manufacturing",
                                    "Downtown Mixed",
                                    "Downtown Core",
                                    "Downtown Residential",
                                    "Downtown Service",
                                    "Transportation",
                                    "Park and Open Space"))

chicago_zoning = left_join(chicago_zoning, code_cw,
                           by = c("ZONE_TYPE")) %>%
                mutate(ZONE_TYPE = as.factor(ZONE_TYPE))

chicago_zoning = chicago_zoning %>%
    mutate(ZONE_TYPE_DESC = case_when(ZONE_TYPE_DESC %in% 
                                       c("Business", "Commercial",
                                         "Manufacturing", 
                                         "Downtown Service",
                                         "Downtown Core",
                                         "Downtown Mixed",
                                         "Planned Manufacturing") ~ 
                                                "Business",
                                      ZONE_TYPE_DESC %in% 
                                       c("Residential",
                                         "Downtown Residential") ~
                                                "Residential",
                                    ZONE_TYPE_DESC %in%
                                        c("Transportation",
                                          "Park and Open Space",
                                          "Planned Development") ~
                                          "Other"))

lon_zoning = map_dbl(map(.x = chicago_zoning$geometry, ~.x[[1]][[1]]),
                     .f = ~colMeans(.x)[1])
lat_zoning = map_dbl(map(.x = chicago_zoning$geometry, ~.x[[1]][[1]]),
                     .f = ~colMeans(.x)[2])

chicago_zoning = chicago_zoning %>%
    mutate(lat_zoning = lat_zoning, 
           lon_zoning = lon_zoning) %>%
    select(geometry, ZONE_TYPE = ZONE_TYPE, SHAPE_AREA = SHAPE_AREA, SHAPE_LEN = SHAPE_LEN,
           lat_zoning, lon_zoning, ZONE_TYPE_DESC) %>%
    arrange(ZONE_TYPE_DESC, lat_zoning, lon_zoning)

p1 = ggplot(data = chicago_zoning) +   
    geom_sf(aes(geometry = geometry, fill = factor(ZONE_TYPE_DESC)), color = NA) 

centroid = read.table("Data/2022_Gaz_tracts_national.txt", header = TRUE) %>%
    as_tibble() %>%
    select(GEOID, lat = INTPTLAT, long = INTPTLONG, land_area_m2 = ALAND, water_area_m2 = AWATER) %>%
    mutate(GEOID = as.character(GEOID),
           GEOID = case_when(str_length(GEOID) == 10 ~ paste0("0", GEOID), 
                             str_length(GEOID) == 11 ~ GEOID))
getSymbols('CPIAUCSL',src='FRED')
CPIAUCSL = as.data.frame(CPIAUCSL) %>%
    rownames_to_column() %>%
    as_tibble() %>%
    dplyr::rename(date = rowname) %>%
    mutate(date = floor_date(lubridate::ymd(date), "month"),
           norm = .$CPIAUCSL[.$date == lubridate::ymd(20150101)],
           CPIAUCSL = CPIAUCSL/norm) %>%
    select(date, CPIAUCSL)


zip_to_census = read_excel("Data/TRACT_ZIP_122019.xlsx") %>%
    group_by(TRACT) %>%
    mutate(sum_RES_RATIO = sum(RES_RATIO)) %>%
    ungroup() %>%
    filter(abs(sum_RES_RATIO - 1) < 1e-10) %>%
    arrange(TRACT)

zillow_zip = read_csv("Data/Zip_zori_sm_sa_month.csv") %>%
    pivot_longer(`2015-03-31`:`2022-12-31`, names_to = "date", values_to = "avg_rent") %>%
    mutate(date = floor_date(lubridate::ymd(date), "month")) %>%
    select(ZIP = RegionName, date, avg_rent) %>%
    inner_join(CPIAUCSL, by = "date") %>%
    mutate(rent = avg_rent/CPIAUCSL) 

zillow_zip = inner_join(zillow_zip, zip_to_census, by = c("ZIP")) %>%
    inner_join(centroid, by = c("TRACT" = "GEOID")) 

zillow_zip = zillow_zip %>%
    filter(lubridate::year(date) == 2019) %>%
    filter(!is.na(RES_RATIO) & !is.na(avg_rent)) %>%
    arrange(TRACT, desc(date)) %>%
    distinct(TRACT, .keep_all = TRUE) 

acs2019 = read_csv("Data/ACSDP5Y2019.DP05-Data.csv", skip = 1) %>%
    select(zip = `Geographic Area Name`, housing_units_value = `Estimate!!Total housing units`) %>%
    mutate(zip = trimws(str_remove_all(zip, "ZCTA5")))

zillow_zip = inner_join(zillow_zip, acs2019, by = c("ZIP" = "zip"))
rm(acs2019)

zillow_zip = zillow_zip %>%
    mutate(total_annual_rent = 12*rent*housing_units_value) %>%
    group_by(TRACT) %>%
    mutate(RES_RATIO = RES_RATIO/sum(RES_RATIO, na.rm = TRUE),
           RES_RATIO = ifelse(is.na(RES_RATIO), 1, RES_RATIO)) %>%
    ungroup() %>%
    group_by(TRACT) %>%
    summarise(avg_rent = sum(RES_RATIO * total_annual_rent)) %>%
    ungroup()


chicago_wac = get_wac_data("il", "2019",  JT_type = "01")
chicago_rac = get_rac_data("il", "2019",  JT_type = "01")
chicago_od = get_od_data("il", "2019", main = TRUE, JT_type = "01")

chicago_od = chicago_od %>%
    filter(str_detect(trimws(tolower(W_CountyName)), "cook") & 
           str_detect(trimws(tolower(R_CountyName)), "cook"))

chicago_wac = chicago_wac %>%
    filter(str_detect(trimws(tolower(CountyName)), "cook"))

chicago_rac = chicago_rac %>%
    filter(str_detect(trimws(tolower(CountyName)), "cook"))

chicago_shape = tigris::tracts(state = trimws(toupper(unique(chicago_wac$StateAbb)))) 


chicago_wac = chicago_wac %>%
  pivot_longer(Total_Jobs:Jobs_Degree, names_to = "category", values_to = "value") %>%
  mutate(Census_Block_Code = substr(Census_Block_Code, 1, nchar(Census_Block_Code) - 4)) %>%
  group_by(Census_Block_Code, category) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Census_Block_Code, desc(value)) %>%
  dplyr::rename(GEOID = Census_Block_Code)

chicago_rac = chicago_rac %>%
  pivot_longer(Total_Jobs:Jobs_Degree, names_to = "category", values_to = "value") %>%
  mutate(Census_Block_Code = substr(Census_Block_Code, 1, nchar(Census_Block_Code) - 4)) %>%
  group_by(Census_Block_Code, category) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Census_Block_Code, desc(value)) %>%
  dplyr::rename(GEOID = Census_Block_Code)


chicago_od = chicago_od %>% 
    mutate(Workplace_Census_Block_Code = substr(Workplace_Census_Block_Code, 1, nchar(Workplace_Census_Block_Code) - 4),
           Residence_Census_Block_Code = substr(Residence_Census_Block_Code, 1, nchar(Residence_Census_Block_Code) - 4)) %>%
    pivot_longer(Total_Job_Flows:No_Services, names_to = "category", values_to = "value") %>%
    group_by(Workplace_Census_Block_Code, Residence_Census_Block_Code, category) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Workplace_Census_Block_Code, Residence_Census_Block_Code, desc(value)) %>%
    dplyr::rename(W_GEOID = Workplace_Census_Block_Code,
                  R_GEOID = Residence_Census_Block_Code)

wac_join = inner_join(chicago_wac, chicago_shape, by = "GEOID") %>%
    left_join(zillow_zip, by = c("GEOID" = "TRACT"))
rac_join = inner_join(chicago_rac, chicago_shape, by = "GEOID") %>%
    left_join(zillow_zip, by = c("GEOID" = "TRACT"))
od_join = inner_join(chicago_od, chicago_shape, by = c("W_GEOID" = "GEOID")) %>%
    inner_join(chicago_shape, by = c("R_GEOID" = "GEOID"))   
od_join = od_join %>%
  left_join(zillow_zip, by = c("W_GEOID" = "TRACT")) %>%
  left_join(zillow_zip, by = c("R_GEOID" = "TRACT")) %>%
  dplyr::rename(w_rent = avg_rent.x,
                r_rent = avg_rent.y)


names(od_join)[names(od_join) == "geometry.x"] = "w_geometry"
names(od_join)[names(od_join) == "geometry.y"] = "r_geometry"
rm(chicago_wac)
rm(chicago_rac)
rm(chicago_od)
gc()



od_join = od_join %>%
    select(W_GEOID, R_GEOID, category, value, 
           W_ALAND = ALAND.x, R_ALAND = ALAND.y,
           W_AWATER = AWATER.x, R_AWATER = AWATER.y,
           W_lat = INTPTLAT.x, R_lat = INTPTLAT.y,
           W_lon = INTPTLON.x, R_lon = INTPTLON.y,
           w_geometry, r_geometry, 
           w_rent, r_rent)


chicago_zoning_tmp = chicago_zoning %>%
    mutate(zoning_area = st_area(.))
chicago_shape_tmp = chicago_shape %>%
    mutate(tract_area = st_area(.))
chicago_shape_tmp = st_transform(chicago_shape_tmp, crs = st_crs(chicago_zoning_tmp))

test = st_intersection(st_make_valid(chicago_zoning_tmp), st_make_valid(chicago_shape_tmp))


test = test %>%
    group_by(GEOID, ZONE_TYPE_DESC) %>%
    summarise(zoning_area = sum(zoning_area)) %>%
    ungroup() 
    
test = test %>%
    group_by(GEOID) %>%
    mutate(share_zone = as.numeric(zoning_area/sum(zoning_area))) %>%
    as_tibble() %>%
    select(-geometry) %>%
    ungroup() 

test2 = left_join(chicago_shape_tmp, test, by= "GEOID") %>%
    as_tibble() %>%
    select(GEOID, ZONE_TYPE_DESC, share_zone) %>%
    filter(!is.na(ZONE_TYPE_DESC)) %>%
    distinct()

rac_join_2 = left_join(rac_join, test2, by = "GEOID")
wac_join_2= left_join(wac_join, test2, by = "GEOID")
od_join_2 = left_join(od_join, test2, by = c("R_GEOID" = "GEOID")) %>%
    dplyr::rename(R_ZONE_TYPE_DESC = ZONE_TYPE_DESC, R_share_zone = share_zone) %>%
    left_join(test2, by = c("W_GEOID" = "GEOID")) %>%
    dplyr::rename(W_ZONE_TYPE_DESC = ZONE_TYPE_DESC, W_share_zone = share_zone)

rm(od_join, rac_join, wac_join, test, test2)


###########################
# Parameters 
wHwL = 2.114517
beta_S = 0.5
beta_U = 0.3
alpha_S = 0.05
alpha_U = 0.01 
gamma = 0.76
ubar = 1
###########################

theta_data = od_join_2 %>%
    filter(category == "Total_Job_Flows") %>%
    distinct(W_GEOID, R_GEOID, value) %>%
    filter(R_GEOID %in% W_GEOID & W_GEOID %in% R_GEOID) 


Lim = 
    wac_join_2 %>%
    filter(category %in% c("Jobs_College", "Jobs_Degree", "Jobs_Low_Education", "Jobs_School_Ed")) %>%
    distinct(GEOID, category, value, .keep_all = TRUE) %>%
    filter(GEOID %in% unique(c(theta_data$W_GEOID, theta_data$R_GEOID))) %>%
    mutate(category = ifelse(category %in% c("Jobs_College", "Jobs_Degree"), "College", "No College")) %>%
    group_by(GEOID, category) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = "category", values_from = "value") %>%
    dplyr::rename(LiS = College, LiU = `No College`) %>%
    mutate(S_share = LiS / (LiS + LiU),
           total = LiS + LiU)


GEOID_nan = c(Lim$GEOID[is.nan(Lim$S_share)], "17031612000")

Lim = Lim %>%
    filter(!is.na(S_share))

theta_data = theta_data %>%
     filter(!(W_GEOID %in% GEOID_nan) & !(R_GEOID %in% GEOID_nan))

pi_ijm_j = od_join_2 %>%
    filter(category == "Total_Job_Flows") %>%
    filter(R_GEOID %in% W_GEOID & W_GEOID %in% R_GEOID) %>%
    filter(!(R_GEOID %in% GEOID_nan) & !(W_GEOID %in% GEOID_nan)) %>%
    distinct(R_GEOID, W_GEOID, category, value) %>%
    group_by(R_GEOID) %>%
    mutate(total_r = sum(value, na.rm = TRUE)) %>%
    group_by(W_GEOID) %>%
    mutate(total_w = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(Lim %>% select(GEOID, S_share, total), by = c("W_GEOID" = "GEOID")) %>%
    mutate(flowS = S_share * value * total / total_w,
           flowU = (1-S_share) * value * total / total_w) %>%
    group_by(R_GEOID) %>%
    mutate(total_r_adj = sum(flowS + flowU, na.rm = TRUE)) %>%
    group_by(W_GEOID) %>%
    mutate(total_w_adj = sum(flowS + flowU, na.rm = TRUE)) %>%
    ungroup()

Rjm = rac_join_2 %>%    
    filter(category %in% c("Jobs_College", "Jobs_Degree", "Jobs_Low_Education", "Jobs_School_Ed")) %>%
    distinct(GEOID, category, value, .keep_all = TRUE) %>%
    filter(GEOID %in% unique(c(theta_data$W_GEOID, theta_data$R_GEOID))) %>%
    filter(!(GEOID %in% GEOID_nan)) %>%
    mutate(category = ifelse(category %in% c("Jobs_College", "Jobs_Degree"), "College", "No College")) %>%
    group_by(GEOID, category) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = "category", values_from = "value") %>%
    dplyr::rename(RjS = College, RjU = `No College`)

Rjm = left_join(Rjm, pi_ijm_j %>% distinct(R_GEOID, total_r_adj), by = c("GEOID" = "R_GEOID")) %>%
    mutate(actual = RjS + RjU)

corr = cor(Rjm$total_r_adj, Rjm$actual)

corplot = ggplot(data = bind_rows(Rjm, tibble(actual = 6000))) + 
    geom_point(mapping = aes(x = actual, y = total_r_adj, color = "Adjusted res. pop.")) + 
    geom_line(mapping = aes(x = actual, y = actual, color = "45 degree line")) + 
    scale_color_manual(values = c("red", "blue")) + 
    labs(x = "Actual res. pop.", y = "Imputed res. pop.", color = "", title = "") + 
    theme_cowplot() + 
    theme(legend.position = "none",
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10)) + 
    xlim(0,6000) + 
    ylim(0,6000)

save_plot("Figures/corplot.pdf", corplot, base_height = 3, base_asp = 1.2)

Rjm = pi_ijm_j %>%
    select(W_GEOID, R_GEOID, flowS, flowU) %>%
    group_by(R_GEOID) %>%
    summarise(RjS = sum(flowS, na.rm = TRUE),
              RjU = sum(flowU, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::rename(GEOID = R_GEOID)

pi_ijm_j = pi_ijm_j %>%
    select(R_GEOID, W_GEOID, flowS, flowU) %>%
    pivot_longer(flowS:flowU, names_to = "m", values_to = "value") %>%
    group_by(R_GEOID, m) %>%
    mutate(pi_ijm_j = value / sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    select(R_GEOID, W_GEOID, m, pi_ijm_j) %>%
    pivot_wider(names_from = "m", values_from = "pi_ijm_j") 


ub = 9869.655 * 12 
cut1 = (0+1250)/CPIAUCSL$CPIAUCSL[CPIAUCSL$date == lubridate::ymd(20190101)]
cut2 = (1251+3333)/CPIAUCSL$CPIAUCSL[CPIAUCSL$date == lubridate::ymd(20190101)]
cut3 = (3334+ub/12)/CPIAUCSL$CPIAUCSL[CPIAUCSL$date == lubridate::ymd(20190101)]
w_i = wac_join_2 %>%
    filter(category %in% c("High_Income_Jobs", "Middle_Income_Jobs", "Low_Income_Jobs")) %>%
    distinct(GEOID, category, .keep_all = TRUE) %>%
    filter(GEOID %in% unique(c(theta_data$W_GEOID, theta_data$R_GEOID))) %>%
    filter(!(GEOID %in% GEOID_nan)) %>%
    select(GEOID, category, value) %>%
    pivot_wider(names_from = "category", values_from = "value") %>%
    mutate(w_i = (12/(High_Income_Jobs + Middle_Income_Jobs + Low_Income_Jobs)) *( (Low_Income_Jobs * cut1/2 + Middle_Income_Jobs * cut2/2 + 
                                                           High_Income_Jobs * cut3/2 ))) %>%
    select(GEOID, w_i)

Lim = left_join(Lim, w_i, by = c("GEOID")) %>%
    mutate(wS = w_i / (S_share + (1-S_share) * 1/wHwL),
           wU = wS/wHwL) %>%
    select(GEOID, w_i, wS, wU, LiS, LiU) 

Lim = left_join(pi_ijm_j, Lim, by = c("W_GEOID" = "GEOID")) 

tau_i = Lim %>%
    select(W_GEOID, wS, wU, LiS, LiU) %>%
    distinct() %>%
    mutate(tau_i = (LiU/LiS) * (beta_S/beta_U) * (wU/wS) - 1) %>%
    select(W_GEOID, tau_i )

narrow1 = Lim %>%
    select(R_GEOID, W_GEOID, flowS, flowU) %>%
    dplyr::rename(S = flowS, U = flowU) %>%
    pivot_longer(S:U, names_to = "m", values_to = "pi_ijm_j")

narrow2 = Lim %>%
    select(R_GEOID, W_GEOID, wS, wU) %>%
    dplyr::rename(S = wS, U = wU) %>%
    pivot_longer(S:U, names_to = "m", values_to = "wim")

narrow1 = left_join(narrow1, narrow2, by = c("R_GEOID", "W_GEOID", "m"))
rm(narrow2)

wbar = narrow1 %>%
    group_by(m, R_GEOID) %>%
    summarise(wbar = sum(pi_ijm_j * wim, na.rm = TRUE),
              check = sum(pi_ijm_j, na.rm = TRUE)) %>%
    ungroup()
rm(narrow1)
if (all(abs(wbar$check - 1) < 1e-10)) wbar = wbar %>% select(-check)
wbar = wbar %>%
    pivot_wider(names_from = "m", values_from = "wbar") %>%
    inner_join(rac_join_2 %>%
                distinct(GEOID, .keep_all = TRUE) %>%
                select(R_GEOID = GEOID, qjr = avg_rent) %>%
                mutate(qjr = ifelse(is.na(qjr), median(qjr, na.rm = TRUE), qjr)), by = "R_GEOID") %>%
    dplyr::rename(wS = S, wU = U) %>%
    inner_join(Rjm, by = c("R_GEOID" = "GEOID")) 

Tjr = wbar %>%
    mutate(Tjr = ((1-gamma)/qjr) * (RjS*wS + RjU*wU))

kappa_ij = od_join_2 %>%
    distinct(W_GEOID, R_GEOID, W_lat, W_lon, R_lat, R_lon) %>%
    filter(W_GEOID %in% R_GEOID & R_GEOID %in% W_GEOID) %>%
    filter(!(W_GEOID %in% GEOID_nan) & !(R_GEOID %in% GEOID_nan)) %>%
    mutate_at(vars(W_lat, R_lat, W_lon, R_lon), as.numeric) %>%
    complete(W_GEOID, R_GEOID)

Wlocs = kappa_ij %>%
    distinct(W_GEOID, W_lat, W_lon) %>%
    filter(!is.na(W_lat) & !is.na(W_lon)) %>%
    dplyr::rename(replacelat= W_lat, replacelon = W_lon)

Rlocs = kappa_ij %>%
    distinct(R_GEOID, R_lat, R_lon) %>%
    filter(!is.na(R_lat) & !is.na(R_lon)) %>%
    dplyr::rename(replacelat= R_lat, replacelon = R_lon)

kappa_ij = left_join(kappa_ij, Wlocs, by = "W_GEOID") %>%
    mutate(W_lat = ifelse(is.na(W_lat), replacelat, W_lat),
           W_lon = ifelse(is.na(W_lon), replacelon, W_lon)) %>%
    select(-replacelat, -replacelon) %>%
    left_join(Rlocs, by = "R_GEOID") %>%
    mutate(R_lat = ifelse(is.na(R_lat), replacelat, R_lat),
           R_lon = ifelse(is.na(R_lon), replacelon, R_lon)) %>%
    select(-replacelat, -replacelon)

kappa_ij = kappa_ij %>%
    mutate(W_lat2 = W_lat*pi/180,
           R_lat2 = R_lat*pi/180,
           W_lon2 = W_lon*pi/180,
           R_lon2 = R_lon*pi/180,
           lat_diff = R_lat2 - W_lat2,
           lon_diff = R_lon2 - W_lon2,
           a = sin(lat_diff/2)^2 + cos(W_lat2)*cos(R_lat2)*sin(lon_diff/2)^2 ,
           c = 2 * atan2(sqrt(a), sqrt(1-a)),
           R = 6371e3, 
           d = R*c,
           d = 1 + d*1/1609.34) %>%
    select(W_GEOID, R_GEOID, kappa_ij = d) %>%
    left_join(distinct(Lim, W_GEOID, wS, wU), by = "W_GEOID") %>%
    left_join(theta_data, by = c("W_GEOID", "R_GEOID")) %>%
    mutate(value= ifelse(is.na(value), 0, value)) %>%
    group_by(R_GEOID) %>%
    mutate(pi_ij_j = value / sum(value, na.rm = TRUE)) %>%
    ungroup() 
    
kappa_ij = kappa_ij %>%
    arrange(W_GEOID, R_GEOID) %>%
    group_by(R_GEOID) %>%
    mutate(i = cur_group_id()) %>%
    ungroup() %>%
    group_by(W_GEOID) %>%
    mutate(j = cur_group_id()) %>%
    ungroup() %>%
    arrange(j, i)

kappa_ij = kappa_ij %>%
    group_by(j) %>%
    mutate(pi_jj = sum(pi_ij_j * (j == i)),
           kappa_jj = sum(kappa_ij * (j == i))) %>%
    ungroup() %>%
    mutate(y = log(pi_ij_j / pi_jj),
           x = log(kappa_ij / kappa_jj),
           W_GEOID = as.factor(W_GEOID),
           R_GEOID = as.factor(R_GEOID))

theta_reg = feols(y ~ x | W_GEOID + R_GEOID, data = kappa_ij)
theta= -10*theta_reg$coefficients

wages = kappa_ij %>%
    distinct(W_GEOID, .keep_all = TRUE) %>% 
    select(W_GEOID, S = wS, U = wU) %>%
    pivot_longer(S:U, names_to = "m", values_to = "wages")

kappa_ij_select = kappa_ij %>%
    select(W_GEOID, R_GEOID, kappa_ij) 

pi_ij = pi_ijm_j %>%
    select(R_GEOID, W_GEOID, S = flowS, U = flowU) %>%
    pivot_longer(S:U, names_to = "m", values_to = "pi_ijm_j")

lambda_ijm = left_join(pi_ij, kappa_ij_select, by = c("R_GEOID", "W_GEOID")) %>%
    left_join(wages, by = c("W_GEOID", "m")) %>%
    mutate(lambda_ijm = pi_ijm_j * ((1e21^(1/theta) * kappa_ij) / wages)^theta,
           check = ((wages / ((1e21^(1/theta)) * kappa_ij))^theta)  * lambda_ijm / pi_ijm_j) 

if (any(abs(lambda_ijm$check[lambda_ijm$W_GEOID == lambda_ijm$R_GEOID & !is.na(lambda_ijm$check)] - 1) > 1e-10)) {
    stop("Normalization not holding.")
}


lambda_ijm_reshape = lambda_ijm %>%
    select(W_GEOID, R_GEOID, m, lambda_ijm) %>%
    distinct() %>%
    pivot_wider(names_from = "m", values_from = "lambda_ijm") %>%
    dplyr::rename(lambdaS = S, lambdaU = U)







Tjr = left_join(Tjr, lambda_ijm_reshape, by = "R_GEOID") %>%
    left_join(kappa_ij_select, by = c("R_GEOID", "W_GEOID")) %>%
    mutate(numerator = ubar * ((1-gamma)^(1-gamma)) * (RjS * wS + RjU * wU)^(1-gamma)) %>%
    inner_join(Lim %>% distinct(W_GEOID, .keep_all = TRUE) %>%
               select(W_GEOID, w_iS = wS, w_iU = wU), by = "W_GEOID") %>%
    group_by(R_GEOID) %>%
    mutate(denomS = gamma((theta-1)/theta) * (Tjr^(1-gamma)) * sum(lambdaS * (w_iS/kappa_ij)^theta)^(1/theta),
           denomU = gamma((theta-1)/theta) * (Tjr^(1-gamma)) * sum(lambdaU * (w_iU/kappa_ij)^theta)^(1/theta)) %>%
    ungroup() %>%
    mutate(sigmaS = log(numerator/denomS)/log(RjS),
           sigmaU = log(numerator/denomU)/log(RjU))


Tib = od_join_2 %>%
    distinct(W_GEOID, W_ZONE_TYPE_DESC, .keep_all = TRUE) %>%
    filter(W_GEOID %in% Tjr$W_GEOID) %>%
    select(W_GEOID, W_ZONE_TYPE_DESC, W_ALAND, W_share_zone) %>%
    filter(!is.na(W_ZONE_TYPE_DESC)) %>%
    filter(W_ZONE_TYPE_DESC == "Business") %>%
    mutate(Tib = W_share_zone * W_ALAND) 

Tib = Tib %>%
    right_join(od_join_2 %>%  
                distinct(W_GEOID, alt_W_ALAND = W_ALAND), by = "W_GEOID") %>%
    mutate(W_share_zone = ifelse(is.na(W_share_zone), median(W_share_zone, na.rm = TRUE), W_share_zone),
           W_ALAND = ifelse(is.na(W_ALAND), alt_W_ALAND, W_ALAND),
           Tib = ifelse(is.na(Tib), W_share_zone * W_ALAND, Tib)) %>%
    arrange(W_GEOID) %>%
    filter(W_GEOID %in% Lim$W_GEOID)




L_reshape = Lim %>%
    select(W_GEOID, wS, wU, LiS, LiU) %>%
    distinct() %>%
    left_join(tau_i, by = "W_GEOID") %>%
    left_join(Tib, by = "W_GEOID") %>%
    mutate(A_i = ((LiS/Tib)^(1-alpha_U-alpha_S-beta_S-beta_U)) * ((wU / beta_U)^(alpha_U + beta_U)) * 
                        (wS * (1+tau_i)/beta_S)^(1-alpha_U - beta_U))



opportunity_zones = read_csv("Data/opportunity_zones_true.csv", col_names = FALSE)
opportunity_zones = opportunity_zones[, 2] %>%
    dplyr::rename(W_GEOID = X2) %>%
    mutate(oppzone = TRUE,
           W_GEOID = as.character(W_GEOID)) 

poppzone_tau_A = L_reshape %>%
    distinct(W_GEOID, A_i) %>%
    left_join(tau_i %>% distinct(W_GEOID, tau_i), by = "W_GEOID") %>%
    mutate(tau_i = 1 + tau_i) %>%
    left_join(opportunity_zones, by = "W_GEOID") %>%
    left_join(od_join_2 %>% distinct(W_GEOID, .keep_all=  TRUE) %>% select(W_GEOID, w_geometry), by = "W_GEOID") %>%
    mutate(oppzone = ifelse(is.na(oppzone), FALSE, oppzone)) %>%
    mutate(A_i = (A_i - mean(A_i, na.rm = TRUE))/sd(A_i, na.rm = TRUE)) %>%
    select(A_i, tau_i, everything()) %>%
    pivot_longer(A_i:tau_i, names_to = "series", values_to = "values") %>%
    group_by(series, oppzone) %>%
    mutate(quintile = ntile(values, 5)) %>%
    group_by(series, quintile, oppzone) %>%
    mutate(min = min(values, na.rm = TRUE),
           max = max(values, na.rm = TRUE),
           label = paste0("[", sprintf('%0.2f', min), ", ", sprintf('%0.2f', max), "]"),
           label = factor(label, levels = unique(label)[order(unique(min))])) %>%
    ungroup()

# fig_oppzone_tau_A = ggplot(data = poppzone_tau_A %>% mutate(label = if_else(!oppzone, NA, label)) %>%
#                                     filter(series == "tau_i"),
#                            mapping = aes(geometry = w_geometry, fill = label)) + 
#                     geom_sf() + 
#                     scale_fill_manual(values = colorRampPalette(c("white","red4"))(5))



# Reshape 
pi_ijS_j_mat = pi_ijm_j %>%
    select(-flowU) %>%
    complete(R_GEOID, W_GEOID, fill = list(flowS = 0)) %>%
    arrange(R_GEOID, W_GEOID) %>%
    pivot_wider(names_from = "R_GEOID", values_from ="flowS") %>%
    select(-W_GEOID) %>%
    mutate_all(~ifelse(is.na(.), 0, .)) %>%
    as.matrix()

pi_ijU_j_mat = pi_ijm_j %>%
    select(-flowS) %>%
    complete(R_GEOID, W_GEOID, fill = list(flowU = 0)) %>%
    arrange(R_GEOID, W_GEOID) %>%
    pivot_wider(names_from = "R_GEOID", values_from ="flowU") %>%
    select(-W_GEOID) %>%
    mutate_all(~ifelse(is.na(.), 0, .)) %>%
    as.matrix()


kappa_ij_mat = kappa_ij %>%
    select(R_GEOID, W_GEOID, kappa_ij) %>%
    arrange(R_GEOID, W_GEOID) %>%
    pivot_wider(names_from = "R_GEOID", values_from = "kappa_ij") %>%
    mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
    select(-W_GEOID) %>%
    as.matrix()

lambda_ijS_mat = lambda_ijm_reshape %>%
    select(-lambdaU) %>%
    complete(W_GEOID, R_GEOID, fill = list(lambdaS = 0)) %>%
    arrange(R_GEOID, W_GEOID) %>%
    pivot_wider(names_from = "R_GEOID", values_from = "lambdaS") %>%
    select(-W_GEOID) %>%
    as.matrix()


lambda_ijU_mat = lambda_ijm_reshape %>%
    select(-lambdaS) %>%
    complete(W_GEOID, R_GEOID, fill = list(lambdaU = 0)) %>%
    arrange(R_GEOID, W_GEOID) %>%
    pivot_wider(names_from = "R_GEOID", values_from = "lambdaU") %>%
    select(-W_GEOID) %>%
    as.matrix()

RjS_vec = Rjm %>%
    arrange(GEOID) %>%
    select(RjS) %>%
    deframe()

RjU_vec = Rjm %>%
    arrange(GEOID) %>%
    select(RjU) %>%
    deframe()

Tjr_vec = Tjr %>%
    distinct(R_GEOID, .keep_all = TRUE) %>%
    arrange(R_GEOID) %>%
    select(Tjr) %>%
    deframe()

w_iS_vec = Lim %>%
    distinct(W_GEOID, .keep_all = TRUE) %>%
    arrange(W_GEOID) %>%
    select(wS) %>%
    deframe() 

w_iU_vec = Lim %>%
    distinct(W_GEOID, .keep_all = TRUE) %>%
    arrange(W_GEOID) %>%
    select(wU) %>%
    deframe() 

qjr_vec = Tjr %>%
    distinct(R_GEOID, .keep_all = TRUE) %>%
    arrange(R_GEOID) %>%
    select(qjr) %>%
    deframe()

LiS_vec = Lim %>%
    distinct(W_GEOID, .keep_all = TRUE) %>%
    arrange(W_GEOID) %>%
    select(LiS) %>%
    deframe()

LiU_vec = Lim %>%
    distinct(W_GEOID, .keep_all = TRUE) %>%
    arrange(W_GEOID) %>%
    select(LiU) %>%
    deframe()


A_i_vec = L_reshape %>%
    distinct(W_GEOID, .keep_all = TRUE) %>%
    arrange(W_GEOID) %>%
    select(A_i) %>%
    deframe() 

sigma_jS_vec = Tjr %>%
    distinct(R_GEOID, .keep_all = TRUE) %>%
    arrange(R_GEOID) %>%
    select(sigmaS) %>%
    deframe()

sigma_jU_vec = Tjr %>%
    distinct(R_GEOID, .keep_all = TRUE) %>%
    arrange(R_GEOID) %>%
    select(sigmaU) %>%
    deframe()

tau_i_vec = tau_i %>%
    arrange(W_GEOID) %>%
    select(tau_i) %>%
    deframe()

Tib_vec = Tib %>%
    arrange(W_GEOID) %>%
    select(Tib) %>%
    deframe()

W_GEOID_order = Tib %>%
    arrange(W_GEOID) %>%
    select(W_GEOID) %>%
    deframe()

R_GEOID_order = Tjr %>%
    distinct(R_GEOID, .keep_all = TRUE) %>%
    arrange(R_GEOID) %>%
    select(R_GEOID) %>%
    deframe()

model = list(tau_i = tau_i_vec, sigma_jU_vec = sigma_jU_vec, sigma_jS_vec = sigma_jS_vec, 
             A_i_vec = A_i_vec, LiU_vec = LiU_vec, LiS_vec = LiS_vec, 
             qjr_vec = qjr_vec, w_iU_vec = w_iU_vec, w_iS_vec = w_iS_vec, 
             Tjr_vec = Tjr_vec, RjU_vec = RjU_vec, RjS_vec = RjS_vec, 
             lambda_ijU_mat = lambda_ijU_mat, lambda_ijS_mat = lambda_ijS_mat,
             kappa_ij_mat = kappa_ij_mat, pi_ijU_j_mat = pi_ijU_j_mat, pi_ijS_j_mat = pi_ijS_j_mat,
             theta = theta, beta_S = beta_S, beta_U = beta_U, alpha_S = alpha_S, alpha_U = alpha_U, 
             gamma = gamma, ubar = ubar, Tib_vec = Tib_vec, R_GEOID_order = R_GEOID_order, 
             W_GEOID_order = W_GEOID_order)

# walk2(.x = model, .y = names(model), .f = ~write.table(.x, file = paste0("Data/Export_R/", .y, ".csv"), 
#                                                        row.names = FALSE, col.names = FALSE, sep = ","))


w = c(model$w_iS_vec, model$w_iU_vec)

itersolve = function(Rj, model, jj, wbarS, wbarU, w_iS, w_iU) {
    out = rep(0, length(Rj)) 
    out[1] = ((model$ubar * ((1-model$gamma)^(1-model$gamma)) * (Rj[1]*wbarS[jj] + Rj[2]*wbarU[jj])^(1-model$gamma)) /
                                (gamma((model$theta-1)/model$theta) * 
                                    sum(model$lambda_ijS_mat[, jj] * (w_iS/model$kappa_ij_mat[, jj])^model$theta)^(1/model$theta) * 
                                        model$Tjr_vec[jj]^(1-model$gamma)) ) - Rj[1]^model$sigma_jS_vec[jj]
                                    

    out[2] = ((model$ubar * ((1-model$gamma)^(1-model$gamma)) * (Rj[1]*wbarS[jj] + Rj[2]*wbarU[jj])^(1-model$gamma)) /
                                (gamma((model$theta-1)/model$theta) * 
                                    sum(model$lambda_ijU_mat[, jj] * (w_iU/model$kappa_ij_mat[, jj])^model$theta)^(1/model$theta) * 
                                        model$Tjr_vec[jj]^(1-model$gamma)) ) - Rj[2]^model$sigma_jU_vec[jj]
                                    

    return(out)
}


solve_model = function(w, model, max_inner, min_err, delta,
                       acceptguessuntil = 40,
                       minWageAdjustmentFactor = 0.01,
                       maxWageAdjustmentFactor = 0.2,
                       accelEndingMaxW = 0.5,
                       accelEndingTol = 20) {

    count_inner = 0 
    err = Inf
    pi_ijS_mat = matrix(0, nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat))
    pi_ijU_mat = matrix(0, nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat))
    wbarS = rep(0, nrow(pi_ijS_mat))
    wbarU = rep(0, nrow(pi_ijU_mat))
    LSsupply = rep(0, length(wbarS))
    LUsupply = rep(0, length(wbarU))
    RjS = rep(0, length(wbarS))
    RjU = rep(0, length(wbarU))
    errvec_outer = rep(NA, length(max_inner))
    delta = delta*rep(1, length(w))

    excessLaborVec = Inf*rep(1, length(w))
    sumExcessLabor = Inf
    largestExcessLabor = Inf
    termCrit = Inf
    StableIter = FALSE
        
        


    while ( (count_inner < max_inner) && (sumExcessLabor > min_err) ) {
        tic()

        count_inner = count_inner + 1 

        w_iS = w[1:nrow(model$kappa_ij_mat)]
        w_iU = w[(nrow(model$kappa_ij_mat)+1):(2*nrow(model$kappa_ij_mat))]

        pi_ijS_mat = (model$lambda_ijS_mat * 
            (matrix(w_iS, nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat)) / model$kappa_ij_mat)^model$theta) / (

                matrix(colSums((model$lambda_ijS_mat * 
                       (matrix(w_iS, nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat)) / model$kappa_ij_mat)^model$theta)),
                       nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat), byrow = TRUE)
            )

          pi_ijU_mat = (model$lambda_ijU_mat * 
            (matrix(w_iU, nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat)) / model$kappa_ij_mat)^model$theta) / (

                matrix(colSums((model$lambda_ijU_mat * 
                       (matrix(w_iU, nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat)) / model$kappa_ij_mat)^model$theta)),
                       nrow = nrow(model$kappa_ij_mat), ncol = ncol(model$kappa_ij_mat), byrow = TRUE)
            )


        wbarS = colSums(pi_ijS_mat * matrix(w_iS, nrow = nrow(pi_ijS_mat), ncol = ncol(pi_ijS_mat)))
        wbarU = colSums(pi_ijU_mat * matrix(w_iU, nrow = nrow(pi_ijU_mat), ncol = ncol(pi_ijU_mat)))

       
        Rj = pmap(.l = list(1:length(wbarS), model$RjS_vec, model$RjU_vec), model = model, 
                  .f = function(x,y,z,model) {
                        nleqslv(c(y,z), itersolve, model = model, jj = x, 
                                wbarS = wbarS, wbarU = wbarU, w_iS = w_iS, w_iU = w_iU)
                  }
        )
        RjS = map_dbl(.x = Rj, .f = ~.x$x[1])
        RjU = map_dbl(.x = Rj, .f = ~.x$x[2])     
       
        LSsupply = rowSums(pi_ijS_mat * matrix(RjS, nrow = nrow(pi_ijS_mat), ncol = ncol(pi_ijS_mat), byrow = TRUE))
        LUsupply = rowSums(pi_ijU_mat * matrix(RjU, nrow = nrow(pi_ijU_mat), ncol = ncol(pi_ijU_mat), byrow = TRUE))

        LSdemand = (model$A_i_vec / (

            ((w_iS * (1 + model$tau_i)/model$beta_S)^(1-model$alpha_U-model$beta_U)) * 
                ((w_iU/model$beta_U)^(model$alpha_U + model$beta_U))

        ))^(1 / (1-model$alpha_U - model$beta_U - model$alpha_S - model$beta_S) ) * model$Tib_vec

        LUdemand = (model$beta_U/model$beta_S) * (w_iS/w_iU) * (1+model$tau_i) * LSdemand

        excessLaborVecNew = c(LSdemand, LUdemand) - c(LSsupply, LUsupply)
        sumExcessLaborNew = sum(abs(excessLaborVecNew))
        errvec_outer[count_inner] = sumExcessLaborNew
        largestExcessLaborNew = max(abs(excessLaborVecNew))

        changeInLargestExcessLabor = largestExcessLaborNew - largestExcessLabor
        acceptGuess = changeInLargestExcessLabor < 0
        tractMadeWorse = abs(excessLaborVecNew) > abs(excessLaborVec)
        numTractsMadeWorse = sum(tractMadeWorse)
    
        excessLaborVec = excessLaborVecNew
        sumExcessLabor = sumExcessLaborNew
        largestExcessLabor = largestExcessLaborNew
        termCrit = sumExcessLabor
 
        

        if (count_inner > acceptguessuntil) {

            if ( (StableIter) && (largestExcessLabor < largestExcessLaborPreStableIter) ) {
                StableIter = FALSE
            }

            if (acceptGuess && !StableIter) {

              
                delta[!tractMadeWorse] = delta[!tractMadeWorse] + 0.1;
                delta = pmin(delta, maxWageAdjustmentFactor)


               
                delta[tractMadeWorse] = delta[tractMadeWorse] - 0.1;
                delta = pmax(delta, minWageAdjustmentFactor)
            
            } else if (!acceptGuess && !StableIter) {
    
                largestExcessLaborPreStableIter = largestExcessLabor;
                StableIter = TRUE;
                delta = rep(1, length(delta)) * 0.025;
            }
            
          
            if (largestExcessLabor < accelEndingTol) {
                maxWageAdjustmentFactor = accelEndingMaxW;
            }
        }
      
        w = w + (delta * excessLaborVec);

        message(paste0("Error = ", sprintf('%0.1f', sumExcessLabor), ". Iteration = ", count_inner))

        toc()

    }

    return(list(w = w, excessLaborVecNew = excessLaborVecNew, errvec_outer = errvec_outer,  err = sumExcessLabor, 
                count_inner = count_inner, delta = delta,
                pi_ijS_mat = pi_ijS_mat, pi_ijU_mat = pi_ijU_mat, 
                wbarS = wbarS, wbarU = wbarU, 
                RjS = RjS, RjU = RjU,
                LiS = LSdemand, LiU = LUdemand,
                pi_ijS_mat = pi_ijS_mat, pi_ijU_mat = pi_ijU_mat))

}

rm(list = setdiff(ls(), c("w", "model", "od_join_2", "rac_join_2", "wac_join_2", "solve_model", "itersolve",
                            "CPIAUCSL", "tau_i", "Tjr", "Lim", "solve_model_bool")))
gc() 
if (solve_model_bool) {

    max_inner = 10
    min_err = 1
    delta = 0.075
    baseline = solve_model(w = w, model = model, max_inner = max_inner, min_err = min_err, delta = delta)


    opportunity_zones = read_csv("Data/opportunity_zones_true.csv", col_names = FALSE)
    opportunity_zones = opportunity_zones[, 2] %>%
        dplyr::rename(W_GEOID = X2) %>%
        mutate(oppzone = TRUE,
            W_GEOID = as.character(W_GEOID)) 

    tau_i_cf = left_join(tau_i, opportunity_zones, by = "W_GEOID") %>%
        mutate(oppzone = ifelse(is.na(oppzone), FALSE, oppzone),
            tau_i1 = case_when(oppzone & tau_i > 0 ~ tau_i * (1 - 0.1),
                                oppzone & tau_i <= 0 ~ tau_i * (1 + 0.1),
                                !oppzone ~ tau_i),
            tau_i2 = case_when(oppzone & tau_i > 0 ~ tau_i * (1 - 0.1),
                                oppzone & tau_i <= 0 ~ tau_i * (1 + 0.1),
                                !oppzone ~ tau_i))

    tau_2 = tau_i_cf %>% 
        arrange(W_GEOID) %>%
        select(tau_i1) %>%
        deframe()

    tau_3 = tau_i_cf %>% 
        arrange(W_GEOID) %>%
        select(tau_i2) %>%
        deframe()

    model2 = model
    model3 = model 
    model2$tau_i = tau_2
    mdoel3$tau_i = tau_3
    max_inner = 10000
    delta = 0.04
    change_tau1 = solve_model(w = w, model = model2, max_inner = max_inner, min_err = min_err, delta = delta,
                            acceptguessuntil = 40,
                            minWageAdjustmentFactor = 0.01,
                            maxWageAdjustmentFactor = 2,
                            accelEndingMaxW = 0.5,
                            accelEndingTol = 20)

    change_tau2 = solve_model(w = w, model = model3, max_inner = max_inner, min_err = min_err, delta = delta,
                            acceptguessuntil = 40,
                            minWageAdjustmentFactor = 0.01,
                            maxWageAdjustmentFactor = 2,
                            accelEndingMaxW = 0.5,
                            accelEndingTol = 20)

    

    saveRDS(baseline, "Data/baseline.rds")
    saveRDS(change_tau1, "Data/smarter_nopos.rds")
    saveRDS(change_tau2, "Data/05_smarter_nopos.rds")

    
} 

baseline = readRDS("Data/baseline.rds")
change_tau = readRDS("Data/smarter_nopos.rds")
change_tau_05 = readRDS("Data/05_smarter_nopos.rds")


change_tau_list = list(baseline = baseline, change_tau = change_tau, change_tau_05 = change_tau_05)

iter_model = function(change_tau, name, tauchange) {

    opportunity_zones = read_csv("Data/opportunity_zones_true.csv", col_names = FALSE)
    opportunity_zones = opportunity_zones[, 2] %>%
        dplyr::rename(W_GEOID = X2) %>%
        mutate(oppzone = TRUE,
            W_GEOID = as.character(W_GEOID)) 

    tau_i_cf = left_join(tau_i, opportunity_zones, by = "W_GEOID") %>%
        mutate(oppzone = ifelse(is.na(oppzone), FALSE, oppzone),
            tau_i = case_when(oppzone & tau_i > 0 ~ tau_i * (1 - tauchange),
                                oppzone & tau_i <= 0 ~ tau_i * (1 + tauchange),
                                !oppzone ~ tau_i))

    tau_2 = tau_i_cf %>% 
        arrange(W_GEOID) %>%
        select(tau_i) %>%
        deframe()

    merge_W = Lim %>%
        distinct(W_GEOID, .keep_all = TRUE) %>%
        arrange(W_GEOID) 

    eq_wages = data.frame(w_baseline = baseline$w, w_cf = change_tau$w, 
                        L_baseline = c(baseline$LiS, baseline$LiU), 
                        L_cf = c(change_tau$LiS, change_tau$LiU), 
                        m = c(rep("S", length(baseline$w)/2), rep("U", length(baseline$w)/2)),
                        W_GEOID = rep(merge_W$W_GEOID, 2)) %>%
                as_tibble() %>%
                left_join(tau_i_cf, by = "W_GEOID") %>%
                dplyr::rename(tau_i_cf = tau_i) %>%
                left_join(tau_i, by = "W_GEOID") %>%
                dplyr::rename(tau_i_baseline = tau_i)


    merge_R = Tjr %>%
        distinct(R_GEOID, .keep_all = TRUE) %>%
        arrange(R_GEOID) 


    piS = as.data.frame(change_tau$pi_ijS_mat) %>%
        mutate(W_GEOID = merge_W$W_GEOID) %>%
        select(W_GEOID, everything()) %>%
        pivot_longer(-W_GEOID, names_to = "R_GEOID", values_to = "pi_ijS")

    piU = as.data.frame(change_tau$pi_ijU_mat) %>%
        mutate(W_GEOID = merge_W$W_GEOID) %>%
        select(W_GEOID, everything()) %>%
        pivot_longer(-W_GEOID, names_to = "R_GEOID", values_to = "pi_ijU")

    piS = left_join(piS, piU, by = c("R_GEOID", "W_GEOID")) %>%
        dplyr::rename(S = pi_ijS, U = pi_ijU) %>%
        pivot_longer(S:U, names_to = "m", values_to = "pi_ijm") %>%
        left_join(eq_wages %>% select(W_GEOID, m, w_cf), by = c("W_GEOID", "m")) %>%
        left_join(tibble(R_GEOID = merge_R$R_GEOID, Tjr = model$Tjr_vec), by = "R_GEOID")

    piS = piS %>%
        group_by(R_GEOID, m) %>%
        mutate(wbar = sum(pi_ijm * w_cf)) %>%
        ungroup() %>%
        select(-pi_ijm, -W_GEOID)

    Tjr = piS %>%
        distinct(R_GEOID, .keep_all = TRUE) %>%
        select(R_GEOID, Tjr)

    wbar = piS %>%
        select(R_GEOID, m, wbar) %>%
        distinct(R_GEOID, m, .keep_all = TRUE) %>%
        pivot_wider(names_from = "m", values_from = "wbar") %>%
        dplyr::rename(wbarS = S, wbarU = U)

    Rjm = tibble(R_GEOID = merge_R$R_GEOID,
                 RjS = change_tau$RjS,
                 RjU = change_tau$RjU) %>%
            left_join(Tjr, by = "R_GEOID") %>%
            left_join(wbar, by = "R_GEOID") %>%
            arrange(R_GEOID)
            
    Rjm = Rjm %>%
            mutate(qjr = ((1-model$gamma)/Tjr) * (RjS * wbarS + RjU * wbarU),
                   qjr_orig = model$qjr_vec,
                   del_rent = log(qjr/qjr_orig),
                   del_rent_actual = qjr - qjr_orig) %>%
            left_join(od_join_2 %>% distinct(R_GEOID, .keep_all = TRUE) %>% select(R_GEOID, r_geometry), by = "R_GEOID") 

    eq_res = data.frame(R_baseline = c(baseline$RjS, baseline$RjU), 
                        R_cf = c(change_tau$RjS, change_tau$RjU),
                        m = c(rep("S", length(baseline$w)/2), rep("U", length(baseline$w)/2)),
                        R_GEOID = rep(merge_R$R_GEOID, 2)) %>%
                as_tibble() 

    baseline_piS = baseline$pi_ijS_mat
    baseline_piU = baseline$pi_ijU_mat
    rownames(baseline_piS) = merge_W$W_GEOID
    colnames(baseline_piS) = merge_R$R_GEOID
    rownames(baseline_piU) = merge_W$W_GEOID
    colnames(baseline_piU) = merge_R$R_GEOID
    baseline_piS = as.data.frame(baseline_piS) %>%
        rownames_to_column() %>%
        dplyr::rename(W_GEOID = rowname) %>%
        pivot_longer(-W_GEOID, names_to = "R_GEOID", values_to = "baseline_pi_ijS")
    baseline_piU = as.data.frame(baseline_piU) %>%
        rownames_to_column() %>%
        dplyr::rename(W_GEOID = rowname) %>%
        pivot_longer(-W_GEOID, names_to = "R_GEOID", values_to = "baseline_pi_ijU")

    cf_piS = change_tau$pi_ijS_mat
    cf_piU = change_tau$pi_ijU_mat
    rownames(cf_piS) = merge_W$W_GEOID
    colnames(cf_piS) = merge_R$R_GEOID
    rownames(cf_piU) = merge_W$W_GEOID
    colnames(cf_piU) = merge_R$R_GEOID
    cf_piS = as.data.frame(cf_piS) %>%
        rownames_to_column() %>%
        dplyr::rename(W_GEOID = rowname) %>%
        pivot_longer(-W_GEOID, names_to = "R_GEOID", values_to = "cf_pi_ijS")
    cf_piU = as.data.frame(cf_piU) %>%
        rownames_to_column() %>%
        dplyr::rename(W_GEOID = rowname) %>%
        pivot_longer(-W_GEOID, names_to = "R_GEOID", values_to = "cf_pi_ijU")

    eq_pi_baseline = left_join(baseline_piU, baseline_piS, by = c("R_GEOID", "W_GEOID")) %>%
        dplyr::rename(S = baseline_pi_ijS, U = baseline_pi_ijU) %>%
        pivot_longer(S:U, names_to = "m", values_to = "baseline_pi_ijm")

    eq_pi_cf = left_join(cf_piU, cf_piS, by = c("R_GEOID", "W_GEOID")) %>%
        dplyr::rename(S = cf_pi_ijS, U = cf_pi_ijU) %>%
        pivot_longer(S:U, names_to = "m", values_to = "baseline_pi_ijm")


    # write_csv(eq_pi_baseline, paste0("Data/eq_pi_baseline_", name, ".csv"))
    # write_csv(eq_pi_cf, paste0("Data/eq_pi_cf_", name, ".csv"))
    # write_csv(eq_res, paste0("Data/eq_res_", name, ".csv"))
    # write_csv(eq_wages, paste0("Data/eq_wages_", name, ".csv"))

    reg_tau = lm(oppzone ~ tau_i_baseline, eq_wages)

    regwages = eq_wages %>%
        left_join(tibble(W_GEOID = model$W_GEOID_order, A_i = model$A_i_vec), by = "W_GEOID") %>%
        mutate(A_i = (A_i - mean(A_i, na.rm = TRUE))/sd(A_i, na.rm = TRUE))

    reg_tau2 = lm(oppzone ~ tau_i_baseline + A_i, regwages)



    s_result = paste0("\\mathbbm{1}\\{\\text{Opportunity Zone in $i$}\\} = \\underset{(\\text{s.e. } ", 
                    sprintf('%0.2f', summary(reg_tau)$coefficients[1,2]), ")}{", 
                    sprintf('%0.2f', summary(reg_tau)$coefficients[1,1]), "} + \\underset{(\\text{s.e. } ", 
                    sprintf('%0.2f', summary(reg_tau)$coefficients[2,2]), ")}{", 
                    sprintf('%0.2f', summary(reg_tau)$coefficients[2,1]), "}\\tau_i + \\epsilon_{i}")
    cat(s_result, file = paste0("Figures/reg_lpm_string_", name, ".tex"))


    s_result_2 = paste0("\\mathbbm{1}\\{\\text{Opportunity Zone in $i$}\\} = \\underset{(\\text{s.e. } ", 
                    sprintf('%0.2f', summary(reg_tau2)$coefficients[1,2]), ")}{", 
                    sprintf('%0.2f', summary(reg_tau2)$coefficients[1,1]), "} + \\underset{(\\text{s.e. } ", 
                    sprintf('%0.2f', summary(reg_tau2)$coefficients[2,2]), ")}{", 
                    sprintf('%0.2f', summary(reg_tau2)$coefficients[2,1]), "}\\tau_i - \\underset{(\\text{s.e. } ",
                    sprintf('%0.2f', summary(reg_tau2)$coefficients[3,2]), ")}{", 
                    sprintf('%0.2f', abs(summary(reg_tau2)$coefficients[3,1])), "}A_i + ", " \\epsilon_{i}")
    cat(s_result_2, file = paste0("Figures/reg_lpm_string2_", name, ".tex"))

    iterbucket = list(logs = c("del_w", "del_L", "del_R", "del_rent"), 
                      levels = c("del_w_actual", "del_L_actual", "del_R_actual", "del_rent_actual"))
    bucketbyfunc = function(bucketvar, bucketvar_name) {
        plotwages = eq_wages %>%
            mutate(del_w = log(w_cf/w_baseline),
                   del_w_actual = w_cf - w_baseline) %>%
            select(W_GEOID, m, del_w, del_w_actual, oppzone) %>%
            left_join(od_join_2 %>% distinct(W_GEOID, .keep_all = TRUE) %>% select(W_GEOID, w_geometry),
                    by = "W_GEOID")

        plotwages = plotwages %>%
            group_by(m) %>%
            mutate(quintile = ntile(.data[[bucketvar[1]]], 5)) %>%
            group_by(m, quintile) %>%
            mutate(min = sprintf('%0.5f', min(del_w, na.rm = TRUE)),
                max = sprintf('%0.5f', max(del_w, na.rm = TRUE)), 
                label = paste0("[", min, ", ", max, "]")) %>%
            ungroup() %>%
            group_by(m) %>%
            mutate(label = factor(label, levels = unique(label)[order(unique(as.numeric(min)))])) %>%
            ungroup()

            
        Rjm = Rjm %>%
            mutate(quintile = ntile(.data[[bucketvar[4]]], 5)) %>%
            group_by(quintile) %>%
            mutate(min = min(del_rent, na.rm = TRUE),
                   max = max(del_rent, na.rm = TRUE),
                   label = paste0("[", sprintf('%0.4f', min), ", ", sprintf('%0.4f', max), "]")) %>%
            ungroup() %>%
            mutate(label = factor(label, levels = unique(label)[order(unique(min))]))

        p1 = ggplot(data = plotwages %>% filter(m == "S"), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            scale_fill_manual(values = brewer.pal(5, "Reds")) +
            labs(fill = "Change in wage", title = "Skilled") + 
            theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.1, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 

        p2 = ggplot(data = plotwages %>% filter(m == "U"), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            scale_fill_manual(values = brewer.pal(5, "Reds")) + 
            labs(fill = "Change in wage", title = "Unskilled") + 
            theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.1, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 

        plotL = eq_wages %>%
            mutate(del_L = log(L_cf /L_baseline),
                   del_L_actual = L_cf - L_baseline) %>%
            select(W_GEOID, m, del_L, del_L_actual, oppzone) %>%
            left_join(od_join_2 %>% distinct(W_GEOID, .keep_all = TRUE) %>% select(W_GEOID, w_geometry),
                    by = "W_GEOID")

        plotL = plotL %>%
            group_by(m) %>%
            mutate(quintile = ntile(.data[[bucketvar[2]]], 5)) %>%
            group_by(m, quintile) %>%
            mutate(min = sprintf('%0.4f', min(del_L, na.rm = TRUE)),
                max = sprintf('%0.4f', max(del_L, na.rm = TRUE)), 
                label = paste0("[", min, ", ", max, "]")) %>%
            ungroup() %>%
            group_by(m) %>%
            mutate(label = factor(label, levels = unique(label)[order(unique(as.numeric(min)))])) %>%
            ungroup()

        p3 = ggplot(data = plotL %>% filter(m == "S"), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
        scale_fill_manual(values = brewer.pal(5, "Reds")) + 
        labs(fill = "Change in employment") + 
        theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.12, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 

        p4 = ggplot(data = plotL %>% filter(m == "U"), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            scale_fill_manual(values = brewer.pal(5, "Reds")) +
        labs(fill = "Change in employment") + 
        theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.12, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 
        cf_w = plot_grid(p1, p2, p3, p4, ncol = 2)
        save_plot(paste0("Figures/w_cf_", name, "_", bucketvar_name, ".pdf"), cf_w, base_height = 5)

        plotwages_oppzone = plotwages %>%
            mutate(label = as.character(oppzone),
                label = ifelse(!oppzone, NA, label)) %>%
            group_by(m, oppzone) %>%
            mutate(quintile = ntile(.data[[bucketvar[1]]], 5)) %>%
            group_by(m, oppzone, quintile) %>%
            mutate(min = min(del_w, na.rm = TRUE),
                max = max(del_w, na.rm = TRUE)) %>%
            group_by(m, oppzone) %>%
            mutate(label = paste0("[", sprintf('%0.4f', min), ", ", sprintf('%0.4f', max), "]"),
                label = factor(label, levels = unique(label)[order(unique(min))])) %>%
            ungroup()

        p5 = ggplot(data = plotwages_oppzone %>% filter(m == "S") %>% filter(oppzone), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            geom_sf(data = plotwages_oppzone %>% filter(!oppzone) %>% filter(m == "S") %>% mutate(label = NA),
                    mapping = aes(geometry = w_geometry, fill = label), color = NA) +
            scale_fill_manual(values = brewer.pal(5, "Reds"), 
                            breaks = sort(unique(plotwages_oppzone$label[plotwages_oppzone$m == "S" & plotwages_oppzone$oppzone ]))) +
            labs(fill = "Change in wage", title = "Skilled") + 
            theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.1, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 
        

        plotL_oppzone = plotL %>%
            mutate(label = as.character(oppzone),
                label = ifelse(!oppzone, NA, label)) %>%
            group_by(m, oppzone) %>%
            mutate(quintile = ntile(.data[[bucketvar[2]]], 5)) %>%
            group_by(m, oppzone, quintile) %>%
            mutate(min = min(del_L, na.rm = TRUE),
                max = max(del_L, na.rm = TRUE)) %>%
            group_by(m, oppzone) %>%
            mutate(label = paste0("[", sprintf('%0.3f', min), ", ", sprintf('%0.3f', max), "]"),
                label = factor(label, levels = unique(label)[order(unique(min))])) %>%
            ungroup()


        p6 = ggplot(data = plotwages_oppzone %>% filter(m == "U") %>% filter(oppzone), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            geom_sf(data = plotwages_oppzone %>% filter(!oppzone) %>% filter(m == "U") %>% mutate(label = NA),
                    mapping = aes(geometry = w_geometry, fill = label), color = NA) +
            scale_fill_manual(values = brewer.pal(5, "Reds"),
                            breaks = sort(unique(plotwages_oppzone$label[plotwages_oppzone$m == "U" & plotL_oppzone$oppzone ]))) +
            labs(fill = "Change in wage", title = "Unskilled") + 
            theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.1, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 

        p7 = ggplot(data = plotL_oppzone %>% filter(m == "S") %>% filter(oppzone), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            geom_sf(data = plotL_oppzone %>% filter(!oppzone) %>% filter(m == "S") %>% mutate(label = NA),
                    mapping = aes(geometry = w_geometry, fill = label), color = NA) +
            scale_fill_manual(values = brewer.pal(5, "Reds"), 
                            breaks = sort(unique(plotL_oppzone$label[plotL_oppzone$m == "S" & plotL_oppzone$oppzone ]))) +
            labs(fill = "Change in employment", title = "") + 
            theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.1, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 

        p8 = ggplot(data = plotL_oppzone %>% filter(m == "U") %>% filter(oppzone), mapping = aes(geometry = w_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            geom_sf(data = plotL_oppzone %>% filter(!oppzone) %>% filter(m == "U") %>% mutate(label = NA),
                    mapping = aes(geometry = w_geometry, fill = label), color = NA) +
            scale_fill_manual(values = brewer.pal(5, "Reds"),
                            breaks = sort(unique(plotL_oppzone$label[plotL_oppzone$m == "U" & plotL_oppzone$oppzone ]))) +
            labs(fill = "Change in employment", title = "") + 
            theme_void() +
        theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.1, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 
        w_cf_oppzone = plot_grid(p5,p6,p7,p8, ncol = 2)

        save_plot(paste0("Figures/w_cf_oppzone_", name, "_", bucketvar_name, ".pdf"), w_cf_oppzone, base_height = 5)

        plotR = eq_res %>%
            mutate(del_R = log(R_cf/ R_baseline),
                   del_R_actual = R_cf - R_baseline) %>%
            left_join(od_join_2 %>% distinct(R_GEOID, .keep_all = TRUE),
                    by = "R_GEOID") %>%
            left_join(opportunity_zones, by = c("R_GEOID" = "W_GEOID")) %>%
            mutate(oppzone = ifelse(is.na(oppzone), FALSE, oppzone)) %>%
            select(r_geometry, m, R_GEOID, del_R, del_R_actual, oppzone) %>%
            group_by(m) %>%
            mutate(quintile = ntile(.data[[bucketvar[3]]], 5)) %>%
            ungroup() %>%
            group_by(m, quintile) %>%
            mutate(min = min(del_R, na.rm = TRUE),
                max = max(del_R, na.rm = TRUE)) %>%
            ungroup() %>%
            group_by(m) %>%
            mutate(label = paste0("[", sprintf('%0.4f', min), ", ", sprintf('%0.4f', max), "]"),
                label = factor(label, levels = unique(label)[order(unique(min))])) %>%
            ungroup()

        p9 = ggplot(data = plotR %>% filter(m == "S"), mapping = aes(geometry = r_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            scale_fill_manual(values = brewer.pal(5, "Reds")) + 
            labs(fill = "Change in pop.", title = "Skilled") + 
            theme_void() +
            theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.12, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 

        p10 = ggplot(data = plotR %>% filter(m == "U"), mapping = aes(geometry = r_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            scale_fill_manual(values = brewer.pal(5, "Reds")) + 
            labs(fill = "Change in pop.", title = "Unskilled") + 
            theme_void() +
            theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.12, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 

        p11 = ggplot(data = Rjm, mapping = aes(geometry = r_geometry, fill = label)) + 
            geom_sf(color = NA) + 
            scale_fill_manual(values = brewer.pal(5, "Reds")) + 
            labs(fill = "Change in rent") + 
            theme_void() +
            theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                plot.title = element_text(size = 8, hjust = 0.5),
                legend.position = c(0.12, 0.4),
                legend.text = element_text(size = 6),
                legend.title = element_text(size = 6)) 


        r_cf = plot_grid(p9,p10, ncol = 2)

        save_plot(paste0("Figures/r_cf_", name, "_", bucketvar_name, ".pdf"), r_cf, base_height = 5*0.75)
        save_plot(paste0("Figures/rent_cf_", name, "_", bucketvar_name, ".pdf"), p11, base_height = 5*0.55)


        # write_csv(plotwages, paste0("Data/plotwages_cf_", name, "_", bucketvar_name, ".csv"))
        # write_csv(plotL, paste0("Data/plot_L_cf_", name, "_", bucketvar_name, ".csv"))
        # write_csv(plotR, paste0("Data/plotR_", name, "_", bucketvar_name, ".csv")) 

    }
    for (ii in 1:length(iterbucket)) {
        bucketbyfunc(iterbucket[[ii]], names(iterbucket)[ii])
    }


    return(invisible(NULL))
}
iterlist = list(change_tau_list, names(change_tau_list), c(0, 0.1, 0.05))
for (ii in 1:length(iterlist)) {
    iter_model(iterlist[[1]][[ii]], iterlist[[2]][ii], iterlist[[3]][ii]) 
}


