library(tidyverse)
library(sf)
library(haven)

rm(list=ls())

#Reading in dataset of all crops

output <-  read_dta("Main_data/final_output_LUMS_Paper_1_updated.dta")%>% 
  select(year, province = province_data4pakistan, district = district_data4pakistan, crop, production_tonnes) %>% 
  arrange(year, district)  %>% 
  filter(district != "") %>% 
  pivot_wider(-province, names_from = district, values_from = production_tonnes) %>% 
  mutate(Awaran= NA,
         'Bajaur Agency' = NA,
         Barkhan = NA,
         Chiniot = NA,
         'FR Bannu' = NA,
         'FR Dera Ismail Khan' = NA,
         'FR Kohat' = NA,
         'FR Lakki Marwat' = NA,
         'FR Peshawar' = NA,
         'FR Tank'  = NA,
         Ghotki = NA,
         # Gwadar = NA,       #################################
         Hangu = NA,
         Harnai = NA,
         Islamabad = NA,
         Jamshoro = NA,
         'Jhal Magsi' = NA,
         Kashmore  = NA,
         'Khyber Agency' = NA,
         'Killa Abdullah' = NA,
         'Kurram Agency' = NA,
         Lehri= NA,
         'Lower Dir'  = NA,
         Mastung = NA,
         Matiari  = NA,
         # 'Mirpur Khas'  = NA,          ###########################
         Musakhel = NA,
         'Nankana Sahib'  = NA,
         'North Waziristan Agency' = NA,
         Nushki = NA,
         'Orakzai Agency' = NA,
         'Qambar Shahdadkot' = NA,
         Shangla = NA,
         Sheerani = NA,
         Sohbatpur = NA,
         'South Waziristan Agency' = NA,
         Sujawal = NA,
         'Tando Allah Yar' = NA,
         'Tando Muhammad Khan' = NA,
         'Tor Ghar' = NA,
         Umerkot = NA,
         'Upper Dir' = NA,
         Washuk = NA,
         'Mohmand Agency' = NA) %>%
  # janitor::clean_names() %>% 
  pivot_longer(Abbottabad:'Mohmand Agency',
               names_to = "district" , 
               values_to = "value") %>% 
  mutate(value = value / 1000,
         domain = "Crop Output") %>% 
  filter(crop != "Sunhemp") %>% #Since unavailable foe 2018 and 2019, which makes blank map
  arrange(crop, district)

  #Reading in data of Total Factor Productivity
tfp <- read_dta("Main_data/TFP_decomposition_updated.dta") %>% 
  select(year, district = district_data4pakistan, TFP) %>% 
  filter(district != "") %>%   # Filtering out Dir, since we have upper or lower dir in shapefile, we don't know which one is it
  pivot_wider(names_from = district, values_from = TFP) %>% 
  mutate(Awaran= NA,
         'Bajaur Agency' = NA,
         Barkhan = NA,
         Chiniot = NA,
         'FR Bannu' = NA,
         'FR Dera Ismail Khan' = NA,
         'FR Kohat' = NA,
         'FR Lakki Marwat' = NA,
         'FR Peshawar' = NA,
         'FR Tank'  = NA,
         Ghotki = NA,
         # Gwadar = NA,                     #########################################
         Hangu = NA,
         Harnai = NA,
         Islamabad = NA,
         Jamshoro = NA,
         'Jhal Magsi' = NA,
         Kashmore  = NA,
         'Khyber Agency' = NA,
         'Killa Abdullah' = NA,
         'Kurram Agency' = NA,
         Lehri= NA,
         'Lower Dir'  = NA,
         Mastung = NA,
         Matiari  = NA,
         # 'Mirpur Khas'  = NA,             ########################
         'Mohmand Agency' = NA,
         Musakhel = NA,
         'Nankana Sahib'  = NA,
         'North Waziristan Agency' = NA,
         Nushki = NA,
         'Orakzai Agency' = NA,
         'Qambar Shahdadkot' = NA,
         Shangla = NA,
         Sheerani = NA,
         Sohbatpur = NA,
         'South Waziristan Agency' = NA,
         Sujawal = NA,
         'Tando Allah Yar' = NA,
         'Tando Muhammad Khan' = NA,
         'Tor Ghar' = NA,
         Umerkot = NA,
         'Upper Dir' = NA,
         Washuk = NA
        ) %>% 
  pivot_longer(Abbottabad:Washuk,
               names_to = "district" , 
               values_to = "value") %>% 
  mutate(domain = "Total Factor Productivity") %>% 
  arrange(year, district)

#Reading in data of input/output
land_productivity <- read_dta("Main_data/Aggregate_Input_Output_updated.dta") %>% 
  select(year, district = district_data4pakistan, total_value, Area_) %>% 
  filter(district != "") %>%   # Filtering out Dir, since we have upper or lower dir in shapefile, we don't know which one is it
  # mutate(total_value = total_value/1000) %>% 
  mutate(land_productivity = total_value/Area_) %>% 
  select(-total_value, -Area_) %>% 
  pivot_wider(names_from = district, values_from = land_productivity) %>% 
  mutate(Awaran= NA,
         'Bajaur Agency' = NA,
         Barkhan = NA,
         Chiniot = NA,
         'FR Bannu' = NA,
         'FR Dera Ismail Khan' = NA,
         'FR Kohat' = NA,
         'FR Lakki Marwat' = NA,
         'FR Peshawar' = NA,
         'FR Tank'  = NA,
         Ghotki = NA,
         # Gwadar = NA,                     #########################################
         Hangu = NA,
         Harnai = NA,
         Islamabad = NA,
         Jamshoro = NA,
         'Jhal Magsi' = NA,
         Kashmore  = NA,
         'Khyber Agency' = NA,
         'Killa Abdullah' = NA,
         'Kurram Agency' = NA,
         Lehri= NA,
         'Lower Dir'  = NA,
         Mastung = NA,
         Matiari  = NA,
         # 'Mirpur Khas'  = NA,             ########################
         'Mohmand Agency' = NA,
         Musakhel = NA,
         'Nankana Sahib'  = NA,
         'North Waziristan Agency' = NA,
         Nushki = NA,
         'Orakzai Agency' = NA,
         'Qambar Shahdadkot' = NA,
         Shangla = NA,
         Sheerani = NA,
         Sohbatpur = NA,
         'South Waziristan Agency' = NA,
         Sujawal = NA,
         'Tando Allah Yar' = NA,
         'Tando Muhammad Khan' = NA,
         'Tor Ghar' = NA,
         Umerkot = NA,
         'Upper Dir' = NA,
         Washuk = NA
  ) %>% 
  pivot_longer(Abbottabad:Washuk,
               names_to = "district" , 
               values_to = "value") %>% 
  mutate(domain = "Land Productivity") %>% 
  arrange(year, district)

labor_productivity <- read_dta("Main_data/Aggregate_Input_Output_updated.dta") %>% 
  select(year, district = district_data4pakistan, total_value, yhours_ ) %>% 
  filter(district != "") %>%   # Filtering out Dir, since we have upper or lower dir in shapefile, we don't know which one is it
  # mutate(yhours_ = yhours * 1000) %>% 
  mutate(labor_productivity = total_value/yhours_) %>% 
  select(-total_value, -yhours_) %>% 
  pivot_wider(names_from = district, values_from = labor_productivity) %>% 
  mutate(Awaran= NA,
         'Bajaur Agency' = NA,
         Barkhan = NA,
         Chiniot = NA,
         'FR Bannu' = NA,
         'FR Dera Ismail Khan' = NA,
         'FR Kohat' = NA,
         'FR Lakki Marwat' = NA,
         'FR Peshawar' = NA,
         'FR Tank'  = NA,
         Ghotki = NA,
         # Gwadar = NA,                     #########################################
         Hangu = NA,
         Harnai = NA,
         Islamabad = NA,
         Jamshoro = NA,
         'Jhal Magsi' = NA,
         Kashmore  = NA,
         'Khyber Agency' = NA,
         'Killa Abdullah' = NA,
         'Kurram Agency' = NA,
         Lehri= NA,
         'Lower Dir'  = NA,
         Mastung = NA,
         Matiari  = NA,
         # 'Mirpur Khas'  = NA,             ########################
         'Mohmand Agency' = NA,
         Musakhel = NA,
         'Nankana Sahib'  = NA,
         'North Waziristan Agency' = NA,
         Nushki = NA,
         'Orakzai Agency' = NA,
         'Qambar Shahdadkot' = NA,
         Shangla = NA,
         Sheerani = NA,
         Sohbatpur = NA,
         'South Waziristan Agency' = NA,
         Sujawal = NA,
         'Tando Allah Yar' = NA,
         'Tando Muhammad Khan' = NA,
         'Tor Ghar' = NA,
         Umerkot = NA,
         'Upper Dir' = NA,
         Washuk = NA
  ) %>% 
  pivot_longer(Abbottabad:Washuk,
               names_to = "district" , 
               values_to = "value") %>% 
  mutate(domain = "Labor Productivity") %>% 
  arrange(year, district)

#Reading in Data 
# data <- read_dta("District_Agri_Output.dta") %>% 
#   select(year, province = province_data4pakistan, district = district_data4pakistan, crop, production_tonnes) %>% 
#   arrange(year, district) %>% 
#   filter(district != "") %>% #Dropping those which aren't matched (e.g. Dir which is upper dir nd lower dir in daat4pak)
#   # filter(year == 2019) %>%   
#   # filter(crop == "Cotton") %>% View()
#   pivot_wider(-province, names_from = district, values_from = production_tonnes) %>% 
#   mutate(Awaran= NA,
#          'Bajaur Agency' = NA,
#          Barkhan = NA,
#          Chiniot = NA,
#          'FR Bannu' = NA,
#          'FR Dera Ismail Khan' = NA,
#         'FR Kohat' = NA,
#         'FR Lakki Marwat' = NA,
#         'FR Peshawar' = NA,
#         'FR Tank'  = NA,
#         Ghotki = NA,
#         Gwadar = NA,
#         Hangu = NA,
#         Harnai = NA,
#         Islamabad = NA,
#         Jamshoro = NA,
#        'Jhal Magsi' = NA,
#        Kashmore  = NA,
#        'Khyber Agency' = NA,
#        'Killa Abdullah' = NA,
#        'Kurram Agency' = NA,
#        Lehri= NA,
#       'Lower Dir'  = NA,
#       Mastung = NA,
#       Matiari  = NA,
#      'Mirpur Khas'  = NA,
#      Musakhel = NA,
#      'Nankana Sahib'  = NA,
#       'North Waziristan Agency' = NA,
#      Nushki = NA,
#       'Orakzai Agency' = NA,
#       'Qambar Shahdadkot' = NA,
#      Shangla = NA,
#      Sheerani = NA,
#      Sohbatpur = NA,
#     'South Waziristan Agency' = NA,
#     Sujawal = NA,
#     'Tando Allah Yar' = NA,
#     'Tando Muhammad Khan' = NA,
#     'Tor Ghar' = NA,
#     Umerkot = NA,
#     'Upper Dir' = NA,
#     Washuk = NA,
#     'Mohmand Agency' = NA) %>%
#     # janitor::clean_names() %>% 
#   pivot_longer(Abbottabad:'Mohmand Agency',
#                names_to = "district" , 
#                values_to = "production") %>% 
#    mutate(production = production / 1000) %>% 
#    arrange(crop, district)

#gini change and land ownership prop change from 2010-2019
gini <- rio::import_list("Main_data/gini_change_for_dashboard.xlsx")

gini_pchange <- gini$aghhdswithland1019_change%>% 
  select(district = district_data4pakistan, ppoint_change) %>% 
  arrange(district) %>% 
  pivot_wider(names_from = district, values_from = ppoint_change) %>% 
  mutate('Bajaur Agency'= NA,
         Chagai = NA,
         'FR Bannu' = NA,
         'FR Dera Ismail Khan'= NA,
         'FR Kohat' = NA,
         'FR Lakki Marwat'= NA,
         'FR Peshawar' = NA,
         'FR Tank'= NA,
         'Khyber Agency'= NA,
         'Kurram Agency'= NA,
         'Lahore' = NA,
         'Lehri' = NA,
         'Mohmand Agency' = NA,
         'North Waziristan Agency'= NA,
         'Orakzai Agency' = NA,
         'Panjgur' = NA, 
         'Sohbatpur' = NA,
         'South Waziristan Agency' = NA,
         'Sujawal' = NA,
         'Tor Ghar'= NA,
         'Jhal Magsi' = NA,
         'Musakhel' = NA,
         'Zhob'= NA
  ) %>% 
  pivot_longer(Abbottabad:Zhob,
               names_to = "district", values_to = "value") %>% 
  mutate(domain = "Land Ownership Percentage Point Change for Ag-employed HHDs 2010-2019") %>% 
  arrange(district)

gini_allhhds <- gini$gini1019_all_hhds %>% 
  select(district = district_data4pakistan, gini_change_1019) %>% 
  arrange(district) %>% 
  pivot_wider(names_from = district, values_from = gini_change_1019) %>% 
  mutate('Bajaur Agency'= NA,
         Chagai = NA,
         'FR Bannu' = NA,
         'FR Dera Ismail Khan'= NA,
         'FR Kohat' = NA,
         'FR Lakki Marwat'= NA,
         'FR Peshawar' = NA,
         'FR Tank'= NA,
         'Khyber Agency'= NA,
         'Kurram Agency'= NA,
         'Lahore' = NA,
         'Lehri' = NA,
         'Mohmand Agency' = NA,
         'North Waziristan Agency'= NA,
         'Orakzai Agency' = NA,
         'Panjgur' = NA, 
         'Sohbatpur' = NA,
         'South Waziristan Agency' = NA,
         'Sujawal' = NA,
         'Tor Ghar'= NA,
         'Jhal Magsi' = NA,
         'Musakhel' = NA,
         'Zhob'= NA
  ) %>% 
  pivot_longer(Abbottabad:Zhob,
               names_to = "district", values_to = "value") %>% 
  mutate(domain = "Land Gini Coeff. Change for All HHds 2010-2019") %>% 
  arrange(district)

gini_aghhds <- gini$gini1019_ag_hhds %>%
  select(district = district_data4pakistan, gini_change_1019) %>% 
  arrange(district) %>% 
  pivot_wider(names_from = district, values_from = gini_change_1019) %>% 
  mutate('Bajaur Agency'= NA,
         Chagai = NA,
         'FR Bannu' = NA,
         'FR Dera Ismail Khan'= NA,
         'FR Kohat' = NA,
         'FR Lakki Marwat'= NA,
         'FR Peshawar' = NA,
         'FR Tank'= NA,
         'Khyber Agency'= NA,
         'Kurram Agency'= NA,
         'Lahore' = NA,
         'Lehri' = NA,
         'Mohmand Agency' = NA,
         'North Waziristan Agency'= NA,
         'Orakzai Agency' = NA,
         'Panjgur' = NA, 
         'Sohbatpur' = NA,
         'South Waziristan Agency' = NA,
         'Sujawal' = NA,
         'Tor Ghar'= NA,
         'Jhal Magsi' = NA,
         'Musakhel' = NA,
         'Zhob'= NA
         ) %>% 
  pivot_longer(Abbottabad:Zhob,
               names_to = "district", values_to = "value") %>% 
  mutate(domain = "Land Gini Coeff. Change for Purely Ag-employed HHds 2010-2019") %>% 
  arrange(district)


data <- bind_rows(output,
                  tfp,
                  land_productivity,
                  labor_productivity)

data1 <- bind_rows(gini_allhhds,
                   gini_aghhds,
                   gini_pchange)

data %>% write_rds("Crop_Output/data/data.RDS")
data1 %>% write_rds("Crop_output/data/data1.RDS")
#   filter(crop == "Cotton") %>%
#   distinct(district) %>% arrange(district) %>%  View()


district_data <- unique(data$district)

#Reading in Shape file
pak_shp <- read_sf("Main_data/shapefile_district/pakistan_indicators.shp") %>% 
  janitor::clean_names() %>% 
  filter(year== 2018) %>% 
  select(province, district, geometry) %>% 
  st_transform(crs= 4326) %>% 
  arrange(district)
  
pak_shp %>% saveRDS("Crop_Output/data/pak_shp.RDS")

district_shp <- unique(pak_shp$district) 
district_data <- unique(gini_pchange$district)
is.element(district_shp, district_data)  #all matched
is.element(district_data, district_shp)  #all matched

x1 <- pak_shp %>% as_tibble() %>% select(-geometry, -province, district)
