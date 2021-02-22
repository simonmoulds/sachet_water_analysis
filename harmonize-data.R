## Author : Simon Moulds
## Date   : November 2020

library(foreign)
library(rgdal)
library(tidyverse)
library(magrittr)
library(sf)

options(stringsAsFactors = FALSE)

## ####################################################### ##
## ####################################################### ##

## Define lookup tables for recoding data

## ####################################################### ##
## ####################################################### ##

## Numeric region codes to names
rgn_codes = data.frame(
    Region=c(
        "Ashanti", "Brong Ahafo", "Central",
        "Eastern", "Greater Accra", "Northern",
        "Upper East", "Upper West", "Volta", "Western"
    ),
    ID=c(6, 7, 2, 5, 3, 8, 9, 10, 4, 1)
)
saveRDS(rgn_codes, "data/map_rgn_name_to_code.rds")

map_drinking_water_source_to_jmp = read.table(header=TRUE, text="
WATER_DRINKING WATER_DRINKING_7
0  0
1  16
2  16
3  17
4  19
5  19
6  21
7  20
8  20
9  18
10 22
11 22
12 21
13 21
14 21
15 22
")
saveRDS(map_drinking_water_source_to_jmp, "data/map_drinking_water_source_to_jmp.rds")

map_hh_water_source_to_jmp =
    map_drinking_water_source_to_jmp %>%
    setNames(c("WATER_HOUSEHOLD", "WATER_HOUSEHOLD_7"))
saveRDS(map_hh_water_source_to_jmp, "data/map_hh_water_source_to_jmp.rds")

## This lookup table is necessary because the GLSS surveys divide
## "pipe-borne outside dwelling" into two parts -- (i) on compound (2);
## (ii) from neighbours house (3) -- whereas the census uses the same
## category for both. 
map_glss_to_census_drinking_water_source = read.table(header=TRUE, text="
s7dq1a1 WATER_DRINKING
0  0
1  1
2  2
3  2
4  3
5  4
6  5
7  6
8  7
9  8
10 9
11 10
12 11
13 12
14 13
15 14
16 15
")
saveRDS(map_glss_to_census_drinking_water_source, "data/map_glss_to_census_drinking_water_source.rds")

map_glss_to_census_hh_water_source =
    map_glss_to_census_drinking_water_source %>%
    setNames(c("s7dq1a2", "WATER_HOUSEHOLD"))
saveRDS(map_glss_to_census_hh_water_source, "data/map_glss_to_census_hh_water_source.rds")

## Descriptions of the various household factors used to
## understand development trends
hh_factors = read.table(header=TRUE, text="
KEY                IMPROVED CATEGORY                                       DESCRIPTION
'HH_DWELLING_1'    TRUE     'Type of dwelling'                             'House, dwelling, flat'
'HH_DWELLING_4'    FALSE    'Type of dwelling'                             'Compound, hut, tent, kiosk, business, other'
'HH_LIGHTING_1'    TRUE     'Source of lighting'                           'Electric'
'HH_LIGHTING_2'    FALSE    'Source of lighting'                           'Gas, wood, other'
'HH_TOILET_1'      TRUE     'Type of toilet'                               'WC'
'HH_TOILET_2'      TRUE     'Type of toilet'                               'KVIP'
'HH_TOILET_3'      FALSE    'Type of toilet'                               'Pit latrine, bucket, other, none'
'HH_FUEL_1'        TRUE     'Type of cooking fuel'                         'Gas, electricity or kerosene'
'HH_FUEL_2'        FALSE    'Type of cooking fuel'                         'Wood, other'
'HH_SOLIDWASTE_1'  TRUE     'Solid waste disposal method'                  'Collection service'
'HH_SOLIDWASTE_4'  FALSE    'Solid waste disposal method'                  'Dump, Burnt, buried, dumped'
'HH_LIQUIDWASTE_1' TRUE     'Liquid waste disposal method'                 'Sewage network'
'HH_LIQUIDWASTE_2' FALSE    'Liquid waste disposal method'                 'Other'
'HH_COMPUTER_1'    TRUE     'Access to computer (desktop or laptop)'       'Has access'
'HH_COMPUTER_2'    FALSE    'Access to computer (desktop or laptop)'       'Does not have access'
'HH_MOBILE_1'      TRUE     'Access to mobile phone'                       'Has access'
'HH_MOBILE_2'      FALSE    'Access to mobile phone'                       'Does not have access'
'HH_HHWATER_1'     TRUE     'Type of water used for non-drinking purposes' 'Improved'
'HH_HHWATER_2'     FALSE    'Type of water used for non-drinking purposes' 'Unimproved'
'HH_OWNERSHIP_1'   TRUE     'Ownership of dwelling'                        'Owned by household'
'HH_OWNERSHIP_2'   FALSE    'Ownership of dwelling'                        'Renting, rent-free, perching, other'
'HH_HEAD_GENDER_1' FALSE    'Gender of household head'                     'Male'
'HH_HEAD_GENDER_2' FALSE    'Gender of household head'                     'Female'
'HH_GEO_1'         FALSE    'Urban or rural'                               'Urban'
'HH_GEO_2'         FALSE    'Urban or rural'                               'Rural'
")
saveRDS(hh_factors, "data/hh_factors.rds")

hh_simple_categories = read.table(header=TRUE, text="
CATEGORY                                       SHORT_CATEGORY
'Type of dwelling'                             'Housing'
'Source of lighting'                           'Lighting'
'Type of toilet'                               'Toilet'
'Type of cooking fuel'                         'Cooking fuel'
'Solid waste disposal method'                  'Solid waste'
'Liquid waste disposal method'                 'Liquid waste'
'Access to computer (desktop or laptop)'       'Technology'
'Access to mobile phone'                       'Technology'
'Type of water used for non-drinking purposes' 'Non-drinking water'
'Ownership of dwelling'                        'Home ownership'
'Gender of household head'                     'Household head'
'Urban or rural'                               'Urban or rural'
")
saveRDS(hh_simple_categories, "data/hh_simple_categories.rds")

## Numeric codes of improved drinking water sources
## in the two datasets: census (2010), GLSS (2013, 2017)
census2010_drink_improved_cats = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
census2010_household_improved_cats = c(1, 2, 3, 4, 5, 6, 7, 8)
glss_drink_improved_cats = c(1,2,3,4,5,6,7,8,9,10,11)

## ####################################################### ##
## ####################################################### ##

## Load census 2010 10% dataset

## ####################################################### ##
## ####################################################### ##

## Data downloaded from:
## http://www.webdeploy.statsghana.gov.gh/nada/index.php/catalog/51

## ## Read dataset
## ds_census2010_pp = read.csv(
##     "data-raw/2010PHC-216D-10_/2010PHC-216D-10_Population@EA.csv"
## ) %>% as_tibble

ds_census2010_pp = read.csv(
    "data-raw/2010PHC-216D-10%/2010PHC-216D-10_Population@EA.csv"
) %>% as_tibble

ds_census2010_hh = read.csv(
    "data-raw/2010PHC-216D-10%/2010PHC-216D-10_Housing@EA.csv"
) %>% as_tibble

## Some datapoints are given to the population-level. Here we recode
## them to the household level for consistency with the other datasets
## used in the analysis:

## (i) Internet access
tmp = ds_census2010_pp %>%
    dplyr::select(HHID, INTERNET) %>% 
    mutate(INTERNET=replace(INTERNET, INTERNET %in% c(2,3), 0)) %>% 
    group_by(HHID) %>%
    summarize(INTERNET=sum(INTERNET)) %>%
    ungroup %>%
    mutate(INTERNET=replace(INTERNET, INTERNET!=0, 1))
ds_census2010_hh %<>% left_join(tmp, by="HHID")

## (ii) Mobile phone usage
tmp = ds_census2010_pp %>%
    dplyr::select(HHID, MOBILE_PHONE) %>%
    mutate(MOBILE_PHONE=replace(MOBILE_PHONE, MOBILE_PHONE %in% c(2,3), 0)) %>%
    group_by(HHID) %>%
    summarize(MOBILE_PHONE=sum(MOBILE_PHONE)) %>%
    ungroup %>%
    mutate(MOBILE_PHONE=replace(MOBILE_PHONE, MOBILE_PHONE!=0, 1))
ds_census2010_hh %<>% left_join(tmp, by="HHID")

## Categorise household water preference to improved/non-improved
ds_census2010_hh %<>%
    ## mutate(WATER_DRINKING = replace_na(WATER_DRINKING, 0)) %>%
    ## mutate(WATER_SOURCE = replace_na(WATER_SOURCE, 0)) %>% 
    mutate(
        WATER_DRINKING_IMPROVED =
            WATER_DRINKING %in% census2010_drink_improved_cats
    ) %>%
    mutate(
        WATER_HOUSEHOLD_IMPROVED =
            WATER_SOURCE %in% census2010_household_improved_cats
    )   

## Select the columns which we will use to quantify development trends
cols = c(
    'HHID', 'REGION', 'TOILET', 'COOKING_FUEL',
    'WATER_DRINKING', 'WATER_SOURCE',
    'CROP_FARMING', 'TREE_GROWING',
    'LIVE_STOCKRAISING', 'FISHING', 'DWELLING', 'LIGHTING',
    'DESKTOP_COMPUTER', 'LIQUID_WASTE', 'RUBBISH',
    'FIXED_PHONELINE', 'INTERNET', 'MOBILE_PHONE', 'TENURE',
    'WATER_DRINKING_IMPROVED', 'WATER_HOUSEHOLD_IMPROVED',
    'HEADSEX', 'URBAN_RURAL',
    'REGDIST',
    'RESTYPE'
)

## NB applying complete.cases() happens to remove all values with RESTYPE != 1
ds =
    ds_census2010_hh %>%
    dplyr::select(cols) %>% 
    filter(complete.cases(.))

## Check only households are selected:
table(ds$RESTYPE)

## To account for entries which were dropped when NAs are dropped,
## entries are re-balanced on the district level
complete_rgn_counts = ds_census2010_hh[["REGDIST"]] %>% table
sub_rgn_counts = ds[["REGDIST"]] %>% table
scaling_factor = (complete_rgn_counts / sub_rgn_counts)

## Create scale factor for each region (x10 because 10% census)

temp_rgn_scale =
    (10. * scaling_factor) %>%
    as.data.frame %>%
    setNames(c("REGDIST", "WH")) %>%
    mutate(REGDIST=as.numeric(as.character(REGDIST)))
    ## mutate(REGDIST=as.numeric(REGDIST))
ds %<>% left_join(temp_rgn_scale, by="REGDIST")

## Assign constant and year, then convert numeric codes to boolean
## variables which can be directly compared between the datasets:
ds[['const']] = 1
ds[['YEAR']] = 2010
ds[['HH_DRINK_SACHET']] = ds[['WATER_DRINKING']] %in% c(9)
ds[['HH_DWELLING_1']] = ds[['DWELLING']] %in% c(1,2,3)
ds[['HH_DWELLING_2']] = ds[['DWELLING']] %in% c(4)
ds[['HH_DWELLING_3']] = !ds[['DWELLING']] %in% c(1,2,3,4)
ds[['HH_DWELLING_4']] = !ds[['DWELLING']] %in% c(1,2,3)
ds[['HH_LIGHTING_1']] = ds[['LIGHTING']] %in% c(1,2)
ds[['HH_LIGHTING_2']] = !ds[['LIGHTING']] %in% c(1,2)
ds[['HH_TOILET_1']] = ds[['TOILET']] %in% c(2)
ds[['HH_TOILET_2']] = ds[['TOILET']] %in% c(4)
ds[['HH_TOILET_3']] = !ds[['TOILET']] %in% c(2,4)
ds[['HH_FUEL_1']] = ds[['COOKING_FUEL']] %in% c(3,4,5)
ds[['HH_FUEL_2']] = !ds[['COOKING_FUEL']] %in% c(3,4,5)
ds[['HH_SOLIDWASTE_1']] = ds[['RUBBISH']] %in% c(1)
ds[['HH_SOLIDWASTE_2']] = ds[['RUBBISH']] %in% c(3,4)
ds[['HH_SOLIDWASTE_3']] = !ds[['RUBBISH']] %in% c(1,3,4)
ds[['HH_SOLIDWASTE_4']] = !ds[['RUBBISH']] %in% c(1)
ds[['HH_LIQUIDWASTE_1']] = ds[['LIQUID_WASTE']] %in% c(1,2,3)
ds[['HH_LIQUIDWASTE_2']] = !ds[['LIQUID_WASTE']] %in% c(1,2,3)
ds[['HH_COMPUTER_1']] = ds[['DESKTOP_COMPUTER']] %in% c(1)
ds[['HH_COMPUTER_2']] = ds[['DESKTOP_COMPUTER']] %in% c(2)
ds[['HH_MOBILE_1']] = ds[['MOBILE_PHONE']] %in% c(1)
ds[['HH_MOBILE_2']] = ds[['MOBILE_PHONE']] %in% c(0)
ds[['HH_HHWATER_1']] = ds[['WATER_HOUSEHOLD_IMPROVED']] %in% c(1)
ds[['HH_HHWATER_2']] = ds[['WATER_HOUSEHOLD_IMPROVED']] %in% c(0)
ds[['HH_OWNERSHIP_1']] = ds[['TENURE']] %in% c(1)
ds[['HH_OWNERSHIP_2']] = !ds[['TENURE']] %in% c(1)
ds[['HH_HEAD_GENDER_1']] = ds[['HEADSEX']] %in% c(1)
ds[['HH_HEAD_GENDER_2']] = ds[['HEADSEX']] %in% c(2)
ds[['HH_GEO_1']] = ds[['URBAN_RURAL']] %in% c(1)
ds[['HH_GEO_2']] = ds[['URBAN_RURAL']] %in% c(2)
ds[['HH_GAMA_1']] = ds[['REGION']] %in% c(3)
ds[['HH_GAMA_2']] = !ds[['REGION']] %in% c(3)

## Convert water types to the seven used in this analysis
ds %<>% left_join(map_drinking_water_source_to_jmp, by="WATER_DRINKING")
ds %<>%
    rename(WATER_HOUSEHOLD = WATER_SOURCE) %>%
    left_join(map_hh_water_source_to_jmp, by="WATER_HOUSEHOLD")

## Distinguish between sachet water drinking when household water
## consumption is (i) and (ii) unimproved
ds %<>%
    mutate(
        WATER_DRINKING_7=replace(
            WATER_DRINKING_7,
            (WATER_DRINKING_7 %in% 18) & (WATER_HOUSEHOLD_IMPROVED),
            17.5
        )
    )

## Combine public and private piped water access, and create
## a separate category for private piped access
ds[['HH_DRINK_PIPED']] = ds[['WATER_DRINKING_7']] %in% c(16, 17)
ds[['HH_DRINK_PIPED_PRIVATE']] = ds[['WATER_DRINKING_7']] %in% c(16)

## Copy temporary data frame back to parent data frame:
ds_census2010 = ds

## ####################################################### ##
## ####################################################### ##

## Process GLSS6 dataset

## ####################################################### ##
## ####################################################### ##

## Data downloaded from:
## http://www.webdeploy.statsghana.gov.gh/nada/index.php/catalog/72.

## Process GLSS6 data. The data comes in multiple files, hence we need to
## load each individually then join together.

ds_glss6_pp =
    read.csv("data-raw/GLSS6_2012-2013/SPSS/AGGREGATES/gha_2013_l.csv") %>%
    as_tibble
ds_glss6_hh =
    read.csv("data-raw/GLSS6_2012-2013/SPSS/AGGREGATES/pov_gha_2013_glss6_updated.csv") %>%
    as_tibble
ds_glss6_hh2 =
    read.csv("data-raw/GLSS6_2012-2013/SPSS/AGGREGATES/gha_2013_h.csv") %>%
    as_tibble

options(stringsAsFactors=TRUE)
ds_glss6_loc_edt =
    read.spss(
        "data-raw/GLSS6_2012-2013/SPSS/PARTA/g6loc_edt.sav",
        to.data.frame=TRUE
    ) %>%
    as_tibble %>%
    mutate(region=as.numeric(region))
options(stringsAsFactors=FALSE)

ds_glss6_pp_sec2c =
    read.csv("data-raw/GLSS6_2012-2013/SPSS/PARTA/sec2c.csv") %>%
    as_tibble
ds_glss6_pp_sec4a =
    read.csv("data-raw/GLSS6_2012-2013/SPSS/PARTA/sec4a.csv") %>%
    as_tibble
ds_glss6_hh_sec6 =
    read.csv("data-raw/GLSS6_2012-2013/SPSS/PARTA/sec6.csv") %>%
    as_tibble
ds_glss6_hh_sec7 =
    read.csv("data-raw/GLSS6_2012-2013/SPSS/PARTA/sec7.csv") %>%
    as_tibble

## As before, some datapoints are given to the population-level.
## Here we recode them to the household level:
ds_glss6_hh[['HHSEX']] = ds_glss6_hh2[['HHSEX']]
ds_glss6_hh[['URBAN_RURAL']] = ds_glss6_hh2[['URBAN_RURAL']]

ds_glss6_loc_edt %<>%
    mutate(district=formatC(district, width=2, flag=0)) %>%
    mutate(region=as.character(region)) %>% 
    unite(district, region, district, sep="", remove=FALSE) %>%
    mutate(district=str_pad(district, width=4, pad=0))

ds_glss6_hh %<>%
    mutate(dist=ds_glss6_loc_edt[['district']]) %>%
    mutate(s6q1=ds_glss6_hh_sec6[['s6q1']])

glss6_sec7_variables = c(
    'HID',
    's7dq16a','s7dq13','s7aq1','s7dq11','s7eq1c',
    's7eq1e','s7dq14a','s7dq14b','s7eq1a','s7eq3d',
    's7eq1b','s7dq16b','s7bq1','s7dq1a1','s7dq1a2','s7eq3b'
)
ds_glss6_hh %<>%
    left_join(
        dplyr::select(
                   ds_glss6_hh_sec7,
                   all_of(glss6_sec7_variables)
               ),
        by='HID'
    )

## Identify improved water sources
ds_glss6_hh[['WATER_DRINKING_IMPROVED']] =
    ds_glss6_hh['s7dq1a1'] %in% glss_drink_improved_cats
ds_glss6_hh[['WATER_HOUSEHOLD_IMPROVED']] =
    ds_glss6_hh[['s7dq1a2']] %in% glss_drink_improved_cats

## Convert GLSS water codes to equivalent census codes, which then allows
## us to use the same lookup table to apply the categorisation used in the
## present analysis:
ds_glss6_hh %<>%
    left_join(map_glss_to_census_drinking_water_source, by='s7dq1a1') %>%
    left_join(map_glss_to_census_hh_water_source, by='s7dq1a2')

## Create temporary data frame for analysis, only including rows
## which are complete:
cols = c(
    'HID', 'REGION', 'dist', 'WTA_S',
    's7dq16a', 's7dq13', 's6q1', 's7aq1',
    's7dq11', 's7eq1c', 's7dq14a', 's7eq1e',
    's7dq14b', 's7eq1a', 's7eq3d', 's7eq1b',
    's7dq16b', 's7bq1',
    's7dq1a1', 's7dq1a2',
    'WATER_DRINKING_IMPROVED',
    'WATER_HOUSEHOLD_IMPROVED',
    'WATER_DRINKING', 'WATER_HOUSEHOLD',
    'HHSEX', 'RURURB'
)
ds = ds_glss6_hh %>% dplyr::select(all_of(cols))
ds %<>% mutate(s7eq3d = replace(s7eq3d, is.na(s7eq3d), 2))
ds %<>% mutate(s7dq16b = replace(s7dq16b, is.na(s7dq16b), 0))
ds %<>% filter(complete.cases(.))

## As before, since we have removed incomplete rows we need to
## rebalance at the regional level:
complete_rgn_counts =
    ds_glss6_hh %>%
    dplyr::select(REGION, WTA_S) %>%
    group_by(REGION) %>%
    summarize(WTA_S_complete=sum(WTA_S)) %>%
    arrange(REGION)

sub_rgn_counts =
    ds %>%
    dplyr::select(REGION, WTA_S) %>%
    group_by(REGION) %>%
    summarize(WTA_S_sub=sum(WTA_S)) %>%
    arrange(REGION)

temp_rgn_scale =
    left_join(complete_rgn_counts, sub_rgn_counts, by="REGION") %>%
    mutate(scale=WTA_S_complete/WTA_S_sub) %>%
    dplyr::select(REGION, scale)

ds %<>%
    left_join(temp_rgn_scale, by="REGION") %>%
    mutate(WH=WTA_S * scale) %>%
    dplyr::select(-scale)

## Assign constants, then convert numeric codes to discrete
## variables which can be directly compared between the datasets:
ds[["const"]] = 1
ds[["YEAR"]] = 2013
ds[['HH_DRINK_SACHET']] = ds[['s7dq1a1']] %in% c(10)
ds[['HH_DWELLING_1']] = ds[['s7aq1']] %in% c(1,2,3)
ds[['HH_DWELLING_2']] = ds[['s7aq1']] %in% c(4)
ds[['HH_DWELLING_3']] = !ds[['s7aq1']] %in% c(1,2,3,4)
ds[['HH_DWELLING_4']] = !ds[['s7aq1']] %in% c(1,2,3)
ds[['HH_LIGHTING_1']] = ds[['s7dq11']] %in% c(1,2)
ds[['HH_LIGHTING_2']] = !ds[['s7dq11']] %in% c(1,2)
ds[['HH_TOILET_1']] = ds[['s7dq16a']] %in% c(2)
ds[['HH_TOILET_2']] = ds[['s7dq16a']] %in% c(4)
ds[['HH_TOILET_3']] = !ds[['s7dq16a']] %in% c(2,4)
ds[['HH_FUEL_1']] = ds[['s7dq13']] %in% c(4,5,6)
ds[['HH_FUEL_2']] = !ds[['s7dq13']] %in% c(4,5,6)
ds[['HH_SOLIDWASTE_1']] = ds[['s7dq14a']] %in% c(1)
ds[['HH_SOLIDWASTE_2']] = ds[['s7dq14a']] %in% c(3)
ds[['HH_SOLIDWASTE_3']] = !ds[['s7dq14a']] %in% c(2,4)
ds[['HH_SOLIDWASTE_4']] = !ds[['s7dq14a']] %in% c(1)
ds[['HH_LIQUIDWASTE_1']] = (ds[['s7dq14b']] %in% c(4)) | (ds[['s7dq14b']] %in% c(2))
ds[['HH_LIQUIDWASTE_2']] = !((ds[['s7dq14b']] %in% c(4)) | (ds[['s7dq14b']] %in% c(2)))
ds[['HH_COMPUTER_1']] = !((ds[['s7eq1c']] %in% c(2)) & (ds[['s7eq1e']] %in% c(2)))
ds[['HH_COMPUTER_2']] = ((ds[['s7eq1c']] %in% c(2)) & (ds[['s7eq1e']] %in% c(2)))
ds[['HH_MOBILE_1']] = !((ds[['s7eq1b']] %in% c(2)) | !(ds[['s7eq1b']] %in% c(1)))
ds[['HH_MOBILE_2']] = ((ds[['s7eq1b']] %in% c(2)) | !(ds[['s7eq1b']] %in% c(1)))
ds[['HH_HHWATER_1']] = ds[['WATER_HOUSEHOLD_IMPROVED']] %in% c(1)
ds[['HH_HHWATER_2']] = ds[['WATER_HOUSEHOLD_IMPROVED']] %in% c(0)
ds[['HH_OWNERSHIP_1']] = ds[['s7bq1']] %in% c(1)
ds[['HH_OWNERSHIP_2']] = !ds[['s7bq1']] %in% c(1)
ds[['HH_HEAD_GENDER_1']] = ds[['HHSEX']] %in% c(1)
ds[['HH_HEAD_GENDER_2']] = ds[['HHSEX']] %in% c(0)
ds[['HH_GEO_1']] = ds[['RURURB']] %in% c(2)
ds[['HH_GEO_2']] = ds[['RURURB']] %in% c(1)
ds[['HH_GAMA_1']] = ds[['REGION']] %in% c(3)
ds[['HH_GAMA_2']] = !ds[['REGION']] %in% c(3)

## Convert water types to the seven used in this analysis
ds %<>% left_join(map_drinking_water_source_to_jmp, by="WATER_DRINKING")
ds %<>% left_join(map_hh_water_source_to_jmp, by="WATER_HOUSEHOLD")

## Distinguish between sachet water drinking when household water
## consumption is (i) and (ii) unimproved
ds %<>%
    mutate(
        WATER_DRINKING_7=replace(
            WATER_DRINKING_7,
            (WATER_DRINKING_7 %in% 18) & (WATER_HOUSEHOLD_IMPROVED),
            17.5
        )
    )

## Combined public and private piped water access, but create
## a separate category for private piped access
ds[['HH_DRINK_PIPED']] = ds[['WATER_DRINKING_7']] %in% c(16, 17)
ds[['HH_DRINK_PIPED_PRIVATE']] = ds[['WATER_DRINKING_7']] %in% c(16)

## Copy temporary data frame back to parent data frame:
ds_glss6 = ds

## ####################################################### ##
## ####################################################### ##

## Process GLSS7 dataset

## ####################################################### ##
## ####################################################### ##

## Data downloaded from:
## http://www.webdeploy.statsghana.gov.gh/nada/index.php/catalog/97

ds_glss7_pp =
    read.csv("data-raw/g7spss/g7aggregates/16_GHA_2017_I.csv") %>%
    as_tibble
ds_glss7_hh =
    read.csv("data-raw/g7spss/g7aggregates/18_GHA_POV_2017_GLSS7.csv") %>%
    as_tibble
dist = read.csv("data-raw/g7spss/g7PartA/g7loc_upd.csv")[['district']]
ds_glss7_hh %<>% mutate(dist=dist)
ds_glss7_hh %<>% mutate(REGION=region)

## Some GLSS7 datapoints need to be acquired from other databases:
glss7_sec7 =
    read.csv("data-raw/g7spss/g7PartA/g7sec7.csv") %>%
    as_tibble
ds_glss7_pp_sec2 =
    read.csv("data-raw/g7spss/g7PartA/g7sec2.csv") %>%
    as_tibble
ds_glss7_pp_sec4 =
    read.csv("data-raw/g7spss/g7PartA/g7sec4-reviewed.csv") %>%
    as_tibble
ds_glss7_pp_sec5c =
    read.csv("data-raw/g7spss/g7PartA/g7sec5c.csv") %>%
    as_tibble
ds_glss7_hh_sec6a =
    read.csv("data-raw/g7spss/g7PartA/g7sec6a.csv") %>%
    as_tibble
ds_glss7_hh_sec7 =
    read.csv("data-raw/g7spss/g7PartA/g7sec7.csv") %>%
    as_tibble
ds_glss7_pp %<>%
    mutate(pid=str_pad(pid, width=2, pad=0)) %>%
    unite(phid, hid, pid, sep="/", remove=FALSE)
ds_glss7_pp %<>%
    left_join(ds_glss7_pp_sec2[c('phid','s2cq1','s2cq3')], by='phid') %>%
    left_join(ds_glss7_pp_sec4[c('phid','s4aq33b','s4aq2','s4aq4','s4aq6','s4aq21')], by='phid')

## As in the previous datasets, some datapoints are given to the
## population-level. Here we recode them into the household level.

## (i) Household computer/tablet usage
tmp = ds_glss7_pp_sec5c %>%
    dplyr::select(hid, s5cq1a, s5cq1b, s5cq1c, s5cq8e) %>%
    group_by(hid) %>%
    summarize_all(sum, na.rm=TRUE) %>%
    mutate_if(is.numeric, function(x) { ifelse(x>=1,1,2) })
ds_glss7_hh %<>% left_join(tmp, by='hid')

## (ii) Household mobile phone usage
ds_glss7_pp_sec5c %<>%
    mutate(s5cq3 = replace(s5cq3, s5cq3 == 2, 0)) %>%
    mutate(s5cq4 = replace(s5cq4, s5cq4 == 2, 0))

tmp = ds_glss7_pp_sec5c %>%
    dplyr::select(hid, s5cq3, s5cq4) %>%
    group_by(hid) %>%
    summarize_all(sum, na.rm=TRUE) %>%
    mutate_if(is.numeric, function(x) { ifelse(x>=1,1,2) })

ds_glss7_hh %<>%
    left_join(tmp, by='hid')

## Copy data from other objects to the main database:
ds_glss7_hh %<>% 
    left_join(ds_glss7_hh_sec6a[c('hid','s6q1_a')])

sec7_cols = c(
    'hid','s7dq26a','s7dq19','s7aq1','s7dq13b',
    's7dq24a','s7dq24b','s7eq1a','s7dq26b','s7bq1',
    's7dq1a1','s7dq1a2'
)

ds_glss7_hh %<>%
    left_join(ds_glss7_hh_sec7[sec7_cols], by='hid')

ds_glss7_hh[['HHSEX']] =
    ds_glss7_pp %>%
    filter(RELAT==1) %>%
    dplyr::select(SEX) %>%
    `[`(,1,drop=TRUE)

ds_glss7_hh %<>% mutate(water_drink=glss7_sec7[['s7dq1a1']])
ds_glss7_hh %<>% mutate(water_household=glss7_sec7[['s7dq1a2']])

## Create logical values on household's water preferences:
ds_glss7_hh[['WATER_DRINKING_IMPROVED']] =
    ds_glss7_hh[['s7dq1a1']] %in% glss_drink_improved_cats
ds_glss7_hh[['WATER_HOUSEHOLD_IMPROVED']] =
    ds_glss7_hh[['s7dq1a2']] %in% glss_drink_improved_cats

## Create temporary data frame:
ds_glss7_hh %<>%
    left_join(map_glss_to_census_drinking_water_source, by='s7dq1a1') %>% 
    left_join(map_glss_to_census_hh_water_source, by='s7dq1a2')

cols = c(
    'hid', 'region', 'dist', 'WTA_S',
    's7dq26a', 's7dq19', 's6q1_a', 's7aq1',
    's7dq13b', 's5cq1a', 's5cq1b', 's5cq1c',
    's7dq24a', 's7dq24b', 's7eq1a', 's5cq8e',
    's5cq3', 's5cq4', 's7dq26b', 's7bq1',
    's7dq1a1', 's7dq1a2',
    'WATER_DRINKING_IMPROVED',
    'WATER_HOUSEHOLD_IMPROVED',
    'WATER_DRINKING', 'WATER_HOUSEHOLD',
    'HHSEX', 'rururb'
)
ds = ds_glss7_hh %>% dplyr::select(cols)

ds %<>%
    mutate(s7dq13b=replace_na(s7dq13b, 0)) %>%
    mutate(s7dq26b=replace_na(s7dq26b, 0)) %>%
    mutate(REGION=region) %>%
    filter(complete.cases(.))

## As before, since we have removed incomplete rows we need to rebalance at the
## regional level:
complete_rgn_counts =
    ds_glss7_hh %>%
    dplyr::select(REGION, WTA_S) %>%
    group_by(REGION) %>%
    summarize(WTA_S_complete=sum(WTA_S)) %>%
    arrange(REGION)

sub_rgn_counts =
    ds %>%
    dplyr::select(REGION, WTA_S) %>%
    group_by(REGION) %>%
    summarize(WTA_S_sub=sum(WTA_S)) %>%
    arrange(REGION)

temp_rgn_scale =
    left_join(complete_rgn_counts, sub_rgn_counts, by="REGION") %>%
    mutate(scale=WTA_S_complete/WTA_S_sub) %>%
    dplyr::select(REGION, scale)

ds %<>%
    left_join(temp_rgn_scale, by="REGION") %>%
    mutate(WH=WTA_S * scale) %>%
    dplyr::select(-scale)

## Assign constants, then convert numeric codes to discrete variables
## which can be directly compared between the datasets:
ds[['const']] = 1
ds[['YEAR']] = 2017
ds[['HH_DRINK_SACHET']] = ds[['s7dq1a1']] %in% 10
ds[['HH_DWELLING_1']] = ds[['s7aq1']] %in% c(1,2,3)
ds[['HH_DWELLING_2']] = ds[['s7aq1']] %in% c(4)
ds[['HH_DWELLING_3']] = !ds[['s7aq1']] %in% c(1,2,3,4)
ds[['HH_DWELLING_4']] = !ds[['s7aq1']] %in% c(1,2,3)
ds[['HH_LIGHTING_1']] = ds[['s7dq13b']] %in% c(0)
ds[['HH_LIGHTING_2']] = !ds[['s7dq13b']] %in% c(0)
ds[['HH_TOILET_1']] = ds[['s7dq26a']] %in% c(2)
ds[['HH_TOILET_2']] = ds[['s7dq26a']] %in% c(4)
ds[['HH_TOILET_3']] = !ds[['s7dq26a']] %in% c(2,4)
ds[['HH_FUEL_1']] = ds[['s7dq19']] %in% c(4,5,6)
ds[['HH_FUEL_2']] = !ds[['s7dq19']] %in% c(4,5,6)
ds[['HH_SOLIDWASTE_1']] = ds[['s7dq24a']] %in% c(1)
ds[['HH_SOLIDWASTE_2']] = ds[['s7dq24a']] %in% c(3)
ds[['HH_SOLIDWASTE_3']] = !ds[['s7dq24a']] %in% c(2,4)
ds[['HH_SOLIDWASTE_4']] = !ds[['s7dq24a']] %in% c(1)
ds[['HH_LIQUIDWASTE_1']] = ((ds[['s7dq24b']] %in% c(2)) | (ds[['s7dq24b']]%in% c(4)))
ds[['HH_LIQUIDWASTE_2']] = !((ds[['s7dq24b']] %in% c(2)) | (ds[['s7dq24b']] %in% c(4)))
ds[['HH_COMPUTER_1']] = !((ds[['s5cq1a']] %in% c(2)) & (ds[['s5cq1b']] %in% c(2)) & (ds[['s5cq1c']] %in% c(2)))
ds[['HH_COMPUTER_2']] = ((ds[['s5cq1a']] %in% c(2)) & (ds[['s5cq1b']] %in% c(2)) & (ds[['s5cq1c']] %in% c(2)))
ds[['HH_MOBILE_1']] = ((ds[['s5cq3']] %in% c(1)) | (ds[['s5cq4']] %in% c(1)))
ds[['HH_MOBILE_2']] = !((ds[['s5cq3']] %in% c(1)) | (ds[['s5cq4']] %in% c(1)))
ds[['HH_HHWATER_1']] = ds[['WATER_HOUSEHOLD_IMPROVED']] %in% c(1)
ds[['HH_HHWATER_2']] = ds[['WATER_HOUSEHOLD_IMPROVED']] %in% c(0)
ds[['HH_OWNERSHIP_1']] = ds[['s7bq1']] %in% c(1)
ds[['HH_OWNERSHIP_2']] = !ds[['s7bq1']] %in% c(1)
ds[['HH_HEAD_GENDER_1']] = ds[['HHSEX']] %in% c(1)
ds[['HH_HEAD_GENDER_2']] = ds[['HHSEX']] %in% c(0)
ds[['HH_GEO_1']] = ds[['rururb']] %in% c(2)
ds[['HH_GEO_2']] = ds[['rururb']] %in% c(1)
ds[['HH_GAMA_1']] = ds[['REGION']] %in% c(3)
ds[['HH_GAMA_2']] = !ds[['REGION']] %in% c(3)

## Convert water types to the seven used in this analysis
ds %<>% left_join(map_drinking_water_source_to_jmp, by="WATER_DRINKING")
ds %<>% left_join(map_hh_water_source_to_jmp, by="WATER_HOUSEHOLD")

## Distinguish between sachet water drinking when household water
## consumption is (i) and (ii) unimproved
ds %<>%
    mutate(
        WATER_DRINKING_7=replace(
            WATER_DRINKING_7,
            (WATER_DRINKING_7 %in% 18) & (WATER_HOUSEHOLD_IMPROVED),
            17.5
        )
    )

## Combined public and private piped water access, but create
## a separate category for private piped access
ds[['HH_DRINK_PIPED']] = ds[['WATER_DRINKING_7']] %in% c(16, 17)
ds[['HH_DRINK_PIPED_PRIVATE']] = ds[['WATER_DRINKING_7']] %in% c(16)

## Copy temporary data frame back to parent data frame
ds_glss7 = ds

## Save data files for later use
saveRDS(ds_census2010, "data/census2010.rds")
saveRDS(ds_glss6, "data/glss6.rds")
saveRDS(ds_glss7, "data/glss7.rds")

## ####################################################### ##
## ####################################################### ##

## Spatial data

## ####################################################### ##
## ####################################################### ##

distGeo216 = st_read(
    dsn="data-raw/Districts_216",
    layer="Map_of_Districts_216"
)
distGeo216 %<>% st_transform(crs=4326)
saveRDS(distGeo216, "data/Map_of_Districts_216.rds")

distGeo170 = st_read(
    dsn="data-raw",
    layer="geo2_gh2010"
) %>%
    st_transform(crs=32630) %>%
    mutate(ID=as.numeric(DIST2010))
distGeo170[["AREA_KM"]] = st_area(distGeo170) / 1000 / 1000
distGeo170 %<>% st_transform(crs=4326)
saveRDS(distGeo170, "data/geo2_gh2010.rds")

regGeo = st_read(
    dsn="data-raw/Regions_10",
    layer="Map_of_Regions_in_Ghana"
) %>%
    st_transform(crs=4326) %>%
    mutate(ID=c(6,7,2,5,3,8,9,10,4,1))
regGeo[["AREA_KM"]] = st_area(regGeo) / 1000 / 1000
saveRDS(regGeo, "data/Map_of_Regions_in_Ghana.rds")

## Ghana neighbouring countries are: Cote D'Ivoire, Burkina Faso, Togo, Benin
togo = readRDS(
    "data-raw/GADM/gadm36_TGO_0_sf.rds"
)
benin = readRDS(
    "data-raw/GADM/gadm36_BEN_0_sf.rds"
)
burkina_faso = readRDS(
    "data-raw/GADM/gadm36_BFA_0_sf.rds"
)
cote_divoire = readRDS(
    "data-raw/GADM/gadm36_CIV_0_sf.rds"
)
ghana = readRDS(
    "data-raw/GADM/gadm36_GHA_0_sf.rds"
)
world = rbind(togo, benin, cote_divoire, ghana, burkina_faso)
world %<>%
    mutate(area=st_area(.)) %>%
    summarise(area=sum(area))
saveRDS(world, "data/gadm36_ghana_neighbours.rds")

ghana_rgns_gaul =
    st_read(dsn="data-raw", layer="GHA_admbndp1_1m_GAUL") %>% 
    cbind(st_coordinates(st_centroid(.)))
saveRDS(ghana_rgns_gaul, "data/GHA_admbndp1_1m_GAUL.rds")

## ####################################################### ##
## ####################################################### ##

## SHDI data

## ####################################################### ##
## ####################################################### ##

## Download the latest SHDI data from Global Data Lab
## https://globaldatalab.org/shdi/download_files/
## [Accessed 22/02/2021]
rgn_codes = readRDS(
    "data/map_rgn_name_to_code.rds"
)
x = read.csv("data-raw/SHDI Complete 4.0 (1).csv") %>%
    filter(country %in% "Ghana") %>%
    dplyr::select(all_of(c("year","region","shdi","healthindex","incindex","edindex","gnic"))) %>%
    filter(!region %in% "Total") %>% 
    mutate(Region=region) %>% left_join(rgn_codes) %>% dplyr::select(-Region) %>%
    arrange(year, ID) ## %>%
    ## mutate(year=paste0("X", year))

HDI_region = x %>%
    dplyr::select(year, shdi, ID) %>%
    rename(YEAR=year, HDI=shdi, REGION=ID)
    
EI_region = x %>%
    dplyr::select(year, edindex, ID) %>%
    rename(YEAR=year, EI=edindex, REGION=ID)

GNI_region = x %>%
    dplyr::select(year, gnic, ID) %>%
    rename(YEAR=year, GNI=gnic, REGION=ID)

HI_region = x %>%
    dplyr::select(year, healthindex, ID) %>%
    rename(YEAR=year, HI=healthindex, REGION=ID)

saveRDS(HDI_region, "data/HDI_ghana_region.rds")
saveRDS(EI_region, "data/EI_ghana_region.rds")
saveRDS(GNI_region, "data/GNI_ghana_region.rds")
saveRDS(HI_region, "data/HI_ghana_region.rds")
