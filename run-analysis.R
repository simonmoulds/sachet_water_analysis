## Author : Simon Moulds
## Date   : November 2020

library(foreign)
library(rgdal)
library(tidyverse)
library(magrittr)
library(sf)
library(reticulate)
library(rmcorr)

options(stringsAsFactors = FALSE)

## Set the significance level used throughout the analysis:
sig_level = 0.05

## Set the output directory
outdir = "figs"

## ####################################################### ##
## ####################################################### ##

## Load data

## ####################################################### ##
## ####################################################### ##

rgn_codes = readRDS(
    "data/map_rgn_name_to_code.rds"
)
map_drinking_water_source_to_jmp = readRDS(
    "data/map_drinking_water_source_to_jmp.rds"
)
map_hh_water_source_to_hmp = readRDS(
    "data/map_hh_water_source_to_jmp.rds"
)
map_glss_to_census_drinking_water_source = readRDS(
    "data/map_glss_to_census_drinking_water_source.rds"
)
map_glss_to_census_hh_water_source = readRDS(
    "data/map_glss_to_census_hh_water_source.rds"
)
hh_factors = readRDS(
    "data/hh_factors.rds"
)
hh_simple_categories = readRDS(
    "data/hh_simple_categories.rds"
)
ds_census2010 = readRDS(
    "data/census2010.rds"
)
ds_glss6 = readRDS(
    "data/glss6.rds"
)
ds_glss7 = readRDS(
    "data/glss7.rds"
)
distGeo216 = readRDS(
    "data/Map_of_Districts_216.rds"
)
distGeo170 = readRDS(
    "data/geo2_gh2010.rds"
)
regGeo = readRDS(
    "data/Map_of_Regions_in_Ghana.rds"
)
world = readRDS(
    "data/gadm36_ghana_neighbours.rds"
)
ghana_rgns_gaul = readRDS(
    "data/GHA_admbndp1_1m_GAUL.rds"
)
HDI_region = readRDS(
    "data/HDI_ghana_region.rds"
)
EI_region = readRDS(
    "data/EI_ghana_region.rds"
)
GNI_region = readRDS(
    "data/GNI_ghana_region.rds"
)
HI_region = readRDS(
    "data/HI_ghana_region.rds"
)

## ####################################################### ##
## ####################################################### ##

## Stack plots

## ####################################################### ##
## ####################################################### ##

## The first set of plots are stack plots showing the change
## in consumption rates of the various drinking water
## sources during the study period
concat_data = function(census2010, glss6, glss7, vars) {
    ds_list = list(census2010[vars], glss6[vars], glss7[vars])
    ds = do.call(rbind, ds_list)
    ds
}
vars = c(
    'const','REGION','YEAR','WH','WATER_DRINKING',
    'HH_GEO_1','WATER_HOUSEHOLD_IMPROVED',
    'WATER_DRINKING_7'
)
ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)

labs = c(
    "Improved: Piped water (private)", # 16
    "Improved: Piped water (public)",  # 17
    "Improved: Sachet water \u2A",     # 17.5
    "Improved: Sachet water \u2020",   # 18
    "Improved: Groundwater",           # 19
    "Improved: Others",                # 20
    "Unimproved: Surface water",       # 21
    "Unimproved: Others"               # 22
)

## Convert to factor so that the figure legend is produced automatically
ds$WATER_DRINKING_7 %<>%
    factor(levels=c(16,17,17.5,18,19,20,21,22), labels=labs)

## Create subsets of the data:

## (i) Entire country
ds_ghana = ds %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WH) %>%
    group_by(YEAR, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    arrange(WATER_DRINKING_7) %>%
    mutate(WATER_DRINKING_7=as.factor(WATER_DRINKING_7)) %>%
    mutate(WH_sum = WH_sum / 1e6) %>%
    mutate(cat="Ghana")

## Compute changes in percentage and absolute terms
make_summary = function(ds) {
    lut = data.frame(WATER_DRINKING_7=labs, CODE=1:8)

    ds_sachet =
        ds %>%
        left_join(lut) %>% 
        filter(CODE %in% c(3,4)) %>%
        group_by(YEAR) %>%
        summarise(SACHET_TOTAL=sum(WH_sum) * 1e6)

    ds_piped = 
        ds %>%
        left_join(lut) %>% 
        filter(CODE %in% c(1,2)) %>%
        group_by(YEAR) %>%
        summarise(PIPED_TOTAL=sum(WH_sum) * 1e6)

    ds_groundwater =
        ds %>%
        left_join(lut) %>%
        filter(CODE %in% c(5)) %>%
        group_by(YEAR) %>%
        summarise(GROUNDWATER_TOTAL=sum(WH_sum) * 1e6)
    
    ds_improved =
        ds %>%
        left_join(lut) %>% 
        filter(CODE %in% c(1:6)) %>%
        group_by(YEAR) %>%
        summarise(IMPROVED_TOTAL=sum(WH_sum) * 1e6)

    ds_total = 
        ds %>%
        group_by(YEAR) %>%
        summarise(TOTAL=sum(WH_sum) * 1e6) %>%
        left_join(ds_sachet) %>%
        left_join(ds_piped) %>%
        left_join(ds_groundwater) %>% 
        left_join(ds_improved) %>% 
        mutate(UNIMPROVED_TOTAL=TOTAL-IMPROVED_TOTAL) %>% 
        mutate(SACHET_FRAC=SACHET_TOTAL/TOTAL) %>%
        mutate(PIPED_FRAC=PIPED_TOTAL/TOTAL) %>%
        mutate(IMPROVED_FRAC=IMPROVED_TOTAL/TOTAL) %>%
        mutate(UNIMPROVED_FRAC=UNIMPROVED_TOTAL/TOTAL)
    
    ds_total
}

ds_ghana_total = make_summary(ds_ghana)

## function to increase vertical spacing between legend keys
## @clauswilke
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  grid::rectGrob(
    width = grid::unit(0.8, "npc"),
    height = grid::unit(0.8, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

# Register new key drawing function, 
# the effect is global & persistent throughout the R session
GeomArea$draw_key = draw_key_polygon3
p = ds_ghana %>%
    ggplot(aes(x=YEAR, y=WH_sum, fill=WATER_DRINKING_7)) +
    geom_area(position='stack', colour='black', size=0.1) +
    scale_fill_brewer(palette="Set1") + 
    ylab(expression(paste("No. of households (\uD7",10^6, ")"))) + 
    xlab("Year") + 
    labs(caption="\u2A Improved non-drinking water source\n\u2020 Unimproved non-drinking water source") + 
    theme(
        legend.title=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.key.size=unit(0.3,"cm"),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides(fill=guide_legend(ncol=2))

cairo_pdf(
    file.path(outdir, "figure2.pdf"),
    width=5, height=4
)
print(p)
dev.off()

## (ii) Urban areas
ds_urban =
    ds %>%
    filter(HH_GEO_1) %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WH) %>%
    group_by(YEAR, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    arrange(WATER_DRINKING_7) %>%
    mutate(WATER_DRINKING_7=as.factor(WATER_DRINKING_7)) %>%
    mutate(WH_sum = WH_sum / 1e6) %>%
    mutate(cat="Urban")

ds_urban_total = make_summary(ds_urban)

## (iii) Rural areas
ds_rural =
    ds %>%
    filter(!HH_GEO_1) %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WH) %>%
    group_by(YEAR, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    arrange(WATER_DRINKING_7) %>%
    mutate(WATER_DRINKING_7=as.factor(WATER_DRINKING_7)) %>%
    mutate(WH_sum = WH_sum / 1e6) %>%
    mutate(cat="Rural")

ds_rural_total = make_summary(ds_rural)

ds_combined = do.call("rbind", list(ds_urban, ds_rural))
ds_combined$cat %<>% factor(levels=c("Urban","Rural"))

p = ds_combined %>% 
    ggplot(aes(x=YEAR, y=WH_sum, fill=WATER_DRINKING_7)) +
    geom_area(position='stack', colour='black', size=0.1) +
    scale_fill_brewer(palette="Set1") + 
    facet_wrap(cat ~ ., ncol=2) + 
    ylab(expression(paste("No. of households (\uD7",10^6, ")"))) + 
    xlab("Year") + 
    labs(caption="\u2A Improved non-drinking water source\n\u2020 Unimproved non-drinking water source") + 
    theme(
        legend.title=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        strip.text=element_text(size=8, margin=margin(t=3, b=2)),
        axis.title=element_text(size=8),
        legend.key.size=unit(0.3,"cm"),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides(fill=guide_legend(ncol=2))

cairo_pdf(
    file.path(outdir, "figure3.pdf"),
    width=5, height=4
)
print(p)
dev.off()

## (iv) Accra
ds_accra = 
    ds %>%
    filter(REGION == 3) %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WH) %>%
    group_by(YEAR, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    arrange(WATER_DRINKING_7) %>%
    mutate(WATER_DRINKING_7=as.factor(WATER_DRINKING_7)) %>%
    mutate(WH_sum = WH_sum / 1e6) %>%
    mutate(cat="Accra")

ds_accra_total = make_summary(ds_accra)

p = ds_accra %>%
    ggplot(aes(x=YEAR, y=WH_sum, fill=WATER_DRINKING_7)) +
    geom_area(position='stack', colour='black', size=0.1) +
    scale_fill_brewer(palette="Set1") + 
    ylab(expression(paste("No. of households (\uD7",10^6, ")"))) + 
    xlab("Year") + 
    labs(caption="\u2A Improved non-drinking water source\n\u2020 Unimproved non-drinking water source") + 
    theme(
        legend.title=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.key.size=unit(0.3,"cm"),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides(fill=guide_legend(ncol=2))

cairo_pdf(
    file.path(outdir, "figure4.pdf"),
    width=5, height=4
)
print(p)
dev.off()

## Now create stack plots showing the percentage of sachet water
## grouped according to household water use

## Create labels for plots
vars = c(
    'const','REGION','YEAR','WH', 'HH_GEO_1',
    'WATER_DRINKING_7', 'WATER_HOUSEHOLD_7'
)
ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)
ds$WATER_DRINKING_7[ds$WATER_DRINKING_7 %in% 17.5] = 18

labs = c(
    "Improved: Piped water (private)", # 16
    "Improved: Piped water (public)",  # 17
    "Improved: Sachet water",          # 18
    "Improved: Groundwater",           # 19
    "Improved: Others",                # 20
    "Unimproved: Surface water",       # 21
    "Unimproved: Others"               # 22
)

## ## Convert to factor so that the figure legend is produced automatically
## ds$WATER_DRINKING_7 %<>%
##     factor(levels=c(16,17,18,19,20,21,22), labels=labs)
## ds$WATER_HOUSEHOLD_7 %<>%
##     factor(levels=c(16,17,18,19,20,21,22), labels=labs)

## Create subsets of the data:

## (i) Entire country
ds_ghana =
    ds %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WATER_HOUSEHOLD_7, WH) %>%
    group_by(YEAR, WATER_HOUSEHOLD_7, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    filter(WATER_DRINKING_7 %in% 18) %>% 
    arrange(WATER_HOUSEHOLD_7) %>%
    mutate(cat="Ghana")

## ds_ghana_tot =
##     ds_ghana %>%
##     ungroup() %>%
##     dplyr::select(YEAR, cat, WH_sum) %>%
##     group_by(YEAR, cat) %>%
##     summarise(WH_year_sum=sum(WH_sum))

## ds_ghana =
##     ds_ghana %>%
##     left_join(ds_ghana_tot) %>%
##     mutate(WH_frac=WH_sum/WH_year_sum*100)

ds_urban =
    ds %>%
    filter(HH_GEO_1) %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WATER_HOUSEHOLD_7, WH) %>%
    group_by(YEAR, WATER_HOUSEHOLD_7, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    filter(WATER_DRINKING_7 %in% 18) %>% 
    arrange(WATER_HOUSEHOLD_7) %>%
    mutate(cat="Urban")

## ds_urban_tot =
##     ds_urban %>%
##     ungroup() %>%
##     dplyr::select(YEAR, cat, WH_sum) %>%
##     group_by(YEAR, cat) %>%
##     summarise(WH_year_sum=sum(WH_sum))

## ds_urban =
##     ds_urban %>%
##     left_join(ds_urban_tot) %>%
##     mutate(WH_frac=WH_sum/WH_year_sum*100)

## ds_urban %>% filter(WATER_HOUSEHOLD_7 %in% 16)

## (iii) Rural areas
ds_rural =
    ds %>%
    filter(!HH_GEO_1) %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WATER_HOUSEHOLD_7, WH) %>%
    group_by(YEAR, WATER_HOUSEHOLD_7, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    filter(WATER_DRINKING_7 %in% 18) %>% 
    arrange(WATER_HOUSEHOLD_7) %>%
    mutate(cat="Rural")

## ds_rural_tot =
##     ds_rural %>%
##     ungroup() %>%
##     dplyr::select(YEAR, cat, WH_sum) %>%
##     group_by(YEAR, cat) %>%
##     summarise(WH_year_sum=sum(WH_sum))

## ds_rural =
##     ds_rural %>%
##     left_join(ds_rural_tot) %>%
##     mutate(WH_frac=WH_sum/WH_year_sum*100)

## ds_rural %>% filter(WATER_HOUSEHOLD_7 %in% 16)
## ds_rural %>% filter(WATER_HOUSEHOLD_7 %in% c(21:22)) %>% group_by(YEAR) %>% summarise(tot_unimproved=sum(WH_frac))

## (iv) Accra
ds_accra = 
    ds %>%
    filter(REGION == 3) %>%
    dplyr::select(REGION, YEAR, WATER_DRINKING_7, WATER_HOUSEHOLD_7, WH) %>%
    group_by(YEAR, WATER_HOUSEHOLD_7, WATER_DRINKING_7) %>%
    summarise(WH_sum=sum(WH)) %>%
    filter(WATER_DRINKING_7 %in% 18) %>% 
    arrange(WATER_HOUSEHOLD_7) %>%
    mutate(cat="Greater Accra")

ds_accra %<>%
    right_join(data.frame(expand.grid(YEAR=c(2010,2013,2017), WATER_HOUSEHOLD_7=c(16:22)), WATER_DRINKING_7=18, cat="Greater Accra")) %>%
    mutate(WH_sum = replace_na(WH_sum, 0)) %>%
    arrange(WATER_HOUSEHOLD_7, YEAR)

## ds_accra_tot =
##     ds_accra %>%
##     ungroup() %>%
##     dplyr::select(YEAR, cat, WH_sum) %>%
##     group_by(YEAR, cat) %>%
##     summarise(WH_year_sum=sum(WH_sum))

## ds_accra =
##     ds_accra %>%
##     left_join(ds_accra_tot) %>%
##     mutate(WH_frac=WH_sum/WH_year_sum*100)

## ds_accra %>% filter(WATER_HOUSEHOLD_7 %in% 16)
## ds_accra %>% filter(WATER_HOUSEHOLD_7 %in% c(21:22)) %>% group_by(YEAR) %>% summarise(tot_unimproved=sum(WH_frac))

ds_combined = do.call("rbind", list(ds_ghana, ds_urban, ds_rural, ds_accra))
ds_combined$cat %<>% factor(levels=c("Ghana", "Urban", "Rural", "Greater Accra"))
ds_combined$WATER_HOUSEHOLD_7 %<>%
    factor(levels=c(16,17,18,19,20,21,22), labels=labs)

ds_combined_tot =
    ds_combined %>%
    ungroup() %>%
    dplyr::select(YEAR, cat, WH_sum) %>%
    group_by(YEAR, cat) %>%
    summarise(WH_year_sum=sum(WH_sum))

ds_combined =
    ds_combined %>%
    left_join(ds_combined_tot) %>%
    mutate(WH_frac=WH_sum/WH_year_sum*100)
## test = ds_combined %>% ungroup() %>% dplyr::select(YEAR, cat, WH_frac) %>% group_by(YEAR, cat) %>% summarise(tot=sum(WH_frac))

p = ds_combined %>% 
    ggplot(aes(x=YEAR, y=WH_frac, fill=WATER_HOUSEHOLD_7)) +
    geom_area(position='stack', colour='black', size=0.1) +
    scale_fill_brewer(palette="Set1") + 
    facet_wrap(cat ~ ., ncol=2) + 
    ylab(expression(paste("Fraction of households consuming sachet water (%)"))) + 
    xlab("Year") + 
    ## labs(caption="\u2A Improved non-drinking water source\n\u2020 Unimproved non-drinking water source") + 
    theme(
        legend.title=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        strip.text=element_text(size=8, margin=margin(t=3, b=2)),
        axis.title=element_text(size=8),
        legend.key.size=unit(0.3,"cm"),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides(fill=guide_legend(ncol=2))
p

cairo_pdf(
    file.path(outdir, "figure5.pdf"),
    width=5, height=5
)
print(p)
dev.off()

## ####################################################### ##
## ####################################################### ##

## Correlation analysis

## ####################################################### ##
## ####################################################### ##

## Correlation between households characteristics or SHDI
## and sachet water consumption at the regional-level:

vars = c(
    'const', 'REGION', 'YEAR', 'WH',
    'HH_DWELLING_1', 'HH_DWELLING_4',
    'HH_LIGHTING_1', 'HH_LIGHTING_2',
    'HH_TOILET_1', 'HH_TOILET_2', 'HH_TOILET_3',
    'HH_FUEL_1', 'HH_FUEL_2',
    'HH_SOLIDWASTE_1', 'HH_SOLIDWASTE_4',
    'HH_LIQUIDWASTE_1', 'HH_LIQUIDWASTE_2',
    'HH_COMPUTER_1', 'HH_COMPUTER_2',
    'HH_MOBILE_1', 'HH_MOBILE_2',
    'HH_HHWATER_1', 'HH_HHWATER_2',
    'HH_OWNERSHIP_1', 'HH_OWNERSHIP_2',
    'HH_HEAD_GENDER_1', 'HH_HEAD_GENDER_2',
    'HH_GEO_1'
)
ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)

## Table - Distribution of household characteristics in 2010, 2013 and 2017
summarise_fun = function(ds) {
    summary_tbl =
        ds %>%
        mutate_at(vars(starts_with("HH")), list(wt = ~. * WH)) %>%
        group_by(YEAR) %>%
        summarize_at(vars(starts_with('HH'), 'WH'), sum, na.rm=TRUE) %>%
        mutate_at(vars(starts_with("HH")), list(pct = ~. / WH * 100)) %>%
        dplyr::select(YEAR, ends_with('_wt_pct')) %>%
        gather(KEY, VALUE, -YEAR) %>%
        spread(YEAR, VALUE) %>%
        mutate(KEY=gsub("_wt_pct$", "", KEY)) %>% 
        left_join(hh_factors, by="KEY")
    summary_tbl
}

summary_tbl_ghana = summarise_fun(ds)

## Urban (excl. Accra)
summary_tbl_urban =
    ds %>%
    filter(HH_GEO_1) %>%
    filter(!REGION %in% 3) %>%
    summarise_fun() %>%
    dplyr::select(-IMPROVED) %>%
    `colnames<-`(c("KEY","urb2010","urb2013","urb2017","CATEGORY","DESCRIPTION"))

## Rural
summary_tbl_rural =
    ds %>%
    filter(!HH_GEO_1) %>%
    summarise_fun() %>%
    dplyr::select(-IMPROVED) %>%
    `colnames<-`(c("KEY","rur2010","rur2013","rur2017","CATEGORY","DESCRIPTION"))

## Urban (Greater Accra only)
summary_tbl_accra =
    ds %>%
    filter(REGION %in% 3) %>%
    filter(HH_GEO_1) %>%
    summarise_fun() %>%
    dplyr::select(-IMPROVED) %>%
    `colnames<-`(c("KEY","acc2010","acc2013","acc2017","CATEGORY","DESCRIPTION"))

summary_tbl =
    summary_tbl_urban %>%
    left_join(summary_tbl_rural) %>%
    left_join(summary_tbl_accra) %>%
    mutate(urb2010=formatC(urb2010, digits=1, format="f")) %>% 
    mutate(urb2013=formatC(urb2013, digits=1, format="f")) %>% 
    mutate(urb2017=formatC(urb2017, digits=1, format="f")) %>% 
    mutate(acc2010=formatC(acc2010, digits=1, format="f")) %>% 
    mutate(acc2013=formatC(acc2013, digits=1, format="f")) %>% 
    mutate(acc2017=formatC(acc2017, digits=1, format="f")) %>%
    mutate(rur2010=formatC(rur2010, digits=1, format="f")) %>% 
    mutate(rur2013=formatC(rur2013, digits=1, format="f")) %>% 
    mutate(rur2017=formatC(rur2017, digits=1, format="f"))

cats = unique(hh_factors$CATEGORY) %>% `[`(!. %in% "Urban or rural")

sink(file.path(outdir, "table3.tex"))
cat("\\begin{tabular}{l r r r r r r r r r}\n")
cat("\\toprule\n")
cat("Characteristic & \\multicolumn{3}{c}{Rural} & \\multicolumn{3}{c}{Urban (excl. Accra)} & \\multicolumn{3}{c}{Urban (Accra only)} \\\\\n")
cat("\\cmidrule(lr){2-4}\n")
cat("\\cmidrule(lr){5-7}\n")
cat("\\cmidrule(lr){8-10}\n")
cat("& 2010 & 2013 & 2017 & 2010 & 2013 & 2017 & 2010 & 2013 & 2017 \\\\\n")
cat("\\midrule\n")
for (cat in cats) {
    cat(cat, "&&&&&&&&&\\\\\n")
    tmp = summary_tbl %>% filter(CATEGORY %in% cat)
    for (i in 1:nrow(tmp)) {
        desc = tmp$DESCRIPTION[i]
        urb2010 = tmp[["urb2010"]][i]
        urb2013 = tmp[["urb2013"]][i]
        urb2017 = tmp[["urb2017"]][i]
        acc2010 = tmp[["acc2010"]][i]
        acc2013 = tmp[["acc2013"]][i]
        acc2017 = tmp[["acc2017"]][i]
        rur2010 = tmp[["rur2010"]][i]
        rur2013 = tmp[["rur2013"]][i]
        rur2017 = tmp[["rur2017"]][i]
        cat("\\hspace{6mm}", desc, " & ", rur2010, " & ", rur2013, " & ", rur2017, " & ", urb2010, " & ", urb2013, " & ", urb2017, " & ", acc2010, " & ", acc2013, " & ", acc2017, "\\\\\n")
    }    
}
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
sink()

## Table - Correlation between regional characteristics and consumption
vars = c(
    'const', 'REGION', 'YEAR', 'WH',
    'HH_DWELLING_1', 'HH_DWELLING_4',
    'HH_LIGHTING_1', 'HH_LIGHTING_2',
    'HH_TOILET_1', 'HH_TOILET_2', 'HH_TOILET_3',
    'HH_FUEL_1', 'HH_FUEL_2',
    'HH_SOLIDWASTE_1', 'HH_SOLIDWASTE_4',
    'HH_LIQUIDWASTE_1', 'HH_LIQUIDWASTE_2',
    'HH_COMPUTER_1', 'HH_COMPUTER_2',
    'HH_MOBILE_1', 'HH_MOBILE_2',
    'HH_HHWATER_1', 'HH_HHWATER_2',
    'HH_OWNERSHIP_1', 'HH_OWNERSHIP_2',
    'HH_HEAD_GENDER_1', 'HH_HEAD_GENDER_2',
    'HH_GEO_1',
    'HH_DRINK_SACHET', 'HH_DRINK_PIPED', 'HH_DRINK_PIPED_PRIVATE'
)
ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)

summarise_fun = function(ds) {
    summary_tbl =
        ds %>%
        mutate_at(vars(starts_with("HH")), list(wt = ~. * WH)) %>%    
        group_by(YEAR, REGION) %>%
        summarize_at(vars(starts_with('HH'), 'WH'), sum, na.rm=TRUE) %>%
        mutate_at(vars(starts_with("HH")), list(pct = ~. / WH)) %>%
        dplyr::select(YEAR, REGION, ends_with('_wt_pct')) %>%
        `colnames<-`(gsub("_wt_pct", "", names(.))) %>% 
        left_join(HDI_region, by=c("YEAR","REGION")) %>%
        left_join(GNI_region, by=c("YEAR","REGION")) %>%
        left_join(EI_region, by=c("YEAR","REGION")) %>%
        left_join(HI_region, by=c("YEAR","REGION"))
    summary_tbl
}

summary_tbl = summarise_fun(ds)
summary_tbl_rural =
    ds %>%
    filter(!HH_GEO_1) %>%
    summarise_fun
summary_tbl_urban_wo_accra =
    ds %>%
    filter(HH_GEO_1) %>%
    filter(!REGION %in% 3) %>%
    summarise_fun
summary_tbl_urban_w_accra =
    ds %>%
    filter(HH_GEO_1) %>%
    summarise_fun
summary_tbl_urban_accra =
    ds %>%
    filter(HH_GEO_1) %>%
    filter(REGION %in% 3) %>%
    summarise_fun

## Rural
## Urban (excl. Accra)
## Urban (incl. Accra)
correlate_factors = c(
    'HH_DWELLING_1', 'HH_DWELLING_4',
    'HH_LIGHTING_1', 'HH_LIGHTING_2',
    'HH_TOILET_1', 'HH_TOILET_2', 'HH_TOILET_3',
    'HH_FUEL_1', 'HH_FUEL_2',
    'HH_SOLIDWASTE_1', 'HH_SOLIDWASTE_4',
    'HH_LIQUIDWASTE_1', 'HH_LIQUIDWASTE_2',
    'HH_COMPUTER_1', 'HH_COMPUTER_2',
    'HH_MOBILE_1', 'HH_MOBILE_2',
    'HH_HHWATER_1', 'HH_HHWATER_2',
    'HH_OWNERSHIP_1', 'HH_OWNERSHIP_2',
    'HH_HEAD_GENDER_1', 'HH_HEAD_GENDER_2',
    'HH_DRINK_SACHET', 'HH_DRINK_PIPED',
    'HH_DRINK_PIPED_PRIVATE'
)
compute_correlation = function(summary_tbl) {
    df_p =
        as.data.frame(matrix(data=NA, nrow=length(correlate_factors), ncol=5)) %>%
        `colnames<-`(c("KEY", "sachet_r","sachet_p","piped_r","piped_p")) %>%
        mutate(KEY=correlate_factors)

    for (i in 1:length(correlate_factors)) {
        factor = correlate_factors[i]
        df = summary_tbl %>% dplyr::select("REGION", "YEAR", "HH_DRINK_SACHET", "HH_DRINK_PIPED", all_of(factor)) %>% arrange(REGION, YEAR) %>% mutate(REGION=as.factor(REGION))
        sachet_test = rmcorr(participant="REGION", measure1="HH_DRINK_SACHET", measure2=factor, dataset=df)
        piped_test = rmcorr(participant="REGION", measure1="HH_DRINK_PIPED", measure2=factor, dataset=df)
        df_p[i, 'sachet_r'] = sachet_test$r
        df_p[i, 'sachet_p'] = sachet_test$p
        df_p[i, 'piped_r'] = piped_test$r
        df_p[i, 'piped_p'] = piped_test$p
    }
    df_p
}

df_p_rural =
    compute_correlation(summary_tbl_rural) %>%
    rename(
        rural_sachet_r=sachet_r,
        rural_sachet_p=sachet_p,
        rural_piped_r=piped_r,
        rural_piped_p=piped_p
    )

df_p_urban =
    compute_correlation(summary_tbl_urban_w_accra) %>%
    rename(
        urban_sachet_r=sachet_r,
        urban_sachet_p=sachet_p,
        urban_piped_r=piped_r,
        urban_piped_p=piped_p
    )

df_p =
    df_p_rural %>%
    left_join(df_p_urban) %>%
    left_join(hh_factors) %>%
    left_join(hh_simple_categories) %>% 
    filter(!is.na(CATEGORY)) %>%
    mutate(CATEGORY=SHORT_CATEGORY) %>% 
    filter(!KEY %in% c("HH_HEAD_GENDER_1","HH_GEO_1")) %>% 
    unite(CATEGORY, CATEGORY, DESCRIPTION, sep=": ")

df_p$CATEGORY[df_p$KEY == "HH_COMPUTER_1"] = "Technology: Has computer"
df_p$CATEGORY[df_p$KEY == "HH_COMPUTER_2"] = "Technology: Does not have computer"
df_p$CATEGORY[df_p$KEY == "HH_MOBILE_1"] = "Technology: Has mobile phone"
df_p$CATEGORY[df_p$KEY == "HH_MOBILE_2"] = "Technology: Does not have mobile phone"

cats = df_p$CATEGORY

n_obs = nrow(df_p)

format_p_value = function(p, sig_level) {
    ## Function to format p-value according
    ## to PLOS One guidance
    p = as.numeric(p)
    if (p <= sig_level) {
        if (p <= 0.001) {
            p = "\\textless{}0.001"
        } else {
            p = formatC(p, digits=3, format="f")
        }
    } else {
        p = ""
    }
    p
}    

format_r_value = function(r, p, sig_level) {
    p = as.numeric(p)
    if (p <= sig_level) {
        r = formatC(r, digits=2, format="f")
    } else {
        r = ""
    }
    r
}

sink(file.path(outdir, "table4.tex"))
cat("\\begin{tabular}{l r r r r r r r r}\n")
cat("\\toprule\n")
cat("Characteristic & \\multicolumn{4}{c}{Rural} & \\multicolumn{4}{c}{Urban} \\\\\n")
cat("\\cmidrule(lr){2-5}\n")
cat("\\cmidrule(lr){6-9}\n")
cat("& Sachet & p-value & Piped & p-value & Sachet & p-value & Piped & p-value \\\\\n")
cat("\\midrule\n")
for (cat in cats) {
    row_ix = which(df_p$CATEGORY %in% cat)    
    rural_piped_r =
        df_p[row_ix, "rural_piped_r", drop=T] %>%
        as.numeric 
    rural_piped_p =
        df_p[row_ix, "rural_piped_p", drop=T] %>%
        as.numeric
    rural_piped_r %<>% format_r_value(rural_piped_p, sig_level)
    rural_piped_p %<>% format_p_value(sig_level)    
    urban_piped_r =
        df_p[row_ix, "urban_piped_r", drop=T] %>%
        as.numeric    
    urban_piped_p =
        df_p[row_ix, "urban_piped_p", drop=T] %>%
        as.numeric
    urban_piped_r %<>% format_r_value(urban_piped_p, sig_level)
    urban_piped_p %<>% format_p_value(sig_level)    
    rural_sachet_r =
        df_p[row_ix, "rural_sachet_r", drop=T] %>%
        as.numeric
    rural_sachet_p =
        df_p[row_ix, "rural_sachet_p", drop=T] %>%
        as.numeric
    rural_sachet_r %<>% format_r_value(rural_sachet_p, sig_level)
    rural_sachet_p %<>% format_p_value(sig_level)    
    urban_sachet_r =
        df_p[row_ix, "urban_sachet_r", drop=T] %>%
        as.numeric
    urban_sachet_p =
        df_p[row_ix, "urban_sachet_p", drop=T] %>%
        as.numeric
    urban_sachet_r %<>% format_r_value(urban_sachet_p, sig_level)
    urban_sachet_p %<>% format_p_value(sig_level)    
    cat(
        cat, " & ",
        rural_sachet_r, " & ", rural_sachet_p, " & ",
        rural_piped_r, " & ", rural_piped_p, " & ",
        urban_sachet_r, " & ", urban_sachet_p, " & ",
        urban_piped_r, " & ", urban_piped_p, " \\\\\n"
    )
}
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
sink()

## Table - Correlation between SHDI and the consumption rate of
## different types of drinking water for different regions:
vars = c(
    'const', 'YEAR', 'WH', 'REGION',
    'HH_DWELLING_1', 'HH_LIGHTING_1', 'HH_TOILET_1',
    'HH_TOILET_2','HH_FUEL_1', 'HH_SOLIDWASTE_1',
    'HH_LIQUIDWASTE_1', 'HH_COMPUTER_1', 'HH_MOBILE_1',
    'HH_HHWATER_1', 'HH_OWNERSHIP_1', 'HH_HEAD_GENDER_1',
    'HH_GEO_1', 'HH_DRINK_SACHET', 'WATER_DRINKING',
    'WATER_HOUSEHOLD_IMPROVED','HH_DRINK_PIPED',
    'HH_DRINK_PIPED_PRIVATE'
)
ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)

shdi =
    HDI_region %>%
    left_join(GNI_region) %>%
    left_join(EI_region) %>%
    left_join(HI_region)

summarise_fun = function(ds) {
    summary_tbl =
        ds %>%
        mutate_at(vars(starts_with('HH'), starts_with('WATER')), list(~. * WH)) %>%
        group_by(YEAR, REGION) %>%
        summarize_at(
            vars(starts_with('HH'), starts_with('WATER'), 'WH'),
            sum, na.rm=TRUE
        ) %>%
        mutate_at(vars(starts_with("HH")), list(~. / WH * 100)) %>%
        ungroup() %>%
        left_join(shdi, by=c("YEAR","REGION"))
    summary_tbl
}

summary_tbl_ghana = summarise_fun(ds)
summary_tbl_urban = ds %>% filter(HH_GEO_1) %>% summarise_fun
summary_tbl_rural = ds %>% filter(!HH_GEO_1) %>% summarise_fun

vars = c(
    'HDI', 'GNI', 'EI', 'HI', 'HH_DWELLING_1',
    'HH_LIGHTING_1', 'HH_TOILET_1', 'HH_TOILET_2',
    'HH_FUEL_1', 'HH_SOLIDWASTE_1', 'HH_LIQUIDWASTE_1',
    'HH_COMPUTER_1', 'HH_MOBILE_1', 'HH_HHWATER_1',
    'HH_OWNERSHIP_1', 'HH_HEAD_GENDER_1', 'HH_GEO_1',
    'HH_DRINK_SACHET', 'WATER_DRINKING', 'WATER_HOUSEHOLD_IMPROVED',
    'HH_DRINK_PIPED', 'HH_DRINK_PIPED_PRIVATE'
)

correlate_factors = c('HDI', 'GNI', 'EI', 'HI')

df_p_ghana =
    compute_correlation(summary_tbl_ghana) %>%
    rename(
        ghana_sachet_r=sachet_r,
        ghana_sachet_p=sachet_p,
        ghana_piped_r=piped_r,
        ghana_piped_p=piped_p
    )

df_p_rural =
    compute_correlation(summary_tbl_rural) %>%
    rename(
        rural_sachet_r=sachet_r,
        rural_sachet_p=sachet_p,
        rural_piped_r=piped_r,
        rural_piped_p=piped_p
    )

df_p_urban =
    compute_correlation(summary_tbl_urban_w_accra) %>%
    rename(
        urban_sachet_r=sachet_r,
        urban_sachet_p=sachet_p,
        urban_piped_r=piped_r,
        urban_piped_p=piped_p
    )

df_p =
    df_p_ghana %>%
    left_join(df_p_rural) %>%
    left_join(df_p_urban) 

df_p %<>%
    mutate(
        CATEGORY=c(
            "Subnational HDI", "Gross National Income",
            "Education Index", "Health Index"
        )
    )

## Use in the following section:
row_ix = which(df_p$CATEGORY %in% "Subnational HDI")
ghana_sachet_r = df_p$ghana_sachet_r[row_ix]
ghana_sachet_p = df_p$ghana_sachet_p[row_ix]
ghana_piped_r = df_p$ghana_piped_r[row_ix]
ghana_piped_p = df_p$ghana_piped_p[row_ix]

cats = df_p$CATEGORY

sink(file.path(outdir, "table5.tex"))
cat("\\begin{tabular}{l r r r r r r r r}\n")
cat("\\toprule\n")
cat("Characteristic & \\multicolumn{4}{c}{Rural} & \\multicolumn{4}{c}{Urban} \\\\\n")
cat("\\cmidrule(lr){2-5}\n")
cat("\\cmidrule(lr){6-9}\n")
cat("& Sachet & p-value & Piped & p-value & Sachet & p-value & Piped & p-value \\\\\n")
cat("\\midrule\n")
for (cat in cats) {
    row_ix = which(df_p$CATEGORY %in% cat)    
    rural_piped_r =
        df_p[row_ix, "rural_piped_r", drop=T] %>%
        as.numeric 
    rural_piped_p =
        df_p[row_ix, "rural_piped_p", drop=T] %>%
        as.numeric
    rural_piped_r %<>% format_r_value(rural_piped_p, sig_level)
    rural_piped_p %<>% format_p_value(sig_level)    
    urban_piped_r =
        df_p[row_ix, "urban_piped_r", drop=T] %>%
        as.numeric    
    urban_piped_p =
        df_p[row_ix, "urban_piped_p", drop=T] %>%
        as.numeric
    urban_piped_r %<>% format_r_value(urban_piped_p, sig_level)
    urban_piped_p %<>% format_p_value(sig_level)    
    rural_sachet_r =
        df_p[row_ix, "rural_sachet_r", drop=T] %>%
        as.numeric
    rural_sachet_p =
        df_p[row_ix, "rural_sachet_p", drop=T] %>%
        as.numeric
    rural_sachet_r %<>% format_r_value(rural_sachet_p, sig_level)
    rural_sachet_p %<>% format_p_value(sig_level)    
    urban_sachet_r =
        df_p[row_ix, "urban_sachet_r", drop=T] %>%
        as.numeric
    urban_sachet_p =
        df_p[row_ix, "urban_sachet_p", drop=T] %>%
        as.numeric
    urban_sachet_r %<>% format_r_value(urban_sachet_p, sig_level)
    urban_sachet_p %<>% format_p_value(sig_level)    
    cat(
        cat, " & ",
        rural_sachet_r, " & ", rural_sachet_p, " & ",
        rural_piped_r, " & ", rural_piped_p, " & ",
        urban_sachet_r, " & ", urban_sachet_p, " & ",
        urban_piped_r, " & ", urban_piped_p, " \\\\\n"
    )
}
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
sink()

## Table - Usage of sachet water among households of different characteristcs.
vars = c(
    'const', 'REGION', 'YEAR', 'WH',
    'HH_DWELLING_1', 'HH_DWELLING_4', 'HH_LIGHTING_1',
    'HH_LIGHTING_2', 'HH_TOILET_1', 'HH_TOILET_2',
    'HH_TOILET_3', 'HH_FUEL_1', 'HH_FUEL_2',
    'HH_SOLIDWASTE_1', 'HH_SOLIDWASTE_4',
    'HH_LIQUIDWASTE_1', 'HH_LIQUIDWASTE_2',
    'HH_COMPUTER_1', 'HH_COMPUTER_2',
    'HH_MOBILE_1', 'HH_MOBILE_2',
    'HH_HHWATER_1', 'HH_HHWATER_2',
    'HH_OWNERSHIP_1', 'HH_OWNERSHIP_2',
    'HH_HEAD_GENDER_1', 'HH_HEAD_GENDER_2',
    'HH_GEO_1', 'HH_GEO_2',
    'HH_DRINK_SACHET'
)

ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)

summarise_fun = function(ds) {
    summary_tbl =
        ds %>%
        mutate_at(vars(starts_with('HH_'), -HH_DRINK_SACHET), list(ZZ_SCALED=~.* WH)) %>% 
        mutate_at(vars(ends_with('ZZ_SCALED')), list(SACHET=~. * HH_DRINK_SACHET)) %>%
        group_by(YEAR) %>%
        summarize_at(vars(starts_with('HH_')), sum, na.rm=TRUE) %>%
        dplyr::select(YEAR, contains('ZZ_SCALED')) %>%
        gather(-YEAR, key=colname, value=value) %>%
        separate(colname, c('KEY','suffix'), sep='_ZZ_') %>%
        spread(key=suffix, value=value) 
    summary_tbl_a =
        summary_tbl %>%
        dplyr::select(-SCALED) %>%
        spread(key=YEAR, value=SCALED_SACHET) %>%
        mutate(X2010_2017=100 * (`2017`-`2010`)/7) %>%
        mutate(X2010_2013=100 * (`2013`-`2010`)/7) %>%
        mutate(X2013_2017=100 * (`2017`-`2013`)/7) %>%
        dplyr::select(-(`2010`:`2017`))
    summary_tbl_b =
        summary_tbl %>%
        mutate(SACHET_PCT=100 * SCALED_SACHET / SCALED) %>%
        dplyr::select(-SCALED_SACHET, -SCALED) %>%
        spread(key=YEAR, value=SACHET_PCT)
    summary_tbl = summary_tbl_b %>% left_join(summary_tbl_a, by='KEY') %>% left_join(hh_factors, by="KEY")
    summary_tbl    
}

## Rural
summary_tbl_rural =
    ds %>%
    filter(!HH_GEO_1) %>%
    summarise_fun() %>%
    dplyr::select(-IMPROVED) %>%
    dplyr::select(-starts_with('X')) %>% 
    `colnames<-`(c("KEY","rur2010","rur2013","rur2017","CATEGORY","DESCRIPTION"))

## Urban (excl. Accra)
summary_tbl_urban =
    ds %>%
    filter(HH_GEO_1) %>%
    filter(!REGION %in% 3) %>%
    summarise_fun() %>%
    dplyr::select(-IMPROVED) %>%
    dplyr::select(-starts_with('X')) %>% 
    `colnames<-`(c("KEY","urb2010","urb2013","urb2017","CATEGORY","DESCRIPTION"))

## Urban (Greater Accra only)
summary_tbl_accra =
    ds %>%
    filter(REGION %in% 3) %>%
    filter(HH_GEO_1) %>%
    summarise_fun() %>%
    dplyr::select(-IMPROVED) %>%
    dplyr::select(-starts_with('X')) %>% 
    `colnames<-`(c("KEY","acc2010","acc2013","acc2017","CATEGORY","DESCRIPTION"))

summary_tbl =
    summary_tbl_urban %>%
    left_join(summary_tbl_rural) %>%
    left_join(summary_tbl_accra) %>%
    mutate(urb2010=formatC(urb2010, digits=1, format="f")) %>% 
    mutate(urb2013=formatC(urb2013, digits=1, format="f")) %>% 
    mutate(urb2017=formatC(urb2017, digits=1, format="f")) %>% 
    mutate(acc2010=formatC(acc2010, digits=1, format="f")) %>% 
    mutate(acc2013=formatC(acc2013, digits=1, format="f")) %>% 
    mutate(acc2017=formatC(acc2017, digits=1, format="f")) %>%
    mutate(rur2010=formatC(rur2010, digits=1, format="f")) %>% 
    mutate(rur2013=formatC(rur2013, digits=1, format="f")) %>% 
    mutate(rur2017=formatC(rur2017, digits=1, format="f"))

cats = unique(hh_factors$CATEGORY) %>% `[`(!. %in% "Urban or rural")

sink(file.path(outdir, "table6.tex"))
cat("\\begin{tabular}{l r r r r r r r r r}\n")
cat("\\toprule\n")
cat("Characteristic & \\multicolumn{3}{c}{Rural} & \\multicolumn{3}{c}{Urban (excl. Accra)} & \\multicolumn{3}{c}{Urban (Accra only)} \\\\\n")
cat("\\cmidrule(lr){2-4}\n")
cat("\\cmidrule(lr){5-7}\n")
cat("\\cmidrule(lr){8-10}\n")
cat("& 2010 & 2013 & 2017 & 2010 & 2013 & 2017 & 2010 & 2013 & 2017 \\\\\n")
cat("\\midrule\n")
for (cat in cats) {
    cat(cat, "&&&&&&&&&\\\\\n")
    tmp = summary_tbl %>% filter(CATEGORY %in% cat)
    for (i in 1:nrow(tmp)) {
        desc = tmp$DESCRIPTION[i]
        urb2010 = tmp[["urb2010"]][i]
        urb2013 = tmp[["urb2013"]][i]
        urb2017 = tmp[["urb2017"]][i]
        acc2010 = tmp[["acc2010"]][i]
        acc2013 = tmp[["acc2013"]][i]
        acc2017 = tmp[["acc2017"]][i]
        rur2010 = tmp[["rur2010"]][i]
        rur2013 = tmp[["rur2013"]][i]
        rur2017 = tmp[["rur2017"]][i]
        cat(
            "\\hspace{6mm}", desc, " & ",
            rur2010, " & ", rur2013, " & ", rur2017, " & ",
            urb2010, " & ", urb2013, " & ", urb2017, " & ",
            acc2010, " & ", acc2013, " & ", acc2017, "\\\\\n"
        )
    }    
}
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
sink()

## Table - Usage of sachet water among households of different
## geographical conditions
vars = c(
    'const', 'REGION', 'YEAR', 'WH',
    'HH_GEO_1', 'HH_GEO_2', 'HH_DRINK_SACHET'
)
ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)

summarise_ghana_fun = function(ds) {
    summary_tbl =
        ds %>%
        mutate(HH_DRINK_SACHET_WH=HH_DRINK_SACHET*WH) %>%
        group_by(YEAR) %>%
        summarize_at(vars(HH_DRINK_SACHET_WH, WH), sum, na.rm=TRUE) %>%
        mutate(HH_DRINK_SACHET_PCT=100 * HH_DRINK_SACHET_WH / WH) %>%
        dplyr::select(-HH_DRINK_SACHET_WH, -WH) %>%
        ungroup() %>% 
        mutate(YEAR=paste0('X', YEAR)) %>%
        spread(key=YEAR, value=HH_DRINK_SACHET_PCT) %>%
        mutate(REGION=0, NAME="Ghana") %>%
        dplyr::select(REGION, X2010, X2013, X2017, NAME)
    summary_tbl
}

summarise_fun = function(ds) {
    summary_tbl =
        ds %>%
        mutate(HH_DRINK_SACHET_WH=HH_DRINK_SACHET*WH) %>%
        group_by(YEAR, REGION) %>%
        summarize_at(vars(HH_DRINK_SACHET_WH, WH), sum, na.rm=TRUE) %>%
        mutate(HH_DRINK_SACHET_PCT=100 * HH_DRINK_SACHET_WH / WH) %>%
        dplyr::select(-HH_DRINK_SACHET_WH, -WH) %>%
        ungroup() %>% 
        mutate(YEAR=paste0('X', YEAR)) %>%
        spread(key=YEAR, value=HH_DRINK_SACHET_PCT) %>%
        left_join(rgn_codes %>% rename(NAME=Region, REGION=ID)) %>%
        arrange(NAME)
    summary_tbl_ghana =
        summarise_ghana_fun(ds) %>%
        rbind(summary_tbl)
    summary_tbl_ghana
}

## Rural
summary_tbl_rural =
    ds %>%
    filter(!HH_GEO_1) %>%
    summarise_fun() %>%
    `colnames<-`(c("REGION","rur2010","rur2013","rur2017","NAME"))

## Urban
summary_tbl_urban =
    ds %>%
    filter(HH_GEO_1) %>%
    summarise_fun() %>%
    `colnames<-`(c("REGION","urb2010","urb2013","urb2017","NAME"))

summary_tbl =
    summary_tbl_urban %>%
    left_join(summary_tbl_rural) %>%
    dplyr::select(-REGION) %>% 
    mutate(urb2010=formatC(urb2010, digits=1, format="f")) %>% 
    mutate(urb2013=formatC(urb2013, digits=1, format="f")) %>% 
    mutate(urb2017=formatC(urb2017, digits=1, format="f")) %>% 
    mutate(rur2010=formatC(rur2010, digits=1, format="f")) %>% 
    mutate(rur2013=formatC(rur2013, digits=1, format="f")) %>% 
    mutate(rur2017=formatC(rur2017, digits=1, format="f"))

rgns = c(
    "Ghana", "Greater Accra", "Ashanti", "Brong Ahafo",
    "Central", "Eastern", "Northern", "Upper East",
    "Upper West", "Volta", "Western"
)

## Write tex table
sink(file.path(outdir, "table7.tex"))
cat("\\begin{tabular}{l r r r r r r }\n")
cat("\\toprule\n")
cat("Region & \\multicolumn{3}{c}{Rural} & \\multicolumn{3}{c}{Urban} \\\\\n")
cat("\\cmidrule(lr){2-4}\n")
cat("\\cmidrule(lr){5-7}\n")
cat("& 2010 & 2013 & 2017 & 2010 & 2013 & 2017 \\\\\n")
cat("\\midrule\n")
for (rgn in rgns) {
    row_ix = which(summary_tbl$NAME %in% rgn)
    urb2010 = summary_tbl[row_ix, "urb2010", drop=T]
    urb2013 = summary_tbl[row_ix, "urb2013", drop=T]
    urb2017 = summary_tbl[row_ix, "urb2017", drop=T]
    rur2010 = summary_tbl[row_ix, "rur2010", drop=T]
    rur2013 = summary_tbl[row_ix, "rur2013", drop=T]
    rur2017 = summary_tbl[row_ix, "rur2017", drop=T]
    cat(
        rgn, " & ",
        rur2010, " & ", rur2013, " & ", rur2017, " & ",
        urb2010, " & ", urb2013, " & ", urb2017,
        "\\\\\n"
    )
}
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
sink()

## Table - Odds ratios of household characteristics as indicators of
## sachet water consumption

## UPDATE: Leaving this out of the analysis because of the problem
## of collinearity. 

## NB some coefficients will be NA, likely because they are collinear.
## By experiment, the following covariates are colinear with one other:
## HH_DWELLING_4, HH_LIGHTING_2, HH_TOILET_3, HH_FUEL_2,
## HH_SOLIDWASTE_4, HH_LIQUIDWASTE_2, HH_COMPUTER_2,
## HH_MOBILE_2, HH_HHWATER_2, HH_OWNERSHIP_2

factors = c(
    'HH_DWELLING_1',
    'HH_LIGHTING_1',
    'HH_TOILET_1', 'HH_TOILET_2',
    'HH_FUEL_1',
    'HH_SOLIDWASTE_1',
    'HH_LIQUIDWASTE_1',
    'HH_COMPUTER_1',
    'HH_MOBILE_1',
    'HH_HHWATER_1',
    'HH_OWNERSHIP_1',
    'HH_GEO_1'
    ## 'HH_HEAD_GENDER_1', 'HH_HEAD_GENDER_2',
    ## 'HH_GEO_1',
    ## 'HH_DRINK_SACHET', 'HH_DRINK_PIPED', 'HH_DRINK_PIPED_PRIVATE'
)    
vars = c('const', 'YEAR', 'REGION', 'WH', 'HH_DRINK_SACHET', factors)

compute_odds_ratio = function(ds) {
    ds %<>% mutate(Y2010=YEAR_2010)
    years_interested = c('Y2010', 'Y2013', 'Y2017')
    ds %<>% mutate(Y2010=YEAR %in% 2010)
    ds %<>% mutate(Y2013=YEAR %in% 2013)
    ds %<>% mutate(Y2017=YEAR %in% 2017)
    cross_terms = rep(NA, length(factors) * length(years_interested))
    count = 1
    for (factor in factors) {
        for (yr in years_interested) {
            newcol = paste0(factor, '_and_', yr)
            ds[[newcol]] = ds[[factor]] & ds[[yr]]
            cross_terms[count] = newcol
            count = count + 1
        }
    }
    terms_combined = c(cross_terms) %>% sort
    n_terms = length(cross_terms)
    terms_combined[seq(1, by=3, to=n_terms)] = cross_terms[grep('Y2010', cross_terms)]
    terms_combined[seq(2, by=3, to=n_terms)] = cross_terms[grep('Y2013', cross_terms)]
    terms_combined[seq(3, by=3, to=n_terms)] = cross_terms[grep('Y2017', cross_terms)]
    form = paste0("HH_DRINK_SACHET~", paste0(c(terms_combined), collapse="+"))
    ## This formula results in a model which is identical to Anson's Python
    ## version, but I don't it's right.
    form = paste0(
        "HH_DRINK_SACHET~",
        paste0(
            "0+const+Y2013+Y2017+", c(years_interested, terms_combined), collapse="+"
        )
    )    
    ds = ds * 1.
    
    ## Dividing weights by 100 seems to give comparable results to Python
    ## - I don't know why, but let's leave it for now    
    ## NB some coefficients will be NA, likely because they are collinear.
    ## By experiment, the following covariates are colinear with one other:
    ## HH_DWELLING_4, HH_LIGHTING_2, HH_TOILET_3, HH_FUEL_2,
    ## HH_SOLIDWASTE_4, HH_LIQUIDWASTE_2, HH_COMPUTER_2,
    ## HH_MOBILE_2, HH_HHWATER_2, HH_OWNERSHIP_2
    mod = glm(form, family='binomial', data=ds, weights=ds[["WH"]] / 100)

    ## Create a null model
    nullform = "HH_DRINK_SACHET~0+Y2010+Y2013+Y2017"
    nullmod = glm(nullform, family='binomial', data=ds, weights=ds[["WH"]] / 100)

    library(MASS)
    ## NB this gives different (but still plausible) results to Python
    conf = confint.default(mod) 
    nullconf = confint.default(nullmod)

    or_df =
        as.data.frame(matrix(data=NA, nrow=length(factors)+1, ncol=10)) %>%
        setNames(c("KEY", "2010_or", "2010_conf_l", "2010_conf_r", "2013_or", "2013_conf_l", "2013_conf_r", "2017_or", "2017_conf_l", "2017_conf_r")) %>%
        mutate(KEY=c("ref", factors))

    for (j in 1:length(years_interested)) {
        yr = gsub("^Y", "", years_interested[j])
        yr_factor = years_interested[j]
        ## fill in data from null model first
        or_df[1, paste0(yr, "_or")] = (
            exp(coef(nullmod)[[yr_factor]]) /
            (1 + exp(coef(nullmod)[[yr_factor]]))
        )
        or_df[1, paste0(yr, "_conf_l")] = (
            exp(nullconf[yr_factor,1]) /
            (1 + exp(nullconf[yr_factor, 1]))
        )    
        or_df[1, paste0(yr, "_conf_r")] = (
            exp(nullconf[yr_factor, 2]) /
            (1 + exp(nullconf[yr_factor, 2]))
        )
        for (i in 1:length(factors)) {
            factor = factors[i]
            row_ix = which(or_df$KEY %in% factor)
            yr_factor = paste0(factors[i], "_and_", years_interested[j])
            or_df[row_ix, paste0(yr, "_or")] = (
                exp(coef(mod)[[yr_factor]]) /
                (1 + exp(coef(mod)[[yr_factor]]))
            )        
            or_df[row_ix, paste0(yr, "_conf_l")] = (
                exp(conf[yr_factor,1]) /
                (1 + exp(conf[yr_factor,1]))
            )        
            or_df[row_ix, paste0(yr, "_conf_r")] = (
                exp(conf[yr_factor,2]) /
                (1 + exp(conf[yr_factor,2]))
            )        
        }
    }
    summary_tbl =
        or_df %>%
        left_join(hh_factors) %>%
        left_join(hh_simple_categories) %>%
        mutate(CATEGORY=SHORT_CATEGORY) %>% 
        unite(CATEGORY, CATEGORY, DESCRIPTION, sep=": ") %>%
        dplyr::select(-IMPROVED, -SHORT_CATEGORY)
    summary_tbl$CATEGORY[summary_tbl$KEY == "ref"] = "Reference"
    summary_tbl$CATEGORY[summary_tbl$KEY == "URBAN_ACCRA"] = "Urban, in Accra"
    summary_tbl$CATEGORY[summary_tbl$KEY == "HH_COMPUTER_1"] = "Technology: Has computer"
    summary_tbl$CATEGORY[summary_tbl$KEY == "HH_MOBILE_1"] = "Technology: Has mobile phone"
    summary_tbl
}

ds = concat_data(ds_census2010, ds_glss6, ds_glss7, vars)
ds %<>% mutate(URBAN_ACCRA=(HH_GEO_1 & (REGION %in% 3)))
factors = c(factors, "URBAN_ACCRA")
ds %<>%
    mutate(YEAR_2010 = YEAR %in% 2010) %>%
    mutate(Y2013 = YEAR %in% 2013) %>%
    mutate(Y2017 = YEAR %in% 2017)
summary_tbl = compute_odds_ratio(ds)
cats = summary_tbl$CATEGORY

## sink(file.path(outdir, "table9.tex"))
## cat("\\begin{tabular}{l r r r }\n")
## cat("\\toprule\n")
## cat("Characteristic & \\multicolumn{3}{c}{Odds ratio} \\\\\n")
## cat("\\cmidrule(lr){2-4}\n")
## cat("& 2010 & 2013 & 2017 \\\\\n")
## cat("\\midrule\n")
## for (cat in cats) {
##     row_ix = which(summary_tbl$CATEGORY %in% cat)
##     myfun = function(row, col) {
##         out =
##             summary_tbl[row, col, drop=T] %>%
##             as.numeric %>%
##             formatC(digits=2, format="f")
##         return(out)
##     }
##     or2010 = myfun(row_ix, "2010_or")
##     or2013 = myfun(row_ix ,"2013_or")
##     or2017 = myfun(row_ix, "2017_or")
##     lconf2010 = myfun(row_ix, "2010_conf_l")
##     lconf2013 = myfun(row_ix, "2013_conf_l")
##     lconf2017 = myfun(row_ix, "2017_conf_l")
##     uconf2010 = myfun(row_ix, "2010_conf_r")    
##     uconf2013 = myfun(row_ix, "2013_conf_r")
##     uconf2017 = myfun(row_ix, "2017_conf_r")
##     conf2010 = paste0("(", lconf2010, "-", uconf2010, ")")
##     conf2013 = paste0("(", lconf2013, "-", uconf2013, ")")
##     conf2017 = paste0("(", lconf2017, "-", uconf2017, ")")
##     cat(
##         cat, " & ",
##         or2010, " & ", or2013, " & ", or2017,
##         "\\\\\n"
##     )
## }
## cat("\\bottomrule\n")
## cat("\\end{tabular}\n")
## sink()

## ####################################################### ##
## ####################################################### ##

## Spatial analysis

## ####################################################### ##
## ####################################################### ##

## Construct a lookup table to map 216 districts to 170 districts
join = st_join(st_centroid(distGeo216[,'ID']), distGeo170[,'ID'])
lut = join %>% as_tibble %>% dplyr::select(ID.x,ID.y)

## Make some small adjustments based on visual inspection
lut$ID.y[lut$ID.x %in% 509] = 509
lut$ID.y[lut$ID.x %in% 626] = 626
lut$ID.y[lut$ID.x %in% 720] = 720
dist216to170 = lut %>% rename(REGDIST=ID.x) %>% rename(REGDIST170=ID.y)

## Identify major cities in Ghana
ghanaCities =
    distGeo216 %>%
    filter(ID %in% c(614,811,304,105)) %>%
    mutate(NAME=c("Sekondi-Takoradi", "Accra", "Kumasi", "Tamale"))

HDI_region = readRDS("data/HDI_ghana_region.rds")
HDI_region %<>%     
    mutate(YEAR=as.numeric(gsub('X', '', YEAR))) %>%
    filter(YEAR %in% c(2010,2013,2017)) %>%
    mutate(YEAR=paste0("X", YEAR)) %>%
    spread(YEAR, HDI) %>%
    rename(ID=REGION)

regGeo %<>% left_join(HDI_region, by='ID')

## Calculate the usage rate of water sources per district
ds_census2010_hh = ds_census2010
ds_census2010_hh$WATER_DRINKING_7[ds_census2010_hh$WATER_DRINKING_7 %in% 17.5] = 18
ds_census2010_hh %<>% left_join(dist216to170)

## census_hh = ds_census2010_hh
ds_census2010_hh %<>% filter(!is.na(WATER_DRINKING))
ds_census2010_hh %<>% left_join(map_drinking_water_source_to_jmp) %>% mutate(WATER_DRINK_SIX=WATER_DRINKING_7)

## Create data frames showing how many households were surveyed
## from each district/region
n_dists = ds_census2010_hh %>%
    dplyr::select(REGDIST170) %>%
    rename(dist=REGDIST170) %>%
    mutate(count = 1) %>%
    group_by(dist) %>%
    summarise(nrow_dist=sum(count)) %>%
    ungroup()

n_regions = ds_census2010_hh %>%
    dplyr::select(REGION) %>%
    mutate(count=1) %>%
    group_by(REGION) %>%
    summarise(nrow_rgn=sum(count)) %>%
    ungroup()

dist_census_water_usage =
    ds_census2010_hh %>%
    mutate(dist = REGDIST170) %>%
    dplyr::select(dist, WATER_DRINK_SIX) %>%
    mutate(count = 1) %>%
    group_by(dist, WATER_DRINK_SIX) %>%
    summarise(sum=sum(count)) %>%
    ungroup() %>% 
    left_join(n_dists, by="dist") %>%
    mutate(pct = sum / nrow_dist) %>%
    dplyr::select(-sum, -nrow_dist)

## Obtain water types
water_types =
    dist_census_water_usage$WATER_DRINK_SIX %>%
    unique %>%
    sort

dist_census_water_usage =
    dist_census_water_usage %>%
    mutate(WATER_DRINK_SIX = paste0("WATER_USAGE_2010_TYPE", WATER_DRINK_SIX)) %>%
    spread(WATER_DRINK_SIX, pct, fill=0)

## Now estimate the most common water source in the district
dist_census_water_usage[["water_common_2010"]] = apply(
    ## pd_dist_census_water_usage[,2:8],
    dist_census_water_usage[,2:8],
    1,
    FUN=function(x) water_types[which.max(x)]
)

## Apply some formatting and join with map of district polygons
dist_census_water_usage %<>%
    rename(DIST2010=dist) %>%
    mutate(DIST2010=as.character(as.numeric(DIST2010)))

## distGeo170 %<>% left_join(pd_dist_census_water_usage)
distGeo170 %<>% left_join(dist_census_water_usage)

## Do the same procedure, but at the regional level
rgn_census_water_usage =
    ds_census2010_hh %>%
    dplyr::select(REGION, WATER_DRINK_SIX) %>%
    mutate(count = 1) %>%
    group_by(REGION, WATER_DRINK_SIX) %>%
    summarise(sum=sum(count)) %>%
    ungroup() %>%
    left_join(n_regions, by="REGION") %>%
    mutate(pct = sum / nrow_rgn) %>%
    dplyr::select(-sum, -nrow_rgn)

pd_rgn_census_water_usage =
    rgn_census_water_usage %>%
    mutate(WATER_DRINK_SIX = paste0("WATER_USAGE_2010_TYPE", WATER_DRINK_SIX)) %>%
    spread(WATER_DRINK_SIX, pct, fill=0)

pd_rgn_census_water_usage[["water_common_2010"]] = apply(
    pd_rgn_census_water_usage[,2:8],
    1,
    FUN=function(x) water_types[which.max(x)]
)

pd_rgn_census_water_usage %<>%
    rename(ID=REGION)

regGeo %<>% left_join(pd_rgn_census_water_usage)

## Compute total number of households in each district
num_hh_2010 =     
    ds_census2010_hh %>%
    dplyr::select(REGDIST170, HHID) %>%
    mutate(count=1) %>% 
    group_by(REGDIST170) %>%
    summarise(NUM_HH_2010=sum(count)) %>%
    ## mutate(NUM_HH_2010=NUM_HH_2010 * 10) %>% 
    rename(DIST2010=REGDIST170) %>%
    mutate(DIST2010=as.character(DIST2010))

distGeo170 %<>%
    left_join(num_hh_2010, by="DIST2010") %>%
    mutate(NUM_HH_2010 = replace_na(NUM_HH_2010, 0)) %>%
    mutate(HH_DENSITY_2010 = NUM_HH_2010 / AREA_KM)

## Compute water consumption at district and regional levels

ds_glss6$WATER_DRINK_SIX=ds_glss6$WATER_DRINKING_7 # FIXME - no need to do this
ds_glss6$WATER_DRINK_SIX[ds_glss6$WATER_DRINK_SIX %in% 17.5] = 18

## Compute weights at district level
dist_weights =
    ds_glss6 %>%
    dplyr::select(dist, WH) %>%
    group_by(dist) %>%
    summarise(wt=sum(WH))

## Compute water usage at the district level
dist_glss6_water_usage =
    ds_glss6 %>%
    dplyr::select(dist, WATER_DRINK_SIX, WH) %>%
    left_join(dist_weights, by='dist') %>%
    mutate(wt=WH/wt) %>% 
    group_by(dist, WATER_DRINK_SIX) %>%
    summarise(sum=sum(wt)) %>%
    ungroup() %>%
    arrange(dist, WATER_DRINK_SIX)

pd_dist_glss6_water_usage =
    dist_glss6_water_usage %>%
    mutate(WATER_DRINK_SIX = paste0("WATER_USAGE_2013_TYPE", WATER_DRINK_SIX)) %>%
    spread(WATER_DRINK_SIX, sum, fill=0)

water_types =
    dist_glss6_water_usage$WATER_DRINK_SIX %>%
    unique %>%
    sort

pd_dist_glss6_water_usage[["water_common_2013"]] = apply(
    pd_dist_glss6_water_usage[,2:8],
    1,
    FUN=function(x) water_types[which.max(x)]
)

pd_dist_glss6_water_usage %<>%
    rename(DIST2010=dist) %>%
    mutate(DIST2010=as.character(as.numeric(DIST2010)))

distGeo170 %<>% left_join(pd_dist_glss6_water_usage)

## Now do the same as above, but at the region level
rgn_weights =
    ds_glss6 %>%
    dplyr::select(REGION, WH) %>%
    group_by(REGION) %>%
    summarise(wt=sum(WH))

rgn_glss6_water_usage =
    ds_glss6 %>%
    dplyr::select(REGION, WATER_DRINK_SIX, WH) %>%
    left_join(rgn_weights, by='REGION') %>%
    mutate(wt=WH/wt) %>% 
    group_by(REGION, WATER_DRINK_SIX) %>%
    summarise(sum=sum(wt)) %>%
    ungroup() %>%
    arrange(REGION, WATER_DRINK_SIX)

## Join with polygons
pd_rgn_glss6_water_usage =
    rgn_glss6_water_usage %>%
    mutate(WATER_DRINK_SIX = paste0("WATER_USAGE_2013_TYPE", WATER_DRINK_SIX)) %>%
    spread(WATER_DRINK_SIX, sum, fill=0)

water_types =
    dist_glss6_water_usage$WATER_DRINK_SIX %>%
    unique %>%
    sort

pd_rgn_glss6_water_usage[["water_common_2013"]] = apply(
    pd_rgn_glss6_water_usage[,2:8],
    1,
    FUN=function(x) water_types[which.max(x)]
)

regGeo %<>% left_join(pd_rgn_glss6_water_usage %>% rename(ID=REGION))

num_hh_2013 =
    ds_glss6 %>%    
    dplyr::select(dist, WH) %>%
    rename(DIST2010=dist) %>% 
    mutate(DIST2010=as.character(as.numeric(DIST2010))) %>% 
    group_by(DIST2010) %>%
    summarise(NUM_HH_2013=sum(WH)) 

distGeo170 %<>%
    left_join(num_hh_2013, by="DIST2010") %>%
    mutate(NUM_HH_2013 = replace_na(NUM_HH_2013, 0)) ## %>%
distGeo170[["HH_DENSITY_2013"]] = distGeo170[["NUM_HH_2013"]] / distGeo170[["AREA_KM"]]

## Repeat the above analysis for GLSS7 (2017) data
ds_glss7$WATER_DRINK_SIX=ds_glss7$WATER_DRINKING_7 # FIXME - no need to do this
ds_glss7$WATER_DRINK_SIX[ds_glss7$WATER_DRINK_SIX %in% 17.5] = 18

dist_weights =
    ds_glss7 %>%
    dplyr::select(dist, WH) %>%
    group_by(dist) %>%
    summarise(wt=sum(WH))

dist_glss7_water_usage =
    ds_glss7 %>%
    dplyr::select(dist, WATER_DRINK_SIX, WH) %>%
    left_join(dist_weights, by='dist') %>%
    mutate(wt=WH/wt) %>% 
    group_by(dist, WATER_DRINK_SIX) %>%
    summarise(sum=sum(wt)) %>%
    ungroup() %>%
    arrange(dist, WATER_DRINK_SIX)

pd_dist_glss7_water_usage =
    dist_glss7_water_usage %>%
    mutate(WATER_DRINK_SIX = paste0("WATER_USAGE_2017_TYPE", WATER_DRINK_SIX)) %>%
    spread(WATER_DRINK_SIX, sum, fill=0)

water_types =
    dist_glss7_water_usage$WATER_DRINK_SIX %>%
    unique %>%
    sort

pd_dist_glss7_water_usage[["water_common_2017"]] = apply(
    pd_dist_glss7_water_usage[,2:8],
    1,
    FUN=function(x) water_types[which.max(x)]
)

pd_dist_glss7_water_usage %<>%
    rename(DIST2010=dist) %>%
    mutate(DIST2010=as.character(as.numeric(DIST2010)))

distGeo170 %<>%
    left_join(pd_dist_glss7_water_usage, by="DIST2010")

rgn_weights =
    ds_glss7 %>%
    dplyr::select(REGION, WH) %>%
    group_by(REGION) %>%
    summarise(wt=sum(WH))

rgn_glss7_water_usage =
    ds_glss7 %>%
    dplyr::select(REGION, WATER_DRINK_SIX, WH) %>%
    left_join(rgn_weights, by='REGION') %>%
    mutate(wt=WH/wt) %>% 
    group_by(REGION, WATER_DRINK_SIX) %>%
    summarise(sum=sum(wt)) %>%
    ungroup() %>%
    arrange(REGION, WATER_DRINK_SIX)

pd_rgn_glss7_water_usage =
    rgn_glss7_water_usage %>%
    mutate(WATER_DRINK_SIX = paste0("WATER_USAGE_2017_TYPE", WATER_DRINK_SIX)) %>%
    spread(WATER_DRINK_SIX, sum, fill=0)

water_types =
    dist_glss7_water_usage$WATER_DRINK_SIX %>%
    unique %>%
    sort

pd_rgn_glss7_water_usage[["water_common_2017"]] = apply(
    pd_rgn_glss7_water_usage[,2:8],
    1,
    FUN=function(x) water_types[which.max(x)]
)

regGeo %<>% left_join(pd_rgn_glss7_water_usage %>% rename(ID=REGION))

num_hh_2017 = 
    ds_glss7 %>%
    dplyr::select(dist, WH) %>%
    rename(DIST2010=dist) %>% 
    mutate(DIST2010=as.character(as.numeric(DIST2010))) %>% 
    group_by(DIST2010) %>%
    summarise(NUM_HH_2017=sum(WH)) 

distGeo170 %<>%
    left_join(num_hh_2017, by="DIST2010") %>%
    mutate(NUM_HH_2017 = replace_na(NUM_HH_2017, 0)) ## %>%
distGeo170[["HH_DENSITY_2017"]] = distGeo170[["NUM_HH_2017"]] / distGeo170[["AREA_KM"]]

## ####################################################### ##
## ####################################################### ##

## Make plots

## ####################################################### ##
## ####################################################### ##

## Create labels for plots
labs = c(
    "Improved: Piped water (private)", # 16
    "Improved: Piped water (public)",  # 17
    "Improved: Sachet water",          # 18
    "Improved: Groundwater",           # 19
    "Improved: Others",                # 20
    "Unimproved: Surface water",       # 21
    "Unimproved: Others"               # 22
)

total_sachet_2010 = sum(distGeo170$water_common_2010 %in% 18)
total_sachet_2013 = sum(distGeo170$water_common_2013 %in% 18)
total_sachet_2017 = sum(distGeo170$water_common_2017 %in% 18)
total_gw_2017 = sum(distGeo170$water_common_2017 %in% 19)

distGeo170$water_common_2010 %<>% factor(levels=c(16,17,18,19,20,21,22), labels=labs)
distGeo170$water_common_2013 %<>% factor(levels=c(16,17,18,19,20,21,22), labels=labs)
distGeo170$water_common_2017 %<>% factor(levels=c(16,17,18,19,20,21,22), labels=labs)

## fill in missing data for 2017
distGeo170$water_common_2017[109] = distGeo170$water_common_2013[109]

tmp =
    distGeo170 %>%
    dplyr::select(water_common_2010, water_common_2013, water_common_2017, geometry) %>%
    gather(VAR, SID, -geometry)

tmp$SID %<>%
    factor()

tmp$VAR %<>%
    factor(
        levels=c(
            "water_common_2010",
            "water_common_2013",
            "water_common_2017"
        ),
        labels=c("2010","2013","2017")
    )

ext = st_bbox(tmp)
xmin = ext$xmin
xmax = ext$xmax
ymin = ext$ymin
ymax = ext$ymax

## ## Load map of regions (10)
## rgns = st_read(
##     dsn="data-raw/Regions_10",
##     layer="Map_of_Regions_in_Ghana"
## ) %>%
##     st_transform(crs=4326) 

## ## Ghana neighbouring countries are: Cote D'Ivoire, Burkina Faso, Togo, Benin
## togo = readRDS("data-raw/GADM/gadm36_TGO_0_sf.rds")
## benin = readRDS("data-raw/GADM/gadm36_BEN_0_sf.rds")
## burkina_faso = readRDS("data-raw/GADM/gadm36_BFA_0_sf.rds")
## cote_divoire = readRDS("data-raw/GADM/gadm36_CIV_0_sf.rds")
## ghana = readRDS("data-raw/GADM/gadm36_GHA_0_sf.rds")
## world = rbind(togo, benin, cote_divoire, ghana, burkina_faso)
## world %<>%
##     mutate(area=st_area(.)) %>%
##     summarise(area=sum(area))

## TODO: check if OK
## rgns = readRDS("data/Map_of_Regions_in_Ghana.rds")
ghana_boundary =
    regGeo %>% 
    ## rgns %>%
    mutate(area = st_area(.)) %>%
    summarise(area=sum(area)) %>%
    mutate(GID_0="GHA", NAME_0="Ghana") %>%
    dplyr::select(-area)

## For now let's not put on country/region boundaries, because the region
## dataset does not correspond exactly with the district boundaries. Hence
## we need to create the datasets by merging (as above), but currently this
## results in several artefacts. Perhaps try doing it in GRASS GIS?
## TODO: create a labelled region plot, for reference
## distGeo170 = st_read(
##     dsn="data-raw",
##     layer="geo2_gh2010"
## ) %>% st_transform(crs=32630) %>% mutate(ID=as.numeric(DIST2010))
## distGeo170[["AREA_KM"]] = st_area(distGeo170) / 1000 / 1000
ghana_cities =
    do.call(rbind, st_geometry(ghanaCities %<>% st_centroid)) %>%
    as_tibble %>%
    setNames(c("lon","lat")) %>%
    mutate(NAME=ghanaCities$NAME)

GeomSf$draw_key = draw_key_polygon3

p = ggplot(data=tmp) +
    geom_sf(data=world, fill="grey", color=NA) +
    geom_sf(aes(fill=SID), size=0.05, color='black') +
    scale_fill_brewer(palette="Set3") +
    ## geom_sf(data=rgns, fill=NA, size=0.3, color='black') +
    geom_sf(data=st_centroid(ghanaCities), size=1) +
    facet_wrap(~VAR, nrow=1) +
    coord_sf(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=TRUE) +
    xlab("") +
    ylab("") +
    theme(
        legend.title=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.key.size=unit(0.3,"cm"),
        strip.text=element_text(size=8, margin=margin(t=3, b=2)),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides(fill=guide_legend(ncol=2))

lut = data.frame(SID=labs, CODE=1:7)
total_2010 = tmp %>% as_tibble %>% dplyr::select(VAR, SID, -geometry) %>% left_join(lut) %>% filter(VAR %in% 2010) %>% mutate(SACHET=(CODE == 3)) %>% summarise(SACHET_TOTAL=sum(SACHET))
cairo_pdf(
    file.path(outdir, "figure6.pdf"),
    width=5, height=4
)
print(p)
dev.off()

## Map of study region
ghana_cities = st_centroid(ghanaCities)
coords = st_coordinates(ghana_cities)
ghana_cities = cbind(ghana_cities, coords)
ghana_cities$nudge_y = c(-0.15, -0.15, -0.15, -0.15)
ghana_cities$nudge_x = c(0.65, 0.25, 0.3, 0.25)

## ghana_rgns =
##     st_read(dsn="data-raw/Regions_10", layer="Map_of_Regions_in_Ghana") %>%
##     st_transform(crs=4326) %>%
##     cbind(st_coordinates(st_centroid(.)))
## ghana_rgns$nudge_y = c(0.2, 0.2, -0.1, 0, -0.15, 0.5, 0, 0, 0, 0)
## ghana_rgns$nudge_x = c(0, 0, 0.2, 0, 0.6, 0, 0, 0, 0.1, 0)
## ghana_rgns$NAME=c("Ashanti", "Brong Ahafo", "Central", "Eastern", "Greater Accra", "Northern", "Upper East", "Upper West", "Volta", "Western")
## ghana_dists =
##     st_read(dsn="data-raw/Districts_216", layer="Map_of_Districts_216") %>%
##     st_transform(crs=4326)

## Study region
library(ggrepel)

## ghana_rgns_gaul =
##     st_read(dsn="data-raw", layer="GHA_admbndp1_1m_GAUL") %>% 
##     cbind(st_coordinates(st_centroid(.)))
ghana_rgns_gaul$nudge_y = c(-0.18, 0, 0, 0, 0.2, 0.7, 0.2, 0.5, 0, 0)
ghana_rgns_gaul$nudge_x = c(0.6, 0, 0, 0, 0, -0.1, 0, 0, 0, 0)

p = ggplot() +
    geom_sf(data=world, fill="grey", color=NA) +
    ## geom_sf(data=ghana_dists, size=0.1, color='white') +
    geom_sf(data=distGeo216, size=0.1, color='white') + #CHECK
    ## geom_sf(data=distGeo170, size=0.025, color='black') +    
    geom_sf(data=ghana_rgns_gaul, fill=NA, size=0.4, color='black') + 
    geom_sf(data=ghana_cities, size=1) +
    geom_text(
        data=ghana_rgns_gaul,
        mapping=aes(X, Y, label=ADM1_NAME),
        size=2,
        fontface="bold",
        nudge_x=ghana_rgns_gaul$nudge_x,
        nudge_y=ghana_rgns_gaul$nudge_y
    )  +
    geom_text(
        data=ghana_cities,
        mapping=aes(X, Y, label=NAME),
        size=2,
        nudge_x=ghana_cities$nudge_x,
        nudge_y=ghana_cities$nudge_y,
        fontface="italic"
    ) +
    coord_sf(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=TRUE) +
    xlab("") +
    ylab("") +
    theme(
        legend.title=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=8)
    )

cairo_pdf(
    file.path(outdir, "figure1.pdf"),
    width=4, height=4
)
print(p)
dev.off()

## ################################### ##
## Choropleths
## ################################### ##

distGeo170 %<>% mutate(sachet_usage_2010 = WATER_USAGE_2010_TYPE18)
distGeo170 %<>% mutate(sachet_usage_2013 = WATER_USAGE_2013_TYPE18)
distGeo170 %<>% mutate(sachet_usage_2017 = WATER_USAGE_2017_TYPE18)

## District-level sachet water consumption

tmp =
    distGeo170 %>%
    dplyr::select(sachet_usage_2010, sachet_usage_2013, sachet_usage_2017, geometry) %>%
    gather(VAR, SID, -geometry)

tmp$VAR %<>% factor(levels=c("sachet_usage_2010", "sachet_usage_2013", "sachet_usage_2017"), labels=c("2010","2013","2017"))

library(RColorBrewer)
pal = colorRampPalette(brewer.pal(9, "PuBu"))

p = ggplot(data=tmp) +
    geom_sf(data=world, fill="grey", color=NA) +
    geom_sf(data=tmp, aes(fill=SID), size=0.1, color='black') +
    scale_fill_distiller(palette="YlGnBu", direction=1) +
    geom_sf(data=st_centroid(ghanaCities), size=1) +
    facet_wrap(~VAR, nrow=1) +
    coord_sf(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=TRUE) +
    xlab("") +
    ylab("") +
    theme(
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.key.height=unit(0.3,"cm"),
        strip.text=element_text(size=8, margin=margin(t=3, b=2)),
        legend.key.width=unit(2, "cm"),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides( # https://ggplot2.tidyverse.org/reference/guide_colourbar.html
        fill=guide_colorbar(
            title="Fraction of households reporting sachet water as primary source",
            title.theme=element_text(size=6),
            title.position="top",
            title.hjust=0,
            frame.colour="black",
            frame.linewidth=0.5,
            ticks=FALSE,
            nbin=30
        )
    )

cairo_pdf(
    file.path(outdir, "figure7.pdf"),
    width=5, height=4
)
print(p)
dev.off()

## District-level sachet water consumption, focusing on south Ghana
common =
    distGeo170 %>%
    dplyr::select(water_common_2010, water_common_2013, water_common_2017, geometry) %>%
    gather(VAR, SID, -geometry)

common$SID %<>%
    factor()

common$VAR %<>%
    factor(
        levels=c("water_common_2010", "water_common_2013", "water_common_2017"),
        labels=c("2010","2013","2017")
    )

common %<>%
    filter(SID %in% "Improved: Sachet water")

pct =
    distGeo170 %>%
    dplyr::select(sachet_usage_2010, sachet_usage_2013, sachet_usage_2017, geometry) %>%
    gather(VAR, SID, -geometry)

pct$VAR %<>%
    factor(
        levels=c("sachet_usage_2010", "sachet_usage_2013", "sachet_usage_2017"),
        labels=c("2010","2013","2017")
    )

library(RColorBrewer)
pal = colorRampPalette(brewer.pal(9, "PuBu"))

p = ggplot(data=pct) +
    geom_sf(data=world, fill="grey", color=NA) +
    geom_sf(data=pct, aes(fill=SID), size=0.1, color='black') +
    geom_sf(data=common, fill=NA, size=0.5, color='black') + 
    scale_fill_distiller(palette="YlGnBu", direction=1) +
    geom_sf(data=st_centroid(ghanaCities), size=1) +
    facet_wrap(~VAR, nrow=3, ncol=1) +
    coord_sf(xlim=c(xmin, xmax), ylim=c(ymin, 7.5), expand=TRUE) +
    xlab("") +
    ylab("") +
    theme(
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.key.height=unit(0.3,"cm"),
        strip.text=element_text(size=8, margin=margin(t=3, b=2)),
        legend.key.width=unit(2, "cm"),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides( # https://ggplot2.tidyverse.org/reference/guide_colourbar.html
        fill=guide_colorbar(
            title="Fraction of households reporting sachet water as primary source",
            title.theme=element_text(size=6),
            title.position="top",
            title.hjust=0,
            frame.colour="black",
            frame.linewidth=0.5,
            ticks=FALSE,
            nbin=30
        )
    )

cairo_pdf(
    file.path(outdir, "figure8.pdf"),
    width=5, height=8
)
print(p)
dev.off()

## Spatial autocorrelation analysis (in Python, but plotting done here)

## Write distGeo170 data to file, and run Python script to compute
## spatial autocorrelation
x = distGeo170 %>%
    as.data.frame %>%
    as_tibble %>%
    mutate(dist=ID) %>% 
    dplyr::select(dist, starts_with('sachet_usage'), starts_with('HH_DENSITY')) %>%
    mutate(sachet_usage_2017=replace_na(sachet_usage_2017, 0))
write.csv(x, "data/data_fromR.csv", row.names=FALSE)

## NB Tried using reticulate package, but this causes R to fail with segmentation fault
## NB This will only work on Linux machines, but it is probably straightforward to
## adapt to Windows or Mac
system("bash run-autocorr-analysis.sh")

distGeo170_lisa = st_read(
    dsn="data/dist_geo_170_updated.gpkg"
) %>%
    mutate(ID=as.numeric(dist)) %>%
    dplyr::select('ID', starts_with('p_'), starts_with('q_'))

## join two data frames by ID
dat = inner_join(
    distGeo170 %>% as.data.frame(),
    distGeo170_lisa %>% as.data.frame(),
    by = "ID"
) ## %>%
    ## dplyr::select(-geometry.y) %>%
    ## rename(geometry=geometry.x)

distGeo170 = dat %>% st_sf(sf_column_name = 'geometry')

## Do not include LISA analysis
## distGeo170 =
##     distGeo170 %>%
##     mutate(highh_2010 = ((p_sim_2010<0.20) & (q_2010 == 1)) * 1) %>%
##     mutate(lowh_2010  = ((p_sim_2010<0.20) & (q_2010 == 3)) * 2) %>%
##     mutate(highh_2013 = ((p_sim_2013<0.20) & (q_2013 == 1)) * 1) %>%
##     mutate(lowh_2013  = ((p_sim_2013<0.20) & (q_2013 == 3)) * 2) %>%
##     mutate(highh_2017 = ((p_sim_2017<0.20) & (q_2017 == 1)) * 1) %>%
##     mutate(lowh_2017  = ((p_sim_2017<0.20) & (q_2017 == 3)) * 2) %>%
##     mutate(lisa_2010 = highh_2010 + lowh_2010) %>%
##     mutate(lisa_2010 = na_if(lisa_2010, 0)) %>% 
##     mutate(lisa_2013 = highh_2013 + lowh_2013) %>%
##     mutate(lisa_2013 = na_if(lisa_2013, 0)) %>% 
##     mutate(lisa_2017 = highh_2017 + lowh_2017) %>%
##     mutate(lisa_2017 = na_if(lisa_2017, 0))

## tmp =
##     distGeo170 %>%
##     dplyr::select(lisa_2010, lisa_2013, lisa_2017, geometry) %>%
##     gather(VAR, SID, -geometry)

## tmp$SID %<>%
##     factor(levels=c(1,2), labels=c("High sachet water consumption and high household density", "Low sachet water consumption and low household density"))

## tmp$VAR %<>% factor(levels=c("lisa_2010", "lisa_2013", "lisa_2017"), labels=c("2010","2013","2017"))

## p = ggplot(data=tmp) +
##     geom_sf(data=world, fill="grey", color=NA) +
##     geom_sf(aes(fill=SID), size=0.05, color='black') +
##     scale_fill_brewer(palette="Set1", na.translate=FALSE) +
##     geom_sf(data=st_centroid(ghanaCities), size=1) +
##     facet_wrap(~VAR, nrow=1) +
##     coord_sf(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand=TRUE) +
##     xlab("") +
##     ylab("") +
##     theme(
##         legend.title=element_blank(),
##         legend.position="bottom",
##         legend.text=element_text(size=6),
##         axis.text=element_text(size=6),
##         axis.title=element_text(size=8),
##         legend.key.size=unit(0.3,"cm"),
##         strip.text=element_text(size=8, margin=margin(t=3, b=2)),
##         legend.key=element_rect(fill="white"),
##         plot.caption=element_text(size=6)
##     ) +
##     guides(fill=guide_legend(ncol=1))

## cairo_pdf(
##     file.path(outdir, "lisa_cluster_map.pdf"),
##     width=5, height=4
## )
## print(p)
## dev.off()

## Tables 2 and 3 (actually combine these)
moranBV_I =
    read.csv("data/moranBV_I_data.csv") %>%
    mutate(var=gsub("sachet_usage_", "", var)) %>%
    rename(Year=var, BV_I=I, BV_p_sim=p_sim, BV_z_norm=z_norm) %>%
    arrange(Year)

moran_I =
    read.csv("data/moran_I_data.csv") %>%
    mutate(var=gsub("sachet_usage_", "", var)) %>%
    rename(Year=var) %>%
    arrange(Year) %>%
    left_join(moranBV_I)

## Write tex table
sink(file.path(outdir, "table2.tex"))
cat("\\begin{tabular}{l r r r}\n")
cat("\\toprule\n")
cat("Year & Moran's I & p-value & z-score \\\\\n")
cat("\\midrule\n")
for (year in c(2010, 2013, 2017)) {
    row_ix = which(moran_I$Year %in% year)
    I = moran_I[row_ix, "I", drop=TRUE] %>%
        as.numeric %>%
        formatC(digits=2, format="f")
    p = moran_I[row_ix, "p_sim", drop=TRUE] %>% as.numeric
    if (p <= 0.001) {
        I_p_sim = paste0("\\textless{}", moran_I[row_ix, "p_sim", drop=TRUE])
    } else {
        I_p_sim = p %>% formatC(digits=3, format="f")
    }    
    I_z_norm =
        moran_I[row_ix, "z_norm", drop=TRUE] %>%
        as.numeric %>%
        formatC(digits=2, format="f")        
    cat(year, " & ", I, " & ", I_p_sim, " & ", I_z_norm, "\\\\\n")
}
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
sink()

## Correlation between regional HDI and use of sachet or piped water as
## primary drinking water source

hdi_2010 = regGeo['X2010']
hdi_2013 = regGeo['X2013']
hdi_2017 = regGeo['X2017']

## piped water
piped_water_usage_2010 = regGeo[['WATER_USAGE_2010_TYPE16']] + regGeo[['WATER_USAGE_2010_TYPE17']]
piped_water_usage_2013 = regGeo[['WATER_USAGE_2013_TYPE16']] + regGeo[['WATER_USAGE_2013_TYPE17']]
piped_water_usage_2017 = regGeo[['WATER_USAGE_2017_TYPE16']] + regGeo[['WATER_USAGE_2017_TYPE17']]

## Correlation between SHDI and sachet/piped water consumption in Ghana
tmp = regGeo %>%
    as_tibble() %>% 
    rename(
        HDI_2010=X2010,
        HDI_2013=X2013,
        HDI_2017=X2017
    ) %>%
    dplyr::select(starts_with('HDI'), ends_with('TYPE18'), ends_with('TYPE16'), ends_with('TYPE17'), REGION) %>%
    mutate(
        SACHET_2017=WATER_USAGE_2017_TYPE18,
        SACHET_2013=WATER_USAGE_2013_TYPE18,
        SACHET_2010=WATER_USAGE_2010_TYPE18,
        PIPED_2017=WATER_USAGE_2017_TYPE16+WATER_USAGE_2017_TYPE17,
        PIPED_2013=WATER_USAGE_2013_TYPE16+WATER_USAGE_2013_TYPE17,
        PIPED_2010=WATER_USAGE_2010_TYPE16+WATER_USAGE_2010_TYPE17
    ) %>%
    dplyr::select(-contains('TYPE')) %>% 
    gather(VAR, SID, -REGION) %>% 
    separate(VAR, into=c("V","YEAR"), sep="_(?=[^_]+$)") %>%
    mutate(YEAR=as.numeric(YEAR)) %>%
    spread(V, SID) %>%
    gather(SOURCE, PCT, -REGION, -YEAR, -HDI)

tmp$YEAR %<>% factor(levels=c(2010, 2013, 2017), labels=c("2010", "2013", "2017"))
tmp$SOURCE %<>% factor(levels=c("PIPED","SACHET"), labels=c("Piped","Sachet"))
tmp$REGION %<>%
    factor(
        levels=c(
            "ASHANTI", "BRONG AHAFO", "CENTRAL", "EASTERN", "GREATER ACCRA",
            "NORTHERN", "UPPER EAST", "UPPER WEST", "VOLTA", "WESTERN"
        ),
        labels=c(
            "Ashanti", "Brong Ahafo", "Central", "Eastern", "Greater Accra",
            "Northern", "Upper East", "Upper West", "Volta", "Western"
        )
    )
tmp$PCT %<>% `*`(100)

p = ggplot(tmp, aes(x=HDI, y=PCT)) + 
    geom_smooth(aes(x=HDI, y=PCT), method='lm', se=FALSE, size=0.2) +
    geom_point(aes(x=HDI, y=PCT, color=YEAR), size=0.5) +
    xlim(c(0.4, 0.7)) +
    scale_y_continuous(limits=c(0,100), breaks=c(0,50,100), labels=c("0", "50", "100")) + 
    facet_grid(REGION ~ SOURCE) +
    xlab("HDI") +
    ylab("Household water preference rate (%)") + 
    theme(
        legend.title=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.key.size=unit(0.3,"cm"),
        strip.text.x=element_text(size=8, margin=margin(t=3, b=2)),
        strip.text.y=element_text(size=6, margin=margin(r=3, l=2)),
        legend.key=element_rect(fill="white"),
        plot.caption=element_text(size=6)
    ) +
    guides(color=guide_legend(ncol=3))
p

cairo_pdf(
    file.path(outdir, "figure9.pdf"),
    width=5, height=8
)
print(p)
dev.off()
