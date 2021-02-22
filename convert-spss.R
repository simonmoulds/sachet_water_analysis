## Author : Simon Moulds
## Date   : Nov 2020-Feb 2021

library(foreign)
library(tidyverse)
library(magrittr)

options(stringsAsFactors = FALSE)

## We do not have permission to distribute the underlying
## census datasets. Hence, to run this script you need to
## download the raw data files from the Ghana Statistical
## Service data repository. You may need to register for
## an account.

## Population & Housing Census 2010
## Downloaded from:
## https://www2.statsghana.gov.gh/nada/index.php/catalog/51/get_microdata
## [Accessed 22/02/2021]

## Unzip raw PHC data
unzip(
    "data-raw/2010\ PHC-10%\ @\ EA.zip",
    exdir="data-raw",
    overwrite=TRUE
)
sav_fn = "data-raw/2010PHC-216D-10%/2010PHC-216D-10%Population@EA.sav"
csv_fn = "data-raw/2010PHC-216D-10%/2010PHC-216D-10_Population@EA.csv"
if (!file.exists(csv_fn)) {
    write.table(
        read.spss(
            "data-raw/2010PHC-216D-10%/2010PHC-216D-10%Population@EA.sav",
            use.value.labels=FALSE
        ),
        file="data-raw/2010PHC-216D-10%/2010PHC-216D-10_Population@EA.csv",
        quote=TRUE,
        sep=","
    )
}
sav_fn = "data-raw/2010PHC-216D-10%/2010PHC-216D-10%Housing@EA.sav"
csv_fn = "data-raw/2010PHC-216D-10%/2010PHC-216D-10_Housing@EA.csv"
if (!file.exists(csv_fn)) {
    write.table(
        read.spss(
            "data-raw/2010PHC-216D-10%/2010PHC-216D-10%Housing@EA.sav",
            use.value.labels=FALSE
        ),
        file="data-raw/2010PHC-216D-10%/2010PHC-216D-10_Housing@EA.csv",
        quote=TRUE,
        sep=","
    )
}

## GLSS6
## Downloaded from:
## https://www2.statsghana.gov.gh/nada/index.php/catalog/72/get_microdata
## [Accessed 22/02/2021]
unzip("data-raw/GLSS6_2012-2013.zip", exdir="data-raw", overwrite=TRUE)
unzip("data-raw/GLSS6_2012-2013/DATA/SPSS/SPSS.zip", exdir="data-raw/GLSS6_2012-2013", overwrite=TRUE)

glss6_files = c(
    "AGGREGATES/gha_2013_l",
    "AGGREGATES/pov_gha_2013_glss6_updated",
    "AGGREGATES/gha_2013_h",
    "PARTA/sec2c",
    "PARTA/sec4a",
    "PARTA/sec6",
    "PARTA/sec7"
)
for (i in 1:length(glss6_files)) {
    sav_fn = file.path(
        "data-raw/GLSS6_2012-2013/SPSS",
        paste0(glss6_files[i], ".sav")
    )
    csv_fn = file.path(
        "data-raw/GLSS6_2012-2013/SPSS",
        paste0(glss6_files[i], ".csv")
    )
    if (!file.exists(csv_fn)) {
        write.table(
            read.spss(sav_fn, use.value.labels=FALSE),
            file=csv_fn,
            quote=TRUE,
            sep=","
        )
    }    
}

options(stringsAsFactors=TRUE)
write.table(
    read.spss(
        "data-raw/GLSS6_2012-2013/SPSS/PARTA/g6loc_edt.sav",
        to.data.frame=TRUE
    ),
    file="data-raw/GLSS6_2012-2013/SPSS/PARTA/g6loc_edt.csv",
    quote=TRUE,
    sep=","
)
options(stringsAsFactors=FALSE)

## GLSS7
## Downloaded from:
## https://www2.statsghana.gov.gh/nada/index.php/catalog/97/get_microdata
## [Accessed 22/02/2021]
unzip("data-raw/GLSS7spss.zip", exdir="data-raw", overwrite=TRUE)
unzip("data-raw/g7spss/g7aggregates.zip", exdir="data-raw/g7spss/g7aggregates", overwrite=TRUE)
unzip("data-raw/g7spss/g7PartA.zip", exdir="data-raw/g7spss/g7PartA", overwrite=TRUE)
glss7_files = c(
    "g7aggregates/16_GHA_2017_I",
    "g7aggregates/18_GHA_POV_2017_GLSS7",
    "g7PartA/g7loc_upd",
    "g7PartA/g7sec7",
    "g7PartA/g7sec2",
    "g7PartA/g7sec4-reviewed",
    "g7PartA/g7sec5c",
    "g7PartA/g7sec6a",
    "g7PartA/g7sec7"
)
for (i in 1:length(glss7_files)) {
    sav_fn = file.path(
        "data-raw/g7spss",
        paste0(glss7_files[i], ".sav")
    )
    csv_fn = file.path(
        "data-raw/g7spss",
        paste0(glss7_files[i], ".csv")
    )
    if (!file.exists(csv_fn)) {
        write.table(
            read.spss(sav_fn, use.value.labels=FALSE),
            file=csv_fn,
            quote=TRUE,
            sep=","
        )
    }    
}
