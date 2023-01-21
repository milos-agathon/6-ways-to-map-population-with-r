setwd("/Users/mpopovic3/Downloads")

# libraries we need
libs <- c(
    "tidyverse", "sf", "classInt", "giscoR", "eurostat",
    "cartogram", "rayshader"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

c <- sf::st_read("/Users/mpopovic3/Downloads/chn_adm_ocha_2020_shp/chn_admbnda_adm2_ocha_2020.shp")
tail(c)
names(c)
nrow(c)
names(c)[5] <- "province"

df <- read.csv("census_county_2020_v1.csv", header = T, sep = ";")
head(df)

d <- df[, c(3, 5)] # province and population_total
names(d) <- c("province", "pop")
head(d)
nrow(d)

f <- d %>%
    group_by(province) %>%
    summarise_at(
        vars(pop),
        list(pop_total = max)
    )

head(f)
nrow(f)
names(f) <- c("province", "pop")

# merge shp and data.frame
ch <- c %>%
    left_join(f, by = "province")
head(ch)

nrow(filter(ch, is.na(pop)))