setwd("/Users/mpopovic3/Downloads")

devtools::install_github("RobWHickman/sf.dotdensity")

# libraries we need
libs <- c(
    "tidyverse", "sf", "classInt", "giscoR", "eurostat",
    "sf.dotdensity"
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

# define longlat projection
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# 1. GET europe SF DATA
#----------------------
nuts3 <- gisco_get_nuts(
    year = "2021",
    epsg = "4326",
    resolution = "3",
    nuts_level = "3"
)

# plot(st_geometry(nuts3))

cntrys <- giscoR::gisco_get_countries(
    year = "2020",
    epsg = "4326",
    resolution = "3",
    region = c("Europe", "Asia")
)
names(cntrys)

non_eu_list <- c("AR", "AZ", "BA", "BY", "GE", "MD", "RU", "UA", "XK")
eu_list <- c(unique(nuts3$CNTR_CODE), "XK")

eu <- cntrys %>%
    filter(CNTR_ID %in% eu_list) %>%
    filter(CNTR_ID != "UK")

non_eu <- cntrys %>%
    filter(CNTR_ID %in% non_eu_list)

# plot(st_geometry(non_eu))

bb <- st_sfc(
    st_polygon(list(cbind(
        c(-10.6600, 36.5500, 36.5500, -10.6600, -10.6600),
        c(34.5000, 34.5000, 71.0500, 71.0500, 34.5000)
    ))),
    crs = crsLONGLAT
)

# get NUTS2-level data on NEETs
indic <- eurostat::get_eurostat("demo_r_pjangrp3",
    time_format = "num"
) %>%
    dplyr::filter(
        age == "TOTAL" &
            sex != "T" &
            unit == "NR" &
            time >= 2020
    ) %>%
    dplyr::select(geo, sex, time, values)

names(indic)[1] <- "NUTS_ID"

# convert to wide format
iwide <- tidyr::pivot_wider(indic, names_from = time, values_from = values)

# Replace values of 2019 with missing values by values of 2018
df <- iwide %>%
    mutate(values = if_else(is.na(`2021`), `2020`, `2021`)) %>%
    select(NUTS_ID, sex, values) %>%
    tidyr::pivot_wider(names_from = sex, values_from = values)

tail(df)



# merge shp and data.frame
d <- nuts3 %>%
    dplyr::left_join(df, by = "NUTS_ID") %>%
    dplyr::filter(!is.na(F))

names(d)
sum(d$M) - sum(d$F)
filter(d, grepl("grad", NAME_LATN))



crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

random_round <- function(x) {
    v <- as.integer(x)
    r <- x - v
    test <- runif(length(r), 0.0, 1.0)
    add <- rep(as.integer(0), length(r))
    add[r > test] <- as.integer(1)
    value <- v + add
    ifelse(is.na(value) | value < 0, 0, value)
    return(value)
}

get_dot_density <- function() {
    num_dots <- ceiling(dplyr::select(as.data.frame(d), F:M) / 10000) %>%
        dplyr::mutate_all(random_round)

    dots <- map_df(
        names(num_dots),
        ~ sf::st_sample(d, size = num_dots[, .x], type = "random") %>%
            sf::st_cast("POINT") %>%
            sf::st_as_sf() %>%
            sf::st_transform(crs = crsLAEA) %>%
            sf::st_coordinates() %>%
            as_tibble() %>%
            setNames(c("long", "lat")) %>%
            dplyr::mutate(sex = factor(.x, levels = names(num_dots)))
    )
    return(dots)
}


dots <- get_dot_density() %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

nrow(filter(dots, sex == "F"))
nrow(filter(dots, sex == "M"))

levels(dots$sex)
# transform the shp into Lambert projection

laeabb <- sf::st_transform(bb, crs = crsLAEA)
b <- st_bbox(laeabb)
bbox <- sf::st_bbox(bb)

# map
p <- ggplot() +
    geom_sf(data = eu, color = "grey20", size = 0.25, fill = "transparent") +
    geom_point(
        data = dots, aes(long, lat, colour = sex),
        size = .01, shape = 19, alpha = .5
    ) +
    coord_sf(
        crs = crsLAEA,
        xlim = c(b["xmin"], b["xmax"]),
        ylim = c(b["ymin"], b["ymax"])
    ) +
    scale_color_manual(
        name = "",
        values = c(
            "#D420A4", "#0BD462"
        ),
        labels = c("Women", "Men"),
        drop = F
    ) +
    guides(col = guide_legend(
        override.aes = list(size = 8, alpha = 1)
    )) +
    theme_minimal() +
    theme(
        text = element_text(family = "Montserrat"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(
            size = 50, color = "grey60", hjust = 0.5, vjust = 10
        ),
        axis.title.y = element_blank(),
        legend.position = c(.35, .9),
        legend.text = element_text(size = 60, color = "grey20"),
        legend.title = element_text(size = 70, color = "grey20"),
        legend.spacing.y = unit(0.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold", size = 120, color = "grey20", hjust = .5
        ),
        plot.caption = element_text(
            size = 50, color = "grey20", hjust = .5, vjust = 1
        ),
        plot.subtitle = element_text(size = 80, color = "grey40", hjust = .5),
        plot.margin = unit(
            c(t = 1, r = -2, b = -1, l = -2), "lines"
        ),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        panel.border = element_blank()
    ) +
    labs(
        x = "©2023 Milos Popovic (https://milospopovic.net) Data: Eurostat",
        y = NULL,
        title = "Sex structure of Europe",
        subtitle = "1 dot = 10000 people",
        caption = ""
    )

ggsave(
    filename = "eur_dot_density_sex_2021.png",
    width = 7, height = 8.5, dpi = 600,
    device = "png", bg = "white", p
)
