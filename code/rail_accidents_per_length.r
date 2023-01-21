################################################################################
#                  Inset graph within map in R
#                  Milos Popovic
#                  2022/02/12
################################################################################

setwd("/Users/mpopovic3/Downloads/")
# my favorite font ❤️
sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# libraries we need
libs <- c("tidyverse", "sf", "grid", "gridExtra", "giscoR", "classInt")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = TRUE))

# 1. LOAD DATA
#---------

# list of European countries
europeList <- function(urlfile, iso2) {
    urlfile <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
    iso2 <- read.csv(urlfile) %>%
        filter(region == "Europe" | alpha.2 == "TR" | alpha.2 == "CY") %>% # filter Europe, Cyprus and Turkey
        select("alpha.2") %>%
        rename(CNTR_ID = alpha.2)
    return(iso2)
}

countries <- europeList()

# load national map of Europe
europeMap <- function(europe, eur) {
    europe <- giscoR::gisco_get_countries(
        year = "2016",
        epsg = "4326",
        resolution = "10",
        region = c("Europe", "Asia")
    ) %>%
        mutate(FID = recode(FID, "UK" = "GB")) %>%
        mutate(FID = recode(FID, "EL" = "GR"))

    eur <- europe %>%
        dplyr::filter(FID %in% countries$CNTR_ID)
    return(eur)
}

e <- europeMap()

non_eu_list <- c(
    "AL", "BA", "BE", "BY", "GB", "GR", "MD", "ME", "MK", "RS", "RU", "UA"
)
eu_list <- c(unique(e$FID))

eu <- e %>%
    filter(FID %in% eu_list)

non_eu <- e %>%
    filter(FID %in% non_eu_list)

# Eurostat data
# rail accident deaths
get_rail_deaths <- function() {
    rail_deaths_df <- eurostat::get_eurostat("tran_sf_railvi",
        time_format = "num"
    ) %>%
        dplyr::filter(
            accident == "TOTAL" &
                victim == "KIL" &
                pers_cat == "TOTAL" &
                time >= 2020
        ) %>%
        dplyr::select(geo, time, values)

    names(rail_deaths_df) <- c("FID", "time", "deaths")

    # convert to wide format
    rail_deaths_wide <- tidyr::pivot_wider(
        rail_deaths_df,
        names_from = time, values_from = deaths
    )

    # Replace values of 2019 with missing values by values of 2018
    rail_deaths <- rail_deaths_wide %>%
        mutate(deaths = if_else(is.na(`2021`), `2020`, `2021`)) %>%
        select(FID, deaths)

    return(rail_deaths)
}

rail_deaths <- get_rail_deaths()


# rail length
get_rail_length <- function() {
    rail_length_df <- eurostat::get_eurostat("rail_if_line_tr",
        time_format = "num"
    ) %>%
        dplyr::filter(
            tra_infr == "TOTAL" &
                n_tracks == "TOTAL" &
                time >= 2019
        ) %>%
        dplyr::select(geo, time, values)

    names(rail_length_df) <- c("FID", "time", "length_km")

    # convert to wide format
    rail_length_wide <- tidyr::pivot_wider(
        rail_length_df,
        names_from = time, values_from = length_km
    )

    # Replace values of 2019 with missing values by values of 2018
    rail_length <- rail_length_wide %>%
        mutate(length_km = if_else(is.na(`2020`), `2019`, `2020`)) %>%
        select(FID, length_km)

    return(rail_length)
}

rail_length <- get_rail_length()

get_joined_df <- function() {
    df <- e %>%
        dplyr::inner_join(rail_deaths, "FID") %>%
        dplyr::inner_join(rail_length, "FID") %>%
        # tidyr::drop_na() %>%
        dplyr::mutate(
            deaths_100km =
                (100 * deaths / length_km)
        )
    return(df)
}

df <- get_joined_df()
tail(df)

# 4. BREAKS
#----------
makeIntervals <- function() {
    d <- tidyr::drop_na(df)
    # let's find a natural interval with quantile breaks
    ni <- classIntervals(d$deaths_100km,
        n = 6,
        style = "jenks"
    )$brks
    # create categories
    labels <- c()
    for (i in 1:length(ni)) {
        labels <- c(labels, paste0(
            round(ni[i], 1),
            "–",
            round(ni[i + 1], 1)
        ))
    }
    labels <- labels[1:length(labels) - 1]

    # finally, carve out the categorical variable
    # based on the breaks and labels above
    d$cat <- cut(d$deaths_100km,
        breaks = ni,
        labels = labels,
        include.lowest = T
    )
    levels(d$cat) # let's check how many levels it has (6)

    # label NAs, too
    lvl <- levels(d$cat)
    lvl[length(lvl) + 1] <- "No data"
    d$cat <- factor(d$cat, levels = lvl)
    d$cat[is.na(d$cat)] <- "No data"
    levels(d$cat)
    return(d)
}

w <- makeIntervals()

# 2. JOIN DATA
#----------
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

spatialJoin <- function() {
    sf_data <- w %>%
        st_transform(crs = crsLAEA) %>%
        drop_na() %>%
        select(NAME_ENGL, deaths_100km, cat, geometry) %>%
        mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "Bosnia and Herzegovina"), "BIH")) %>%
        mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "Russian Federation"), "Russia")) %>% # shorten names
        mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "North Macedonia"), "N. Macedonia")) %>%
        mutate(NAME_ENGL = replace(NAME_ENGL, str_detect(NAME_ENGL, "United Kingdom"), "UK"))
    return(sf_data)
}

sfd <- spatialJoin()

# 3. BOUNDING BOX
#----------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

bbox <- function(bb, laeabb, b) {
    bb <- st_sfc(
        st_polygon(list(cbind(
            c(-10.6600, 36.5500, 36.5500, -10.6600, -10.6600),
            c(34.5000, 34.5000, 71.0500, 71.0500, 34.5000)
        ))),
        crs = crsLONGLAT
    )

    laeabb <- st_transform(bb, crs = crsLAEA)
    box <- st_bbox(laeabb)
    return(box)
}

b <- bbox()

# 5. PLOT
#----------

orderDF <- function() {
    a <- sfd %>%
        sf::st_drop_geometry()
    a$deaths_100km <- as.numeric(as.character(a$deaths_100km))
    a <- subset(a, !is.na(deaths_100km))
    attach(a)
    a <- a[order(NAME_ENGL, deaths_100km), ]
    return(a)
}

nd <- orderDF()

# boxplot
# boxplot
women_science_boxplot <- function() {
    dat <- nd
    l <- ggplot(dat, aes(
        x = reorder(NAME_ENGL, deaths_100km),
        y = round(deaths_100km, 2), fill = cat
    )) +
        geom_bar(stat = "identity") +
        geom_text(
            data = subset(dat, deaths_100km < .5),
            aes(label = round(deaths_100km, 2)),
            position = position_stack(vjust = .5),
            hjust = 0.5,
            size = 2.75,
            color = "grey10",
            family = "Montserrat"
        ) +
        geom_text(
            data = subset(dat, deaths_100km > .5),
            aes(label = round(deaths_100km, 2)),
            position = position_stack(vjust = .5),
            hjust = 0.5,
            size = 2.75,
            color = "white",
            family = "Montserrat"
        ) +
        scale_fill_manual(
            guide = guide_legend(),
            values = rev(c(
                "#3f1651", "#612b70", "#963586",
                "#cb4978", "#e9716a", "#f89f5b"
            )),
            name = ""
        ) +
        theme(
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            text = element_text(family = "Montserrat"),
            strip.text = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.line = element_line(colour = NA),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y = element_text(
                margin = unit(c(3, 0, 0, 0), "mm"), colour = "grey10", size = 8, hjust = 0
            ),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_text(),
            plot.title = element_text(size = 8, color = "grey20", hjust = .5),
            legend.key = element_rect(fill = NA),
            legend.position = "none", legend.direction = "horizontal"
        ) +
        xlab("") +
        ylab("") +
        coord_flip()
    return(l)
}

boxp <- women_science_boxplot()

# map
women_science_map <- function() {
    shp_data <- sfd

    p <-
        ggplot() +
        geom_sf(
            data = shp_data,
            aes(fill = cat),
            color = "white",
            size = 0.15
        ) +
        geom_sf(
            data = eu, color = "white", size = 0.125, fill = "transparent"
        ) +
        geom_sf(
            data = non_eu, color = "white", size = 0.125, fill = "grey80"
        ) +
        coord_sf(
            crs = crsLAEA,
            xlim = c(b["xmin"], b["xmax"]),
            ylim = c(b["ymin"], b["ymax"])
        ) +
        labs(
            y = "", subtitle = "",
            x = "",
            title = "Rail accident deaths per 100 km of rail",
            caption = "©2023 Milos Popovic https://milospopovic.net\nSource: Eurostat TRAN_SF_RAILVI & RAIL_IF_LINE_TR"
        ) +
        scale_fill_manual(
            name = "",
            values = rev(c(
                "grey80", "#3f1651", "#612b70",
                "#963586", "#cb4978", "#e9716a", "#f89f5b"
            )),
            drop = F
        ) +
        guides(fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.15, units = "mm"),
            keywidth = unit(15, units = "mm"),
            title.position = "top",
            title.hjust = 0.5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "bottom"
        )) +
        theme_minimal() +
        theme(
            text = element_text(family = "Montserrat"),
            panel.background = element_blank(),
            legend.background = element_blank(),
            legend.position = c(.45, -.02),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.title = element_text(
                size = 14, color = "#3f1651", hjust = 0.5, vjust = 1
            ),
            plot.subtitle = element_text(
                size = 11, color = "#7a2b41", hjust = 0.5, vjust = 0, face = "bold"
            ),
            plot.caption = element_text(
                size = 8, color = "grey60", hjust = .5, vjust = -6
            ),
            axis.title.x = element_text(
                size = 10, color = "grey20", hjust = 0.5, vjust = -6
            ),
            legend.text = element_text(size = 9, color = "grey20"),
            legend.title = element_text(size = 11, color = "grey20"),
            strip.text = element_text(size = 12),
            plot.margin = unit(c(t = -4, r = 0, b = -4, l = 10), "lines"),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()
        )
    return(p)
}

map <- women_science_map()

# combine
InsetGraphMap <- function() {
    m <- map
    bp <- boxp
    vp <- viewport(width = 0.3, height = 0.85, x = 0.15, y = 0.5)
    png("rail_accidents.png", height = 4000, width = 4400, res = 600)
    # clear current device
    grid.newpage()
    print(m)
    print(bp + labs(title = ""), vp = vp)
    dev.off()
    return()
}

InsetGraphMap()
