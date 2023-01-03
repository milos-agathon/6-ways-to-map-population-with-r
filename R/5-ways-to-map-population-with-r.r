################################################################################
#                 5 ways to map population with R
#                 Milos Popovic
#                 2022/12/10
################################################################################

setwd("C:/users/milos/Downloads")

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

# 1. DATA
#--------

# define longlat projection
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# get NUTS3 shapefile
deu_nuts3 <- giscoR::gisco_get_nuts(
    year = "2021",
    epsg = "4326",
    resolution = "3",
    nuts_level = "3",
    country = "DE"
)

# get population data
pop_df <- eurostat::get_eurostat("demo_r_pjangrp3",
    time_format = "num"
) %>%
    dplyr::filter(
        sex == "T" &
            unit == "NR" &
            age == "TOTAL" &
            grepl("DE", geo) &
            time == 2021
    ) %>%
    dplyr::select(geo, values)

names(pop_df)[1] <- "NUTS_ID"

# merge shp and data.frame
df <- deu_nuts3 %>%
    left_join(pop_df, by = "NUTS_ID")
head(df)

# 2. CONSTANTS
#--------------

# ggplot2 theme

theme_for_the_win <- function() {
    theme_minimal() +
        theme(
            text = element_text(family = "Montserrat"),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(
                size = 50, color = "grey80", hjust = 0.5, vjust = 10
            ),
            axis.title.y = element_blank(),
            legend.position = c(.5, 1.025),
            legend.text = element_text(size = 60, color = "grey20"),
            legend.title = element_text(size = 70, color = "grey20"),
            legend.spacing.y = unit(0.25, "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(
                face = "bold", size = 100, color = "#3f95a6", hjust = .5
            ),
            plot.caption = element_text(
                size = 50, color = "grey20", hjust = .5, vjust = 1
            ),
            plot.subtitle = element_text(size = 60, color = "#a04f68", hjust = .5),
            plot.margin = unit(
                c(t = 0, r = 0, b = 0, l = 0), "lines"
            ),
            plot.background = element_blank(),
            panel.background = element_blank(),
            legend.background = element_blank(),
            panel.border = element_blank()
        )
}

# Lambert projection
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# colors
cols <- rev(c(
    "#140e26", "#451a40",
    "#7d1d53", "#b32957", "#d45450",
    "#eb804e", "#ffdc58"
))
# newcol <- colorRampPalette(cols)
# ncols <- 7
# cols2 <- newcol(ncols)

# 3. POLYGONS
#------------

get_polygon <- function() {
    # st_area returns square meters so we get square km by dividing the result by 1000 squared
    df$area_sqkm <- as.numeric(sf::st_area(df) / 1000000)

    deu_polygon <- df %>%
        st_transform(crs = crsLAEA) %>%
        dplyr::mutate(pop_per_sqr_km = values / area_sqkm)

    return(deu_polygon)
}

deu_polygon <- get_polygon()

# min/max values
vmin <- min(deu_polygon$pop_per_sqr_km, na.rm = T)
vmax <- max(deu_polygon$pop_per_sqr_km, na.rm = T)

# bins
brk <- round(classIntervals(deu_polygon$pop_per_sqr_km,
    n = 6,
    style = "equal"
)$brks, 0) %>%
    head(-1) %>%
    tail(-1) %>%
    append(vmax)

# breaks
breaks <- c(vmin, brk)

make_polygon_map <- function() {
    p1 <-
        ggplot(deu_polygon) +
        geom_sf(aes(fill = pop_per_sqr_km),
            color = "grey20",
            size = .1
        ) +
        scale_fill_gradientn(
            name = "",
            colours = cols,
            breaks = breaks,
            labels = round(breaks, 0),
            limits = c(vmin, vmax)
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
        theme_for_the_win() +
        labs(
            y = "",
            subtitle = "",
            x = "",
            title = "",
            caption = ""
        )
    return(p1)
}

map1 <- make_polygon_map()
ggsave(
    filename = "germany_population_polygon.png",
    width = 7, height = 8.5, dpi = 600, device = "png",
    bg = "white", map1
)

# 4. POINTS
#-----------

# normalize population size
df$pop_1000s <- df$values / 1000

# min/max values
vmin <- min(df$pop_1000s, na.rm = T)
vmax <- max(df$pop_1000s, na.rm = T)

# bins
brk <- round(classIntervals(df$pop_1000s,
    n = 6,
    style = "equal"
)$brks, 0) %>%
    head(-1) %>%
    tail(-1) %>%
    append(vmax)

# breaks
breaks <- c(vmin, brk)

deu_points <- df %>%
    sf::st_centroid()

make_point_map <- function() {
    p2 <-
        ggplot(deu_points) +
        geom_sf(aes(
            fill = pop_1000s,
            color = pop_1000s, size = pop_1000s
        )) +
        scale_fill_gradientn(
            name = "",
            colours = cols,
            breaks = breaks,
            labels = round(breaks, 0),
            limits = c(vmin, vmax)
        ) +
        scale_color_gradientn(
            name = "",
            colours = cols,
            breaks = breaks,
            labels = round(breaks, 0),
            limits = c(vmin, vmax)
        ) +
        scale_size(
            breaks = breaks,
            range = c(1, 8),
            limits = c(vmin, vmax),
            name = ""
        ) +
        guides(
            color = "none",
            #size = "none",
            size = guide_legend(override.aes = list(alpha = 1)),
            fill = guide_legend(
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
            )
        ) +
        theme_for_the_win() +
        labs(
            y = "",
            subtitle = "",
            x = "",
            title = "",
            caption = ""
        )
    return(p2)
}

map2 <- make_point_map()
ggsave(
    filename = "germany_population_point.png",
    width = 7, height = 8.5, dpi = 600, device = "png",
    bg = "white", map2
)


# 5. CARTOGRAM
#-------------



get_cartogram <- function() {
    deu_cart <- df %>%
        st_transform(crs = crsLAEA) %>%
        cartogram::cartogram_cont(
            weight = "pop_1000s",
            itermax = 5
        )
    return(deu_cart)
}

deu_cart <- get_cartogram()

make_cartogram <- function() {
    p2 <-
        ggplot(deu_cart) +
        geom_sf(aes(fill = pop_1000s),
            color = "grey20",
            size = .1
        ) +
        scale_fill_gradientn(
            name = "",
            colours = cols,
            breaks = breaks,
            labels = round(breaks, 0),
            limits = c(vmin, vmax)
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
        theme_for_the_win() +
        labs(
            y = "",
            subtitle = "",
            x = "",
            title = "",
            caption = ""
        )
    return(p2)
}

map2 <- make_cartogram()
ggsave(
    filename = "germany_cartogram.png",
    width = 7, height = 8.5, dpi = 600,
    device = "png", bg = "white", map2
)

# Non-contiguous Area Cartogram
get_ncontig_cartogram <- function() {
    deu_ncart <- df %>%
        st_transform(crs = crsLAEA) %>%
        cartogram::cartogram_ncont(
            weight = "pop_1000s",
            inplace = F
        )
    return(deu_ncart)
}

deu_ncart <- get_ncontig_cartogram()

make_ncontig_cartogram <- function() {
    p2b <-
        ggplot(deu_ncart) +
        geom_sf(aes(fill = pop_1000s),
            color = NA,
            size = 0
        ) +
        geom_sf(
            data = deu_nuts3, fill = "transparent",
            color = "grey20", size = .1
        ) +
        scale_fill_gradientn(
            name = "",
            colours = cols,
            breaks = breaks,
            labels = round(breaks, 0),
            limits = c(vmin, vmax)
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
        theme_for_the_win() +
        labs(
            y = "",
            subtitle = "",
            x = "",
            title = "",
            caption = ""
        )
    return(p2b)
}

map2b <- make_ncontig_cartogram()
ggsave(
    filename = "germany_ncontig_cartogram.png",
    width = 7, height = 7.5, dpi = 600, device = "png",
    bg = "white", map2b
)

# Non-overlapping Circles Cartogram
get_dorling_cartogram <- function() {
    deu_dorling_cart <- df %>%
        st_transform(crs = crsLAEA) %>%
        cartogram::cartogram_dorling(
            weight = "pop_1000s"
        )
    return(deu_dorling_cart)
}

deu_dorling_cart <- get_dorling_cartogram()

make_dorling_cartogram <- function() {
    p2c <-
        ggplot(deu_dorling_cart) +
        geom_sf(aes(fill = pop_1000s),
            color = NA,
            size = 0
        ) +
        scale_fill_gradientn(
            name = "",
            colours = cols,
            breaks = breaks,
            labels = round(breaks, 0),
            limits = c(vmin, vmax)
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
        theme_for_the_win() +
        labs(
            y = "",
            subtitle = "",
            x = "",
            title = "",
            caption = ""
        )
    return(p2c)
}

map2c <- make_dorling_cartogram()
ggsave(
    filename = "germany_dorling_cartogram.png",
    width = 7, height = 7.5, dpi = 600, device = "png",
    bg = "white", map2c
)





# 3. GRID
#--------

# 4. DOT DENSITY
#---------------

# 5. 3D SPIKE MAP
#----------------

pop_df <- sf::st_read("D:/population grid 2018/JRC_POPULATION_2018.shp") %>%
    st_transform(crs = crsLAEA)

head(pop_df)

# bounding box

bb <- st_sfc(
    st_polygon(list(cbind(
        c(-10.6600, 36.5500, 36.5500, -10.6600, -10.6600),
        c(34.5000, 34.5000, 71.0500, 71.0500, 34.5000)
    ))),
    crs = crsLONGLAT
)

laeabb <- st_transform(bb, crs = crsLAEA)
b <- st_bbox(laeabb)

cntrys <- giscoR::gisco_get_countries(
    year = "2020",
    epsg = "4326",
    resolution = "3",
    region = c("Europe", "Asia")
)
names(cntrys)

europeList <- function() {
    urlfile <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"

    iso2 <- read.csv(urlfile) %>%
        dplyr::filter(region == "Europe") %>%
        dplyr::select("name", "alpha.2") %>%
        dplyr::rename(
            iso2 = alpha.2,
            country = name
        ) %>%
        dplyr::mutate(
            country = replace(
                country, str_detect(country, "Bosnia and Herzegovina"), "Bosnia-Herzegovina"
            )
        ) %>%
        dplyr::mutate(
            country = replace(
                country, str_detect(country, "United Kingdom of Great Britain and Northern Ireland"), "United Kingdom"
            )
        )

    return(iso2)
}

iso2 <- europeList()
eu_list <- c(unique(iso2$iso2))
no_data_list <- c("BY", "MD", "RU", "UA", "TR")
eu_list <- eu_list[!(eu_list %in% no_data_list)]

eu <- cntrys %>%
    mutate(CNTR_ID = recode(CNTR_ID, "UK" = "GB")) %>%
    mutate(CNTR_ID = recode(CNTR_ID, "EL" = "GR")) %>%
    filter(CNTR_ID %in% eu_list) %>%
    st_transform(crs = crsLAEA)

plot(sf::st_geometry(eu))

pop_df$TOT_P_2018[pop_df$TOT_P_2018 == 0] <- NA

ni <- classInt::classIntervals(pop_df$TOT_P_2018,
    n = 8,
    style = "quantile"
)$brks
# create categories
labels <- c()
for (i in 1:length(ni)) {
    labels <- c(labels, paste0(
        round(ni[i], 0),
        "–",
        round(ni[i + 1], 0)
    ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable
# based on the breaks and labels above
pop_df$cat <- cut(pop_df$TOT_P_2018,
    breaks = ni,
    labels = labels,
    include.lowest = T
)

# label NAs, too
lvl <- levels(pop_df$cat)
lvl[length(lvl) + 1] <- "No people"
pop_df$cat <- factor(pop_df$cat, levels = lvl)
pop_df$cat[is.na(pop_df$cat)] <- "No people"

levels(pop_df$cat)

yind <- st_distance(
    st_point(c(b[["xmin"]], b[["ymin"]])),
    st_point(c(b[["xmin"]], b[["ymax"]]))
)
xind <- st_distance(
    st_point(c(b[["xmin"]], b[["ymin"]])),
    st_point(c(b[["xmax"]], b[["ymin"]]))
)


if (yind > xind) {
    y_rat <- 1
    x_rat <- xind / yind
} else {
    x_rat <- 1
    y_rat <- yind / xind
}

size <- 8000
pop_rast <- stars::st_rasterize(pop_df %>%
    select(TOT_P_2018, geometry),
nx = floor(size * x_rat), ny = floor(size * y_rat)
)

mat <- matrix(pop_rast$TOT_P_2018,
    nrow = floor(size * x_rat), ncol = floor(size * y_rat)
)

cols <- rev(c(
    "#140e26", "#451a40",
    "#7d1d53", "#b32957", "#d45450",
    "#eb804e", "#f9ad51", "#ffdc58"
))

texture <- grDevices::colorRampPalette(cols, bias = 3)(256)

# Create the initial 3D object
mat %>%
    height_shade(texture = texture) %>%
    plot_3d(
        heightmap = mat,
        # This is my preference, I don't love the `solid` in most cases
        solid = FALSE,
        soliddepth = 0,
        # You might need to hone this in depending on the data resolution;
        # lower values exaggerate the height
        z = 35,
        # Set the location of the shadow, i.e. where the floor is.
        # This is on the same scale as your data, so call `zelev` to see the
        # min/max, and set it however far below min as you like.
        shadowdepth = 0,
        # Set the window size relatively small with the dimensions of our data.
        # Don't make this too big because it will just take longer to build,
        # and we're going to resize with `render_highquality()` below.
        windowsize = c(800, 800),
        # This is the azimuth, like the angle of the sun.
        # 90 degrees is directly above, 0 degrees is a profile view.
        phi = 85,
        zoom = .55,
        # `theta` is the rotations of the map. Keeping it at 0 will preserve
        # the standard (i.e. north is up) orientation of a plot
        theta = 0,
        background = "#ffdc58"
    )

# Use this to adjust the view after building the window object
render_camera(phi = 45, zoom = .55, theta = -20)

render_highquality(
    # We test-wrote to this file above, so we know it's good
    filename = "eur_population_3d.png",
    # See rayrender::render_scene for more info, but best
    # sample method ('sobol') works best with values over 256
    samples = 450,
    preview = FALSE,
    # Turn light off because we're using environment_light
    light = TRUE,
    lightdirection = c(290, 290, 280, 280),
    lightcolor = c(cols[1], "white", colors[2], "white"),
    lightintensity = c(2000, 50, 2000, 50),
    lightaltitude = c(10, 80, 10, 80),
    # All it takes is accidentally interacting with a render that takes
    # hours in total to decide you NEVER want it interactive
    interactive = FALSE,
    # HDR lighting used to light the scene
    # environment_light = "assets/env/phalzer_forest_01_4k.hdr",
    # # environment_light = "assets/env/small_rural_road_4k.hdr",
    # # Adjust this value to brighten or darken lighting
    # intensity_env = 1.5,
    # # Rotate the light -- positive values move it counter-clockwise
    # rotate_env = 130,
    # This effectively sets the resolution of the final graphic,
    # because you increase the number of pixels here.
    # width = round(6000 * wr), height = round(6000 * hr),
    width = 8000, height = 8000
)

