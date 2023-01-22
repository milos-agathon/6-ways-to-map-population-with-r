################################################################################
#                 6 ways to map population with R
#                 Milos Popovic
#                 2023/01/12
################################################################################
setwd("C:/users/milos/Downloads")
# libraries we need
libs <- c(
    "tidyverse", "sf", "classInt", "giscoR", "eurostat",
    "cartogram", "rayshader"
)

# install.packages("xfun")
# remotes::install_github("tylermorganwall/rayshader")
# remotes::install_github("tylermorganwall/rayrender")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# CONSTANTS
#------------
# define projections
# longlat
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Lambert
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# ggplot2 theme
theme_for_the_win <- function() {
    theme_minimal() +
        theme(
            text = element_text(family = "Montserrat"),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            legend.position = c(.5, 1),
            legend.text = element_text(size = 60, color = "grey20"),
            legend.title = element_text(size = 70, color = "grey20"),
            legend.spacing.y = unit(0.25, "cm"),
            legend.spacing.x = unit(0.25, "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(
                c(t = 0, r = 0, b = 0, l = 0), "lines"
            ),
            plot.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.background = element_rect(fill = "#f5f5f2", color = NA),
            legend.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.border = element_blank(),
        )
}

# colors
cols <- rev(c(
    "#140e26", "#451a40",
    "#7d1d53", "#b32957",
    "#eb804e", "#ffdc58",
    "#eae2b7"
))

# DATA
#-----
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
) |>
    dplyr::filter(
        sex == "T" &
            unit == "NR" &
            age == "TOTAL" &
            grepl("DE", geo) &
            time == 2021
    ) |>
    dplyr::select(geo, values)

names(pop_df)[1] <- "NUTS_ID"

# merge shp and data.frame
df <- deu_nuts3 |>
    left_join(pop_df, by = "NUTS_ID")
head(df)

# 1. POLYGONS
#------------

get_polygon <- function() {
    # st_area returns square meters so we get square km by dividing the result by 1000
    df$area_sqkm <- as.numeric(sf::st_area(df) / 1000000)

    deu_polygon <- df |>
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
)$brks, 0) |>
    head(-1) |>
    tail(-1) |>
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
    width = 6, height = 8.5, dpi = 600, device = "png",
    bg = "#f5f5f2", map1
)

# 2. POINTS
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
)$brks, 0) |>
    head(-1) |>
    tail(-1) |>
    append(vmax)

# breaks
breaks <- c(vmin, brk)

deu_points <- df |>
    sf::st_centroid()

deu_coords <- deu_points |>
    dplyr::mutate(
        long = unlist(map(geometry, 1)),
        lat = unlist(map(geometry, 2))
    ) |>
    dplyr::select(NAME_LATN, long, lat, pop_1000s) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::arrange(desc(pop_1000s))

# function for labelling regions
label_regions <- function() {
    ggrepel::geom_text_repel(deu_coords[1:5, ],
        mapping = aes(x = long, y = lat, label = NAME_LATN),
        colour = "grey20",
        family = "Montserrat",
        fontface = "bold",
        size = 20,
        segment.colour = "grey20",
        segment.alpha = .9,
        segment.linetype = 3,
        segment.size = .25,
        nudge_x = .95,
        nudge_y = .15,
        direction = "x"
    )
}

make_point_map <- function() {
    p2 <-
        ggplot() +
        geom_sf(
            data = deu_polygon,
            fill = "transparent",
            color = "grey20",
            size = .1
        ) +
        geom_sf(
            data = deu_points,
            mapping = aes(
                size = pop_1000s,
                geometry = geometry
            ), color = cols[5],
            alpha = .5
        ) +
        label_regions() +
        scale_size(
            breaks = breaks,
            range = c(1, 10),
            labels = round(breaks, 0),
            limits = c(vmin, vmax),
            name = ""
        ) +
        guides(
            color = "none",
            size = guide_legend(
                direction = "horizontal",
                title.position = "top",
                title.hjust = 0.5,
                label.hjust = 0,
                nrow = 1,
                byrow = F,
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
    width = 6, height = 8.5, dpi = 600, device = "png",
    bg = "white", map2
)

# 3. CARTOGRAM
#-------------

get_cartogram <- function() {
    deu_cart <- df |>
        sf::st_transform(crs = crsLAEA) |>
        cartogram::cartogram_cont(
            weight = "pop_1000s",
            itermax = 5
        ) |>
        sf::st_transform(crs = crsLONGLAT)
    return(deu_cart)
}

deu_cart <- get_cartogram()

make_cartogram <- function() {
    p3a <-
        ggplot(deu_cart) +
        geom_sf(aes(fill = pop_1000s),
            color = "grey20",
            size = .1
        ) +
        label_regions() +
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
    return(p3a)
}

map3a <- make_cartogram()
ggsave(
    filename = "germany_cartogram.png",
    width = 6, height = 8.5, dpi = 600,
    device = "png", bg = "white", map3a
)

# Non-contiguous Area Cartogram
get_ncontig_cartogram <- function() {
    deu_ncart <- df |>
        sf::st_transform(crs = crsLAEA) |>
        cartogram::cartogram_ncont(
            weight = "pop_1000s",
            inplace = F
        )
    return(deu_ncart)
}

deu_ncart <- get_ncontig_cartogram()

make_ncontig_cartogram <- function() {
    p3b <-
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
    return(p3b)
}

map3b <- make_ncontig_cartogram()
ggsave(
    filename = "germany_ncontig_cartogram.png",
    width = 7, height = 7.5, dpi = 600, device = "png",
    bg = "white", map3b
)

# Non-overlapping Circles Cartogram
get_dorling_cartogram <- function() {
    deu_dorling_cart <- df |>
        st_transform(crs = crsLAEA) |>
        cartogram::cartogram_dorling(
            weight = "pop_1000s"
        )
    return(deu_dorling_cart)
}

deu_dorling_cart <- get_dorling_cartogram()

make_dorling_cartogram <- function() {
    p3c <-
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
    return(p3c)
}

map3c <- make_dorling_cartogram()
ggsave(
    filename = "germany_dorling_cartogram.png",
    width = 7, height = 7.5, dpi = 600, device = "png",
    bg = "white", map3c
)


# 4. DOT DENSITY
#---------------

get_dot_density <- function() {
    num_dots <- ceiling(dplyr::select(as.data.frame(df), pop_1000s))
    deu_dots <- map_df(
        names(num_dots),
        ~ sf::st_sample(df, size = num_dots[, .x], type = "random") |>
            sf::st_cast("POINT") |>
            sf::st_coordinates() |>
            as_tibble() |>
            setNames(c("long", "lat"))
    )
    return(deu_dots)
}

deu_dots <- get_dot_density()

make_dot_density_map <- function() {
    p4 <-
        ggplot(deu_dots) +
        geom_sf(
            data = deu_nuts3, fill = "transparent",
            color = "grey20", size = .1
        ) +
        geom_point(
            data = deu_dots, aes(x = long, y = lat),
            color = cols[5], size = .1, shape = 19, alpha = .2
        ) +
        label_regions() +
        theme_for_the_win() +
        labs(
            y = "",
            subtitle = "",
            x = "",
            title = "",
            caption = ""
        )
    return(p4)
}

map4 <- make_dot_density_map()
ggsave(
    filename = "germany_dot_density.png",
    width = 6, height = 8.5, dpi = 600, device = "png",
    bg = "white", map4
)

# 5. GRID MAP
#------------
get_grid <- function() {
    # get Germany shapefile
    deu_sf <- giscoR::gisco_get_countries(
        year = "2020",
        epsg = "4326",
        resolution = "3",
        country = "Germany"
    )

    deu_sf_transf <- deu_sf |>
        sf::st_transform(3575)

    deu_grid <- deu_sf_transf |>
        sf::st_make_grid(cellsize = 50000) |>
        sf::st_intersection(deu_sf_transf) |>
        st_cast("MULTIPOLYGON") |>
        sf::st_sf() |>
        dplyr::mutate(id = row_number()) |>
        sf::st_transform(crs = crsLONGLAT)


    return(deu_grid)
}

deu_grid <- get_grid()

get_aggregated_grid <- function() {
    deu_grid_final <- deu_grid |>
        sf::st_join(deu_points) |>
        dplyr::group_by(id) |>
        dplyr::summarise_at(
            vars(pop_1000s),
            list(pop_sum = sum)
        ) |>
        drop_na(pop_sum) |>
        sf::st_sf() |>
        sf::st_transform(crs = crsLONGLAT)
    return(deu_grid_final)
}

deu_grid_final <- get_aggregated_grid()

# min/max values
vmin <- min(deu_grid_final$pop_sum, na.rm = T)
vmax <- max(deu_grid_final$pop_sum, na.rm = T)

# bins
brk <- round(classIntervals(deu_grid_final$pop_sum,
    n = 6,
    style = "equal"
)$brks, 0) |>
    head(-1) |>
    tail(-1) |>
    append(vmax)

# breaks
breaks <- c(vmin, brk)

make_grid_map <- function() {
    p5 <-
        ggplot(deu_grid_final) +
        geom_sf(aes(fill = pop_sum),
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
        label_regions() +
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
    return(p5)
}

map5 <- make_grid_map()
ggsave(
    filename = "germany_population_grid.png",
    width = 6, height = 8.5, dpi = 600, device = "png",
    bg = "#f5f5f2", map5
)

# 6. SPIKE MAP
#-------------
# Create the initial 3D object
make_raster_matrix <- function() {
    pop_rast <- terra::rasterize(
        deu_grid_final,
        terra::rast(deu_grid_final, resolution = .01),
        deu_grid_final$pop_sum
    ) |> terra::na.omit()

    pop_mat <- rayshader::raster_to_matrix(pop_rast)

    return(pop_mat)
}

pop_mat <- make_raster_matrix()

h <- 762
w <- 916

texture <- grDevices::colorRampPalette(cols, bias = 3)(21)

# Create the initial 3D object
pop_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
        heightmap = pop_mat,
        solid = F,
        soliddepth = 0,
        z = 20,
        shadowdepth = 0,
        shadow_darkness = .75,
        windowsize = c(w, h),
        phi = 65,
        zoom = .6,
        theta = -30,
        background = "white"
    )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 35, zoom = .6, theta = -20)

rayshader::render_highquality(
    filename = "germany_population_3d.png",
    samples = 500,
    preview = T,
    light = T,
    lightdirection = 0,
    lightcolor = "white",
    lightintensity = 1000,
    interactive = F,
    width = w, height = h
)
