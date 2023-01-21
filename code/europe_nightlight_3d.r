# NIGHTLIGHT
#----------------
setwd("/Users/mpopovic3/Desktop/nightlight")

# remotes::install_github("https://github.com/tylermorganwall/rayshader")
# remotes::install_github("https://github.com/tylermorganwall/rayrender")

# libraries we need
libs <- c(
    "tidyverse", "sf", "terra", "giscoR", "eurostat",
    "rayshader", "rfigshare"
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

# 1. NIGHTLIGHT DATA
# list of all nightlight raster files from FigShare
x <- rfigshare::fs_details("9828827", mine = F, session = NULL)
urls <- jsonlite::fromJSON(jsonlite::toJSON(x$files), flatten = T)

# download harmonized nightlight data for 1992-2020
urls$download <- paste0(urls$download_url, "/", urls$name)
urls <- urls[-28, ] # get rid of the 2013 duplicate
url <- urls[, 5][[30]]

for (u in url) {
    download.file(u, basename(u), mode = "wb")
}


# define projections
# longlat
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Lambert
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# enlist raster files, merge them into a single file and re-project
rastfiles <- list.files(
    path = getwd(),
    pattern = ".tif$",
    all.files = T,
    full.names = F
)


builtup_raster <- terra::rast(rastfiles)

bb <- sf::st_sfc(
    sf::st_polygon(list(cbind(
        c(-25.6600, 60.5500, 60.5500, -25.6600, -25.6600),
        c(25.5000, 25.5000, 71.0500, 71.0500, 25.5000)
    ))),
    crs = crsLONGLAT
)

# list of European countries
europeList <- function() {
    urlfile <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
    iso2 <- read.csv(urlfile) %>%
        filter(region == "Europe" |
            alpha.2 == "TR" |
            alpha.2 == "CY" |
            alpha.2 == "GE" |
            alpha.2 == "AR" |
            alpha.2 == "AZ") %>% # filter Europe, Cyprus and Turkey
        select("alpha.2") %>%
        rename(CNTR_ID = alpha.2)
    return(iso2)
}

countries <- europeList()

# load national map of Europe
europeMap <- function() {
    europe <- giscoR::gisco_get_countries(
        year = "2016",
        epsg = "4326",
        resolution = "3",
        region = c("Europe", "Asia")
    ) %>%
        mutate(FID = recode(FID, "UK" = "GB")) %>%
        mutate(FID = recode(FID, "EL" = "GR"))

    eur <- europe %>%
        dplyr::filter(FID %in% countries$CNTR_ID)
    return(eur)
}

eur <- europeMap() |>
    sf::st_crop(sf::st_bbox(bb))

plot(eur)

eur_ext <- eur %>%
    terra::vect()

rc <- terra::crop(builtup_raster, eur_ext, mask = T)
plot(rc)

burast <- terra::project(rc, crsLAEA)
plot(burast)

laeabb <- sf::st_transform(bb, crs = crsLAEA)
b <- sf::st_bbox(laeabb)

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

rcr <- burast |>
    terra::aggregate(fact = 2) |>
    raster::raster()

mat <- rayshader::raster_to_matrix(rcr)

cols <- c(
    "#182833", "#FFD966"
)

texture <- grDevices::colorRampPalette(cols, bias = 3)(256)

h <- 4841
w <- 6955

# Create the initial 3D object
mat %>%
    rayshader::height_shade(texture = texture) %>%
    rayshader::plot_3d(
        heightmap = mat,
        solid = F,
        soliddepth = 0,
        z = 15,
        shadowdepth = 0,
        shadow_darkness = .75,
        windowsize = c(800, 800),
        phi = 65,
        zoom = .4,
        theta = -20,
        background = "#0f1a22"
    )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 35, zoom = .35, theta = -30)

rayshader::render_highquality(
    filename = "europe_nightlight_3d.png",
    samples = 450,
    preview = T,
    light = T,
    lightdirection = c(290, 290, 280, 280),
    lightcolor = cols[1],
    lightintensity = 2500,
    lightaltitude = c(10, 80, 10, 80),
    interactive = F,
    width = w, height = h
)


# 4. ANNOTATE MAP
#----------------
# read image
map <- magick::image_read("/Users/mpopovic3/Downloads/germany_builtup_3d.png")

# set font color
clr <- "#9a031e"

# Title
map_title <- magick::image_annotate(map, "Buildings in Germany",
    font = "Georgia",
    color = alpha(clr, .65), size = 400, gravity = "north",
    location = "+0+80"
)

# Caption
map_final <- magick::image_annotate(map_title, glue::glue(
    "©2023 Milos Popovic (https://milospopovic.net) | ",
    "Data: Copernicus Land Monitoring Service, Global Land Cover"
),
font = "Georgia", location = "+0+50",
color = alpha(clr, .45), size = 100, gravity = "south"
)

magick::image_write(map_final, glue::glue("germany_builtup_annotated.png"))
