# BUILT-UP AREAS
#----------------
setwd("/Users/mpopovic3/Desktop/builtup")

# remotes::install_github("https://github.com/tylermorganwall/rayshader")
# remotes::install_github("https://github.com/tylermorganwall/rayrender")

# libraries we need
libs <- c(
    "tidyverse", "sf", "terra", "giscoR", "eurostat",
    "rayshader"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}
3
# load libraries
invisible(lapply(libs, library, character.only = T))

sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# define projections
# longlat
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# get France shapefile
shp <- giscoR::gisco_get_countries(
    year = "2020",
    epsg = "4326",
    resolution = "3",
    country = "Netherlands"
)

# enlist raster files, merge them into a single file and re-project
rastfiles <- list.files(
    path = getwd(),
    pattern = ".tif$",
    all.files = T,
    full.names = F
)

img <- list.files(getwd(), "tif$", full.names = T)
ic <- sprc(lapply(img, terra::rast))
builtup_raster <- mosaic(ic)
shp_ext <- shp %>%
    terra::vect()

rc <- terra::crop(builtup_raster, shp_ext, mask = T)
plot(rc)

burast <- terra::aggregate(rc, fact = 3)

b <- c(
    xmin = 3.363095, xmax = 7.21627,
    ymin = 50.75099, ymax = 53.55556
)

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

rcr <- rc |>
    raster::raster()

mat <- rayshader::raster_to_matrix(rcr)

cols <- c(
    "#ffe1a8", "#dc9476", 
    "#9a5751", "#472d30"
)

texture <- grDevices::colorRampPalette(cols, bias = 3)(256)

h <- 3884
w <- 2827

# Create the initial 3D object
mat %>%
    rayshader::height_shade(texture = texture) %>%
    rayshader::plot_3d(
        heightmap = mat,
        solid = F,
        soliddepth = 0,
        zscale = 5,
        shadowdepth = 0,
        windowsize = c(800, 800),
        phi = 65,
        zoom = .7,
        theta = 0,
        background = "#ffe1a8"
    )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 65, zoom = .7, theta = -20)

rayshader::render_highquality(
    filename = "/Users/mpopovic3/Downloads/netherlands_builtup_3d.png",
    samples = 450,
    preview = T,
    light = T,
    lightdirection = c(290, 290, 280, 280),
    lightcolor = c(cols[1], "white", cols[2], "white"),
    lightintensity = c(2000, 50, 2000, 50),
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
