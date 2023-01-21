# 5. 3D SPIKE MAP
#----------------
setwd("/Users/mpopovic3/Downloads/JRC_GRID_2018")
# libraries we need
libs <- c(
    "tidyverse", "sf", "classInt", "giscoR", "eurostat",
    "rayshader"
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

# define projections
# longlat
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Lambert
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

pop_df <- sf::st_read("JRC_POPULATION_2018.shp") %>%
    sf::st_transform(crs = crsLAEA)

head(pop_df)

# bounding box
bb <- sf::st_sfc(
    sf::st_polygon(list(cbind(
        c(-10.6600, 36.5500, 36.5500, -10.6600, -10.6600),
        c(34.5000, 34.5000, 71.0500, 71.0500, 34.5000)
    ))),
    crs = crsLONGLAT
)

laeabb <- sf::st_transform(bb, crs = crsLAEA)
b <- sf::st_bbox(laeabb)

pop_df$TOT_P_2018[pop_df$TOT_P_2018 == 0] <- NA

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

size <- 4000
pop_rast <- stars::st_rasterize(pop_df %>%
    select(TOT_P_2018, geometry),
nx = floor(size * x_rat), ny = floor(size * y_rat)
)

mat <- matrix(pop_rast$TOT_P_2018,
    nrow = floor(size * x_rat), ncol = floor(size * y_rat)
)

cols <- c(
    "#3b1c8c", "#21908d", "#5ac865", "#f9e721"
)

texture <- grDevices::colorRampPalette(cols, bias = 3)(256)

# Create the initial 3D object
mat %>%
    rayshader::height_shade(texture = texture) %>%
    rayshader::plot_3d(
        heightmap = mat,
        solid = F,
        soliddepth = 0,
        z = 50,
        shadowdepth = 0,
        shadow_darkness = .75,
        windowsize = c(800, 800),
        phi = 65,
        zoom = .65,
        theta = -30,
        background = "white"
    )

# Use this to adjust the view after building the window object
# rayshader::render_camera(phi = 65, zoom = .5, theta = -30)

rayshader::render_highquality(
    filename = "eur_population_3d.png",
    samples = 450,
    preview = T,
    light = T,
    lightdirection = c(290, 290, 280, 280),
    lightcolor = c(cols[1], "white", cols[2], "white"),
    lightintensity = 2500,
    lightaltitude = c(10, 80, 10, 80),
    interactive = F,
    width = 8000, height = 8000
)
