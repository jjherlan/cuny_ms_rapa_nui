require(devtools)
require(tidyverse)

require(rgl)

install.packages("rgl")

install_github("tylermorganwall/rayshader")

require(rayshader)
require(rayimage)
require(rayrender)
require(rgl)

# Sample data
# Create a temp file to hold downloaded data

mydata <- tempfile()

# Download raster image, "mydata" into the destination file

download.file("https://tylermw.com/data/dem_01.tif.zip", mydata)

# Use the raster package to unzip and load the raster image

localtif = raster::raster(unzip(mydata, "dem_01.tif"))

#Optional: delete original files now that we have our image (localtif) attached

unlink(mydata)
rm(mydata)

# Create a matrix from the raster image using a rayshader function

elmat = raster_to_matrix(localtif)

# Before 

elmat %>%
  plot_map()

# Sphere shade function onto t atexture

elmat %>%
  sphere_shade(texture = 'desert') %>% # texture of the map
  plot_map()

elmat %>%
  sphere_shade(sunangle = 45, texture = 'desert') %>% # texture of the map
  plot_map()

# Hexcode, or color string

elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_map()

# Ray tracing simulates a variety of optical effects: 
# Reflection, refraction, doft shadow, etc.
# The numeric varibale suggests intensity of light & quality

elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  plot_map()

elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 2.5) %>%
  plot_map()

# Leave at 0.5

elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  plot_map()

# Ambient occlusion shadoow layer
# Atmospheric scattering

elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0.5) %>%
  plot_map()

# Ambient occlusion shadoow layer
# Atmospheric scattering
# 

# use "plot_3d" to create a 3D model of the matrix and 

# Open an rgl window

require(rgl)

rgl::open_3D()

elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0.5) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, 
          zoom = 0.75, phi = 45, windowsize = c(1000, 800))

# turn the rgl window into an image
render_snapshot()

# save the 3D model as a Stereolithography file, saved in

save_3dprint("elmat.stl", maxwidth = 150, unit = "mm")

























