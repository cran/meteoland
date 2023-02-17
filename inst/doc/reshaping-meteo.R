## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(meteoland)
library(stars)
library(dplyr)

## ----data_preparation, echo=FALSE---------------------------------------------
unformatted_meteo <- tibble(
  date = rep(seq(from = as.Date("2022-12-01"), to = as.Date("2022-12-05"), by = 1), 3),
  station = rep(letters[1:3], each = 5),
  latitude = rep(c(41.35, 40.11, 42.00), each = 5),
  longitude = rep(c(-0.33, 0.12, 1.12), each = 5),
  min_temp = rnorm(15, 10, 6),
  max_temp = min_temp + rnorm(15, 10, 3),
  rh = rnorm(15, 50, 25)
) |>
  dplyr::mutate(
    rh = dplyr::if_else(rh > 100, 100, rh),
    rh = dplyr::if_else(rh < 20, 20, rh)
  )

## ----meteoland_meteo_example--------------------------------------------------
meteoland_meteo_example

## ----meteo_names--------------------------------------------------------------
names(meteoland_meteo_example)

## ----unformatted_meteo--------------------------------------------------------
unformatted_meteo

## ----unformatted_transformation-----------------------------------------------
ready_meteo <- unformatted_meteo |>
  # convert names to correct ones
  dplyr::mutate(
    MinTemperature = min_temp,
    MaxTemperature = max_temp,
    MeanRelativeHumidity = rh
  ) |>
  # transform to sf (WGS84)
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = sf::st_crs(4326)
  )

ready_meteo

## ----meteospain, eval = FALSE-------------------------------------------------
#  library(meteospain)
#  get_meteo_from(
#    "meteogalicia",
#    meteogalicia_options('daily', as.Date("2022-12-01"), as.Date("2022-12-05"))
#  ) |>
#    meteospain2meteoland()

## ----worldmet, eval = FALSE---------------------------------------------------
#  library(worldmet)
#  worldmet::importNOAA("081120-99999", year = 2022) |>
#    worldmet2meteoland()

## ----raster_meteo_preparing, echo = FALSE-------------------------------------
raster_meteo_reference <- interpolate_data(
  raster_to_interpolate_example, meteoland_interpolator_example, verbose = FALSE
)

## -----------------------------------------------------------------------------
raster_meteo_reference

## ----raster_to_points---------------------------------------------------------
points_meteo_reference <- names(raster_meteo_reference) |>
  # for each variable
  purrr::map(
    # take the variable raster
    ~ raster_meteo_reference[.x] |>
      # convert to sf
      sf::st_as_sf(as_points = TRUE, na.rm = FALSE) |>
      # pivot the data for dates to be in one column
      tidyr::pivot_longer(cols = -geometry, names_to = "dates", values_to = .x) |>
      # convert to tibble to fasten the process
      dplyr::as_tibble() |>
      # convert to date and create stationID
      dplyr::mutate(
        dates = as.Date(dates),
        stationID = as.character(geometry)
      )
  ) |>
  # join all variables
  purrr::reduce(dplyr::left_join) |>
  # create the points sf object
  sf::st_as_sf()

points_meteo_reference

## ----raster_interpolator------------------------------------------------------
with_meteo(points_meteo_reference) |>
  create_meteo_interpolator()

