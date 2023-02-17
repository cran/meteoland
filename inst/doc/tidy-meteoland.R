## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(meteoland)
library(stars)
library(dplyr)

## ----points_to_interpolate_example--------------------------------------------
points_to_interpolate_example

## ----raster_to_interpolate_example--------------------------------------------
raster_to_interpolate_example

## ----meteoland_meteo_example--------------------------------------------------
meteoland_meteo_example

## ----meteo_names--------------------------------------------------------------
names(meteoland_meteo_example)

## ----quick_interpolation------------------------------------------------------
# creating the interpolator object
interpolator <- with_meteo(meteoland_meteo_example) |>
  create_meteo_interpolator()

# performing the interpolation
points_interpolated <- points_to_interpolate_example |>
  interpolate_data(interpolator)
points_interpolated

## ----non_mandatory_vars_in_meteo, error=TRUE----------------------------------
meteo_without_temp <- meteoland_meteo_example
meteo_without_temp[["MinTemperature"]] <- NULL
meteo_without_temp[["MaxTemperature"]] <- NULL
with_meteo(meteo_without_temp)

## ----interpolatior_params-----------------------------------------------------
# parameters
get_interpolation_params(interpolator)

## ----interpolated_data--------------------------------------------------------
# interpolated meteo for the first location
points_interpolated[["interpolated_data"]][1]

## ----long---------------------------------------------------------------------
tidyr::unnest(points_interpolated, cols = "interpolated_data")

## ----interpolator_class-------------------------------------------------------
class(interpolator)

## ----interpolator_description-------------------------------------------------
interpolator

## ----get_interpolation_params-------------------------------------------------
get_interpolation_params(interpolator)

## ----set_interpolation_params-------------------------------------------------
# wind_height parameter
get_interpolation_params(interpolator)$wind_height

# set a new wind_height parameter and check
interpolator <- set_interpolation_params(interpolator, params = list(wind_height = 5))
get_interpolation_params(interpolator)$wind_height

## ----writing_interpolator-----------------------------------------------------
temporal_folder <- tempdir()
write_interpolator(interpolator, file.path(temporal_folder, "interpolator.nc"))
# file should exists now
file.exists(file.path(temporal_folder, "interpolator.nc"))

## ----reading_interpolator-----------------------------------------------------
file_interpolator <- read_interpolator(file.path(temporal_folder, "interpolator.nc"))
# the read interpolator should be identical to the one we have already
identical(file_interpolator, interpolator)

## ----interpolator_calibration-------------------------------------------------
# min temperature N and alpha before calibration
get_interpolation_params(interpolator)$N_MinTemperature
get_interpolation_params(interpolator)$alpha_MinTemperature

# calibration
interpolator <- interpolator_calibration(
  interpolator,
  variable = "MinTemperature",
  N_seq = c(5, 20),
  alpha_seq = c(1, 10),
  update_interpolation_params = TRUE
)

# parameters after calibration
get_interpolation_params(interpolator)$N_MinTemperature
get_interpolation_params(interpolator)$alpha_MinTemperature

## ----preparing_interpolator---------------------------------------------------
interpolator <- with_meteo(meteoland_meteo_example) |>
  create_meteo_interpolator() |>
  interpolator_calibration(
    variable = "MinTemperature",
    N_seq = c(5, 20),
    alpha_seq = c(1, 10),
    update_interpolation_params = TRUE
  ) |>
  interpolator_calibration(
    variable = "MaxTemperature",
    N_seq = c(5, 20),
    alpha_seq = c(1, 10),
    update_interpolation_params = TRUE
  ) |>
  interpolator_calibration(
    variable = "DewTemperature",
    N_seq = c(5, 20),
    alpha_seq = c(1, 10),
    update_interpolation_params = TRUE
  ) |>
  write_interpolator(
    filename = file.path(temporal_folder, "interpolator.nc"),
    .overwrite = TRUE
  )

## ----cross_validation---------------------------------------------------------
cross_validation <- interpolation_cross_validation(interpolator, verbose = FALSE)
cross_validation$errors
cross_validation$station_stats
cross_validation$dates_stats
cross_validation$r2

## ----summarise_interpolated_data----------------------------------------------
summarise_interpolated_data(
  points_interpolated,
  fun = "mean",
  frequency = "week"
)

## ----erosivity_one_location---------------------------------------------------
precipitation_rainfall_erosivity(
  points_interpolated$interpolated_data[[1]],
  longitude = sf::st_coordinates(points_interpolated$geometry[[1]])[,1],
  scale = 'month'
)

## ----erosivity_mutate---------------------------------------------------------
points_interpolated |>
  mutate(erosivity = precipitation_rainfall_erosivity(
    interpolated_data,
    longitude = sf::st_coordinates(geometry)[,1],
    scale = 'month'
  ))

## ----interpolation_piped------------------------------------------------------
points_interpolated <- points_to_interpolate_example |>
  interpolate_data(interpolator) |>
  summarise_interpolated_data(
    fun = "mean",
    frequency = "week"
  ) |>
  summarise_interpolated_data(
    fun = "max",
    frequency = "month"
  ) |>
  mutate(
    monthly_erosivity = precipitation_rainfall_erosivity(
      interpolated_data,
      longitude = sf::st_coordinates(geometry)[,1],
      scale = 'month'
    )
  )

points_interpolated

## ----raster_to_interpolate----------------------------------------------------
raster_to_interpolate_example

## ----raster_interpolation-----------------------------------------------------
raster_interpolated <- raster_to_interpolate_example |>
  interpolate_data(interpolator)

raster_interpolated

## ----raster_temporal_agg------------------------------------------------------
summarise_interpolated_data(
  raster_interpolated,
  fun = "mean",
  frequency = "week"
)

## ----raster_piped-------------------------------------------------------------
monthly_mean_temperature <- raster_to_interpolate_example |>
  interpolate_data(interpolator, variables = "Temperature") |>
  summarise_interpolated_data(
    fun = "max",
    frequency = "month",
    variable = "MeanTemperature"
  )

plot(monthly_mean_temperature)

