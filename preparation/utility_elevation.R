library(sf)
library(terra)
library(geodist)
library(tidyverse)



## adds to Elevation stats to geometry data frame
addElevationData <- function(df, dem, minDist = 100, minWalkingDist = 200) {
  df.ext <- df %>%
    add_column(elev_end = NA) %>%
    add_column(elev_min = NA) %>%
    add_column(elev_max = NA) %>%
    add_column(elev_cum = NA) %>%
    add_column(slope_max = NA) %>%
    add_column(slope_min = NA)




  start_time <- Sys.time()
  for (i in 1:nrow(df.ext)) {
    print(paste0(i, " out of ", nrow(df.ext)))

    linestring.xy <- df[i, ]
    stats <- stats(linestring.xy, dem, minDist = minDist)
    df.ext[i, ]$elev_end <- stats$elev_end
    df.ext[i, ]$elev_min <- stats$elev_min
    df.ext[i, ]$elev_max <- stats$elev_max


    if (df.ext[i, ]$distance >(minWalkingDist/1000)) {
      df.ext[i, ]$slope_max <- stats$slope_max
      df.ext[i, ]$slope_min <- stats$slope_min
      df.ext[i, ]$elev_cum <- stats$elev_cum
    }
  }

  end_time <- Sys.time()
  print(paste0("Elapsed time: ", end_time - start_time))

  return(df.ext)
}






stats <- function(linestring.xy, dem, minDist) {
  if (!(sf::st_is_longlat(linestring.xy)) || !(sf::st_is_longlat(dem))) {
    stop("Must be lat/long format")
  }

  m_xy <- sf::st_coordinates(linestring.xy)
  z <- as.numeric(terra::extract(dem, m_xy[, 1:2], method = "simple")[[1]])

  elev_start <- utils::head(z, 1)
  elev_end <- utils::tail(z, 1)

  elev_min <- min(z)
  elev_max <- max(z)

  m_xyz <- cbind(m_xy[, 1:2], z)


  ## Distances between each points (in meters)
  sequential_distances <- geodist::geodist(m_xyz[, 1:2], sequential = TRUE, measure = "haversine")
  # sequential_distances<- geodist::geodist(m[, 1:2], sequential = TRUE, measure="cheap")

  ## Elevation difference (in meters)
  elevation_differences <- diff(z)

  ## Slopes

  ## Al together
  d_e <- as.data.frame(cbind(sequential_distances, elevation_differences))
  d_e <- ensureMinimalDistance(d_e, minDist = minDist)

  # d<- cbind(d_e, slopes)

  slopes <- d_e$elevation_differences / d_e$sequential_distances
  slope_max <- max(abs(slopes))
  slope_min <- min(abs(slopes))

  elev_cum <- sum(abs(d_e$elevation_differences))


  return(list(
    elev_start = elev_start,
    elev_end = elev_end,
    elev_min = elev_min,
    elev_max = elev_max,
    elev_cum = elev_cum,
    slope_max = slope_max,
    slope_min = slope_min
  ))
}


## minDist in meters
ensureMinimalDistance <- function(matrix, minDist) {
  done <- FALSE

  while (!done) {
    short <- which(matrix$sequential_distances < minDist)
    if (length(short) > 0 && short[1] < nrow(matrix)) {
      current_index <- short[1]
      matrix[current_index, ] <- matrix[current_index, ] + matrix[current_index + 1, ]
      matrix <- matrix[-(current_index + 1), ]
    } else {
      done <- TRUE
    }
  }

  last <- nrow(matrix)

  ## if last seqment is too short, and there are more than two seqments
  if (matrix[last, ]$sequential_distances < minDist && last > 2) {
    matrix[last, ] <- matrix[last, ] + matrix[last - 1, ]
    matrix <- matrix[-(last - 1), ]
  }

  ## assign new rownames
  rownames(matrix) <- seq(length = nrow(matrix))

  return(matrix)
}
