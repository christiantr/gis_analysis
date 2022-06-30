
getClosest <- function(srcList, dst, server, invert = FALSE) {
  minDist <- 10^10
  closest <- list(0)
  g <- list(0)
  for (i in 1:nrow(srcList)) {
    src <- srcList[i, ]
    # print(paste0(src$Name, "-->", dst$osm_id))
    if (invert == FALSE) {
      actualsource <- src
      actualdestination <- dst
    }
    else {
      actualsource <- dst
      actualdestination <- src
    }

    routefoot <- osrmRoute(src = actualsource, dst = actualdestination, returnclass = "sf", overview = "full", osrm.server = server)


    if (routefoot$distance < minDist) {
      minDist <- routefoot$distance
      g <- routefoot
      closest <- actualsource
      if (invert == TRUE) {
        closest <- actualdestination
      }
    }
  }

  if (invert == FALSE) {
    return(list(src = closest, dst = dst, geometry = g))
  }
  else {
    return(list(src = dst, dst = closest, geometry = g))
  }
}

getClosestLS <- function(srcList, dst, server, invert = FALSE) {
  cl <- getClosest(srcList, dst, server, invert)
  src <- cl$src
  dst <- cl$dst


  geometry <- cl$geometry
  if (invert == FALSE) {
    d <- st_sf(data.frame(srcID = src$id, dstID = dst$osm_id, duration = geometry$duration, distance = geometry$distance, geometry = geometry$geometry))
  }
  if (invert == TRUE) {
    d <- st_sf(data.frame(srcID = src$osm_id, dstID = dst$id, duration = geometry$duration, distance = geometry$distance, geometry = geometry$geometry))
  }
  return(d)
}

getAllClosestLS <- function(srcList, dstCandidates, server, invert = FALSE) {
  col <- list(0)
  for (i in 1:nrow(dstCandidates))
  {
    print(paste0(i, " of ", nrow(dstCandidates)))

    n <- getClosestLS(srcList, dstCandidates[i, ], server = u, invert)
    if (i == 1) {
      col <- n
    }
    else {
      col <- dplyr::bind_rows(col, n)
    }
  }
  return(col)
}
