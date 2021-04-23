#' Species-specific threshold maps
#'
#'  Classify predicted species distribution maps into binary raster and visualize
#'
#' @export
#' @param x vector or matrix
#' @param y raster layers in a stack, brick or list
#' @return A map, histogram and calculated area in km2
#'
binary_rasters <- function(x, y) { # x is a vector, y is a list of raster

   # unit testing and background checks

  if(class(x) != "numeric") {
    stop("Please provide a numeric input")
  }

  # check  y is a list, else convert to a list and further check if
  # if list list elements are raster layers

  if(class(y) != "list"){
    y <- as.list(y)

    for (i in seq_along(y)) {
     if(class(y[[i]]) !=RasterLayer)
     stop("elments of the list must be raster layers")
   }
  }

  if (length(x) != length(y)) {
    stop("`x` and `y` must be the same length", call. = FALSE)
  }


  binary_list <- list()
  for (i in seq_along(y)) {
    binary_list[[i]] <- matrix(c(0, y[i], 0, y[i],1, 1),
                               ncol = 3, byrow=TRUE)
  }
  ras_rcl<- purrr::map2(x, binary_list,
                        function(.x, .y) raster::reclassify(.x, .y))

  map_sum <- sum(brick(ras_rcl))

  ras_area <- sapply(ras_rcl,
                     function(x) sum(x[]==1, na.rm = TRUE)*prod(res(x)) * 1e-6)

  avg_area <- sum(ras_area)/length(ras_rcl)

  par(mfrow =c(1, 2))

  # histogram  plot
  hist(ras_area, col="green",
       main="varaibility in area covered by species",
       text(x= 5000, y=1, paste("mean_area=", avg_area,"km2"),
            cex = 1.5))
  # map plot
  plot(map_sum,
       main= "species diversity map")
}




