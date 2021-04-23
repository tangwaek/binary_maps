#' Species-specific threshold maps
#'
#'  Classify predicted species distribution maps into binary raster and visualize
#'
#' @export
#' @param x vector or matrix
#' @param y raster layers in a stack, brick or list
#' @return A map, histogram and calculated area in km2
#'
binary_rasters <- function(x, y) { # x is list a raster, y is a vector

  # unit testing and background checks

  if(class(y) != "numeric") {
    stop("Please provide a numeric input")
  }

 # check  x is a list, else convert to a list and further check if
 # if list list elements are raster layers

   if(class(x) != "list"){
     x <- as.list(x)

     for (i in seq_along(x)) {
       if(class(x[[i]]) !=RasterLayer)
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

  # map plot
  plot(map_sum,
       main= "species diversity map")

  # pie chart
  lbls <- paste("species", 1:length(ras_area), sep="")
  pie(ras_area, lbls,
      main = "species by area (km2)",
      col = rainbow(length(ras_area)))

  par(mfrow=c(1,1))
   print(
     paste("average area covered by species = ",round(avg_area, 2),"km2", sep="" ))
}




