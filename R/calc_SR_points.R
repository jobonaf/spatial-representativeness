library(futile.logger)
library(glue)

# for each point SR: area and population
SR_points <- function(r, pop, zones, points, cont=F, unc_based=T, pollutant, ...) {
  source("/lustre/arpa/bonafeg/scratch/spatial-representativeness/R/spat_repr.R")
  library(raster)
  library(magrittr)
  XY <- coordinates(points)
  sa <- sp <- NULL
  bb <- list()
  for (i in 1:nrow(XY)) {
    flog.info(glue("Processing site {points@data$Station[i]}"))
    x <- XY[i,1]
    y <- XY[i,2]
    if(unc_based) {
      same_zone(r,x,y,zones) %>% in_uncertainty(.,x,y, pollutant=pollutant, ...)  -> SR
    } else {
      same_zone(r,x,y,zones) %>% similar(.,x,y, ...)  -> SR
    }
    if(cont) SR %>% contiguous(.,x,y) -> SR
    bb <- c(bb,SR)
    sa <- c(sa, SR %>% region_area())
    sp <- c(sp, SR %>% region_pop(pop))
  }
  df <- data.frame(points@coords, points@data, SR_Area=sa, SR_Pop=sp)
  BB <- brick(bb)
  names(BB) <- points@data$Station
  return(list(SR_regions=BB, SR_data=df))
}

