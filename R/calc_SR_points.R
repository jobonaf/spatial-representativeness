library(futile.logger)
library(glue)

# for each point SR: area and population
SR_points <- function(r, pop, zones, points, ...) {
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
    same_zone(r,x,y,zones) %>% similar(.,x,y) %>% contiguous(.,x,y) -> SR
    bb <- c(bb,SR)
    sa <- c(sa, SR %>% region_area())
    sp <- c(sp, SR %>% region_pop(pop))
  }
  df <- data.frame(points@coords, points@data, SR_Area=sa, SR_Pop=sp)
  BB <- brick(bb)
  names(BB) <- points@data$Station
  return(list(SR_regions=BB, SR_data=df))
}


pop <- readRDS("/lustre/arpa/bonafeg/scratch/spatial-representativeness/data/pop.rds")
  for (pollutant in c("PM10","NO2")) {
    # load data
    load(glue("data/zones_{pollutant}.rda"))
    pp <- readRDS(glue("data/stations_FVG_{pollutant}.rds"))
    for (mm in c("FARM","KED")) {
      for( year in 2015:2020) {
      library(glue)
        flog.info(glue("Processing {mm} {pollutant} {year}"))
      
      modfile <- glue("data/{mm}_{pollutant}_{year}.rds")
      if(file.exists(modfile)){
        r <- readRDS(modfile)
        
        # processing
        sr <-SR_points(r, pop, zfvg, pp[pp$SourceType=="BKG",])
        saveRDS(sr, file = glue("out/SR_{mm}_{pollutant}_{year}.rds"))
      }
    }
  }
}

