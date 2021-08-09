source("R/calc_SR_points.R")
pop <- readRDS("/lustre/arpa/bonafeg/scratch/spatial-representativeness/data/pop.rds")
for (pollutant in c("PM10","NO2","O3")) {
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

