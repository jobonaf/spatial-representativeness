source("R/calc_SR_points.R")
library(dplyr)
mm <- "FARM"
pop <- readRDS("/lustre/arpa/bonafeg/scratch/spatial-representativeness/data/pop.rds")
SR <- NULL
for (pollutant in c("PM10","NO2","O3")) {
  # load data
  load(glue("data/zones_{pollutant}.rda"))
  pp <- readRDS(glue("data/stations_FVG_{pollutant}.rds"))
  for( year in 2017:2020) {
    library(glue)
    flog.info(glue("Processing {mm} {pollutant} {year}"))
    modfile <- glue("data/{mm}_{pollutant}_{year}.rds")
    r <- readRDS(modfile)
    
    # processing
    rts <- c(0.02, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5)
    for (rel_tol in rts) {
      for (cc in c(T,F)) {
        sr <-SR_points(r, pop, zfvg, pp[pp$SourceType=="BKG",], rel_tolerance=c(-rel_tol,+rel_tol), cont = cc)
        SR <- bind_rows(SR, sr$SR_data %>% mutate(contiguous=cc, year=year, rel_tol=rel_tol, pollutant=pollutant, model=mm))
      }
    }
  }
}
saveRDS(SR, file = glue("SR_sensitivity_{mm}.rds"))
