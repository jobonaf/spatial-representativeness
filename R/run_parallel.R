
# for each cell: area and population
SR_raster <- function(r, pop, zones, ...) {
  library(doParallel)  #Foreach Parallel Adaptor 
  library(foreach)     #Provides foreach looping construct
  library(raster)
  #Define how many cores you want to use
  UseCores <- detectCores() -1
  if(is.na(UseCores)) UseCores<- 1
  #Register CoreCluster
  cl       <- makeCluster(UseCores)
  registerDoParallel(cl)
  
  XY <- coordinates(r)[which(!is.na(values(r))),]
  dd <- foreach(i=1:nrow(XY)) %dopar% {
    library(magrittr)
    source("/lustre/arpa/bonafeg/scratch/spatial-representativeness/R/spat_repr.R")
    load("/lustre/arpa/bonafeg/scratch/spatial-representativeness/data/geodata.rda")
    x <- XY[i,1]
    y <- XY[i,2]
    same_zone(r,x,y,zones) %>% similar(.,x,y) %>% contiguous(.,x,y) -> SR
    c(SR %>% region_area(),
      SR %>% region_pop(pop))
  }
  stopCluster(cl)
  out <- data.frame(XY,t(simplify2array(dd)))
  colnames(out) <- c("x","y","SR_Area","SR_Pop")
  return(out)
}
