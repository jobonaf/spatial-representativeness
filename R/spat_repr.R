library(raster)
library(dartle)

# calculate region of a raster with values similar to those in xy
similar <- function(r,x,y,
                    rel_tolerance=c(-0.2,+0.2),
                    abs_tolerance=c(-2,+2)) {
  v <- raster::extract(r,cbind(x,y))      # value in the selected site
  out <- (r >= (v+v*rel_tolerance[1]) &   # cells included in the required relative or...
            r <= (v+v*rel_tolerance[2])) | 
    (r >= (v+abs_tolerance[1]) &          # ...absolute range
       r <= (v+abs_tolerance[2]))
  return(out)
}

# calculate region of a raster with values included in
# the FAIRMODE-style uncertainty range with respect to xy
in_uncertainty <- function(r,x,y,pollutant) {
  v <- raster::extract(r,cbind(x,y)) # value in the selected site
  u <- U_obs_95_year(v,pollutant)    # uncertainty
  out <- r >= (v-u) & r <= (v+u)     # cells included in the required uncertainty range
  return(out)
}
  
# keep only contiguous region where xy is
contiguous <- function(r,x,y, directions=4) {
  nas <- which(is.na(values(r)))         # to keep NAs
  cl <- clump(r, directions=directions)# each discontiguous area has a distinct ID
  vc <- raster::extract(cl,cbind(x,y)) # area ID in the selected site
  out <- (cl == vc)                    # keep only the contiguous area
  values(out) <- tidyr::replace_na(values(out),FALSE) 
  values(out)[nas] <- NA                 # keep NAs
  return(out)
}


# keep only the same zone where xy is
same_zone <- function(r,x,y,zones) {
  library(rgeos)
  rz <- rasterize(x = zones, y = r)
  out <- r
  out[is.na(rz) | !(rz==raster::extract(rz,cbind(x,y)))] <- NA
  out
}

# area of the region
region_area <- function(.region) {
  sum(values(.region),na.rm=T)*prod(res(.region))
}

# population in the region
region_pop <- function(.region,pop,...) {
  sum(values(pop*.region),na.rm=T)
}

