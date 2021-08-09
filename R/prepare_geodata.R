pollutant <- "PM10"
countries <- "Italy"
library(futile.logger)

# air quality index: kriging
library(raster)
library(glue)
for (pollutant in c("PM10","NO2")) {
  for (year in 2015:2020) {
    ff <- glue("/lustre/arpa/operative/data/ariareg/databases/airq_reg/1001F0B0D0_{year}/kriging_CivilYear/",
               "{year}0101/data/output/{pollutant}_{year}_CivilYear_Avg_DLGS155_02_2011_ukriging.asc")
    if(!file.exists(ff)) ff <- glue("/lustre/arpa/operative/data/ariareg/databases/airq_reg/1001F0B0D0_{year}/kriging_CivilYear/",
                                    "{year}0101/data/output/{pollutant}_{year}_CivilYear_Avg_DLGS155_02_2011_ukriging_wet.asc")
    flog.info(ff)
    r <- raster(ff)
    crs(r) <- "+init=epsg:32633"
    saveRDS(r, file = glue("data/KED_{pollutant}_{year}.rds"))
  }
}
rk <- r

# air quality index: CTM as is
for (pollutant in c("PM10","NO2","O3")) {
  for (year in 2017:2020) {
    ff <- glue("/lustre/arpa/operative/data/ariareg/databases/airq_reg/1001F0B0D0_{year}/kriging_CivilYear/",
               "{year}0101/data/input/guida/{pollutant}_{year}_CivilYear_Avg.nc")
    if(!file.exists(ff)) ff <- glue("/lustre/arpa/operative/data/ariareg/databases/airq_reg/1001F0B0D0_{year}/kriging_CivilYear/",
                                    "{year}0101/data/input/guida/{pollutant}w_{year}_CivilYear_Avg.nc")
    flog.info(ff)
    r <- raster(ff, varname=glue("c_{pollutant}"))
    crs(r) <- "+init=epsg:32633 +units=km"
    r <- projectRaster(r, rk)
    saveRDS(r, file = glue("data/FARM_{pollutant}_{year}.rds"))
  }
}

# population
pop <- readRDS("/lustre/arpa/bonafeg/data/geo/Popolazione/Pop_FARMFVG_2km.rds")
crs(pop) <- "+init=epsg:32633 +units=km"
pop <- projectRaster(pop,r)
saveRDS(pop, file = "data/pop.rds")

# air quality zones
Zones <- rgdal::readOGR("/lustre/arpa/bonafeg/data/geo/ZoneAria/AQZonesEEA2021/Countries.shp")
# select pollutant and countries
for (pollutant in c("PM10","NO2","O3")) {
  flog.info(glue("Preparing zones for {pollutant}"))
  sel.poll <- sapply(strsplit(as.character(Zones@data$Pollutant),";"), function(x)any(x==pollutant))
  sel.country <- Zones@data$CountryOrT %in% countries
  zones <- Zones[sel.poll & sel.country,]
  # simplify geometries
  library(rgeos)
  zz <- gSimplify(zones, tol = mean(res(r))/10, topologyPreserve = T)
  zones <- SpatialPolygonsDataFrame(zz, zones@data)
  # keep only FVG
  library(sp)
  zones <- spTransform(zones, CRSobj = crs(pop))
  zfvg <- zones[zones@data$ZoneId %in% c("ZON.IT0607","ZON.IT0608","ZON.IT0609"),]
  zfvg <- gBuffer(zfvg, byid = T, width = mean(res(r))/4)
  save(zones, zfvg, file = glue("data/zones_{pollutant}.rda"))
}

# stations coordinates
library(glue)
library(sp)
source("~/src/carminio/R/read_bronx.R")
for (pollutant in c("PM10","NO2","O3")) {
  flog.info(glue("Preparing stations for {pollutant}"))
  FUN <- ifelse(pollutant%in%c("PM10","PM2.5"),get_daily,get_hourly)
  dd <- FUN(pollutant = pollutant, year = 2020)
  dd %>% 
    filter(!is.na(VALUE)) %>%
    distinct(LOC_CODE,UTM_X_33,UTM_Y_33,PDM_SOURCE_CODE,POST_TYPE_CODE) %>%
    transmute(Station=LOC_CODE,x=UTM_X_33,y=UTM_Y_33,
              SourceType=PDM_SOURCE_CODE,SiteType=POST_TYPE_CODE) ->pp
  coordinates(pp) <- ~x+y
  proj4string(pp) <- "+init=epsg:32633"
  saveRDS(pp, file=glue("data/stations_FVG_{pollutant}.rds"))
}
