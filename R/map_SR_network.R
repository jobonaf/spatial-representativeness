library(futile.logger)
library(glue)

# maps SR stations
for (pollutant in c("PM10","NO2")) {
  
  Dat <- NULL
  # map SR network
  for( year in 2015:2020) {
    sr <- readRDS(glue("out/SR_{pollutant}_{year}.rds"))
    library(tidyr)
    rasterToPoints(sr$SR_regions) %>%
      as_tibble() %>%
      gather(Station,SR_region,-x:-y) %>%
      left_join(sr$SR_data %>% rename(x_station=x, y_station=y)) %>%
      mutate(Year=year) %>%
      bind_rows(Dat) -> Dat
  }
  load(glue("data/zones_{pollutant}.rda"))
  pop <- readRDS("data/pop.rds")
  rz <- rasterize(x = zfvg, y = sr$SR_regions[[1]])
  rasterToPoints(rz) %>% as_tibble() %>% 
    left_join(tibble(ZoneId=zfvg@data$ZoneId, layer=1:nrow(zfvg@data))) %>%
    right_join(Dat) %>%
    left_join(rasterToPoints(pop) %>% as_tibble()) %>%
    group_by(x,y,Year,ZoneId) %>%
    summarise(Area=prod(res(rz)/1000), Pop=mean(population, na.rm=T), n_stations=sum(SR_region, na.rm = T)) -> pdat
  ggplot() +
    geom_tile(data=pdat %>% group_by(x,y,Year) %>%
                summarise(n_stations=sum(n_stations, na.rm = T)), 
              aes(x=x,y=y,fill =n_stations)) +
    facet_wrap("Year") +
    scale_fill_gradientn(colors=c("gray90","olivedrab","orange","black"),
                         values = scales::rescale(c(0, 1, 2, 10)),
                         breaks=c(0, 1, 2, 10)) + 
    ggtitle(pollutant)+
    theme_void() -> p
  ggsave(p, filename = glue("SR_mapNetwork_{pollutant}.pdf"), height=5, width = 7)
  ggplot() +
    geom_bar(data=pdat %>% group_by(Year,ZoneId,n_stations) %>%
                summarise(Area=sum(Area,na.rm=T), Pop=sum(Pop,na.rm = T)) %>%
                gather(Index,Value,Area:Pop), 
              aes(x=Year,y=Value,fill =n_stations),
             stat="identity") +
    facet_grid(Index~ZoneId,scale="free") +
    scale_fill_gradientn(colors=c("gray90","olivedrab","orange","black"),
                         values = scales::rescale(c(0, 1, 2, 10)),
                         breaks=c(0, 1, 2, 10)) + 
    ggtitle(pollutant)+
    theme_bw() -> p
  ggsave(p, filename = glue("SR_barplotNetwork_{pollutant}.pdf"), height=5, width = 7)
}
