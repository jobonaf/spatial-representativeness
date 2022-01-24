library(futile.logger)
library(glue)

# maps SR stations
for (pollutant in c("PM10","NO2","O3")) {
  for (mm in c("FARM","KED")) {
    
    Dat <- NULL
    # map SR network
    for( year in 2015:2020) {
      modfile <- glue("out/SR_{mm}_{pollutant}_{year}.rds")
      if(file.exists(modfile)) {
        sr <- readRDS(modfile)
        library(tidyr)
        rasterToPoints(sr$SR_regions) %>%
          as_tibble() %>%
          gather(Station,SR_region,-x:-y) %>%
          left_join(sr$SR_data %>% rename(x_station=x, y_station=y)) %>%
          mutate(Year=year) %>%
          bind_rows(Dat) -> Dat
      }
    }
    load(glue("data/zones_{pollutant}.rda"))
    pop <- readRDS("data/pop.rds")
    rz <- rasterize(x = zfvg, y = sr$SR_regions[[1]])
    rasterToPoints(rz) %>% as_tibble() %>% 
      left_join(tibble(ZoneId=zfvg@data$ZoneId, layer=1:nrow(zfvg@data))) %>%
      right_join(Dat) %>%
      left_join(rasterToPoints(pop) %>% as_tibble()) %>%
      group_by(x,y,Year,ZoneId) %>%
      summarise(Area=prod(res(rz)/1000), Pop=mean(population, na.rm=T), 
                n_stations=sum(SR_region, na.rm = T)) -> pdat
    ggplot() +
      geom_tile(data=pdat %>% group_by(x,y,Year) %>%
                  summarise(n_stations=cut(sum(n_stations, na.rm = T),
                                           breaks=c(0,1,2,5,10,30),
                                           include.lowest=T,right=F)), 
                aes(x=x,y=y,fill =n_stations)) +
      facet_wrap("Year") +
      scale_fill_manual(values=c("gray90","olivedrab","orange","sienna","gray20"),
                        guide = "bins",
                        name="no. of stations\ncovering") +
      guides(fill = guide_bins(label.vjust=1, show.limits=T))+
      ggtitle("spatial representativeness coverage",
              subtitle=glue("model: {mm}, pollutant: {pollutant}, index: annual average"))+
      theme_void()+
      theme(aspect.ratio = 1)-> p
    ggsave(p, filename = glue("SR_mapNetwork_{mm}_{pollutant}.pdf"), height=5, width = 7)
    ggplot() +
      geom_bar(data=pdat %>% group_by(Year,ZoneId,
                                      n_stations=cut(n_stations,
                                                     breaks=c(0,1,2,5,10,30),
                                                     include.lowest=T,right=F)) %>%
                 summarise(Area=sum(Area,na.rm=T), Pop=sum(Pop,na.rm = T)) %>%
                 gather(Index,Value,Area:Pop)%>%
                 mutate(Index=recode(Index,Area="area (km2)",Pop="population")), 
               aes(x=Year,y=Value,fill =n_stations),
               stat="identity") +
      facet_grid(Index~ZoneId,scale="free",switch = "y") +
      scale_fill_manual(values=c("gray90","olivedrab","orange","sienna","gray20"),
                        guide = "bins",
                        name="no. of stations\ncovering") +
      guides(fill = guide_bins(label.vjust=1, show.limits=T))+
      ggtitle("spatial representativeness coverage",
              subtitle=glue("model: {mm}, pollutant: {pollutant}, index: annual average"))+
      ylab("")+
      theme_bw()+
      theme(strip.placement="outside",
            strip.background = element_blank()) -> p
    ggsave(p, filename = glue("SR_barplotNetwork_{mm}_{pollutant}.pdf"), height=5, width = 7)
  }
}
