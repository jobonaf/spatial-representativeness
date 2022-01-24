library(futile.logger)
library(glue)
library(dplyr)
library(raster)
library(ggplot2)
library(ggrepel)

# maps SR stations
for (pollutant in c("PM10","NO2","O3")) {
  for (mm in c("FARM","KED")) {
    pdf(glue("SR_map_{mm}_{pollutant}.pdf"), height=8, width = 9)
    for( year in 2015:2020) {
      srfile <- glue("out/SR_{mm}_{pollutant}_{year}.rds")
      if(file.exists(srfile)){
        sr <- readRDS(srfile)
        library(tidyr)
        library(forcats)
        rasterToPoints(sr$SR_regions) %>%
          as_tibble() %>%
          gather(Station,SR_region,-x:-y) %>%
          left_join(sr$SR_data %>% rename(x_station=x, y_station=y)) -> pdat #%>%
        #mutate(Station=fct_reorder(Station,SR_Pop,mean)) -> pdat
        ggplot() +
          geom_tile(data=pdat, aes(x=x,y=y,fill =SR_region), show.legend = F) +
          facet_wrap("Station") +
          geom_point(data=sr$SR_data %>%mutate(Station=fct_reorder(Station,SR_Pop,mean)), aes(x=x,y=y),
                     shape=21, color="black")+
          geom_text_repel(data=sr$SR_data%>%mutate(Station=fct_reorder(Station,SR_Pop,mean)), 
                          aes(x=x,y=y,label=glue("area:{signif(SR_Area/10^6,3)}km2\npop:{signif(SR_Pop,3)}")),
                          size=3, color="grey20")+
          scale_fill_stepsn(values=c(0,1),colors=c("grey80","orange"),na.value = "grey90")+
          ggtitle("spatial representativeness",
                  subtitle=glue("model: {mm}, pollutant: {pollutant}, index: annual average, year: {year}"))+
          theme_void()+
          theme(aspect.ratio = 1) -> p
        print(p)
      }
    }
    dev.off()
  }
}
