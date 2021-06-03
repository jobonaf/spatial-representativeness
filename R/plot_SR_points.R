library(futile.logger)
library(glue)


# scatter plot represented area vs pop
for (pollutant in c("PM10","NO2")) {
  pdf(glue("SR_AreaVsPop_{pollutant}.pdf"))
  for( year in 2015:2020) {
    sr <- readRDS(glue("out/SR_{pollutant}_{year}.rds"))
    library(dplyr) 
    as.data.frame(pp@coords) %>% 
      cbind(pp@data) %>% left_join(sr$SR_data) %>%
      filter(!is.na(SR_Area)) -> pdat
    library(ggplot2)
    library(ggrepel)
    ggplot(pdat, aes(x=SR_Area, y=SR_Pop, label=Station , color=Station )) +
      geom_point(show.legend=F) +
      geom_text_repel(segment.size=0,show.legend=F,size=3)+
      ggtitle(paste(pollutant,year))+
      theme_bw() -> p
    print(p)
  }
  dev.off()
}
