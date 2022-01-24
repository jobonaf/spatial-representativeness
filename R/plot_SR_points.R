library(futile.logger)
library(glue)


# scatter plot represented area vs pop
for (pollutant in c("PM10","NO2","O3")) {
  pp <- readRDS(glue("data/stations_FVG_{pollutant}.rds"))
  for (mm in c("FARM","KED")) {
    pdf(glue("SR_AreaVsPop_{mm}_{pollutant}.pdf"))
    for( year in 2017:2020) {
      modfile <- glue("out/SR_{mm}_{pollutant}_{year}.rds")
      if(file.exists(modfile)) {
        sr <- readRDS(modfile)
        library(dplyr) 
        as_tibble(pp@coords) %>% 
          bind_cols(pp@data) %>% left_join(sr$SR_data) %>%
          filter(!is.na(SR_Area)) -> pdat
        library(ggplot2)
        library(ggrepel)
        ggplot(pdat, aes(x=SR_Area, y=SR_Pop, label=Station , color=Station )) +
          geom_point(show.legend=F) +
          geom_text_repel(segment.size=0,show.legend=F,size=3)+
          ggtitle("spatial representativeness",
                  subtitle=glue("model: {mm}, pollutant: {pollutant}, index: annual average, year: {year}"))+
          theme_bw() -> p
        print(p)
      }
    }
    dev.off()
  }
}

# boxplot represented area vs pop
for (pollutant in c("PM10","NO2","O3")) {
  pp <- readRDS(glue("data/stations_FVG_{pollutant}.rds"))
  for (mm in c("FARM","KED")) {
    Dat <- NULL
    for( year in 2017:2020) {
      modfile <- glue("out/SR_{mm}_{pollutant}_{year}.rds")
      if(file.exists(modfile)) {
        sr <- readRDS(modfile)
        library(dplyr) 
        as_tibble(pp@coords) %>% 
          bind_cols(pp@data) %>% left_join(sr$SR_data) %>%
          filter(!is.na(SR_Area)) %>%
          mutate(Year=year) %>%
          bind_rows(Dat) -> Dat
      }
    }
    library(tidyr)
    library(forcats)
    Dat %>% mutate(SR_Area=SR_Area/10^6) %>% 
      gather(Index,Value,SR_Area:SR_Pop) -> pdat
    library(ggplot2)
    ggplot(pdat%>%
             mutate(Station=fct_reorder2(Station,Value,Index,function(x,y)median(x[y=="SR_Pop"])))%>%
             mutate(Index=recode(Index,SR_Area="SR area (km2)",SR_Pop="SR population")), 
           aes(x=Station, y=Value, group=Station )) +
      geom_boxplot()+
      ggtitle("spatial representativeness",
              subtitle=glue("model: {mm}, pollutant: {pollutant}, index: annual average, years: {paste(range(Dat$Year),collapse='-')}"))+
      facet_wrap("Index", scale="free", nrow = 1, strip.position = "bottom") +
      ylab("")+
      coord_flip() +
      theme_bw() +
      theme(strip.placement="outside",
            strip.background = element_blank()) -> p
    ggsave(p, filename = glue("SR_boxplot_{mm}_{pollutant}.pdf"))
  }
}

