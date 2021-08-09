mm <- "FARM"
library(tidyr)
library(dplyr)
library(glue)
srs <- readRDS(glue("/lustre/arpa/bonafeg/scratch/spatial-representativeness/out/SR_sensitivity_{mm}.rds"))
srs %>%
  mutate(SR_Area=SR_Area/1000000) %>%
  pivot_longer(SR_Area:SR_Pop,names_to="index") -> pdat

pdat %>% distinct(x,y,Station) -> pp
coordinates(pp) <- ~x+y
crs(pp) <- "+init=epsg:32633"
library(sp)
pz <- NULL
for(poll in c("PM10","NO2")) {
  load(glue("/lustre/arpa/bonafeg/scratch/spatial-representativeness/data/zones_{poll}.rda"))
  pz <- bind_rows(pz, bind_cols(pp@data, over(pp,zfvg) %>% dplyr::select(ZoneId) %>% droplevels(),  pollutant=poll))
}
pdat <- left_join(pdat, pz)

library(ggplot2)
pdf("SR_sensitivity.pdf",width=12,height=9)
for (poll in unique(pdat$pollutant)) {
  for (idx in unique(pdat$index)) {
    Idx <- recode(idx,
                  SR_Area="area", 
                  SR_Pop="population")
    IdxUnit <- recode(idx,
                   SR_Area=as.expression("area"~("km"^2)), 
                   SR_Pop=as.expression("population"~("-")))
    ggplot(pdat %>% filter(pollutant==poll,index==idx), 
           aes(x=rel_tol, y=value, fill=contiguous, col=contiguous,
               group=paste(Station, contiguous, rel_tol))) +
      geom_vline(xintercept = 0.2, linetype="dashed")+
      geom_boxplot(outlier.alpha = 0.7, outlier.size = 0.8, alpha=0.2)+
      ggtitle(label = "spatial representativeness sensitivity",
              subtitle=glue("model: {mm}, pollutant: {poll}, AQ index: annual average, ",
                            "SR index: {Idx}, years: {paste(range(unique(pdat$year)),collapse='-')}")) +
      ylab(IdxUnit) +
      xlab("relative tolerance")+
      scale_fill_discrete(name="contiguity\ncriterion")+
      scale_color_discrete(name="contiguity\ncriterion")+
      scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
      scale_y_continuous(labels = scales::number_format(accuracy = 1))+
      facet_wrap(~Station, scales="free_y")+
      theme_bw()+
      theme(panel.grid.minor = element_blank(),
            strip.placement="outside",
            strip.background = element_blank())-> p
    print(p)
  }
}
dev.off()

