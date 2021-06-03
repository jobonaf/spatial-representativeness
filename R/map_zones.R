#library(devtools)
#install_github("hunzikp/MapColoring")
#install.packages("CEoptim")

load("data/geodata.rda")


## Use loose definition of adjacent
library(rgeos)
adj.zon <- gIntersects(gBuffer(zones, width=1, byid=TRUE), byid=TRUE)

## Get Optimal contrast colors
library(RColorBrewer)
library(MapColoring)
cand.colors <- brewer.pal(7, "Set2")
opt.colors <- getOptimalContrast(x=adj.zon, col=cand.colors)
zones@data$color <- opt.colors
zz <- fortify(zones) %>% left_join(zones@data %>% mutate(id=rownames(zones@data)))

# plot map
ggplot(zz) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=color, text=ZoneId), 
               show.legend = F, col="grey20", size=0.2) + 
  scale_colour_identity(aes(x=long, y=lat, group=group, fill=color, text=ZoneId)) + 
  theme_void()
