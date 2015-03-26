library(dplyr)
library(maps)
library(ggplot2)
library(animation)

load('bikeshareCities')
load('dates')
mapWorld <- borders("world", colour="gray50", fill="gray50")
latw <- c(-70, 90) 
longw <- c(-160, 160) 
mp <- NULL
mp <- ggplot() + mapWorld 
latlimits <- c(30, 65) 
longlimits <- c(-20, 50) 
ep <- NULL
ep <- ggplot() + mapWorld

oopt = ani.options(interval = 0.3)
bike_animation = function(){
  for (i in 1:max(cities$idx)){
    init <- cities %>% filter(idx <= i)
    year <- dates$year[which(dates$idx == i)]
    if(year <= 2007){
      print(ep + coord_cartesian(xlim = longlimits, ylim = latlimits) + geom_point(data = init, aes(x = lon, y = lat, size = bike, colour = status))+ geom_text(aes(x = 42, y = 32), label = year, colour = 'yellow', size = 10)+
        scale_colour_manual(values = c("yellow","red", "green"))+
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect('#00FFFF'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ))
    }else{
      print(mp + coord_cartesian(xlim = longw, ylim = latw) + geom_point(data = init, aes(x = lon, y = lat, size = as.numeric(bike), colour = status))+ 
              geom_text(aes(x = 130, y = -55), label = year, size = 10)+
              scale_colour_manual(values = c("yellow","red", "green"))+scale_size(range = c(0, 3))+
              theme(
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                panel.background = element_rect('#00FFFF'),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "none"
              )
        )
    }
    animation::ani.pause()
  }
}
saveHTML(bike_animation(), autoplay = FALSE, loop = FALSE, verbose = TRUE, outdir = "images", htmlfile ='share.html', ani.height = 500, ani.width = 800,  single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")



