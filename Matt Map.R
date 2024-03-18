#---------------------------------------------------------------------------------------
#----------------------------------Packages---------------------------------------------
#---------------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggplot2)
#remotes::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

#---------------------------------------------------------------------------------------
#----------------------------------Functions--------------------------------------------
#---------------------------------------------------------------------------------------

plain_map <- function () {
  theme(legend.position = "none", plot.background = element_rect(fill = "white", 
                                                                 colour = "white"), panel.grid.major = element_line(colour = "transparent"), 
        panel.grid.minor = element_blank(), panel.border = element_blank(), 
        panel.background = element_blank(), axis.title.y = element_blank(), 
        axis.title.x = element_blank(), axis.ticks = element_blank(), 
        axis.text = element_blank(), legend.title = element_blank(), 
        legend.text = element_text(size = 12))
}

#---------------------------------------------------------------------------------------
#----------------------------------Load Data--------------------------------------------
#---------------------------------------------------------------------------------------

#https://catalog.data.gov/dataset/tiger-line-shapefile-2019-nation-u-s-current-county-and-equivalent-national-shapefile
counties<-st_read('C:/Users/david.rae/Downloads/tl_2019_us_county/tl_2019_us_county.shp')


#---------------------------------------------------------------------------------------
#----------------------------------Inputs-----------------------------------------------
#---------------------------------------------------------------------------------------

#county of analysis
selctedcounty <- 'Multnomah'

#center point
points_df <- data.frame(Lon = -122.5912, Lat = 45.5179 )

#circle radius in miles 
mileradius <- 10

#---------------------------------------------------------------------------------------
#----------------------------------Create layers----------------------------------------
#---------------------------------------------------------------------------------------

#transform to standard crs
counties <- st_transform(counties,4326)


#create a state layer
statelayer <- counties %>%
  group_by(STATEFP)%>%
  summarize()

#layer of just the selected county
selectedcounty<-counties %>% 
  filter(NAME == selctedcounty)

#create the point layer

points <- points_df  %>% 
  st_as_sf(coords=c("Lon","Lat"))
st_crs(points) <- 4326

#select the region around the selected county
zoomin <- c(xmin = as.numeric(points_df[1] - 1), xmax = as.numeric(points_df[1]+1), ymin =as.numeric(points_df[2]-.55) , ymax = as.numeric(points_df[2]+.65))

#crop the entire county shapefile to include bordering states  
regionbox <- counties %>%
    st_crop(zoomin) 

#crop the state boundaries as well
statelayer_cropped <- statelayer %>%
  st_crop(zoomin) 

zoomin <- st_as_sfc(st_bbox(zoomin))
st_crs(zoomin) <- 4326

statelayer_cropped <- st_intersection(statelayer, st_as_sfc(st_bbox(zoomin)))

#create a circle that is one mile in radius
circle_sf <- st_buffer(points, dist = mileradius * 1609.344)
st_crs(circle_sf) <- 4326


#---------------------------------------------------------------------------------------
#----------------------------------Create Map-------------------------------------------
#---------------------------------------------------------------------------------------

  ggplot() +
  #selected county
  geom_sf(data=selectedcounty,aes(),alpha=1, fill = 'grey',inherit.aes=FALSE)+
  #10 mile circle
  geom_sf(data=circle_sf, fill = 'light blue', color = 'red',inherit.aes=FALSE)+  
  #counties 
  geom_sf(data=regionbox,aes(),alpha=0, inherit.aes=FALSE)+
  #state boundaries - thickner than the counties
  geom_sf(data=statelayer_cropped,aes(),fill = 'transparent',color ='black',lwd=1, inherit.aes=FALSE)+
  #labels
  geom_sf_label_repel(data=regionbox,aes(label = NAME), seed = 10,
                      fill = alpha(c("white"),0)) + 
  #simple theme
  plain_map()



