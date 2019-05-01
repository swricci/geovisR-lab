#final viz product
#use ggplot and interaction with plotly to visuzalize noise propagation model results

library(ggplot2)
library(sf)
library(raster)
library(plotly)

#to do: clip states and FKNMS zones to same extent as vis levels raster (st_intersection?)
#decide on color palette - color brewer
#interaction with plotly?

#data preperation
states<-st_read("/Users/swricci/Documents/gis711_database/US/states_48.shp")
states_reproj<-st_transform(states,32617)

fknms_zones<-st_read("/Users/swricci/Documents/gis714_geocomp/dBSea_results/FKNMS_marine_zones.shp")
fknms_reproj<-st_transform(fknms_zones,32617)

boatSPL<-raster("passenger_captblinky_vislevelsSPL.asc")
crs(boatSPL)<-CRS("+init=epsg:32617")
boatSPL_proj<-projectRaster(boatSPL,crs=CRS("+init=epsg:4326"))


#plot with base plot
plot(boatSPL_proj)
plot(st_geometry(fknms_zones),add=T)


#get data ready to plot
studyarea<-st_bbox(c(xmin= 24.31, ymin= -82.25, xmax= 24.73,ymax= -80.75),crs = st_crs(4326))
studyarea_proj<-extent(boatSPL)

fknms_crop<-st_crop(fknms_reproj,studyarea_proj)
fknms<-st_transform(fknms_crop,4326)

states_crop<-st_crop(states_reproj,studayarea_proj)
fl.keys<-st_transform(states_crop,4326)

boat_df<-as.data.frame(boatSPL_proj,xy=T)
colnames(boat_df)<-c("x","y","SPL")

SPL.map<-ggplot()+
  geom_raster(data=boat_df, aes(x=x,y=y,fill = SPL,text=paste('SPL (dB):', round(SPL,2))))+
  scale_fill_viridis_c(option = "magma")+
  geom_sf(data=fknms, aes(text=Name),color = "white",fill="gray",alpha=0.6)+
  geom_sf(data=fl.keys, color = "black", fill="white")+
  #coord_sf()+
  theme_dark()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(
    title = "Vessel noise propagation in FKNMS",
    y = "Latitude",
    x = "Longitude"
  )

g<-ggplotly(SPL.map, tooltip = "text")

Sys.setenv("plotly_username"="swricci")
Sys.setenv("plotly_api_key"="LJSpcq9tYuOX9ucM31lv")

chart_link = api_create(g, filename="FKNMS-vessel-vis")
chart_link
