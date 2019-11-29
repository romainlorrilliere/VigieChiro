############################################################
##     les fonctions pour le rapportage national annuel   ##
############################################################


require(data.table)
require(ggmap)
require(ggplot2)



run.rmd <- function(file.rmd="bilan_main.rmd",file.out=NULL){
    if(is.null(file.out)) file.out <- paste0("bilan_vigie_chiro_",substr(Sys.time(),1,4),".html")
    rmarkdown::render(file.rmd,output_file=file.out)
}




map_fr_vigie_chiro <- function(dsample,last_year=NULL) {
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)




    if(is.null(last_year)) last_year <- as.numeric(substr(Sys.time(),1,4))

    dsample$id_site <- dsample[,"num_site_txt"]

    dsample_y <- unique(subset(dsample,year == last_year,select=c("id_site","longitude","latitude","sample_cat")))
    dsample_before <- unique(subset(dsample,year < last_year,select=c("id_site","longitude","latitude","sample_cat")))
    dsample_y$last_year <- as.character(last_year)
    dsample_before$last_year <- paste("avant",last_year)

    dsample_before <- subset(dsample_before, !(id_site %in% dsample_y$id_site))

    dloc <- rbind(dsample_before,dsample_y)

    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))


    vecAlpha <- c("2018"=1,"avant 2018"=.3)
    vecCol <- c("2018"="#b2182b","avant 2018"="#67a9cf")

    gg <- ggplot()+ facet_wrap(.~sample_cat,nrow=1)
        gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)+ geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_point(data=dloc,aes(x=longitude, y=latitude,colour=last_year,alpha=last_year))
    gg <- gg + coord_sf(xlim=c(-5,9),ylim=c(41.5,52))
    gg <- gg + scale_color_manual(values=vecCol)+scale_alpha_manual(values=vecAlpha)
    gg <- gg + labs(colour="",alpha="",x="",y="")
    print(gg)



}




leaflet_fr_vigie_chiro <- function(dsample,last_year=NULL) {
    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)




    if(is.null(last_year)) last_year <- as.numeric(substr(Sys.time(),1,4))

    dsample$id_site <- paste0(dsample$longitude,"_",dsample$latitude)

    dsample_y <- unique(subset(dsample,year == last_year,select=c("id_site","longitude","latitude")))
    dsample_before <- unique(subset(dsample,year < last_year,select=c("id_site","longitude","latitude")))
    dsample_y$last_year <- as.character(last_year)
    dsample_before$last_year <- paste("avant",last_year)

    dsample_before <- subset(dsample_before, !(id_site %in% dsample_y$id_site))

    dloc <- rbind(dsample_before,dsample_y)


    library(leaflet)
    leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

    factpal <- colorFactor(c("navy", "red"), domain = c("2018", "avant 2018"))


  leaflet(width=900, height=650) %>%
  # base map
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircles(data=dloc,
             lng=~longitude, lat=~latitude,
             radius=~ifelse(last_year==2018,6,10),
             color=~factpal(last_year), weight=1, opacity=1)


}


temp_var_site_vigie_chiro <- function(dsample) {
    dagg <- aggregate(num_site_txt ~ year + sample_cat, data=dsample,length)
    colnames(dagg)[ncol(dagg)] <- "nbsample"

    gg <- ggplot(data=dagg,aes(x=year,y=nbsample,colour=sample_cat)) + geom_point() + geom_line()
gg

}
