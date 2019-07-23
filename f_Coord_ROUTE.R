Test=T

##########INPUTS################
#layers : download on : http://www.professionnels.ign.fr/route500#tab-3
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)
# bs,bm,bl = buffers in meters

" extraction of data from road sections and railways."

Coord_CLC=function(points,names_coord,bs,bm,bl,layer1,layer2)
{
  
  
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  library(rgeos)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv")) 
  CoordH=names_coord
  #library(Rnightlights)
  
  BufferSmall=bs
  BufferMedium=bm
  BufferLarge=bl 
  
  #récupération des couches 
  Sys.time()
  ROUTE=shapefile(layer1) # 10 mn
  Sys.time()
  Fer=shapefile(layer2) #All "En service" 
  Sys.time()
  plot(Fer)
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
 
  OccSL$id=c(1:nrow(OccSL))
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  #CRS.new <- CRS(proj4string(CarthageP))
  OccSL_L93=spTransform(OccSL,CRS(proj4string(ROUTE)))
  
  
  ##########################################
  ##########################################
  #############  Routes    #################
  ##########################################
  ##########################################
  
  ########
  #Buffer S
  ########
  
  BufferS=gBuffer(OccSL_L93,width=BufferSmall,byid=T)
  SpRoutelistS=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpRoutelistS[[k]]=intersect(ROUTE,BufferS[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  
  
  SpRoute=do.call(rbind,SpRoutelistS) # 0.05 sec / pol
  
  Sys.time()
  LengthB=gLength(SpRoute,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpRoute$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpRo_S"
  OccSL_L93Re=merge(OccSL_L93,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_S[is.na(OccSL_L93Re$SpRo_S)]=0
  spplot(OccSL_L93Re,zcol="SpRo_S",col="transparent")
  
  ########
  #Buffer M
  ########
  BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)
  SpRoutelistM=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpRoutelistM[[k]]=intersect(ROUTE,BufferM[((k-1)*1000+1)
                                              :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  
  SpRoute=do.call(rbind,SpRoutelistM) # 0.05 sec / pol
  Sys.time()

  LengthB=gLength(SpRoute,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpRoute$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpRo_M"
  OccSL_L93Re=merge(OccSL_L93Re,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_M[is.na(OccSL_L93Re$SpRo_M)]=0
  spplot(OccSL_L93Re,zcol="SpRo_M",col="transparent")
  
  ########
  #Buffer L
  ########
  BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)
  SpRoutelistL=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpRoutelistL[[k]]=intersect(ROUTE,BufferL[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  SpRoute=do.call(rbind,SpRoutelistL) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferL) # 0.05 sec / buffer
  Sys.time()
  LengthB=gLength(SpRoute,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpRoute$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpRo_L"
  OccSL_L93Re=merge(OccSL_L93Re,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_L[is.na(OccSL_L93Re$SpRo_L)]=0
  spplot(OccSL_L93Re,zcol="SpRo_L",col="transparent")
  
  
  ##########################################
  ##########################################
  ##########Voies ferrées  #################
  ##########################################
  ##########################################
  
  
  ########
  #Buffer S
  ########
  
  BufferS=gBuffer(OccSL_L93,width=BufferSmall,byid=T)
  SpFerlistS=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpFerlistS[[k]]=intersect(Fer,BufferS[((k-1)*1000+1)
                                              :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  
  SpFer=do.call(rbind,SpFerlistS) # 0.05 sec / pol
  
  Sys.time()
  LengthB=gLength(SpFer,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpFer$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpFe_S"
  OccSL_L93Re=merge(OccSL_L93Re,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_S[is.na(OccSL_L93Re$SpRo_S)]=0
  spplot(OccSL_L93Re,zcol="SpFe_S",col="transparent")
  
  ########
  #Buffer M
  ########
  BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)
  SpFerlistM=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpFerlistM[[k]]=intersect(Fer,BufferM[((k-1)*1000+1)
                                              :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  
  SpFer=do.call(rbind,SpFerlistM) # 0.05 sec / pol
  Sys.time()
  
  LengthB=gLength(SpFer,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpFer$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpFe_M"
  OccSL_L93Re=merge(OccSL_L93Re,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_M[is.na(OccSL_L93Re$SpRo_M)]=0
  spplot(OccSL_L93Re,zcol="SpFe_M",col="transparent")
  
  ########
  #Buffer L
  ########
  BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)
  SpFerlistL=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpFerlistL[[k]]=intersect(Fer,BufferL[((k-1)*1000+1)
                                              :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  SpFer=do.call(rbind,SpFerlistL) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferL) # 0.05 sec / buffer
  Sys.time()
  LengthB=gLength(SpFer,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpFer$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpFe_L"
  OccSL_L93Re=merge(OccSL_L93Re,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93Re$SpRo_L[is.na(OccSL_L93Re$SpRo_L)]=0
  spplot(OccSL_L93Re,zcol="SpFe_L",col="transparent")
  
  ##########################################
  ##########################################
  #############  Reseaux   #################
  ##########################################
  ##########################################
  
  #
  
  OccSL_ARajouter=subset(OccSL_L93Re,select=grepl("Sp",names(OccSL_L93Re)))
  
  Reseau=data.frame(cbind(coordinates(OccSL),as.data.frame(OccSL_ARajouter)))
  
  NewName=paste0(FOccSL,"_Reseau.csv")
  
  fwrite(Reseau,NewName)
  
  coordinates(Reseau) <- CoordH
  
  SelCol=sample(names(OccSL_ARajouter),1)
  spplot(Reseau,zcol=SelCol,main=SelCol)
  class(Reseau)
  
}

if(Test)
{
  #for testing
  Coord_CLC(
    #points="./vigiechiro/GIS/PA_Thymus nitens" #table giving coordinates in WGS84
    #points ="C:/Users/Adeline/Desktop/chiro/Fichiers_obtenus/CoordWGS84_SpNuit2_Seuil90_DataLP_PF_exportTot" #table giving coordinates in WGS84 
    points ="C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/SysGrid__75_100"
    ,names_coord=c("Group.1","Group.2") #vector of two values giving 
    ,bs=50
    ,bm=500
    ,bl=5000
    ,layer1="C:/Users/Cimcä/Desktop/Chiro/Fichiers SIG/ROUTE500_2-1__SHP_LAMB93_FXX_2018-04-09/TRONCON_ROUTE.shp"
    ,layer2="C:/Users/Cimcä/Desktop/Chiro/Fichiers SIG/ROUTE500_2-1__SHP_LAMB93_FXX_2018-04-09/TRONCON_VOIE_FERREE.shp"
     )
}