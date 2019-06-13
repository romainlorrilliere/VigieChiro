
# =============================================================================================

testProtocole <- function(tabChiro=NA,tabDirect=NA,repertoire=getwd(),
                          dateFichier="2011_06_23",espece = "",
                          repertoireSortie="testProtocole"){ #tab chiro et chiroDirect
  dir.create(repertoireSortie,showWarnings=FALSE)
  if(is.na(tabChiro)){
    nomFichierChiro <- paste(repertoire,"/SaisieChiro_Result/",
                             dateFichier,"-chiro.txt",sep="")
    tabChiro <- read.table(nomFichierChiro,header=TRUE)
  }
  if(is.na(tabDirect)){
    nomFichierDirect <- paste(repertoire,"/SaisieChiro_Result/",
                              dateFichier,"-chiroDirect.txt",sep="")
    tabDirect <-  read.table(nomFichierDirect,header=TRUE)
  }
  if (espece != ""){
    tabChiro <- subset(tabChiro,ESPECE == espece)
    tabDirect <-  subset(tabDirect,ESPECE == espece)
  }

  vecDecalage <- seq(0,3,0.1)
  data.compare <- data.frame(ID_TRONCON = NA, NB_DIRECT = NA, NB_CHIRO = NA,
                             NB_ANGLAIS_mean = NA, NB_ANGLAIS_med = NA,
                             NB_ANGLAIS_min = NA, NB_ANGLAIS_max = NA)
  for(t in  as.character(unique(tabDirect$ID_TRONCON_SESSION))){
    ###cat(t,"\n")
    tabChiroT <- subset(tabChiro,as.character(ID_TRONCON_SESSION) == t)
    tabDirectT <- subset(tabDirect,as.character(ID_TRONCON_SESSION) == t)
    vecObsAnglais <- vector()
    for(d in vecDecalage){
     obsAnglais <- sum(
                       ifelse(
                              (((tabDirectT$TEMPS_FIN - d) %% 3.3) <
                               (0.3 +tabDirectT$TEMPS_DUREE)),
                           #   tabDirectT$TEMPS_DUREE %/% 3.3 + 1,
                              1,
                              0))

      vecObsAnglais <- c(vecObsAnglais,obsAnglais)
    }
     data.compareT <- data.frame(ID_TRONCON = t, NB_DIRECT = nrow(tabDirectT),
                                 NB_CHIRO = sum(tabChiroT$EFFECTIF),
                                 NB_ANGLAIS_mean = mean(vecObsAnglais),
                                 NB_ANGLAIS_med = median(vecObsAnglais),
                                 NB_ANGLAIS_min = min(vecObsAnglais),
                                 NB_ANGLAIS_max = max(vecObsAnglais))
    data.compare <- rbind(data.compare,data.compareT)
  }
  data.compare <- data.compare[-1,]
  if(espece != "")
    sp <- paste("_",espece,sep="")
  else
    sp <- espece
  fileWrite <- paste(repertoire,"/",repertoireSortie,
                     "/table_compare",sp,".csv",sep="")
  write.csv(data.compare,fileWrite,row.names=FALSE)
  cat("  <--",fileWrite,"\n")
  boxplot(data.compare[,2:7],main=espece)
  filePlot <- paste(repertoire,"/",repertoireSortie,
                    "/boxplot_compare",sp,".png",sep="")
  savePlot(filePlot,type="png")
  return(data.compare)
}
