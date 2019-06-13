



testProtocole.autoSp <- function(repertoire="/home/romain/2-Divers Boss/moulinetteChiro",
                          repertoireSortie="testProtocole",
                          dateFichier="2011_02_22",
                          espece=""){ #tab chiro et chiroDirect

# tabChiro=NA;tabDirect=NA;repertoire=getwd();repertoireSortie="testProtocole";dateFichier="2011_01_13";espece=""
##  dir.create(repertoireSortie,showWarnings=FALSE)
 ## if(is.na(tabChiro)){
  
    nomFichierChiro <- paste(repertoire,"/SaisieChiro_Result/",
                             dateFichier,"-chiro.txt",sep="")
    tabChiro <- read.table(nomFichierChiro,header=TRUE)
#  }
##  if(is.na(tabDirect)){
    nomFichierDirect <- paste(repertoire,"/SaisieChiro_Result/",
                              dateFichier,"-chiroDirect.txt",sep="")
    tabDirect <-  read.table(nomFichierDirect,header=TRUE)
##  }
browser()
    tabChiro <- subset(tabChiro,TYPE_SUIVI == "routier")
    tabDirect <- subset(tabDirect,TYPE_SUIVI == "routier")
    
 #   vecEspece <- unique(c(levels(tabDirect$ESPECE),levels(tabChiro$ESPECE)))

    vecEspece <- unique(tabChiro$ESPECE)
    
    for(sp in vecEspece)
      testProtocole(tabChiro,tabDirect,espece=sp)

  testProtocole(tabChiro,tabDirect,espece="")
    
    

}




## =================================================================================


testProtocole <- function(tabChiro,tabDirect,
                          repertoire="/home/romain/2-Divers Boss/moulinetteChiro",
                          repertoireSortie="testProtocole",
                          dateFichier="2011_01_13",
                          espece=""){ #tab chiro et chiroDirect

### tabChiro=NA;tabDirect=NA;repertoire=getwd();repertoireSortie="testProtocole";dateFichier="2011_01_13";espece=""
##  dir.create(repertoireSortie,showWarnings=FALSE)
##  if(is.na(tabChiro)){
## nomFichierChiro <- paste(repertoire,"/SaisieChiro_Result/",
##                            dateFichier,"-chiro.txt",sep="")
##   tabChiro <- read.table(nomFichierChiro,header=TRUE)
###  }
###  if(is.na(tabDirect)){
##   nomFichierDirect <- paste(repertoire,"/SaisieChiro_Result/",
##                             dateFichier,"-chiroDirect.txt",sep="")
##   tabDirect <-  read.table(nomFichierDirect,header=TRUE)
##  }
  if (espece != ""){
    tabChiro <- subset(tabChiro,ESPECE == espece)
    tabDirect <-  subset(tabDirect,ESPECE == espece)
  }

  vecDecalage <- seq(0,3,0.1)
  data.compare <- data.frame(ID_TRONCON = NA, NB_DIRECT = NA, NB_CHIRORed = NA,
                             NB_CHIRONonRed = NA,NB_ANGLAIS_zeroRed = NA,
                             NB_ANGLAIS_meanRed = NA, NB_ANGLAIS_medRed = NA,
                             NB_ANGLAIS_minRed = NA, NB_ANGLAIS_maxRed = NA,
                             NB_ANGLAIS_zeroNonRed = NA,
                             NB_ANGLAIS_meanNonRed = NA, NB_ANGLAIS_medNonRed = NA,
                             NB_ANGLAIS_minNonRed = NA, NB_ANGLAIS_maxNonRed = NA)
  for(t in  as.character(unique(tabDirect$ID_TRONCON_SESSION))){
###cat(t,"\n")
    if (t == "Cir138-2010-Pass2-Tron3") browser()
    tabChiroT <- subset(tabChiro,as.character(ID_TRONCON_SESSION) == t)
    tabDirectT <- subset(tabDirect,as.character(ID_TRONCON_SESSION) == t)
   
    vecChiroNonRed <- vector()
    for(i in 1:nrow(tabDirectT)){
      tempsDebut <- tabDirectT$TEMPS_DEBUT[i]
      tempsFin <- tabDirectT$TEMPS_FIN[i]
      vecChiroNonRed <- c(vecChiroNonRed,(as.numeric(any(tabChiroT$TEMPS > tempsDebut & tabChiroT$TEMPS < tempsFin))))
     }
    
  vecObsAnglaisNonRed <- vector()
  vecObsAnglaisRed <- vector()
  
  for(d in vecDecalage){
    obsAnglaisNonRed <- sum(
                              ifelse(
                                     (((tabDirectT$TEMPS_FIN - d) %% 3.3) <
                                      (0.3 +tabDirectT$TEMPS_DUREE)),
                                     1,
                                     0))

      vecObsAnglaisNonRed <- c(vecObsAnglaisNonRed,obsAnglaisNonRed)
      obsAnglaisRed <- sum(
                           ifelse(
                                  (((tabDirectT$TEMPS_FIN - d) %% 3.3) <
                                   (0.3 +tabDirectT$TEMPS_DUREE)),
                                  tabDirectT$TEMPS_DUREE %/% 3.3 + 1,
                                  0))

      vecObsAnglaisRed <- c(vecObsAnglaisRed,obsAnglaisRed)
    }
    data.compareT <- data.frame(ID_TRONCON = t, NB_DIRECT = nrow(tabDirectT),
                                NB_CHIRORed = sum(tabChiroT$EFFECTIF),
                                NB_CHIRONonRed = sum(vecChiroNonRed),
                                NB_ANGLAIS_zeroRed = vecObsAnglaisRed[1],
                                NB_ANGLAIS_meanRed = mean(vecObsAnglaisRed),
                                NB_ANGLAIS_medRed = median(vecObsAnglaisRed),
                                NB_ANGLAIS_minRed = min(vecObsAnglaisRed),
                                NB_ANGLAIS_maxRed = max(vecObsAnglaisRed),
                                NB_ANGLAIS_zeroNonRed = vecObsAnglaisNonRed[1],
                                NB_ANGLAIS_meanNonRed = mean(vecObsAnglaisNonRed),
                                NB_ANGLAIS_medNonRed = median(vecObsAnglaisNonRed),
                                NB_ANGLAIS_minNonRed = min(vecObsAnglaisNonRed),
                                NB_ANGLAIS_maxNonRed = max(vecObsAnglaisNonRed))

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
  boxplot(data.compare[,c(2,3,6,4,11)],main=espece)
  filePlot <- paste(repertoire,"/",repertoireSortie,
                    "/boxplot_compare",sp,".png",sep="")
  savePlot(filePlot,type="png")
  return(data.compare)
}



testDecalage <- function(){
vecDecalage <- seq(0,3,0.1)
  a <- c(0, 0.3, 3.3, 3.6, 6.6, 6.9, 9.9, 10.2, 13.2, 13.5)
  mat_a <- a
  for(d in vecDecalage)  mat_a<-  rbind(mat_a,a+d)
  matplot(t(mat_a),type='l')

  b <- rep(c(1,0),5)

  d <- c(0.4,0.6,2,2.1,2.2,5,6.7)
  e <- rep(0.5,length(d))
  plot(a,b,type='s')
  points(d,e)
  plot(a+.2,b,type='s',ADD=TRUE)



  (d %% 3.3) < 0.3
  ((d+0.1) %% 3.3) < 0.3
  ((d+0.2) %% 3.3) < 0.3
  ((d+0.3) %% 3.3) < 0.3
  ((d-0.1) %% 3.3) < 0.3
  ((d-0.2) %% 3.3) < 0.3
  ((d-0.3) %% 3.3) < 0.3

}



