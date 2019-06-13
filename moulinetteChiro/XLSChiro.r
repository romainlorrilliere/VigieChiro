## Programme de mise en forme des table PASSAGE TRONCON_PASSAGE CHIRO
## pour l'utilisé il faut mettre l'ensemble des fichiers au format xls dans le répertoire "saisieChiroFileAttenteCSV" .


## 		source("cleanchiro.r")

##		fonction à utiliser : 
##				chiro.auto(import = c(DIRECT = TRUE,ORTHO = FALSE, HABITAT = TRUE, MAMMIFERE = FALSE)

library(xlsReadWrite)

## ------------------------------------------
lecture.xls.sheet <- function(file.name,import){
  cat(" --> ",file.name,"\n")
  flush.console()
                                        #browser()
  vecOnglet <- c("Fichier_Audio","Météo","Identifications", "IdentifDIRECT", 
                 "IdentificationOrthoptere","Fiche_Saisie_Habitat","Saisie_Mammifère")
  vecNomOnglet <-c("FICHIER","METEO","IDENTIF","DIRECT",
                   "ORTHOPTERE","HABITAT","MAMMIFERE")
  
  db <- list(FICHIER = NA,METEO = NA,IDENTIF = NA, DIRECT = NA,
             ORTHOPTERE = NA, HABITAT = NA, MAMMIFERE =NA)
  
  db[[1]] <- read.xls(file.name,colNames=TRUE,sheet = vecOnglet[1])

  db[[2]] <- read.xls(file.name,colNames=TRUE,sheet = vecOnglet[2],
                      dateTimeAs = "isodatetime",
                      colClasses = c(NA,NA,"isodate",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  db[[3]] <- read.xls(file.name,colNames=TRUE,sheet = vecOnglet[3],
                      dateTimeAs = "isodatetime",
                      colClasses = c(NA,NA,"isodate",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  
  if(import["DIRECT"]) db[[4]] <- read.xls(file.name,colNames=TRUE,sheet = vecOnglet[4],
                                           dateTimeAs = "isodatetime",
                                           colClasses = c(NA,NA,"isodate",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  ##  if(length(which(colnames(tab) == "Durée.séquence"))>0) {
  ##    if(is.character(s4$Durée.séquence){ 
  ##      s4 <- read.xls(file.name,colNames=TRUE,sheet = "IdentifDIRECT",
  ##         dateTimeAs="isodatetime",
  ##         colClasses=c(NA,NA,"isodate",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  ##     }
  ##  }
  ##   else {
  ##   if(is.character(s4$Temps.fin){ 
  ##      s4 <- read.xls(file.name,colNames=TRUE,sheet = "IdentifDIRECT",
  ##        dateTimeAs="isodatetime",
  ##        colClasses=c(NA,NA,"isodate",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  ##     }
  ##   }
  if(import["ORTHO"]) db[[5]] <- read.xls(file.name,colNames=TRUE,sheet = vecOnglet[5])
  if(import["HABITAT"])db[[6]] <- read.xls(file.name,colNames=TRUE,sheet = vecOnglet[6])
  if(import["MAMMIFERE"])db[[7]] <- read.xls(file.name,colNames=TRUE,sheet = vecOnglet[7])

  return(db)  
}



## ----------------------------------------------------------------
make.session <- function(tabSession,tabMeteo,tabIdentif){
  vecOperateur <- vector()
  vecDatePassage <- unique(tabIdentif$Date)
  for(session in vecDatePassage)
    vecOperateur <-  c(vecOperateur,as.character(tabIdentif$Enregistré.par[which(tabIdentif$Date==session)][1] ))
  p = 1 # numeros passage
  new.tab <- data.frame(
                        paste(
                              gsub("-Chiro.xls", "",tabSession$Rennomer.le.fichier.Excel[1], perl=TRUE),
                              "-Pass",tabSession[which(tabSession[,2]=="Passage")[p],3],sep=""), # "REF_PASSAGE"
                        tabSession$Numéros.ciruit[1], # "ID_CIRCUIT"
                        vecDatePassage[p], # "DATE"
                        tabSession[which(tabSession[,2]=="Passage")[p],3],  # "NUMEROS_PASSAGE"
                        vecOperateur[p], # "OPERATEUR_MANIP"
                        tabIdentif$Suivi.routier.ou.piéton[1], # TYPE_SUIVI
                        tabSession$Rennomer.le.fichier.Excel[1], # "NOM_FICHIER_XLS"
                        tabMeteo[p,6], #"TEMPERATURE_DEBUT
                        tabMeteo[p,7], #"TEMPERATURE_FIN
                        tabMeteo[p,8], #"VENT"
                        tabMeteo[p,9], #"NUAGE"
                        tabMeteo[p,4], #"HEURE_DEBUT"
                        tabMeteo[p,5], #"HEURE_FIN"
                        NA, #"PASSAGE_VIDE"
                        !(length(grep("audio",
                                      as.vector(as.character(tabSession[(which(tabSession[,2] == "Passage")[p] + 1):
                                                                        (which(tabSession[,2] == "Passage")[p] + 10),
                                                                        5]))))>0), #"PASSAGE_VALIDE_ENTIER"
                        "" #"REMARQUE_PASSAGE"
                        )     
  colnames(new.tab) = c("REF_SESSION","ID_CIRCUIT","DATE","NUMEROS_PASSAGE",
            "OPERATEUR_MANIP","TYPE_SUIVI","NOM_FICHIER_XLS","TEMPERATURE_DEBUT","TEMPERATURE_FIN",
            "VENT","NUAGE","HEURE_DEBUT","HEURE_FIN","SESSION_VIDE","SESSION_VALIDE","REMARQUE_SESSION")
  tab <- new.tab
  p <- p + 1
  passageSup <-  sum(
                     tabSession[((which(tabSession[,2]=="Passage")[p]) + 1):
                                ((which(tabSession[,2]=="Passage")[p]) + 10),
                                3],na.rm=TRUE)>0
  while(passageSup){
    new.tab <-  data.frame(
                           paste(gsub("-Chiro.xls", "",
                                      tabSession$Rennomer.le.fichier.Excel[1], perl=TRUE),"-Pass",
                                 tabSession[which(tabSession[,2]=="Passage")[p],3],sep=""), # "REF_PASSAGE"
                           tabSession$Numéros.ciruit[1], # "ID_CIRCUIT"
                           vecDatePassage[p], # "DATE"
                           tabSession[which(tabSession[,2]=="Passage")[p],3],  # "NUMEROS_PASSAGE"
                           vecOperateur[p], # "OPERATEUR_MANIP"
                           tabIdentif$Suivi.routier.ou.piéton[1], # TYPE_SUIVI
                           tabSession$Rennomer.le.fichier.Excel[1], # "NOM_FICHIER_XLS"
                           tabMeteo[p,6], #"TEMPERATURE_DEBUT
                           tabMeteo[p,7], #"TEMPERATURE_FIN
                           tabMeteo[p,8], #"VENT"
                           tabMeteo[p,9], #"NUAGE"
                           tabMeteo[p,4], #"HEURE_DEBUT"
                           tabMeteo[p,5], #"HEURE_FIN"
                           NA, #"PASSAGE_VIDE"
                           !(length(grep("audio",
                                         as.vector(as.character(tabSession[(which(tabSession[,2] == "Passage")[p] + 1):
                                                                           (which(tabSession[,2] == "Passage")[p] + 10),
                                                                           5]))))>0), #"PASSAGE_VALIDE_ENTIER"
                           "" #"REMARQUE_PASSAGE"
                           )
    colnames(new.tab) <- c("REF_SESSION","ID_CIRCUIT","DATE","NUMEROS_PASSAGE",
                           "OPERATEUR_MANIP","TYPE_SUIVI","NOM_FICHIER_XLS","TEMPERATURE_DEBUT","TEMPERATURE_FIN",
                           "VENT","NUAGE","HEURE_DEBUT","HEURE_FIN","SESSION_VIDE","SESSION_VALIDE","REMARQUE_SESSION")
    tab <- rbind(tab,new.tab)
    p <- p + 1
    passageSup <- sum(tabSession[((which(tabSession[,2] == "Passage")[p]) + 1):
                                 ((which(tabSession[,2] == "Passage")[p]) + 10),
                                 3], na.rm = TRUE) > 0
  }
  return(tab)
}



## -------------------------------------------------------
##  fabrique les enregistrements de la table TRONCON du fichier en cour de traitement.
make.troncon <- function(tab,tabSession){
  ## recherche du nombre de troncon
  nbPassage <- nrow(tabSession)                 
  nbTroncon <- 13

  debutTabPassage <- 3 
  finTabPassage <- debutTabPassage+nbTroncon - 1
  tabPassage <- tab[debutTabPassage:finTabPassage,2:5] 
  p <- 1
  
  newLocalTab <- data.frame(
                            paste(paste(gsub("-Chiro.xls", "", tab$Rennomer.le.fichier.Excel[1], perl = TRUE),
                                        "-Pass",p,"-Tron",sep=""), tabPassage[,1], sep = ""), #"REF_TRONCON"
                            rep(paste(gsub("-Chiro.xls", "", tab$Rennomer.le.fichier.Excel[1], perl = TRUE),
                                      "-Pass",p,sep=""), nbTroncon),#"ID_PASSAGE"
                            paste("C", tab$Numéros.ciruit[1], "T" , 1:nbTroncon , sep = ""), # "ID_TRONCON"
                            p,#NUMEROS_PASSAGE
                            1 : nbTroncon, #NUMEROS_TRONCON
                            tabPassage$Temps.tronçon, #	troncon.temps(tab,nbTroncon), #TEMPS
                            (7200 / tabPassage$Temps.tronçon), # 	troncon.vitesse(tab,nbTroncon),#VITESSE
                            paste(gsub("-Chiro.xls", "", tab$Rennomer.le.fichier.Excel[1], perl = TRUE),
                                  "-Tron", 1 : nbTroncon, "-Chiro.wav", sep = ""), #NOM_FICHIER_WAV
                            tabPassage[,3], #"TRONCON_VIDE" : TRONCON absent | TRONCON présent une fois & (espèce = "VIDE" | "")
                            tabPassage[,4],# PROBLEME_TRONCON		
                            ifelse(tabPassage[,4] == "fichier audio illisible", TRUE, FALSE),#"TRONCON_VALIDE"
                            "") #"REMARQUE_TRONCON")  
  colnames(newLocalTab) <- c("REF_TRONCON_SESSION","ID_SESSION",
                             "ID-TRONCON-SIG","NUMEROS_PASSAGE","NUMEROS_TRONCON","TEMPS",
                             "VITESSE","NOM_FICHIER_WAV","PROBLEME_TRONCON","TRONCON_VIDE",
                             "TRONCON_VALIDE","REMARQUE_TRONCON")
  new.tab <- newLocalTab	
  if(nbPassage > 1)
    for(p in 2 : nbPassage){
      debutTabPassage <- 15 * (p - 1) + 3 
      finTabPassage <- debutTabPassage + nbTroncon - 1
      tabPassage <-  tab[debutTabPassage:finTabPassage,2:5] 
      
      newLocalTab <- data.frame(
                                paste(paste(gsub("-Chiro.xls", "", tab$Rennomer.le.fichier.Excel[1], perl = TRUE),
                                            "-Pass", p, "-Tron", sep = ""), tabPassage[,1], sep = ""), #"REF_TRONCON"
                                rep(paste(gsub("-Chiro.xls", "", tab$Rennomer.le.fichier.Excel[1], perl = TRUE),
                                          "-Pass", p, sep = ""), nbTroncon),#"ID_PASSAGE"
                                paste("C",tab$Numéros.ciruit[1],"T",1:nbTroncon,sep="") , # "ID_TRONCON"
                                p, #NUMEROS_PASSAGE
                                1 : nbTroncon, #NUMEROS_TRONCON
                                tabPassage$Temps.tronçon, #	troncon.temps(tab,nbTroncon), #TEMPS
                                (7200 / tabPassage$Temps.tronçon), # 	troncon.vitesse(tab,nbTroncon),#VITESSE
                                paste(gsub("-Chiro.xls", "", tab$Rennomer.le.fichier.Excel[1], perl = TRUE),
                                      "-Tron", 1 : nbTroncon, "-Chiro.wav", sep = ""), #NOM_FICHIER_WAV
                                tabPassage[,3], #"TRONCON_VIDE" : TRONCON absent | TRONCON présent une fois & (espèce = "VIDE" | "")
                                tabPassage[,4],# PROBLEME_TRONCON		
                                ifelse(tabPassage[,4] == "fichier audio illisible", TRUE, FALSE),#"TRONCON_VALIDE"
                                "" #"REMARQUE_TRONCON")  
                                )
      colnames(newLocalTab) <- c("REF_TRONCON_SESSION","ID_SESSION","ID-TRONCON-SIG",
                                 "NUMEROS_PASSAGE","NUMEROS_TRONCON","TEMPS","VITESSE","NOM_FICHIER_WAV",
                                 "PROBLEME_TRONCON","TRONCON_VIDE","TRONCON_VALIDE","REMARQUE_TRONCON")
      new.tab <- rbind(new.tab,newLocalTab)	
    }
  return(new.tab)
}

## -------------------------------------------------------------------
## Calcul du temps du troncon, 
## problème il faut d'abord vérifié à quelle troncon la donnée doit être attribuée
troncon.temps <- function(tab,nbTroncon){
  temps <-  rep(NA,nbTroncon)
  for (tron in 5 : (nbTroncon + 5)){
    temps[as.numeric(gsub(
                          paste("Cir", tab$Numéros.ciruit[1], "-2006-Pass",
                                tab$N..Passage[1], "-Tron", sep = ""),
                          "",
                          gsub("-Chiro.wav", "",
                               tab$Renomer.le.fichier.Excel[tron],
                               perl = TRUE),
                          perl = TRUE))] <- tab$Temps.tronçon[tron]
  }
  return(temps)
}

## --------------------------------------------------
## Calcul de la vitesse du troncon, 
## problème il faut d'abord vérifié à quelle troncon la donnée doit être attribuée
troncon.vitesse <- function(tab,nbTroncon){
  temps <- rep(NA,nbTroncon)
  for (tron in 5 : (nbTroncon + 5)){
    temps[as.numeric(gsub(
                          paste("Cir", tab$Numéros.ciruit[1],
                                "-2006-Pass", tab$N..Passage[1],
                                "-Tron", sep = ""),
                          "",
                          gsub("-Chiro.wav", "",
                               tab$Renomer.le.fichier.Excel[tron],
                               perl = TRUE),
                          perl = TRUE))] <- (7200 / tab$Temps.tronçon[tron])
  }
  return(temps)
}




## ---------------------------------------------------------------
## "TRONCON_VIDE" : TRONCON absent | TRONCON présent une fois & (espèce = "VIDE" | "")
troncon.vide <- function(tab,nbTroncon){
  tronconEmpty <- rep("Non",nbTroncon)
  for (tron in 1 : nbTroncon) {
    index.troncon <- which(tab$N..du.tron == tron)
    if (length(index.troncon) < 2) {
      if (length(index.troncon) == 0) {
        tronconEmpty[tron] <- "Oui"
      }
      else{
        if ((tab$Espèce[index.troncon] == "VIDE")|
            (tab$Espèce[index.troncon] == "")|
            (tab$Espèce[index.troncon] == "PROBLEME")) {
          tronconEmpty[tron] <- "Oui"
        }
      }
    }
  }
  return(tronconEmpty)
}


##---------------------------------------------------------
##"TRONCON_VALIDE" : espèce == "PROBLEME")
troncon.valide <- function(tab,nbTroncon)
{
  tronconValide <- rep("Oui",nbTroncon)
  tronconValide[tab$N..du.tron[which((tab$Espèce == "PROBLEME") |
                                     (tab$Espèce == "problème") |
                                     (tab$Espèce == "probleme"))]] <- "Non"
  return(tronconValide)
}




## -------------------------------------------------------
## fabrique les enregistrements de la table CHIRO du fichier en cour de traitement.
make.chiro <- function(tab,troncon)
{
  p <- 1 # numéros du passage pour la boucle
  pMax <- max(tab$N..Passage,na.rm = TRUE)# nombre de passage
  tabPass <- subset(tab,N..Passage == p)
  tronconPass <- subset(troncon,NUMEROS_PASSAGE == p)
  new.tabPass <- data.frame(
                            paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1, stop = 4),
                                  "-Pass", tabPass[,2], "_" , 1 : dim(tabPass)[1], sep = ""), # "REF_CHIRO"
                            ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),"-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S",trunc(tabPass$Temps..s. / (tronconPass$TEMPS[tabPass$N..du.tronçon] / 5)) + 1,sep = ""),paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),"-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S1",sep = "")), # "ID_SECTEUR_SESSION"
                            paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1, stop = 4),
                                  "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, sep = ""), # "ID_TRONCON_SESSION"
                            p,#NUMEROS_PASSAGE
                            tabPass$Analysé.par, # "OPERATEUR_IDENTIFICATION"
                            ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,"routier","pedestre"), # TYPE_SUIVI
                            tabPass$Temps..s., #TEMPS"
                            nomEspece(tabPass$Espèce), # "ESPECE"
                            tabPass$Effectif, # "EFFECTIF"
                            tabPass$Déclenché.par, # "DECLENCHEMENT"
                            tabPass$Type.de.cri, 
                            tabPass$Degré.de.confiance, # "DEGRES_CONFIANCE"
                            tabPass$Remarque, # "REMARQUE"
                            tabPass$Transmission.des.données #TRANSMISSION_DONNEES
                            )
  colnames(new.tabPass) <- c("REF_CHIRO","ID_SECTEUR_SESSION","ID_TRONCON_SESSION",
                             "NUMEROS_PASSAGE","OPERATEUR_IDENTIFICATION","TYPE_SUIVI",
                             "TEMPS","ESPECE","EFFECTIF","DECLENCHEMENT","TYPE_CRI","DEGRES_CONFIANCE",
                             "REMARQUE","TRANSMISSION_DONNEES")
  new.tab <- new.tabPass
  if(pMax > 1) {
    for ( p in 2:pMax) {
      tabPass <- subset(tab,N..Passage == p)
      tronconPass <- subset(troncon,NUMEROS_PASSAGE == p)
      new.tabPass <- data.frame(
                                paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1, stop = 4),
                                      "-Pass", tabPass[,2], "_", 1 : dim(tabPass)[1], sep = ""), # "REF_CHIRO"
                                ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,
                                       paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),
                                             "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S",
                                             trunc(tabPass$Temps..s. / (tronconPass$TEMPS[tabPass$N..du.tronçon] / 5)) + 1,sep = ""),
                                       paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),
                                             "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S1",sep = "")), # "ID_SECTEUR_SESSION"
                                paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),
                                      "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, sep = ""), # "ID_TRONCON_SESSION"
                                p, #NUMEROS_PASSAGE
                                tabPass$Analysé.par, # "OPERATEUR_IDENTIFICATION"
                                ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,"routier","pedestre"), # TYPE_SUIVI
                                tabPass$Temps..s., #TEMPS"
                                nomEspece(tabPass$Espèce), # "ESPECE"
                                tabPass$Effectif, # "EFFECTIF"
                                tabPass$Déclenché.par, # "DECLENCHEMENT"
                                tabPass$Type.de.cri,
                                tabPass$Degré.de.confiance, # "DEGRES_CONFIANCE"
                                tabPass$Remarque, # "REMARQUE"
                                tabPass$Transmission.des.données #TRANSMISSION_DONNEES
                                )
      colnames(new.tabPass) <- c("REF_CHIRO","ID_SECTEUR_SESSION","ID_TRONCON_SESSION",
                                 "NUMEROS_PASSAGE","OPERATEUR_IDENTIFICATION","TYPE_SUIVI","TEMPS","ESPECE","EFFECTIF",
                                 "DECLENCHEMENT","TYPE_CRI","DEGRES_CONFIANCE","REMARQUE","TRANSMISSION_DONNEES")
      new.tab <- rbind(new.tab,new.tabPass)
    }
  }

  new.tab <- new.tab[which(new.tab$ESPECE != ""),] # ne concerve que les lignes saisies
  new.tab <- new.tab[which(new.tab$ESPECE != "VIDE"),] # ne concerve pas les lignes annonçant un problème
  return(new.tab)
}

## -------------------------------------------------------
nomEspece <- function(vecSp){
  vecSp <- sub(' +$','',vecSp)
  vecSp <- sub(' ','_',vecSp)
  vecSp <- sub('/','_',vecSp)
  vecSp <- sub("sp.","sp",vecSp)
  vecSp <- ifelse(vecSp == "?","sp",vecSp)
  return(vecSp)
}


## ------------------------------------------------------------
##  fabrique les enregistrements de la table CHIRO du fichier en cour de traitement.
make.chiroDirect <- function(tab,troncon)
{
  p <- 1 # numéros du passage pour la boucle
  pMax <- max(tab$N..Passage, na.rm = TRUE)# nombre de passage
  tabPass <- subset(tab,N..Passage == p)
  tronconPass <- subset(troncon, NUMEROS_PASSAGE == p)
  new.tabPass <- data.frame(
                            paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1, stop = 4),
                                  "-Pass", tabPass[,2], "_", 1 : dim(tabPass)[1], "_D", sep = ""), # "REF_CHIRO"
                            ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,
                                   paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),
                                         "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S",
                                         trunc(tabPass$Temps..s. / (tronconPass$TEMPS[tabPass$N..du.tronçon] / 5)) + 1,sep = ""),
                                   paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),
                                         "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S1",sep = "")), # "ID_SECTEUR_SESSION"
                            paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1, stop = 4),
                                  "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, sep = ""), # "ID_TRONCON_SESSION"
                            p,#NUMEROS_PASSAGE
                            tabPass$Analysé.par, # "OPERATEUR_IDENTIFICATION"
                            ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,"routier","pedestre"), # TYPE_SUIVI
                            tempsDebut(tabPass), 
                            tempsFin(tabPass),
                            tempsDuree(tabPass),
                            nomEspece(tabPass$Espèce), # "ESPECE"
                            tabPass$Type.de.cri,
                            tabPass$Degré.de.confiance, # "DEGRES_CONFIANCE"
                            tabPass$Remarque, # "REMARQUE"
                            tabPass$Transmission.des.données #TRANSMISSION_DONNEES
                            )
  colnames(new.tabPass) <- c("REF_CHIRO","ID_SECTEUR_SESSION",
                             "ID_TRONCON_SESSION","NUMEROS_PASSAGE","OPERATEUR_IDENTIFICATION",
                             "TYPE_SUIVI","TEMPS_DEBUT","TEMPS_FIN","TEMPS_DUREE","ESPECE",
                             "TYPE_CRI","DEGRES_CONFIANCE","REMARQUE","TRANSMISSION_DONNEES")
  new.tab <- new.tabPass
  
  if(pMax > 1){
    for ( p in 2:pMax)
      {
        tabPass <- subset(tab,N..Passage == p)
        tronconPass <- subset(troncon, NUMEROS_PASSAGE == p)
	new.tabPass <- data.frame(
                                  paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1, stop = 4),
                                        "-Pass", tabPass[,2], "_", 1 : dim(tabPass)[1], "D", sep = ""), # "REF_CHIRO"
                                  ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,
                                         paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),
                                               "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S",
                                               trunc(tabPass$Temps..s. / (tronconPass$TEMPS[tabPass$N..du.tronçon] / 5)) + 1,sep = ""),
                                         paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1,stop = 4),
                                               "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, "-S1",sep = "")), # "ID_SECTEUR_SESSION"
                                  paste("Cir", tabPass[,1], "-", substr(tabPass[,3], start = 1, stop = 4),
                                        "-Pass", tabPass[,2], "-Tron", tabPass$N..du.tronçon, sep = ""), # "ID_TRONCON_SESSION"
                                  p, #NUMEROS_PASSAGE
                                  tabPass$Analysé.par, # "OPERATEUR_IDENTIFICATION"
                                  ifelse(length(grep("routier",tabPass$Suivi.routier.ou.piéton[1]))>0,"routier","pedestre"), # TYPE_SUIVI
                                  tempsDebut(tabPass), 
                                  tempsFin(tabPass),
                                  tempsDuree(tabPass),
                                  nomEspece(tabPass$Espèce), # "ESPECE"
                                  tabPass$Type.de.cri,
                                  tabPass$Degré.de.confiance, # "DEGRES_CONFIANCE"
                                  tabPass$Remarque, # "REMARQUE"
                                  tabPass$Transmission.des.données #TRANSMISSION_DONNEES
                                  )
	colnames(new.tabPass) <- c("REF_CHIRO","ID_SECTEUR_SESSION","ID_TRONCON_SESSION",
                                   "NUMEROS_PASSAGE","OPERATEUR_IDENTIFICATION","TYPE_SUIVI","TEMPS_DEBUT","TEMPS_FIN",
                                   "TEMPS_DUREE","ESPECE","TYPE_CRI","DEGRES_CONFIANCE","REMARQUE","TRANSMISSION_DONNEES")
        new.tab <- rbind(new.tab,new.tabPass)
      }
  }
  new.tab <- new.tab[which(new.tab$ESPECE != ""),] # ne concerve que les lignes saisies
  new.tab <- new.tab[which(new.tab$ESPECE != "VIDE"),] # ne concerve pas les lignes annonçant un problème
  return(new.tab)
}

## -------------------------------------------------------
tempsDebut <- function(tab){
  if(length(which(colnames(tab) == "Temps..s."))>0) return(tab$Temps..s.)
  else return(tab$Temps.début)
} 

## -------------------------------------------------------
tempsFin <-  function(tab){
  if(length(which(colnames(tab) == "Temps.fin"))>0) return(tab$Temps.fin)
  else return(tab$Temps..s. + tab$Durée.séquence)
}

## -------------------------------------------------------
tempsDuree <-  function(tab){
  if(length(which(colnames(tab) == "Durée.séquence"))>0) return(tab$Durée.séquence)
  else return(tab$Temps.fin  - tab$Temps.début)
}




## ------------------------------------------------------	
## fabrique les enregistrements de la table SECTEUR du fichier en cour de traitement.
make.secteur <- function(t_chiro, t_troncon, t_session)
{
  pMax <- max(t_session$NUMEROS_PASSAGE)# nombre de passage
  p <- 1

  t_chiroPass <- subset(t_chiro,NUMEROS_PASSAGE == p)
  t_tronconPass <- subset(t_troncon,(NUMEROS_PASSAGE == p & !is.na(TEMPS)))
  t_sessionPass <- subset(t_session,NUMEROS_PASSAGE == p)
  nbTronconPass <- max(t_tronconPass$NUMEROS_TRONCON)
  
  new.tab <- data.frame(
                        paste("Cir", t_sessionPass$ID_CIRCUIT[1], "-", substr(t_sessionPass$DATE[1], start = 1, stop = 4),
                              "-Pass", p, "-Tron", rep(1 : nbTronconPass, each = 5), "-S", 1 : 5, sep = ""), # "REF_SECTEUR"
                        paste("C", t_sessionPass$ID_CIRCUIT[1], "T", rep(1 : nbTronconPass, each = 5), "S", 1 : 5, sep = ""), # "ID_SECTEUR"
                        t_sessionPass$ID_CIRCUIT[1], # CIRCUIT
                        t_session$TYPE_SUIVI[1],
                        rep(1 : nbTronconPass,each = 5),   # NUMEROS_TRONCON
                        p, # NUMEROS_PASSAGE
                        1 : 5, # NUMEROS_SECTEUR
                        substr(t_sessionPass$DATE[1], start = 1, stop = 4) # ANNEE
                        )
  colnames(new.tab) <- c( "REF_SECTEUR_SESSION","CODE_SIG","ID_CIRCUIT","TYPE_SUIVI",
                         "NUMEROS_TRONCON","NUMEROS_PASSAGE","NUMEROS_SECTEUR","ANNEE")
  
  vecNomSp <- c("VIDE","AUTRE","sp","Barbastella_barbastellus","Eptesicus_serotinus",
                "Hypsugo_savi","Miniopterus_schreibersi","Myotis_daubentoni","Myotis_natterii",
                "Myotis_sp","Nyctalus_lasiopterus","Nyctalus_leisleri","Nyctalus_noctula",
                "Pipistrellus_kulhii","Pipistrellus_nathusius",
                "Pipistrellus_nathusius_kulhii","Pipistrellus_pipistrellus","Pipistrellus_pygmaeus",
                "Plecotus_sp","Rhinolophus_ferrumequinum",
                "Rhinolophus_hipposideros","Tadarida_teniotis")            
  tabCroisePass <- matrix(0,nrow(new.tab),length(vecNomSp))
  rownames(tabCroisePass) <- new.tab[,1]
  colnames(tabCroisePass) <- vecNomSp
  tabCroisePass <- as.data.frame(tabCroisePass)
  
  vec_idSec <- rownames(tabCroisePass)
  for(idSec in vec_idSec){   
    t_chiroPassSec <- subset(t_chiroPass,ID_SECTEUR_SESSION==idSec)
    if(nrow(t_chiroPassSec) > 0){
      tabCroisePass[idSec,"VIDE"] <- FALSE
      vecNomSpSec <- unique(t_chiroPassSec$ESPECE)
      for(nomSp in vecNomSpSec){
        if(length(which(vecNomSp == nomSp)) > 0) {
          tabCroisePass[idSec,nomSp] <- sum(subset(t_chiroPassSec,ESPECE == nomSp)$EFFECTIF)
        }
        else {
          cat("!!",idSec," : ",nomSp, '!!\n')
          tabCroisePass[idSec,"AUTRE"] <- tabCroisePass[idSec,"AUTRE"] +
            sum(subset(t_chiroPassSec,ESPECE == nomSp)$EFFECTIF)
        }
      }
    }
    else{
      tabCroisePass[idSec,"VIDE"] <- TRUE
    }
  }
  dataCroisePass <- cbind(new.tab, tabCroisePass)
  dataCroise <- dataCroisePass

  if(pMax > 1){ 
    for (p in 2 : pMax) {
      t_chiroPass <- subset(t_chiro,NUMEROS_PASSAGE == p)
      t_tronconPass <- subset(t_troncon,(NUMEROS_PASSAGE == p & !is.na(TEMPS)))
      t_sessionPass <- subset(t_session,NUMEROS_PASSAGE == p)
      nbTronconPass <- max(t_tronconPass$NUMEROS_TRONCON)
      
      new.tab = data.frame(
        paste("Cir", t_sessionPass$ID_CIRCUIT[1], "-", substr(t_sessionPass$DATE[1], start = 1, stop = 4),
              "-Pass", p, "-Tron", rep(1 : nbTronconPass, each = 5), "-S", 1 : 5, sep = ""), # "REF_SECTEUR"
        paste("C", t_sessionPass$ID_CIRCUIT[1], "T", rep(1 : nbTronconPass, each = 5), "S", 1 : 5,sep = ""), # "ID_SECTEUR"
        t_sessionPass$ID_CIRCUIT[1], # CIRCUIT
        t_session$TYPE_SUIVI[1],
        rep(1 : nbTronconPass, each = 5),   # NUMEROS_TRONCON
        p, # NUMEROS_PASSAGE
        1:5, # NUMEROS_SECTEUR
        substr(t_sessionPass$DATE[1],start=1,stop=4) # ANNEE
	)
      colnames(new.tab) <- c( "REF_SECTEUR_SESSION","CODE_SIG","ID_CIRCUIT","TYPE_SUIVI",
                             "NUMEROS_TRONCON","NUMEROS_PASSAGE",
                             "NUMEROS_SECTEUR","ANNEE")
      
      vecNomSp <- c("VIDE","AUTRE","sp","Barbastella_barbastellus","Eptesicus_serotinus",
                    "Hypsugo_savi","Miniopterus_schreibersi","Myotis_daubentoni","Myotis_natterii",
                    "Myotis_sp","Nyctalus_lasiopterus","Nyctalus_leisleri",
                    "Nyctalus_noctula","Pipistrellus_kulhii","Pipistrellus_nathusius",
                    "Pipistrellus_nathusius_kulhii","Pipistrellus_pipistrellus",
                    "Pipistrellus_pygmaeus","Plecotus_sp","Rhinolophus_ferrumequinum",
                    "Rhinolophus_hipposideros","Tadarida_teniotis")            
      tabCroisePass <- matrix(0,nrow(new.tab),length(vecNomSp))
      rownames(tabCroisePass) <- new.tab[,1]
      colnames(tabCroisePass) <- vecNomSp
      tabCroisePass <- as.data.frame(tabCroisePass)
      
      vec_idSec <- rownames(tabCroisePass)

      for(idSec in vec_idSec){   
        t_chiroPassSec <- subset(t_chiroPass,ID_SECTEUR_SESSION==idSec)
        if(nrow(t_chiroPassSec) > 0){
          tabCroisePass[idSec,"VIDE"] <- FALSE
          vecNomSpSec <- unique(t_chiroPassSec$ESPECE)
          for(nomSp in vecNomSpSec){
            if(length(which(vecNomSp == nomSp)) > 0) {
              tabCroisePass[idSec,nomSp] <- sum(subset(t_chiroPassSec,ESPECE == nomSp)$EFFECTIF)
            }
            else {
              cat("!!",idSec," : ",nomSp, '!!\n')
              tabCroisePass[idSec,"AUTRE"] <- tabCroisePass[idSec,"AUTRE"] + sum(subset(t_chiroPassSec,ESPECE == nomSp)$EFFECTIF)
            }
            
          }
        }
        else{
          tabCroisePass[idSec,"VIDE"] <- TRUE
        }
      }
      dataCroisePass <- cbind(new.tab,tabCroisePass)
      dataCroise <- rbind(dataCroise,dataCroisePass)
    }      
  }
  
  dataCroise$VIDE <- as.logical(dataCroise$VIDE)
  return(dataCroise)
}


## -------------------------------------------
secteur.vide <- function(chiro,troncon)
{
  secteurEmpty <- rep("Non",nbTroncon*5)
  nbTroncon <- dim(troncon)[1]
  for(T in 1 : nbTroncon) {
    if(troncon$TRONCON_VIDE[T] == "Oui") {
      secteurEmpty[((T * 5) - 4) : (T * 5)] = "Oui"
    }
    else {
      for(S in 1 : 5) {
        id.secteur <- paste(troncon$REF_PASSAGE[1], "-Tron", T, "-S", S, sep = "")
        if (length(which(chiro$ID_SECTEUR == id.secteur)) == 0) {	
          secteurEmpty[(5 * (T - 1)) + S] <- "Oui"
        }
      }
    }
  }
  return(secteurEmpty)
}




## -----------------------------------------------
make.habitat <- function(habitat.import,session,chiro){
                                        #browser()
  habitat <- habitat.import[1:100,1:10]
  new.data <- data.frame(
                         paste("Cir",session$ID_CIRCUIT, "-", substr(session$DATE, start = 1, stop = 4), 
                               "-Tron", habitat$Tronçon, "-S", habitat$Point,"-", 
                               substr(habitat[,4], start = 9, stop = 11),sep=""),  # REF_HABITAT
                         session$ID_CIRCUIT,
                         substr(session$DATE, start = 1, stop = 4), 
                         paste("Cir",session$ID_CIRCUIT, "-", substr(session$DATE, start = 1, stop = 4), 
                               "-Tron", habitat$Tronçon,sep=""),
                         habitat$Tronçon,
                         paste("Cir",session$ID_CIRCUIT, "-", substr(session$DATE, start = 1, stop = 4), 
                               "-Tron", habitat$Tronçon, "-S", habitat$Point,sep="") ,
                         habitat$Point,
                         paste("C",session$ID_CIRCUIT,"T", habitat$Tronçon, "S", habitat$Point,sep=""),
                         substr(habitat[,4], start = 9, stop = 11),
                         chiro$TYPE_SUIVI[1],
                         habitat[,5],
                         habitat[,6],
                         habitat[,7],
                         habitat[,8],
                         habitat[,9],
                         habitat[,10])
  colnames(new.data) <- c("REF_HABITAT","ID_CIRCUIT","ANNEE","ID_TRONCON",
                          "NUMEROS_TRONCON","ID_SECTEUR","NUMEROS_SECTEUR","CODE_SIG","PRI_SEC",
                          "HAB_NIV1","HAB_NIV2","HAB_NIV3","HAB_NIV4","HAB_NIV5","HAB_NIV6")
  
  return(new.data)
}

## -------------------------------------------------
## Enregistre dans un fichier la table correspondante
save.chiro <- function(tab,nom.table)
{
  nomFichier <- paste("C:/Documents and Settings/christian kerbiriou/Mes documents/CHIROPTERE/moulinette/SaisieChiro_Result/",
                      format(Sys.time(), "%Y_%m_%d"),"-",nom.table,".txt",sep="")
  write.table(tab,nomFichier,row.names = FALSE)
  cat("          <-- ",nomFichier,"\n") 
}



## -------------------------------------------------
chiro.auto <- function(import = c(DIRECT = TRUE,ORTHO = FALSE, HABITAT = TRUE, MAMMIFERE = FALSE))
{
  names(import) = c("DIRECT","ORTHO","HABITAT","MAMMIFERE") 
  cat("------------------------------------------------\n")
  cat("   Traitement des fichiers du répertoire \n")
  cat("------------------------------------------------\n")

  repertoire <- 'C:/Documents and Settings/christian kerbiriou/Mes documents/CHIROPTERE/moulinette/' 
  setwd(repertoire)
  list.fichier <- dir(paste(repertoire,"saisieChiroFileAttenteXLS",sep=""),full.names = TRUE)
  i <- 1
  db <- lecture.xls.sheet(list.fichier[i],import)
### print(dim(db[[6]]))
  ## cat(" Session")
  session <- make.session(db[[1]], db[[2]], db[[3]])           
  ## cat(" - Troncon")
  troncon <- make.troncon(db[[1]], session)
  ## cat(" - Chiro")
  chiro <- make.chiro(db[[3]], troncon)
  if (import["DIRECT"]) {
    ## cat(" - ChiroDirect")
    chiroDirect <- make.chiroDirect(db[[4]],troncon)
  }
  if (import["ORTHO"]) {
    ## cat(" - Ortho")
    ortho <- make.chiroDirect(db[[5]],troncon)
  }

  if (import["HABITAT"]) {
    ## cat(" - habitat")
    habitat <- make.habitat(db[["HABITAT"]],session,chiro)
  }
  ## cat(" - Secteur")
  secteur <- make.secteur(chiro,troncon,session)
  session.global <- session
  troncon.global <- troncon
  secteur.global <- secteur
  chiro.global <- chiro
  if (import["DIRECT"]) chiroDirect.global <- chiroDirect
  if (import["ORTHO"]) ortho.global <- ortho
  if (import["HABITAT"]) habitat.global <-  habitat

  if (length(list.fichier) > 1){
    for (i in 2 : length(list.fichier)){
      db <- lecture.xls.sheet(list.fichier[i],import)
###   print(dim(db[[6]]))
      ## cat(" Session")
      session <- make.session(db[[1]], db[[2]], db[[3]])           
      ## cat(" - Troncon")
      troncon <- make.troncon(db[[1]], session)
      nbTroncon <- dim(troncon)[1]
      ## cat(" - Chiro")
      chiro = make.chiro(db[[3]], troncon)
      if (import["DIRECT"]){
        ## cat(" - ChiroDirect")
        chiroDirect = make.chiroDirect(db[[4]],  troncon)
      }
      if (import["ORTHO"]){
        ## cat(" - Ortho")
        ortho = make.chiroDirect(db[[5]],  troncon)
      }
      if (import["HABITAT"]) {
        ## cat(" - habitat")
        habitat <- make.habitat(db[[6]],session,chiro)
      }
      ## cat(" - Secteur")
      secteur <- make.secteur(chiro,troncon, session)
      session.global <- rbind(session.global, session)
      troncon.global <- rbind(troncon.global, troncon)
      secteur.global <- rbind(secteur.global, secteur)
      chiro.global <- rbind(chiro.global, chiro)
      if (import["DIRECT"]) chiroDirect.global <- rbind(chiroDirect.global, chiroDirect)
      if (import["ORTHO"]) ortho.global <- rbind(ortho.global, ortho)
      if (import["HABITAT"]) habitat.global <-  rbind(habitat.global,habitat)
    }
  }
  save.chiro(session.global, "session")
  save.chiro(troncon.global, "troncon")
  save.chiro(secteur.global, "secteur")
  save.chiro(chiro.global, "chiro")
  if (import["DIRECT"]) save.chiro(chiroDirect.global, "chiroDirect")
  if (import["ORTHO"]) save.chiro(ortho.global, "ortho")
  if (import["HABITAT"]) save.chiro(habitat.global, "habitat")
}

# =============================================================================================




testProtocole.autoSp <- function(tabChiro=NA,tabDirect=NA,repertoire=getwd(),
                          repertoireSortie="testProtocole",
                          dateFichier="2011_01_13",
                          espece=""){ #tab chiro et chiroDirect

# tabChiro=NA;tabDirect=NA;repertoire=getwd();repertoireSortie="testProtocole";dateFichier="2011_01_13";espece=""
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

vecEspece <- unique(tabDirect$ESPECE)

  for(sp in vecEspece)
    testProtocole(tabChiro,tabDirect,espece=sp)


}




## =================================================================================


testProtocole <- function(tabChiro=NA,tabDirect=NA,repertoire=getwd(),
                          repertoireSortie="testProtocole",
                          dateFichier="2011_01_13",
                          espece=""){ #tab chiro et chiroDirect

### tabChiro=NA;tabDirect=NA;repertoire=getwd();repertoireSortie="testProtocole";dateFichier="2011_01_13";espece=""
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


