###############################################
####   Script de prepartation des data et d'analyse des tendances
##############################################




my_read_delim <- function(file="data/DataRP_SpTron_90.csv",sep=c("\t",";",","),dec=c(".",","),as.tibble=TRUE,print_head=TRUE,max_col_head=10,print_summary=FALSE) {

   # file="data/DataRP_SpTron_90.csv";sep=c("\t",";",",");dec=c(".",",") ##

    nextSep <- TRUE
    nbSep <- length(sep)
    i <- 0

    cat("Opening:",file,"\n    with with decimal '",dec[1],"' and separator",sep="")

    while(nextSep & i < nbSep) {
        i <- i + 1
        cat("  '",sep[i],"'")
#browser()
        d <- try(read.delim(file,header=TRUE,stringsAsFactor=FALSE,sep=sep[i],dec=dec[1]),silent=TRUE)

        if(class(d)[1]=="try-error") {
            nextSep <- TRUE
        } else {
            nextSep <- ncol(d) ==  1
            }
     }

    if(ncol(d)>1) {
        vecdec <- dec[-1]
        vecdec <- vecdec[which(vecdec != sep[i])]
        if(length(vecdec>0)) {
            thecolclass <- sapply(d,class)
            numericNotFound <- !("numeric" %in% thecolclass)
            if(numericNotFound) {
                cat("\n Test for other decimal character\n",sep="")
                colchar <- which(thecolclass=="character")
                j <- 0
                while(numericNotFound & j < length(colchar)) {
                    j <- j+1
                    b <- 0
                    while(numericNotFound & b < length(dec)) {
                        b <- b + 1
                        veccol <- d[,i]
                        veccol <- ifelse(is.na(veccol) | veccol == "" | veccol== " ",-99999,veccol)
                        veccol <- sub(dec[1],vecdec[b],veccol)
                        veccol <- as.numeric(veccol)
                        numericNotFound <- any(is.na(veccol))

                    }

                }
                if(!numericNotFound) {
                    cat("Decimal character founded: '",vecdec[b],"'\n",sep="")
                    cat("Opening:",file,"\n    with with decimal '",vecdec[b],"' and separator '",sep[i],"'\n",sep="")
                    d <- read.table(file,header=TRUE,stringsAsFactor=FALSE,sep=sep[i],dec=vecdec[b])

                }
            }
        }
    }else{
        cat("Only 1 column founded!!!\n    ---> Check that the separator is in the proposed separators!\n")
    }

    cat("\n")
    cat("Dimension:\n")
    print(dim(d))
    if(print_head) {
        cat("Head:\n")
        print(d[1:5,1:(min(max_col_head,ncol(d)))])
    }

    if(print_summary) {
        cat("Summary:\n")
        print(summary(d))
    }

    if(as.tibble) {
        library(dplyr)
        dd <- try(as_tibble(d))
        if(class(dd)[1]=="try-error") cat("The conversion to table format did not work.... \n Output at dataframe format!! \n") else d <- dd
    }


    return(d)
}



prepa_data <- function(d=NULL,dsite=NULL,dpart=NULL,dSR=NULL,id=NULL,output=TRUE,seuilProbPip=.75) {
    library(dplyr)
    library(readr)
    library(lubridate)
    library(ggplot2)

   d <- NULL ;    dsite <- NULL;  dpart <- NULL;dSR=NULL; id <- NULL;seuilProbPip=c(.75,.8)

    if(is.null(id))
        id <- Sys.Date()

    if(is.null(d))
        d <- my_read_delim("data/DataRP_SpTron_90.csv")
   if("num_micro" %in% colnames(d)) colnames(d)[colnames(d)=="num_micro"] <- "micro_droit"

    if(is.null(dpart))
        dpart <-my_read_delim("data/p_export.csv",sep=";")

    if(is.null(dsite))
        dsite <- my_read_delim("data/sites_localites.txt")
   if("id_site" %in% colnames(dsite)) colnames(dsite)[colnames(dsite)=="id_site"] <- "idsite"

    dsite <- dsite[,c("idsite","longitude","latitude")]
    dsite <- aggregate(.~idsite,data=dsite,mean)


   if(is.null(dSR))
        dSR <- my_read_delim("data/SRmed.csv")
   if("MicroDroit" %in% colnames(dSR)) colnames(dSR)[colnames(dSR)=="MicroDroit"] <- "micro_droit"


    if(length(seuilProbPip) == 1) seuilProbPip <- data.frame(seuil= rep(seuilProbPip,2),expansion_direct = c("direct","exp")) else seuilProbPip <- data.frame(seuilProbPip = seuilProbPip,expansion_direct = c("direct","exp"))


    dim(d)
    d <- inner_join(d,dpart)
    d <- inner_join(d,dsite)
    d <- inner_join(d,dSR)
    dim(d)
## num_micro = micro_right

## ajouter colonne de verification


    cat("Ajout des colonnes de date: date_format_full, date_format, year, month, julian\n")
    d$date_format_full<- as.POSIXlt(d$date_debut,zone = "CET",format="%d/%m/%Y %H:%M")
    d$date_format <- format(d$date_format_full,format="%Y-%m-%d")

    d$year <- year(d$date_format)
    d$month <- month(d$date_format)
    d$julian <- yday(d$date_format)

    cat("Ajout de la colonne expansion_direct (exp, direct, NA)\n")
    d$expansion <- ifelse(d$micro_droit,d$canal_expansion_temps == "DROITE" & d$canal_enregistrement_direct != "DROITE" ,d$canal_expansion_temps == "GAUCHE" & d$canal_enregistrement_direct != "GAUCHE")
    d$direct <- ifelse(d$micro_droit,d$canal_enregistrement_direct == "DROITE" & d$canal_expansion_temps != "DROITE",d$canal_enregistrement_direct == "GAUCHE" & d$canal_expansion_temps != "GAUCHE")
    d$expansion_direct <- ifelse(d$expansion,"exp",ifelse(d$direct,"direct",NA))

    cat("Ajout qq flag de validité\n")
    gg <- ggplot(data=subset(d,!is.na(expansion_direct)),aes(IndiceProbPip)) + geom_histogram() + facet_wrap(.~expansion_direct)
    gg <- gg + geom_vline(data=seuilProbPip,aes(xintercept=seuil),colour="red")
    ggfile <- paste("output/indicePropPip_",id,".png",sep="")
    cat("  -> [PNG]",ggfile)
    ggsave(ggfile,gg)

    d <- full_join(d,seuilProbPip)
    d$PropPip_good <-d$IndiceProbPip>d$seuilProbPip




    filecsv <- paste("data/data_vigieChiro_",id,".csv",sep="")
    cat("   CSV -> ",filecsv)
    write_csv(dd,filecsv)
    cat("   DONE !\n")

    if(output) return(dd)

}




summary_vigie_chiro <- function(d) {
    library(ggplot2)
    library(lubridate)

    vecSp <- c("Pippip","Plaint","Eptser","Tetvir","Nyclei","Yerray","Phanan","Pleaus","Plaalb","Minsch","Testes","Epheph","Pipkuh","Plasab","Pippyg","Leppun","Rusnit","Sepsep","Myodau","Phofem","Barfis","MyoGT","Mimasp","Phogri","Nycnoc","Phafal","Roeroe","Myonat","Isopyr","Urosp","Ratnor","Barbar","Pipnat","Pleaur","Hypsav","Metbra","Myomys","Lamsp.","Antsp","Eupsp","Cyrscu","Decalb","Plaaff","Rhifer","Tadten","Nyclas","Myoema","Rhasp","Cympud","Tyllil","Plafal","Myobec","Eptnil","Rhihip")
    d <- subset(d,espece %in% vecSp)

    ## et pour dur pas pour expansion mais peu servir en direct
    ## en direct doit être sup à 1.5



     vecSp <- c("Pippip","Eptser","Nyclei","Pipkuh","Pippyg","Myodau","Nycnoc","Barbar","Pipnat")

    dy_site<- unique(d[,c("idsite","year","date")])
    dy_site <- as.data.frame(table(dy_site[,1:2]))
    dy_site <- dy_site[order(dy_site$idsite,dy_site$year),]
    colnames(dy_site)[3] <- "nb_sample"
    dy_site$year <- as.numeric(as.character(dy_site$year))

    firstY <- aggregate(year~idsite,data=subset(dy_site,nb_sample>0),FUN=min)
    colnames(firstY)[2] <- "first_year"
    firstY$first_year <- as.numeric(as.character(firstY$first_year))

    firstYnb <-as.data.frame(table(firstY$first_year))
    colnames(firstYnb) <- c("first_year","nb_site")
      firstYnb$first_year <- as.numeric(as.character(firstYnb$first_year))

    firstY <- inner_join(firstY,firstYnb)
    firstY$first_year_nb <- paste(firstY$first_year," (",firstY$nb_site,")",sep="")

    dy_site <- inner_join(dy_site,firstY)

    dy_site$year <- ifelse(dy_site$year < dy_site$first_year,NA,dy_site$year)



    gg <- ggplot(data=subset(dy_site,nb_sample>0),aes(x=year,y=nb_sample,group=idsite)) + facet_wrap(.~first_year_nb)
    gg <- gg + geom_line() + geom_jitter()
    ggsave("output/nb_sample_year.png",gg)



    dym_site <-  unique(d[,c("idsite","year","month","date")])
    dym_site <- as.data.frame(table(dym_site[,1:3]))
    dym_site <- dym_site[order(dym_site$idsite,dym_site$year,dym_site$month),]
    colnames(dym_site)[4] <- "nb_sample"
    dym_site$year <- as.numeric(as.character(dym_site$year))
    dym_site <- subset(dym_site,nb_sample > 0)


    hist(d$month)

gg <- ggplot(data=d,aes(temps_enr)) + facet_wrap(espece~.,scales="free") + geom_histogram()
    ggsave("output/temps_enr_sp.png",gg)

gg <- ggplot(data=d,aes(nb_contacts)) + facet_wrap(espece~.,scales="free") + geom_histogram()
    ggsave("output/nb_contacts_sp.png",gg)


    d_seq_tps <- aggregate(temps_enr ~participation + espece,data=d, sum)

}



