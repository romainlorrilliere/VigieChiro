###############################################
####   Script de prepartation des data et d'analyse des tendances
##############################################


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Generic function to read separatd column file
##' @param file path file
##' @param sep vector of separator tested default c("\t",";",",")
##' @param dec vector of decimal tested  default c(".",",")
##' @param as.tibble logical to get a tibble format, default TRUE
##' @param print_head logical to print to screen the head of data
##' @param max_col_head the maximal number of column if the head of data is printed
##' @param print_summary logical to print to screen the summary of the data
##' @return data.frame or tibble
##' @author Romain Lorrilliere
my_read_delim <- function(file,sep=c("\t",";",","),dec=c(".",","),as.tibble=TRUE,print_head=TRUE,max_col_head=10,print_summary=FALSE) {

    ## file="data/DataRP_SpTron_90.csv";sep=c("\t",";",",");dec=c(".",",") ##

    nextSep <- TRUE
    nbSep <- length(sep)
    i <- 0

    cat("\nOpening:",file,"\n    with with decimal '",dec[1],"' and try separators",sep="")

    while(nextSep & i < nbSep) {
        i <- i + 1
        cat("\n '",sep[i],"'")
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

    pbEncoding_columns <- grep("Ã",d)
    if(length(pbEncoding_columns)>0) {
        cat("\nCharacter do not recognised in ",length(pbEncoding_columns)," columns\n",sep="")
        cat("  --> Convert encoding to UTF-8\n")
        for(j in pbEncoding_columns) Encoding(d[,j]) <- "UTF-8"
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

    cat("\n   DONE!\n\n")
    return(d)
}




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Add absence in data set of observation
##' @param d data,
##' @param col_gr character vector of the column names that will be uses to construct the sample unit example : date, site. The species column could be add in this vector, default NULL
##' @param col_sp name of the species name column default NULL
##' @param col_value vectors of colmuns that will be update with absence, defaut NULL, if null all columns not present in col_gr and col_sp will be update with absence
##' @param dall if there are not all sample in d you can import a table with all sample with the column of col_gr, default NULL
##' @param as.tibble  logical to get a tibble format, default TRUE
##' @return update of d (data.frame or tibble) with 0 in the col_value(s) when species are absente
##' @author
add_abs <- function(d,col_gr=NULL,col_sp=NULL,col_value=NULL,col_info_gr=NULL,dall=NULL,as.tibble=TRUE) {
    ##     col_value = c("nb_contacts","temps_enr"); col_sp = "col_sp"; col_gr = c("col_sample","expansion_direct")


    cat("\nAjout des absences\n")
    if(class(d)[1]=="data.frame") d <- as_tibble(d)

    if(!is.null(col_info_gr)){
        dgr <- unique(d %>% select(one_of(c(col_gr,col_info_gr))))
        dgr_seul <- unique(d %>% select(one_of(col_gr)))
        if(nrow(dgr) != nrow(dgr_seul)) {
            stop("ERROR ! l'association des colonnes:\n",colnames(dgr),"\nn'est pas unique\n")
        }
        d <- d %>% select( -one_of(col_info_gr))
    }

    if(is.null(dall)) {
        l_gr <- list()

        col_exp <- c(col_sp,col_gr)
        for(i in 1:length(col_exp))
            l_gr[[i]] <- unique(pull(d,col_exp[i]))

        dall <-(expand.grid(l_gr,stringsAsFactors=FALSE))
        colnames(dall) <- col_exp
    }

    d <- full_join(d,dall)
    if(is.null(col_value)) {
        d[is.na(d)] <- 0
    } else {
        for(j in col_value)
            d[is.na(d[,j]),j] <- 0
    }


    if(!is.null(col_info_gr))
        d <- full_join(d,dgr)



    if(as.tibble) {

        library(dplyr)
        dd <- try(as_tibble(d))
        if(class(dd)[1]=="try-error") {
            cat("The conversion to table format did not work.... \n Output at dataframe format!! \n")
            d <- dd
            d <- d[order(d),]
        }else {
             d <- d %>% arrange()

        }
    } else {
            d <- dd
            d <- d[order(d),]

    }



    cat("\n   DONE!\n\n")

    return(d)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param id
##' @param d
##' @param dpart
##' @param dsite
##' @param dSR
##' @param output
##' @param col_sample
##' @param col_sp
##' @param col_date
##' @param col_site
##' @param col_IndiceDurPip
##' @param col_IndiceProbPip
##' @param col_SampleRate
##' @param col_tron
##' @param col_nbcontact
##' @param col_temps
##' @param seuilProbPip
##' @param seuilInfDurPipDirect
##' @param seuilSR
##' @param col_sp
##' @param seuilSR_inf
##' @param seuilSR_sup
##' @param list_sample_cat
##' @param aggregate_site
##' @param add_absence
##' @param list_sp
##' @param first_columns
##' @return
##' @author Romain Lorrilliere
prepa_data <- function(id="DataRP_SpTron_90",
                       d="data/DataRP_SpTron_90.csv",dpart="data/p_export.csv",dsite ="data/sites_localites.txt",dSR="data/SRmed.csv",
                       output=TRUE,
                       col_sample="participation",col_sp="espece",col_date="date_debut",col_site="site",col_IndiceDurPip="IndiceDurPip",col_IndiceProbPip="IndiceProbPip",col_SampleRate="SampleRate",col_tron="Tron",col_nbcontact="nb_contacts",col_temps="temps_enr",
                       seuilProbPip=c(.9,.85),seuilInfDurPipDirect=1.5,seuilSupDurPipExp=0.5,
                       seuilSR=tibble(expansion_direct=c("exp","direct","direct"),col_sp=c(NA,NA,"Nycnoc"),seuilSR_inf=c(441000,96000,44100),seuilSR_sup=c(2000000,384000,384000)),
                       list_sample_cat=c("pedestre","routier"),
                       aggregate_site=TRUE,add_absence=TRUE,
                       list_sp= c("Pippip","Plaint","Eptser","Tetvir","Nyclei","Yerray","Phanan","Pleaus","Plaalb","Minsch","Testes","Epheph","Pipkuh","Plasab","Pippyg","Leppun","Rusnit","Sepsep","Myodau","Phofem","Barfis","MyoGT","Mimasp","Phogri","Nycnoc","Phafal","Roeroe","Myonat","Isopyr","Urosp","Ratnor","Barbar","Pipnat","Pleaur","Hypsav","Metbra","Myomys","Lamsp.","Antsp","Eupsp","Cyrscu","Decalb","Plaaff","Rhifer","Tadten","Nyclas","Myoema","Rhasp","Cympud","Tyllil","Plafal","Myobec","Eptnil","Rhihip"),
                       first_columns=c("participation","idobservateur","date_format","year","month","julian","ordre_passage","sample_cat","num_site","Tron","expansion_direct","PropPip_good","DurPip_good","SampleRate_good","strict_selection","flexible_selection","espece","nb_contacts","temps_enr","longitude","latitude","num_site_txt"),
                       only_first_columns=FALSE,excluded_columns="commentaire") {
    library(dplyr)
    library(readr)
    library(lubridate)
    library(ggplot2)

#    d = "data/DataRP_SpTron_90.csv" ;   dpart = "data/p_export.csv";    dsite ="data/sites_localites.txt"; dSR="data/SRmed.csv";    id = NULL;seuilProbPip=c(.75,.8);seuilInfDurPipDirect=1.5;  seuilSupDurPipExp=0.5;  seuilSR=tibble(expansion_direct=c("exp","direct","direct"),col_sp=c(NA,NA,"Nycnoc"),seuilSR_inf=c(441000,96000,44100),seuilSR_sup=c(2000000,384000,384000));    add_absence=TRUE;                       first_columns=c("participation","idobservateur","date_format","year","month","julian","ordre_passage","sample_cat","num_site","Tron","expansion_direct","PropPip_good","DurPip_good","SampleRate_good","strict_selection","flexible_selection","espece","nb_contacts","temps_enr","longitude","latitude","num_site_txt");list_sample_cat=c("pedestre","routier"); col_sample="participation";col_sp="espece";col_IndiceDurPip="IndiceDurPip"; col_IndiceProbPip="IndiceProbPip";col_SampleRate="SampleRate"; col_date="date_debut";col_site="site";col_tron="Tron";col_nbcontact="nb_contacts";col_temps="temps_enr";aggregate_site=TRUE;list_col_sp= c("Pippip","Plaint","Eptser","Tetvir","Nyclei","Yerray","Phanan","Pleaus","Plaalb","Minsch","Testes","Epheph","Pipkuh","Plasab","Pippyg","Leppun","Rusnit","Sepsep","Myodau","Phofem","Barfis","MyoGT","Mimasp","Phogri","Nycnoc","Phafal","Roeroe","Myonat","Isopyr","Urosp","Ratnor","Barbar","Pipnat","Pleaur","Hypsav","Metbra","Myomys","Lamsp.","Antsp","Eupsp","Cyrscu","Decalb","Plaaff","Rhifer","Tadten","Nyclas","Myoema","Rhasp","Cympud","Tyllil","Plafal","Myobec","Eptnil","Rhihip")

    if(is.null(id))
        id <- Sys.Date()

    if(class(d)[1]=="character")
        d <- my_read_delim(d,print_head=FALSE)
    if("num_micro" %in% colnames(d)) colnames(d)[colnames(d)=="num_micro"] <- "micro_droit"

    cat("\n ##Dimension de la table: \n")
    print(dim(d))

    if(!is.null(dpart)) {
        if(class(dpart)[1]=="character")
            dpart <-my_read_delim(dpart,print_head=FALSE)
        d <- inner_join(d,dpart)
        cat("\n ##Dimension de la table: \n")
        print(dim(d))
    }


    if(!("sample_cat" %in% colnames(d))) {
        cat("\nAjout de la colonne sample_cat du type de suivie: pedestre, routier, fixe\n")
        d$sample_cat <- NA
        d$sample_cat[grep("estre",pull(d,col_site))] <- "pedestre"
        d$sample_cat[grep("ixe",pull(d,col_site))] <- "fixe"
        d$sample_cat[grep("outier",pull(d,col_site))] <- "routier"
    }

      if(!("num_site" %in% colnames(d))) {
        cat("\nAjout de la colonne du numeros de site\n")
        d$num_site <- as.numeric(ifelse(d$sample_cat == "routier",
                                        gsub("Vigie-chiro - Routier-","",d$site),
                                 ifelse(d$sample_cat=="pedestre",
                                        gsub("Vigiechiro - Pédestre-","",d$site),NA)))
        d$num_site_txt<- sprintf(paste("%0",max(nchar(d$num_site)),"d",sep=""), d$num_site)
       }


    cat("\nAjout des colonnes de date: date_format_full, date_format, year, month, julian\n")
    cat("-----------------------------------------\n\n")

    if(!(col_date %in% colnames(d))) {
        cat("\n  - La colonne enregistrée pour les dates n'est pas présentes dans les colonnes des données:\n",colnames(d),"\n")
        col_temps <- readline("Saisir le nom de la colonne des dates:")
        while(!(col_temps %in% colnames(d)))
            col_temps <- readline("Saisir le nom de la colonne des dates:")

    }

    d$date_format_full<- as.POSIXct(pull(d,col_date),zone = "CET",format="%d/%m/%Y %H:%M")
    d$date_format <- format(d$date_format_full,format="%Y-%m-%d")

    d$year <- year(d$date_format)
    d$month <- month(d$date_format)
    d$julian <- yday(d$date_format)


    cat("\nAjout de la colonne ordre_passage\n")
    cat("-----------------------------------------\n\n")


    library(data.table)

    seuilPassage <- c(150,220,285)
    seuilPassage <- tibble(julian=seuilPassage,date_year= format(as.POSIXct(paste(seuilPassage,2000),zone = "CET",format="%j %Y"),format="%d/%m"))

    period <- tibble(passage=as.character(1:2),period=c(paste(seuilPassage$date_year[1]," -> ",seuilPassage$date_year[2],sep=""),paste(seuilPassage$date_year[2]," -> ",seuilPassage$date_year[3],sep="")))

    dparti <- unique(select(d,c("idsite","date_format","julian","year")))
    dparti <- dparti %>% arrange()
    dparti$idsite_year <- paste(dparti$idsite,dparti$year,sep="_")
    ddparti<- data.table(dparti)
    ddparti <- ddparti[,ordre_passage:= 1:.N, by = idsite_year]
    dparti <- as_tibble(ddparti)

    dnb <- aggregate(ordre_passage ~ idsite_year, dparti,max)
    colnames(dnb)[2] <- "nombre_de_passage"
    dparti <- inner_join(dparti,dnb)


    dparti$passage <- ifelse(dparti$julian>=seuilPassage$julian[1] & dparti$julian<seuilPassage$julian[2],1,ifelse(dparti$julian>=seuilPassage$julian[2] & dparti$julian<seuilPassage$julian[3],2,""))

    dparti <- full_join(dparti,period)

    dparti$ordre_passage_f <- as.factor(dparti$ordre_passage)
    dparti$nombre_de_passage_f<- as.factor(dparti$nombre_de_passage)

    gg <- ggplot(data=dparti,aes(x=julian,group=period,fill=period)) + facet_grid(ordre_passage~nombre_de_passage)
    gg <- gg + geom_histogram(alpha=.8)
    gg <- gg + geom_vline(data=seuilPassage,aes(xintercept=julian))
    gg <- gg + labs(x="Jour julien",title="Date des passages,\nen fonction du nombre de passage (colonne) et du passage (ligne)",fill="Période",subtitle=id)
   ggfile <- paste("output/date_passage_distribution_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg,width=13,height=11)


    gg <- ggplot(data=dparti,aes(y=julian,x=ordre_passage,colour=nombre_de_passage_f))
    gg <- gg + geom_jitter(alpha=0.5,width=0.1)
    gg <- gg + geom_hline(data=seuilPassage,aes(yintercept=julian))
    gg <- gg + geom_label(data=seuilPassage,aes(y=seuilPassage$julian,label=date_year,x=6),colour="black")
    gg <- gg + labs(x="Les passages",title="Date des passages en fonction du passage",colour="Nombre de passages",subtitle=id)
    ggfile <- paste("output/date_passage_plot",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg,width=8,height=7)

    d <- inner_join(d,dparti)


    if(!is.null(dsite)){
        if(class(dsite)[1]=="character")
            dsite <- my_read_delim(dsite,print_head=FALSE)
        if("id_site" %in% colnames(dsite)) colnames(dsite)[colnames(dsite)=="id_site"] <- "idsite"
        dsite <- dsite[,c("idsite","longitude","latitude")]
        dsite <- aggregate(.~idsite,data=dsite,mean)

        d <- inner_join(d,dsite)
        cat("\n ##Dimension de la table: \n")
        print(dim(d))
    }


    if(!is.null(dSR)){
        if(class(dSR)[1]=="character")
            dSR <- my_read_delim(dSR,print_head=FALSE)
    if("MicroDroit" %in% colnames(dSR)) colnames(dSR)[colnames(dSR)=="MicroDroit"] <- "micro_droit"

        d <- inner_join(d,dSR)
        cat("\n ##Dimension de la table: \n")
        print(dim(d))

    }

    if(length(seuilProbPip) == 1) seuil_ProbPip <- tibble(seuil_ProbPip= rep(seuilProbPip,2),expansion_direct = c("direct","exp")) else seuil_ProbPip <- tibble(seuil_ProbPip = seuilProbPip,expansion_direct = c("direct","exp"))

    seuilInf_DurPip <- tibble(seuilInf_DurPip=c(seuilInfDurPipDirect,0),expansion_direct = c("direct","exp"))
    seuilSup_DurPip <- tibble(seuilSup_DurPip=c(5,seuilSupDurPipExp),expansion_direct = c("direct","exp"))

    seuil_DurPip <- inner_join(seuilInf_DurPip,seuilSup_DurPip)

    if(!(is.null(excluded_columns)) & length(intersect(colnames(d),excluded_columns))>0) {
        cat("\nExclusion des colonnes\n")
        cat("  ",excluded_columns,"\n")
         d <- d %>% select(-one_of(excluded_columns))
        }

    if(!is.null(list_sp)) {
        cat("\nSelection des sps parmi\n")
        cat("  ",list_sp,"\n")
        d <- d[d[[col_sp]] %in% list_sp,]
        cat("\n ##Dimension de la table: \n")
        print(dim(d))
    }


    if(!is.null(list_sample_cat)) {
        cat("\nSelection des type de suivi\n")
        cat("  ",list_sample_cat,"\n")
        d <- d[d[["sample_cat"]] %in% list_sample_cat,]
        cat("\n ##Dimension de la table: \n")
        print(dim(d))
    }


    cat("\nAjout de la colonne expansion_direct (exp, direct, NA)\n")
    cat("-----------------------------------------\n\n")
    d$expansion <- ifelse(d$micro_droit,d$canal_expansion_temps == "DROITE" & d$canal_enregistrement_direct != "DROITE" ,d$canal_expansion_temps == "GAUCHE" & d$canal_enregistrement_direct != "GAUCHE")
    d$direct <- ifelse(d$micro_droit,d$canal_enregistrement_direct == "DROITE" & d$canal_expansion_temps != "DROITE",d$canal_enregistrement_direct == "GAUCHE" & d$canal_expansion_temps != "GAUCHE")
    d$expansion_direct <- ifelse(d$expansion,"exp",ifelse(d$direct,"direct",NA))

    cat("\nAjout qq flag de validité\n")
            cat("-----------------------------------------\n\n")

    cat("\n   - IndiceProbPip\n")
    print(seuil_ProbPip)

    gg <- ggplot(data=subset(d,!is.na(expansion_direct)),aes(IndiceProbPip)) + geom_histogram() + facet_grid(expansion_direct~.)
    gg <- gg + geom_vline(data=seuil_ProbPip,aes(xintercept=seuil_ProbPip),colour="red",size=2,alpha = .8)
    gg <- gg + labs(x="indicePropPip",title=paste("indicePropPip",id))
    ggfile <- paste("output/indicePropPip_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)

    d <- full_join(d,seuil_ProbPip)
    d$PropPip_good <- (pull(d,col_IndiceProbPip) >= pull(d,"seuil_ProbPip"))
    cat("\n ##Dimension de la table: \n")
    print(dim(d))


    cat("\n   - IndiceProbPipParticipation_med & IndiceProbPipParticipation_max\n")

    dd <- unique(d%>%select(c("num_site","Tron","date_format","sample_cat","expansion_direct","IndiceProbPip")))

    dparti <- aggregate(IndiceProbPip ~ num_site + date_format + sample_cat + expansion_direct ,dd,median)
    colnames(dparti)[5] <- "IndiceProbPipParticipation_med"

    ddparti <- aggregate(IndiceProbPip ~ num_site + date_format+ sample_cat + expansion_direct,dd,max)
    colnames(ddparti)[5] <- "IndiceProbPipParticipation_max"
    dparti <- inner_join(dparti,ddparti)


    dd2 <- unique(d%>%select(c("num_site","date_format","sample_cat","expansion_direct","IndiceProbPip")))
    ddparti2 <- aggregate(IndiceProbPip ~ num_site + date_format+ sample_cat + expansion_direct,dd2,length)
    colnames(ddparti2)[5] <- "IndiceProbPipParticipation_nb"

    ggparti2 <- melt(ddparti2,id.vars=c("num_site","date_format","sample_cat","expansion_direct"))
    gg <- ggplot(data=ggparti2,aes(value)) + geom_histogram() + facet_grid(sample_cat~expansion_direct)
    gg <- gg + labs(x="indiceProbPip",title=paste("indiceProbPip",id))
    ggfile <- paste("output/indiceProbPipParticipation_nbValueDiff_hist_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)


    gg <- ggplot(data=dparti,aes(x=IndiceProbPipParticipation_med,y=IndiceProbPipParticipation_max,colour=sample_cat,group=sample_cat))+ facet_grid(sample_cat~expansion_direct)
    gg <- gg + geom_abline(slope=1,intercept=0,size=2,alpha=.3)
    gg <- gg + geom_smooth()
    gg <- gg + geom_point()
    gg <- gg + labs(title=paste("IndicePropPip des partitions",id),colour="")

    ggfile <- paste("output/indiceProbPipParticipation_plot_med_max_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)

    ggparti <- melt(dparti,id.vars=c("num_site","date_format","sample_cat","expansion_direct"))
    gg <- ggplot(data=ggparti,aes(value)) + geom_histogram() + facet_grid(variable~expansion_direct)
   ## gg <- gg + geom_vline(data=seuil_ProbPip,aes(xintercept=seuil_ProbPip),colour="red",size=2,alpha = .8)
gg
    gg <- gg + labs(x="indiceProbPip",title=paste("indiceProbPip",id))
    ggfile <- paste("output/indiceProbPipParticipation_hist_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)



    cat("\n   - IndiceDurPip\n")
    print(seuil_DurPip)

    seuil_DurPip$seuilSup_DurPip_fig <- seuil_DurPip$seuilSup_DurPip + 0.5
    seuil_DurPip$seuilInf_DurPip_fig <- seuil_DurPip$seuilInf_DurPip - 0.5

    gg <- ggplot(data=subset(d,!is.na(expansion_direct)),aes(IndiceDurPip)) + geom_histogram() + facet_grid(expansion_direct~.)
    gg <- gg + geom_vline(data=seuil_DurPip,aes(xintercept=seuilInf_DurPip_fig),colour="red",size=2,alpha=.6)
    gg <- gg + geom_vline(data=seuil_DurPip,aes(xintercept=seuilSup_DurPip_fig),colour="red",size=2,alpha=.6)
    gg <- gg + labs(x="indiceDurPip",title=paste("indiceDurPip",id))
    ggfile <- paste("output/indiceDurPip_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)

    d <- full_join(d,seuil_DurPip)
    d$DurPip_good <- (pull(d,col_IndiceDurPip) >= pull(d,"seuilInf_DurPip")) & (pull(d,col_IndiceDurPip) <= pull(d,"seuilSup_DurPip"))

    cat("\n ##Dimension de la table: \n")
    print(dim(d))




    cat("\n   - IndiceDurPipParticipation_med & IndiceDurPipParticipation_max\n")

    dd <- unique(d%>%select(c("num_site","Tron","date_format","sample_cat","expansion_direct","IndiceDurPip")))

    dparti <- aggregate(IndiceDurPip ~ num_site + date_format + sample_cat + expansion_direct ,dd,median)
    colnames(dparti)[5] <- "IndiceDurPipParticipation_med"

    ddparti <- aggregate(IndiceDurPip ~ num_site + date_format+ sample_cat + expansion_direct,dd,max)
    colnames(ddparti)[5] <- "IndiceDurPipParticipation_max"
    dparti <- inner_join(dparti,ddparti)


    dd2 <- unique(d%>%select(c("num_site","date_format","sample_cat","expansion_direct","IndiceDurPip")))
    ddparti2 <- aggregate(IndiceDurPip ~ num_site + date_format+ sample_cat + expansion_direct,dd2,length)
    colnames(ddparti2)[5] <- "IndiceDurPipParticipation_nb"

    ggparti2 <- melt(ddparti2,id.vars=c("num_site","date_format","sample_cat","expansion_direct"))
    gg <- ggplot(data=ggparti2,aes(value)) + geom_histogram() + facet_grid(sample_cat~expansion_direct)
    gg <- gg + labs(x="indiceDurPip_nb",title=paste("indiceDurPip, nombre de valeur par participation",id))
    ggfile <- paste("output/indiceDurPipParticipation_nbValueDiff_hist_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)


    gg <- ggplot(data=dparti,aes(x=IndiceDurPipParticipation_med,y=IndiceDurPipParticipation_max,colour=sample_cat,group=sample_cat))+ facet_grid(sample_cat~expansion_direct)
    gg <- gg + geom_abline(slope=1,intercept=0,size=2,alpha=.3)
    gg <- gg + geom_smooth()
    gg <- gg + geom_point()
    gg <- gg + labs(title=paste("IndicePropPip des partitions",id),colour="")

    ggfile <- paste("output/indiceDurPipParticipation_plot_med_max_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)

    ggparti <- melt(dparti,id.vars=c("num_site","date_format","sample_cat","expansion_direct"))
    gg <- ggplot(data=ggparti,aes(value)) + geom_histogram() + facet_grid(variable~expansion_direct)
   ## gg <- gg + geom_vline(data=seuil_DurPip,aes(xintercept=seuil_DurPip),colour="red",size=2,alpha = .8)
gg
    gg <- gg + labs(x="indiceDurPip_pariticipation_nb",title=paste("indiceDurPip, nombre de valeur par participation",id))
    ggfile <- paste("output/indiceDurPipParticipation_hist_",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg)



    cat("\n   - SampleRate\n")
   print(seuilSR)
   seuilSR_gen <- subset(seuilSR,is.na(col_sp),select=c("expansion_direct","seuilSR_inf","seuilSR_sup"))
    d <- full_join(d,seuilSR_gen)

    seuilSR_sp <- subset(seuilSR,!is.na(col_sp))
    colnames(seuilSR_sp)[3:4] <- paste(colnames(seuilSR_sp)[3:4],"sp",sep="_")
    colnames(seuilSR_sp)[colnames(seuilSR_sp)=="col_sp"] <- col_sp
    cat("\n ##Dimension de la table: \n")
    print(dim(d))


    d <- full_join(d,seuilSR_sp)

    d$seuilSR_inf <- ifelse(is.na(d$seuilSR_inf_sp),d$seuilSR_inf,d$seuilSR_inf_sp)
    d$seuilSR_sup <- ifelse(is.na(d$seuilSR_sup_sp),d$seuilSR_sup,d$seuilSR_sup_sp)



    d <- d[,!(colnames(d) %in% c("seuilSR_inf_sp","seuilSR_sup_sp"))]
    d$SampleRate_good <- pull(d,col_SampleRate) >= d$seuilSR_inf & pull(d,col_SampleRate) <= d$seuilSR_sup

    cat("\n ##Dimension de la table: \n")
    print(dim(d))


    cat("\n   - strict_selection <- SampleRate_good & DurPip_good &  PropPip_good\n")
    d$strict_selection <- d$SampleRate_good & d$DurPip_good &  d$PropPip_good

    cat("\n   - flexible_selection <- SampleRate_good \n")
    d$flexible_selection <- d$SampleRate_good

    cat("Exclusion des data pour lesquelles la catégorie expansion_direct n'a pas pu être affectée\n")


    d <- subset(d,!(is.na(expansion_direct)))
    cat("\n ##Dimension de la table: \n")
    print(dim(d))

    if(aggregate_site) {

        cat("\nAggregation des données aux sites\n")
        cat("-----------------------------------------\n\n")
        if(!(col_tron %in% colnames(d))) {
            cat("\n  - La colonne enregistrée pour les tronçons et ou les points n'est pas présentes dans les colonnes des données:\n",colnames(d),"\n")
            col_tron <- readline("Saisir le nom de la colonne des tronçons/points:")
            while(!(col_tron %in% colnames(d)))
                col_tron <- readline("Saisir le nom de la colonne des tronçons/points:")
        }


        if(!(col_nbcontact %in% colnames(d))) {
            cat("\n  - La colonne enregistrée pour les nombres de contacts n'est pas présentes dans les colonnes des données:\n",colnames(d),"\n")
            col_nbcontact <- readline("Saisir le nom de la colonne des nombres de contacts:")
            while(!(col_nbcontact %in% colnames(d)))
                col_nbcontact <- readline("Saisir le nom de la colonne des des nombres de contacts:")

        }



          if(!(col_temps %in% colnames(d))) {
              cat("\n  - La colonne enregistrée pour les durées des séquences n'est pas présentes dans les colonnes des données:\n",colnames(d),"\n")
            col_temps <- readline("Saisir le nom de la colonne des durées des séquences:")
              while(!(col_temps %in% colnames(d)))
                  col_temps <- readline("Saisir le nom de la colonne des durées des séquences:")

        }


        dd <- d[,c(col_sample,"expansion_direct",col_sp,col_nbcontact,col_temps,col_tron,"strict_selection","flexible_selection")]

        colpart <- c(col_sample,"expansion_direct",col_sp,setdiff(colnames(d),c(colnames(dd),col_IndiceDurPip,col_IndiceProbPip,col_SampleRate,"seuil_ProbPip","PropPip_good","seuilInf_DurPip","DurPip_good","expansion","direct","sampleRate_good")))

        ddpart <-  unique(d[,colpart])

        ddpart_sp_ed <- unique(d[,c(col_sample,"expansion_direct",col_sp)])

        if(nrow(ddpart_sp_ed) != nrow(ddpart)) {
            stop("ERROR ! l'association des colonnes:\n",colnames(ddpart_sp_ed),"\nn'est pas unique\n")

        }

        cat("\n  - Data avec filtre strict\n")
        form <- as.formula(paste(col_nbcontact," ~  ",col_sample," + ",col_sp," + expansion_direct"))
        dd_strict_contact <- aggregate(form,subset(d,strict_selection),sum)

        form <- as.formula(paste(col_temps," ~ ",col_sample," + ",col_sp," + expansion_direct"))
        dd_strict_temps <- aggregate(form,subset(d,strict_selection),sum)

        form <- as.formula(paste(col_tron," ~  ",col_sample))
        dd_strict_nbtron <- aggregate(form ,unique(d[d$strict_selection,c(col_sample,col_tron)]),length)
        colnames(dd_strict_nbtron)[2] <- paste("nb_",col_tron,sep="")

        dd_strict <- inner_join(dd_strict_contact,dd_strict_temps)

        cat("\n ##Dimension de la table (selection strict): \n")
        print(dim(dd_strict))

        if(add_absence) {
            dd_strict <- add_abs(dd_strict,c(col_sample,"expansion_direct",col_sp))
            cat("\n ##Dimension de la table (selection strict): \n")
            print(dim(dd_strict))

        }
        dd_strict <- inner_join(dd_strict,dd_strict_nbtron)

        colnames(dd_strict)[4:6] <- paste(colnames(dd_strict)[4:6],"strict",sep="_")

        cat("\n  - Data avec filtre fléxible\n")
        form <- as.formula(paste(col_nbcontact," ~ ",col_sample," + ",col_sp," + expansion_direct"))
        dd_flexible_contact <- aggregate(form ,subset(d,flexible_selection),sum)

        form <- as.formula(paste(col_temps," ~ ",col_sample," + ",col_sp," + expansion_direct"))
        dd_flexible_temps <- aggregate(form ,subset(d,flexible_selection),sum)

        form <- as.formula(paste(col_tron," ~ ",col_sample))
        dd_flexible_nbtron <- aggregate(form,unique(d[d$flexible_selection,c(col_sample,col_tron)]),length)
        colnames(dd_flexible_nbtron)[2] <- paste("nb_",col_tron,sep="")

        dd_flexible <- inner_join(dd_flexible_contact,dd_flexible_temps)

        cat("\n ##Dimension de la table (selection flexible): \n")
        print(dim(dd_flexible))

        if(add_absence) {
            dd_flexible <- add_abs(dd_flexible,c(col_sample,"expansion_direct",col_sp))
        cat("\n ##Dimension de la table (selection flexible): \n")
            print(dim(dd_flexible))
        }

        dd_flexible <- inner_join(dd_flexible,dd_flexible_nbtron)
        colnames(dd_flexible)[4:6] <- paste(colnames(dd_flexible)[4:6],"flexible",sep="_")

        dd <- full_join(dd_strict,dd_flexible)
        dd <-full_join(dd,ddpart)

        d <- dd

        cat("\n ##Dimension de la table: \n")
        print(dim(d))

        col_tron_agg <- paste("nb_",col_tron,sep="")

        i_first_columns <- c(which(first_columns == col_tron),which(first_columns == col_nbcontact),which(first_columns == col_temps))
        names(i_first_columns) <- c(col_tron_agg,col_nbcontact,col_temps)

        i_first_columns <- sort(i_first_columns)

        first_columns_aggregate <- c(first_columns[1:(i_first_columns[1]-1)],
                                     paste(names(i_first_columns[1]),c("strict","flexible"),sep="_"))
        if((i_first_columns[1]+1)>(i_first_columns[2]-1))
            first_columns_aggregate <- c(first_columns_aggregate ,first_columns[(i_first_columns[1]+1):(i_first_columns[2]-1)])

        first_columns_aggregate <- c(first_columns_aggregate ,paste(names(i_first_columns[2]),c("strict","flexible"),sep="_"))

        if((i_first_columns[2]+1)>(i_first_columns[3]-1))
            first_columns_aggregate <- c(first_columns_aggregate ,first_columns[(i_first_columns[2]+1):(i_first_columns[3]-1)])
        first_columns_aggregate <- c(first_columns_aggregate, paste(names(i_first_columns[3]),c("strict","flexible"),sep="_"), first_columns[(i_first_columns[3]+1):length(first_columns)])

        first_columns <- first_columns_aggregate
    }else{
        if(add_absence) {



            colpart <- setdiff(colnames(d),c(col_sample,"expansion_direct",col_sp,col_nbcontact,col_temps,col_tron,col_IndiceDurPip,col_IndiceProbPip,"PropPip_good","DurPip_good","sampleRate_good", "seuilSR_inf","seuilSR_sup","SampleRate_good"))

            d <- add_abs(d,col_gr=c(col_sample,"expansion_direct",col_tron),col_sp=col_sp,col_value=c(col_nbcontact,col_temps),col_info_gr=colpart)
            cat("\n ##Dimension de la table: \n")
            print(dim(d))

        }
    }

    cat("\nChangement de l'ordre des colonnes\n")
    cat("-----------------------------------------\n\n")
    cat(first_columns,"...\n")

    first_columns_abs <- setdiff(first_columns,colnames(d))

    if(length(first_columns_abs)>0){
        cat("\n",length(first_columns_abs),"colonne(s) non présente(s) dans les données:\n  ", first_columns_abs,"\n")
        first_columns <- setdiff(first_columns,first_columns_abs)
    }
    if(only_first_columns) colOrder <- first_columns else colOrder <- c(first_columns,setdiff(colnames(d),first_columns))

    d <- d %>% select(colOrder)
    d <- arrange(d)

    filecsv <- paste("data/data_vigieChiro_",id,"_",ifelse(aggregate_site,"site","TronPoint"),"_",ifelse(is.null(list_sp),"allSp",ifelse(length(list_sp)<6,paste(list_sp,collapse="-"),paste(length(list_sp),"sp",sep=""))),"_",ifelse(add_absence,"withAbs","presence="),".csv",sep="")
    cat("\n   --> [CSV]",filecsv)
    write.table(d,filecsv,sep="\t",dec=".",row.names=FALSE)
    cat("   DONE !\n")

    cat("\n ##Dimension de la table: \n")
    print(dim(d))


    if(output) return(d)

}


historic_tron <- function(d,id=NULL) {


    d <- subset(d,nb_contacts > 0)
    d_first_year <- aggregate(year~num_site_txt,d,min)
    colnames(d_first_year)[2] <- "first_year"
    d_last_year <- aggregate(year~num_site_txt,d,max)
    colnames(d_last_year)[2] <- "last_year"

    d <- inner_join(d,d_first_year)
    d <- inner_join(d,d_last_year)


    dunique <- unique(d %>% select(c("num_site_txt","year","Tron","sample_cat")))
   ## dd <- add_abs(dunique,col_gr=c("num_site_txt","year","sample_cat"),col_sp="Tron",col_value="survey")

    dtron <- aggregate(year~num_site_txt+sample_cat+Tron,dunique,length)
    colnames(dtron)[4] <- "nb_survey"

    dmaxYear <- aggregate(nb_survey~num_site_txt+sample_cat,dtron,max)
    colnames(dmaxYear)[3] <- "nbYear_ef"

    dtron <- inner_join(dtron,dmaxYear)

    dunique <- unique(dunique %>% select(-one_of(c("year"))))

    dunique <- inner_join(dunique,d_first_year)
    dunique <- inner_join(dunique,d_last_year)


    dd <- inner_join(dtron,dunique)
    dd$nbYear_th<- dd$last_year - dd$first_year + 1

    dd$pro_th <- dd$nb_survey / dd$nbYear_th
    dd$pro_ef <- dd$nb_survey / dd$nbYear_ef

    dsite <- unique(dd %>% select(c("num_site_txt","sample_cat","first_year","nbYear_ef","nbYear_th")))

    dsite$namesite <- paste(dsite$first_year," - ",dsite$num_site_txt," (",dsite$nbYear_ef,")",sep="")

    dsite <- dsite %>% arrange(first_year,namesite)

    dsite$numsite <- 1:nrow(dsite)
    dsite$numsite <- sprintf("%03d", dsite$numsite)

    dsite$namesite <- paste(dsite$first_year," - ",dsite$numsite," (",dsite$nbYear_ef,")",sep="")


    dd <- inner_join(dd,dsite)

    dd <- dd %>% arrange(num_site_txt,Tron)


    gg <- ggplot(data=subset(dd,nbYear_ef >1),aes(x=num_site_txt,y=Tron,colour=pro_ef,size=nbYear_ef)) + facet_wrap(.~sample_cat,scales="free",nrow=2)
    gg <- gg + geom_point()
    gg <- gg + scale_colour_gradient2(low="#a50026",mid = "#fee08b",high="#063402",midpoint = .75)
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5,size=8))
    gg <- gg + labs(x="Les sites",y="Les tronçons ou points",title=paste("Qualité de suivis -",id),size="Nombre\n d'années\n de suivie\n du site",colour="Proportion\n des années\n suivis\n du tronçons")
    ggfile <- paste("output/surveyTroncon",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg,width=18,height=8)



      filecsv <- paste("output/data_survey_troncon.csv",sep="")
    cat("\n   --> [CSV]",filecsv)
    write.table(d,filecsv,sep="\t",dec=".",row.names=FALSE)
    cat("   DONE !\n")



}





summary_sp <- function(d,id=NULL,save=c("length","wide"),output="") {

    d <- aggregate(cbind(d$nb_contacts_strict,d$nb_contacts_flexible)~num_site_txt+ year + expansion_direct + espece,d,max)
    colnames(d)[5:6] <- c("nb_contacts_strict","nb_contacts_flexible")
    d$occ_strict <- ifelse(d$nb_contacts_strict>0,1,0)
    d$occ_flexible <- ifelse(d$nb_contacts_flexible>0,1,0)

    dagg <- aggregate(cbind(d$occ_strict,d$occ_flexible)~year + expansion_direct + espece, d, sum)
    colnames(dagg)[4:5] <- c("cc_strict","occ_flexible")


    dtot <-  aggregate(cbind(d$occ_strict,d$occ_flexible)~ espece, d, sum)
    colnames(dtot)[2:3] <- c("tot_strict","tot_flexible")

    dtot$name <- paste(dtot$espece," (",dtot$tot_strict,")",sep="")

    dagg <- inner_join(dagg,dtot)


    dagg$name <- factor(dagg$name, levels = dtot$name[order(dtot$tot_strict,decreasing=TRUE)])

    gg <- ggplot(d=subset(dagg,tot_strict>10),aes(x=year,y=cc_strict,group=expansion_direct,colour=expansion_direct))+facet_wrap(.~name,scales="free_y",ncol=5)
    gg <- gg + geom_point() + geom_line()
    gg <- gg + labs(y="occurence",colour="",title=paste("les occurences -",id))
    ggfile <- paste("output/occurence_especes",id,".png",sep="")
    cat("\n  -> [PNG]",ggfile,"\n")
    ggsave(ggfile,gg,width=16,height=8)

    if("wide" %in% c(save,output)) {
        library(reshape2)
        dout_wide <- dcast(d[,c("num_site_txt","expansion_direct","espece","year","nb_contacts_strict")],num_site_txt+expansion_direct+espece~year)
        dout_wide <- dout_wide[order(dout_wide$num_site_txt,dout_wide$espece,dout_wide$expansion_direct),]
    }

    if("wide" %in% c(save)) {

        filecsv <- paste("output/data_abondance_site_year_WIDE_",id,".csv",sep="")
        cat("\n   --> [CSV]",filecsv)
        write.table(dout_wide,filecsv,sep="\t",dec=".",row.names=FALSE)
        cat("   DONE !\n")

    }


    if("wide" %in% c(save)) {

        filecsv <- paste("output/data_abondance_site_year_LENGTH_",id,".csv",sep="")
        cat("\n   --> [CSV]",filecsv)
        write.table(d,filecsv,sep="\t",dec=".",row.names=FALSE)
        cat("   DONE !\n")

    }




    if(output == "wide")  return(dout_wide)

    if(output == "length")   return(d)

}





summary_vigie_chiro <- function(d) {
    library(ggplot2)
    library(lubridate)

    vecSp <- c("Pippip","Plaint","Eptser","Tetvir","Nyclei","Yerray","Phanan","Pleaus","Plaalb","Minsch","Testes","Epheph","Pipkuh","Plasab","Pippyg","Leppun","Rusnit","Sepsep","Myodau","Phofem","Barfis","MyoGT","Mimasp","Phogri","Nycnoc","Phafal","Roeroe","Myonat","Isopyr","Urosp","Ratnor","Barbar","Pipnat","Pleaur","Hypsav","Metbra","Myomys","Lamsp.","Antsp","Eupsp","Cyrscu","Decalb","Plaaff","Rhifer","Tadten","Nyclas","Myoema","Rhasp","Cympud","Tyllil","Plafal","Myobec","Eptnil","Rhihip")
    d <- subset(d,col_sp %in% vecSp)



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

    gg <- ggplot(data=d,aes(temps_enr)) + facet_wrap(col_sp~.,scales="free") + geom_histogram()
    ggsave("output/temps_enr_sp.png",gg)

    gg <- ggplot(data=d,aes(nb_contacts)) + facet_wrap(col_sp~.,scales="free") + geom_histogram()
    ggsave("output/nb_contacts_sp.png",gg)


    d_seq_tps <- aggregate(temps_enr ~col_sample + col_sp,data=d, sum)

}



