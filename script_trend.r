###############################################
####   Script de prepartation des data et d'analyse des tendances
##############################################

vecPackage=c("reshape2","data.table","lubridate","ggplot2","maps","readr","tidyr","arm","scales","glmmTMB","plyr","beepr")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)


source("../Vigie-Chiro_scripts/functions/GLMs/f_Sp_GLM_short.R")

require(dplyr)
require(tidyr)
require(readr)
require(lubridate)
require(ggplot2)
require(arm)
require(scales)
require(data.table)
require(glmmTMB)
require(plyr)
require(beepr)
require(corrplot)



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


    ##   pbEncoding_columns <- grep("Ã",d)
    ##
    ##   cat("oooo",pbEncoding_columns)
    ##
    ##  pbEncoding_columns <- grep("A",d)
    ##
    ##   cat("iiii",pbEncoding_columns)
    ##
    ##
    ##   if(length(pbEncoding_columns)>0) {
    ##       cat("\nCharacter do not recognised in ",length(pbEncoding_columns)," columns\n",sep="")
    cat("  --> Convert encoding from UTF-8\n")
    cl <- as.vector(lapply(d,class))
    cl <- which(cl=="character")

    for(j in cl) {
        cat(colnames(d)[j],"")
        Encoding(d[,j]) <- "UTF-8"
    }
    ## }


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


my_head <- function(d,nbcol=NULL) {
    d <- as.data.frame(d)
    print(dim(d))
    if(is.null(nbcol)) head(d) else head(d[,1:nbcol])
}



my_summary <- function(d,nbcol=NULL) {
    d <- as.data.frame(d)
    print(dim(d))
    if(is.null(nbcol)) summary(d) else summary(d[,1:nbcol])
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
add_abs <- function(d,col_gr=NULL,col_sp=NULL,col_value=NULL,col_info_gr=NULL,dall=NULL,as.tibble=TRUE,fig=FALSE,rep="output",id=NULL) {
    ##     col_value = c("nb_contacts","temps_enr"); col_sp = "col_sp"; col_gr = c("col_sample","expansion_direct")


    cat("\nAjout des absences\n")
    if(class(d)[1]=="data.frame") d <- as_tibble(d)

    if(!is.null(col_info_gr)){
        dgr <- unique(d %>% select(one_of(c(col_gr,col_info_gr))))
        dgr_seul <- unique(d %>% select(one_of(col_gr)))
        if(nrow(dgr) != nrow(dgr_seul)) {
            nbValuePerParticipation <- dgr %>%
                group_by(participation,expansion_direct,Tron) %>%
                summarise_each(funs(length))
            tnb <- as.data.frame(nbValuePerParticipation[,4:ncol(nbValuePerParticipation)])
            tnb$allGood <- apply(tnb==1,1,all)
            partinb <- as.data.frame(nbValuePerParticipation[,1:3])
            nbValuePerParticipation <- cbind(partinb,tnb)
            nbValuePerParticipation <- nbValuePerParticipation[c(col_gr,"allGood")]

            cat("ERROR ! l'association des colonnes:\n",colnames(dgr),"\nn'est pas unique\n")
            cat("pour ",nrow(subset(nbValuePerParticipation,!allGood))," tronçon/point\n ces troncons sont eclues\n",sep="")


            d <- inner_join(d,nbValuePerParticipation)
            d <- subset(d,allGood)
            d <- d %>% select( -one_of("allGood"))
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
    } else {
        library(tidyr)
        dall <- unite_(dall, "id", colnames(dall),remove=FALSE)

        vec_sp <- unique(pull(d,col_sp))
        dall_sp <-  (expand.grid(dall$id,vec_sp,stringsAsFactors=FALSE))
        colnames(dall_sp) <- c("id",col_sp)
        dall <- full_join(dall,dall_sp)

        dall <- select(dall,-c("id"))

    }


    if(fig) {

        library(ggplot2)
        if(is.null(col_value))
            col_value <- setdiff(colnames(d),c(col_gr,col_sp,col_info_gr))

        form <- as.formula(paste(col_value[1],"~ espece"))
        dsumsp <-  aggregate(form, d, sum)
        colnames(dsumsp)[2] <- c("nb")

        dsumsp$name <- paste(dsumsp$espece," (",dsumsp$nb,")",sep="")
        dgg <- inner_join(d,dsumsp)
        dgg$name <- factor(dgg$name, levels = dsumsp$name[order(dsumsp$nb,decreasing=TRUE)])

        for(val in col_value) {
            dgg$value <- pull(dgg,val)
            print(summary(dgg$value))
            titre <- paste("Distribution par espèce de ",val," (",id,")",sep="")
            gg <- ggplot(data=dgg,aes(value,group=expansion_direct,fill=expansion_direct))+facet_wrap(.~name,scale="free_y")
            gg <- gg + geom_histogram(position="dodge")#position="identity")
                                        #gg <- gg + scale_y_log10()
            gg <- gg + labs(title=titre,x=val,colour="")
            ggfile <- paste(ifelse(is.null(rep),"",paste(rep,"/",sep="")),"distribution_sp_",val,"_",id,".png",sep="")
            cat("\n  -> [PNG]",ggfile,"\n")

            if(length(unique(pull(d,col_sp)))> 16)
                ggsave(ggfile,gg,width=18,height=8) else ggsave(ggfile,gg)
        }
    }



    d <- full_join(d,dall)
    if(is.null(col_value)) {
        d[is.na(d)] <- 0
    } else {
        for(j in col_value)
            d[is.na(d[,j]),j] <- 0
    }

    if(fig) {

        library(ggplot2)
        if(is.null(col_value))
            col_value <- setdiff(colnames(d),c(col_gr,col_sp,col_info_gr))

        dgg <- inner_join(d,dsumsp)
        dgg$name <- factor(dgg$name, levels = dsumsp$name[order(dsumsp$nb,decreasing=TRUE)])

        for(val in col_value) {
            dgg$value <- pull(dgg,val)
            print(summary(dgg$value))
            titre <- paste("Distribution par espèce de ",val," avec les absences (",id,")",sep="")
            gg <- ggplot(data=dgg,aes(value,group=expansion_direct,fill=expansion_direct))+facet_wrap(.~name,scale="free_y")
            gg <- gg + geom_histogram(position="dodge")
            ##   gg <- gg + scale_y_log10()
            gg <- gg + labs(title=titre,x=val,colour="")
            ggfile <- paste(ifelse(is.null(rep),"",paste(rep,"/",sep="")),"distribution_sp_",val,"_absence_",id,".png",sep="")
            cat("\n  -> [PNG]",ggfile,"\n")

            if(length(unique(pull(d,col_sp)))> 16)
                ggsave(ggfile,gg,width=18,height=8) else ggsave(ggfile,gg)
        }
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

    ## d = "data/DataRP_SpTron_90.csv" ;   dpart = "data/p_export.csv";    dsite ="data/sites_localites.txt"; dSR="data/SRmed.csv";    id = NULL;seuilProbPip=c(.75,.8);seuilInfDurPipDirect=1.5;  seuilSupDurPipExp=0.5;  seuilSR=tibble(expansion_direct=c("exp","direct","direct"),col_sp=c(NA,NA,"Nycnoc"),seuilSR_inf=c(441000,96000,44100),seuilSR_sup=c(2000000,384000,384000));    add_absence=TRUE;                       first_columns=c("participation","idobservateur","date_format","year","month","julian","ordre_passage","sample_cat","num_site","Tron","expansion_direct","PropPip_good","DurPip_good","SampleRate_good","strict_selection","flexible_selection","espece","nb_contacts","temps_enr","longitude","latitude","num_site_txt");list_sample_cat=c("pedestre","routier"); col_sample="participation";col_sp="espece";col_IndiceDurPip="IndiceDurPip"; col_IndiceProbPip="IndiceProbPip";col_SampleRate="SampleRate"; col_date="date_debut";col_site="site";col_tron="Tron";col_nbcontact="nb_contacts";col_temps="temps_enr";aggregate_site=TRUE;list_sp= c("Pippip","Plaint","Eptser","Tetvir","Nyclei","Yerray","Phanan","Pleaus","Plaalb","Minsch","Testes","Epheph","Pipkuh","Plasab","Pippyg","Leppun","Rusnit","Sepsep","Myodau","Phofem","Barfis","MyoGT","Mimasp","Phogri","Nycnoc","Phafal","Roeroe","Myonat","Isopyr","Urosp","Ratnor","Barbar","Pipnat","Pleaur","Hypsav","Metbra","Myomys","Lamsp.","Antsp","Eupsp","Cyrscu","Decalb","Plaaff","Rhifer","Tadten","Nyclas","Myoema","Rhasp","Cympud","Tyllil","Plafal","Myobec","Eptnil","Rhihip");only_first_columns=FALSE;excluded_columns="commentaire"

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
        ##    d$num_site <- as.numeric(ifelse(d$sample_cat == "routier",
        ##                                  gsub("Vigie-chiro - Routier-","",d$site),
        ##                             ifelse(d$sample_cat=="pedestre",
        ##                                    gsub("Vigiechiro - Pédestre-","",d$site),NA)))
        d$num_site <- as.numeric(ifelse(d$sample_cat == "routier",
                                        substr(d$site,23,nchar(d$site)),
                                 ifelse(d$sample_cat=="pedestre",
                                        substr(d$site,23,nchar(d$site)),NA)))
                                        #    browser()

        d$num_site_txt<- sprintf(paste("%0",max(nchar(as.character(d$num_site))),"d",sep=""), d$num_site)
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


    if(add_absence) {
        cat("\nAjout des abscences au niveaux des tronçons\n")
        cat("-----------------------------------------\n\n")

        colpart <- setdiff(colnames(d),c(col_sample,"expansion_direct",col_sp,col_nbcontact,col_tron))
        dall <- unique(d%>% select(c(col_sample,col_tron,"expansion_direct")))

        d <- add_abs(d,col_gr=c(col_sample,"expansion_direct",col_tron),col_sp=col_sp,col_value=col_nbcontact,col_info_gr=colpart,dall=dall)
        cat("\n ##Dimension de la table: \n")
        print(dim(d))
    }


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

    ## partie de verification sans intéret
    ##    cat("\n   - IndiceProbPipParticipation_med & IndiceProbPipParticipation_max\n")
    ##
    ##    dd <- unique(d%>%select(c("num_site","Tron","date_format","sample_cat","expansion_direct","IndiceProbPip")))
    ##
    ##    dparti <- aggregate(IndiceProbPip ~ num_site + date_format + sample_cat + expansion_direct ,dd,median)
    ##    colnames(dparti)[5] <- "IndiceProbPipParticipation_med"
    ##
    ##    ddparti <- aggregate(IndiceProbPip ~ num_site + date_format+ sample_cat + expansion_direct,dd,max)
    ##    colnames(ddparti)[5] <- "IndiceProbPipParticipation_max"
    ##    dparti <- inner_join(dparti,ddparti)
    ##
    ##
    ##    dd2 <- unique(d%>%select(c("num_site","date_format","sample_cat","expansion_direct","IndiceProbPip")))
    ##    ddparti2 <- aggregate(IndiceProbPip ~ num_site + date_format+ sample_cat + expansion_direct,dd2,length)
    ##    colnames(ddparti2)[5] <- "IndiceProbPipParticipation_nb"
    ##
    ##    ggparti2 <- melt(ddparti2,id.vars=c("num_site","date_format","sample_cat","expansion_direct"))
    ##    gg <- ggplot(data=ggparti2,aes(value)) + geom_histogram() + facet_grid(sample_cat~expansion_direct)
    ##    gg <- gg + labs(x="indiceProbPip",title=paste("indiceProbPip",id))
    ##    ggfile <- paste("output/indiceProbPipParticipation_nbValueDiff_hist_",id,".png",sep="")
    ##    cat("\n  -> [PNG]",ggfile,"\n")
    ##    ggsave(ggfile,gg)
    ##
    ##
    ##    gg <- ggplot(data=dparti,aes(x=IndiceProbPipParticipation_med,y=IndiceProbPipParticipation_max,colour=sample_cat,group=sample_cat))+ facet_grid(sample_cat~expansion_direct)
    ##    gg <- gg + geom_abline(slope=1,intercept=0,size=2,alpha=.3)
    ##    gg <- gg + geom_smooth()
    ##    gg <- gg + geom_point()
    ##    gg <- gg + labs(title=paste("IndicePropPip des partitions",id),colour="")
    ##
    ##    ggfile <- paste("output/indiceProbPipParticipation_plot_med_max_",id,".png",sep="")
    ##    cat("\n  -> [PNG]",ggfile,"\n")
    ##    ggsave(ggfile,gg)
    ##
    ##    ggparti <- melt(dparti,id.vars=c("num_site","date_format","sample_cat","expansion_direct"))
    ##    gg <- ggplot(data=ggparti,aes(value)) + geom_histogram() + facet_grid(variable~expansion_direct)
    ##   ## gg <- gg + geom_vline(data=seuil_ProbPip,aes(xintercept=seuil_ProbPip),colour="red",size=2,alpha = .8)
    ##gg
    ##    gg <- gg + labs(x="indiceProbPip",title=paste("indiceProbPip",id))
    ##    ggfile <- paste("output/indiceProbPipParticipation_hist_",id,".png",sep="")
    ##    cat("\n  -> [PNG]",ggfile,"\n")
    ##    ggsave(ggfile,gg)
    ##
    ##
    ##
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

    filecsv <- paste("data/data_vigieChiro_",id,"_","TronPoint","_",ifelse(is.null(list_sp),"allSp",ifelse(length(list_sp)<6,paste(list_sp,collapse="-"),paste(length(list_sp),"sp",sep=""))),"_",ifelse(add_absence,"withAbs","presence="),".csv",sep="")
    cat("\n   --> [CSV]",filecsv)
    write.table(d,filecsv,sep="\t",dec=".",row.names=FALSE)
    cat("   DONE !\n")




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

        colpart <- c(col_sample,"expansion_direct","strict_selection","flexible_selection",col_sp,setdiff(colnames(d),colnames(dd)))

        ddpart <-  unique(d[,colpart])
        ##ddpart2 <- unique(d[,c("participation","expansion_direct",col_sp,"seuil_ProbPip","PropPip_good","seuilInf_DurPip","DurPip_good","expansion","direct","SampleRate_good")])

        ddpart <- arrange(ddpart)
        ddpart_sp_ed <- unique(d[,c(col_sample,"expansion_direct",col_sp)])

        if(nrow(ddpart_sp_ed) != nrow(ddpart)) {

            nbValuePerParticipation <- ddpart %>%
                group_by(participation,expansion_direct,espece) %>%
                summarise_each(funs(length))
            tnb <- as.data.frame(nbValuePerParticipation[,4:ncol(nbValuePerParticipation)])
            tnb$allGood <- apply(tnb==1,1,all)
            partinb <- as.data.frame(nbValuePerParticipation[,1:3])
            nbValuePerParticipation <- cbind(partinb,tnb)
            nbValuePerParticipation <- nbValuePerParticipation[c(colnames(ddpart_sp_ed),"allGood")]

            cat("ERROR ! l'association des colonnes:\n",paste(colnames(ddpart_sp_ed),collapse=", "),"\nn'est pas unique\n")
            cat("pour ",nrow(subset(nbValuePerParticipation,!allGood))," tronçon/point\n ces troncons sont eclues\n",sep="")


            ddpart <- inner_join(ddpart,nbValuePerParticipation)
            ddpart <- subset(ddpart,allGood)
            ddpart <- ddpart %>% select( -one_of("allGood"))

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



## renvoie la categorie EBCC de la tendance en fonction
## trend l'estimateur de la tendance
## pVal la p value
## ICinf ICsup l intervalle de confiance a 95 pourcent
affectCatEBCC <- function(trend,pVal,ICinf,ICsup){

    catEBCC <- ifelse(pVal>0.05,
               ifelse(ICinf < 0.95 | ICsup > 1.05,"Incertain","Stable"),
               ifelse(trend<1,
               ifelse(ICsup<0.95,"Fort déclin","Déclin modéré"),
               ifelse(ICinf>1.05,"Forte augmentation","Augmentation modée")))
    return(catEBCC)
}




summary_sp <- function(d,id=NULL,save=c("length","wide"),output="") {

    ds <- aggregate(nb_contacts_strict~num_site_txt+ year + expansion_direct + espece,d,max,na.rm=TRUE)
    ds$occ_strict <- ifelse(ds$nb_contacts_strict>0,1,0)

    df <- aggregate(nb_contacts_flexible~num_site_txt+ year + expansion_direct + espece,d,max,na.rm=TRUE)
    df$occ_flexible <- ifelse(df$nb_contacts_flexible>0,1,0)

    d <- full_join(ds,df)


    dagg <- aggregate(cbind(d$occ_strict,d$occ_flexible)~year + expansion_direct + espece, d, sum,na.rm=TRUE)
    colnames(dagg)[4:5] <- c("cc_strict","occ_flexible")


    dtot <-  aggregate(cbind(d$occ_strict,d$occ_flexible)~ espece, d, sum,na.rm=TRUE)
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
        write.table(dout_wide,filecsv,sep="\t",dec=".",row.names=FALSE,quote=TRUE)
        cat("   DONE !\n")

    }


    if("length" %in% c(save)) {

        filecsv <- paste("output/data_abondance_site_year_LENGTH_",id,".csv",sep="")
        cat("\n   --> [CSV]",filecsv)
        write.table(d,filecsv,sep="\t",dec=".",row.names=FALSE,quote=TRUE)
        cat("   DONE !\n")

    }




    if(output == "wide")  return(dout_wide)

    if(output == "length")   return(d)

}



main.glm <- function(id=NULL,
                     donneesAll=list("data/data_vigieChiro_DataRP_SpTron_90_site_54sp_withAbs.csv","data/data_vigieChiro_DataRP_SpTron_50_site_54sp_withAbs.csv"),donneesName=c("90","50"),method="glmmTMB",
                     col_sp="espece",
                     col_date_julien="julian",
                     col_site="site",col_nbcontact="nb_contacts",assessIC= TRUE,listSp=NULL,tabsp="library/SpeciesList.csv",
                     annees=NULL,figure=TRUE,
                     description=c("Abondances brutes","Occurrences","Proportion","Nombre de sites"),
                     tendanceSurFigure=TRUE,tendanceGroupSpe = FALSE,
                     seuilOccu=14,   seuilSignif=0.05,seuilAbond=NA,ecritureStepByStep=TRUE,doBeep=FALSE) {

    ##    id="testGLMTMB";donneesAll=list("data/data_vigieChiro_DataRP_SpTron_90_site_54sp_withAbs.csv","data/data_vigieChiro_DataRP_SpTron_50_site_54sp_withAbs.csv");donneesName=c("90","50");assessIC= TRUE;listSp=NULL;annees=NULL;figure=TRUE;description=c("Abondances brutes","Occurrences","Proportion","Nombre de sites");tendanceSurFigure=TRUE;tendanceGroupSpe = FALSE;seuilOccu=14;seuilAbond=NA;ecritureStepByStep=FALSE;   seuilSignif <- 0.05;tabsp="library/SpeciesList.csv"; col_sp="espece";col_date_julien="julian";col_site="site";col_nbcontact="nb_contacts_strict";col_year="year"
    ##  donneesAll=data;listSp=sp;annees=firstYear:lastYear;figure=TRUE;description=TRUE;tendanceSurFigure=TRUE;tendanceGroupSpe = FALSE;
    ##                   seuilOccu=14;seuilAbond=NA;ecritureStepByStep=TRUE

                                        #donneAll = data;listSp=NULL;annees=NULL;echantillon=1;methodeEchantillon=NULL;
                                        #figure=TRUE;description=TRUE;tendanceSurFigure=TRUE;tendanceGroupSpe = FALSE;
                                        #seuilOccu=14;seuilAbond=NA;ecritureStepByStep=FALSE
    require(arm)
    require(ggplot2)
    require(data.table)

    if(is.null(id))
        id <- Sys.Date()
    nb_data <- length(donneesAll)

    if(length(donneesName) != nb_data) stop("le nombre de nom des donnees 'donneesName' ne correspond pas au nombre de données !!\n")

    donnees <- NULL
    for(i in 1:length(donneesAll)) {

        if(class(donneesAll[[i]])[1] == "character")
            donneesAll[[i]] <- fread(donneesAll[[i]])

        if("data" %in% colnames(donneesAll[[i]])) {
            colnames(donneesAll[[i]])[colnames(donneesAll[[i]])=="data"] <- "data_ex"
        }

        donneesAll[[i]]  <- data.table(data=donneesName[i],donneesAll[[i]] )
        donnees <- rbind(donnees,donneesAll[[i]] )
    }


    if(!(is.null(listSp)))
        donnees <- subset(donnees,espece %in% listSp) else listSp <- unique(donnees$espece)

    dir.create(paste("Output/",id,sep=""),showWarnings=FALSE)
    filesaveAn <-  paste("Output/",id,"/variationsAnnuellesEspece_",id,".csv",sep = "")
    filesaveTrend <-  paste("Output/",id,"/tendanceGlobalEspece_",id,".csv",sep = "")
    filesavedgg <-  paste("Output/",id,"/table_ggplot_",id,".csv",sep = "")

    ##   fileSaveGLMs <-  paste("Output/",id,"/listGLM_",id,sep = "")


    ## tabsp table de reference des especes
    if(class(tabsp) == "character")
        tabsp <- fread(tabsp)
    colnames(tabsp)[colnames(tabsp)=="Esp"] <- "espece"
    colnames(tabsp)[colnames(tabsp)=="NomFR"] <- "nom_espece"
    tabsp <- subset(tabsp,Group == "bat")


                                        #donnees <- filtreAnalyse(donnees,tabsp)


    ##vpan vecteur des panels de la figure
    allowedValue <- c("Occurrences","Nombre de sites","Proportion","Abondances brutes")
    noAllowedDescription <- setdiff(description,allowedValue)
    if(length(noAllowedDescription)>0) {
        cat(" WARNINGS !!! description value not allowed: ",noAllowedDescription,"\n")
        cat(" These values are exclude \n")
        description <- intersect(description,allowedValue)
        cat("   -> ", description,"\n")
    }

    vpan <- c("Variation abondance",description)


    ## description des data

    dysite <- aggregate(nb_contacts_strict~year + num_site_txt + expansion_direct + espece +data ,donnees,max)
    dysite$occ <- ifelse(dysite$nb_contacts_strict > 0, 1,0)
    dy <- aggregate(cbind(dysite$nb_contacts_strict,dysite$occ)~ year +expansion_direct + espece + data,dysite,sum)
    colnames(dy)[5:6] <- c("nb_contacts","occ")


    dsample <- aggregate(nb_contacts_strict~year + expansion_direct + espece + data,unique(donnees[,c("nb_contacts_strict","year","num_site_txt","expansion_direct","espece","data")]),length)
    colnames(dsample)[5] <- "nb_site"
    dy <- inner_join(dy,dsample)
    dy$proportion <- dy$occ / dy$nb_site




    ## Ordre de traitement des especes


    spOrdre <- aggregate(nb_contacts_strict~espece,data=subset(donnees,data=donneesName[1]),sum)
    spOrdre <- inner_join(spOrdre,tabsp)

    spOrdre <- spOrdre[order(spOrdre$nb_contacts_strict,decreasing = TRUE),]


    listSp <- spOrdre$espece[spOrdre$espece %in% listSp]

    nbSp <- length(listSp)
    ## analyse par espece

    ## affichage des especes conservé pour l'analyse
    cat("\n",nbSp," Espèces conservées pour l'analyse\n\n",sep="")
    rownames(tabsp) <- tabsp$espece
    tabCons <-data.table(subset(spOrdre,select=c("espece","nom_espece")))
    print(tabCons)

    cat("\n",sep="")
    cat(paste(listSp,collapse=", "))

    cat("\n\n",sep="")
    flush.console()

    dy <- inner_join(dy,tabCons)



    dy_direct <- subset(dy,expansion_direct == "direct", select= setdiff(colnames(dy),"expansion_direct"))
    colnames(dy_direct)[4:7] <- paste( colnames(dy_direct)[4:7],"direct",sep="_")
    dy_exp <-  subset(dy,expansion_direct == "exp",select= setdiff(colnames(dy),"expansion_direct"))
    colnames(dy_exp)[4:7] <- paste( colnames(dy_exp)[4:7],"exp",sep="_")
    dDescri <- full_join(dy_direct,dy_exp)

    ## initialisation de la liste de sauvegarde
    dgg <- NULL
    i <- 1
    for(j in c(6,7,8,5)) {
        name <- colnames(dy)[j]
        cat(name,"->",allowedValue[i],"\n")
        dgg <- rbind(dgg,data.table(id=id,data=dy$data,espece=dy$espece,nom_espece=dy$nom_espece,year=dy$year,val=dy[,j],LL = NA,UL=NA,catPoint=NA,pval=NA,courbe= dy$expansion_direct,courbe2=paste(name,dy$expansion_direct,sep="_"),panel=allowedValue[i]))
        i <- i+1
    }


    dAn <- NULL
    dTrend <- NULL
    i <- 0

    for (sp in listSp) {
###       if(sp=="PHYCOL")
        i <- i + 1
        ##       sp <- listSp[i]
        ##
        nomSp <- tabCons$nom_espece[i]
        cat("\n========================================\n",sep="")
        cat("(",i,"/",nbSp,") ",sp," | ", nomSp,"\n",sep="")
        cat("========================================\n",sep="")

        flush.console()
        ## d data pour l'espece en court
        for(dn in donneesName) {
            ## dn <- donneesName[1]
            cat("         -  data:",dn," -\n")
            cat("         ================\n")

            d <- subset(donnees,espece==sp & data == dn)

            ## des variable annees
            annee <- sort(unique(donnees$year))
            nbans <- length(annee)
            pasdetemps <- nbans-1
            firstY <- min(annee)
            lastY <- max(annee)



            ## info sp

#### shortlist espece fait partie des especes indiactrice reconnue ?'echelle national
### shortlist <- tabsp[sp,"shortlist"]
            ## indic espèce utilis?our le calcul des indicateurs par groupe de specialisation
            ##indic <- tabsp[sp,"indicateur"]

            cat("\nModèle variation abondance\n--------------------\n")

            ## GLM variation d abondance
            if(method=="GLM") {
                formule <- as.formula("nb_contacts_strict~as.factor(num_site_txt)+I(julian^2) + expansion_direct + nb_Tron_strict +longitude + latitude + sample_cat + as.factor(year)")

                glm1 <- try(glm(formule,data=d,family=quasipoisson),silent=TRUE)
                if(class(glm1)[1] != "try-error") {
                    sglm1 <- summary(glm1)
                    sglm1 <- coefficients(sglm1)
                    sglm1 <- tail(sglm1,pasdetemps)
                    coefan <- as.numeric(as.character(sglm1[,1]))
                    ## coefannee vecteur des variation d'abondance par annee back transformee
                    coefannee <- c(1,exp(coefan))
                    erreuran <- as.numeric(as.character(sglm1[,2]))
                    ## erreur standard back transformee
                    erreurannee1 <- c(0,erreuran*exp(coefan))
                    pval <- c(1,as.numeric(as.character(sglm1[,4])))

                    ## calcul des intervalle de confiance
                    if(assessIC) {
                        library(arm)
                        glm1.sim <- sim(glm1)
                        ic_inf_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.025),pasdetemps)))
                        ic_sup_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.975),pasdetemps)))
                    } else {
                        ic_inf_sim <- NA
                        ic_sup_sim <- NA

                    }


                    ## tab1 table pour la realisation des figures
                    tab1 <- data.table(id = id,data=dn,espece = sp, nom_espece = nomSp,year=annee,val=coefannee,
                                       LL=ic_inf_sim,UL=ic_sup_sim,
                                       catPoint=ifelse(pval<seuilSignif,"significatif",NA),pval,
                                       courbe="abondance",courbe2=vpan[1],
                                       panel=vpan[1])
                    ## netoyage des intervalle de confiance superieur très très grande
                    if(assessIC) {
                        tab1$UL <- ifelse( tab1$val==0,NA,tab1$UL)
                        tab1$UL <-  ifelse(tab1$UL == Inf, NA,tab1$UL)
                        tab1$UL <-  ifelse(tab1$UL > 1.000000e+20, NA,tab1$UL)
                        tab1$UL[1] <- 1
                        tab1$val <-  ifelse(tab1$val > 1.000000e+20,1.000000e+20,tab1$val)
                    }
                    ## indice de surdispersion

                    if(assessIC) dispAn <- glm1$deviance/glm1$null.deviance else dispAn <- glm1$deviance/glm1$nulldev


                    ## tabAn table de sauvegarde des resultats
                    tabAn1 <- data.table(id,data=dn,espece=sp, nom_espece= nomSp ,year = tab1$year,
                                         abondance_relative=round(tab1$val,3),
                                         IC_inferieur = round(tab1$LL,3), IC_superieur = round(tab1$UL,3),
                                         erreur_standard = round(erreurannee1,4),
                                         p_value = round(tab1$pval,3),significatif = !is.na(tab1$catPoint),
                                         dispersion=dispAn)

                    tabAn1 <- inner_join(tabAn1,dDescri)
                } # END if(class(glm1)[1] != "try-error")

            } # END if(method=="GLM")



            if(method == "glmmTMB") {
                repout <- paste("output/",id,"/",sep="")
                md1 <- Sp_GLM_short(dataFile=id,varInterest="nb_contacts_strict",listEffects=c("year","poly(julian,2)","sample_cat","nb_Tron_strict","temps_enr_strict","latitude","longitude","expansion_direct"),interactions=NA,formulaRandom="+(1|site)",selSample=1e10,tagModel=paste0("GLMalphatest_VarAnFY",id,"_",sp),family="nbinom2",asfactor="year",data=d,repout=repout,checkRepout=TRUE,saveFig=TRUE,output=TRUE,doBeep=doBeep)
                smd1 <- md1[[2]]
                smd1 <- smd1[2:nbans,]

                coefan <- smd1[,2]
                ## coefannee vecteur des variation d'abondance par annee back transformee
                coefannee <- c(1,exp(coefan))
                erreuran <- smd1[,3]
                ## erreur standard back transformee
                erreurannee1 <- c(0,erreuran*exp(coefan))
                pval <- c(1,smd1[,5])
                vif1 <- c(0,smd1[,6])

                md1IC <- confint(md1[[1]])
                md1IC <- md1IC[2:nbans,]
                ic_inf_sim <-  c(1,exp(md1IC[,1]))
                ic_sup_sim <-  c(1,exp(md1IC[,2]))

                tab1 <- data.table(id = id,data=dn,espece = sp, nom_espece = nomSp,year=annee,val=coefannee,
                                   LL=ic_inf_sim,UL=ic_sup_sim,
                                   catPoint=ifelse(pval<seuilSignif,"significatif",NA),pval,
                                   courbe="abondance",courbe2=vpan[1],
                                   panel=vpan[1])

                ## netoyage des intervalle de confiance superieur très très grande
                if(assessIC) {
                    tab1$UL <- ifelse( tab1$val==0,NA,tab1$UL)
                    tab1$UL <-  ifelse(tab1$UL == Inf, NA,tab1$UL)
                    tab1$UL <-  ifelse(tab1$UL > 1.000000e+20, NA,tab1$UL)
                    tab1$UL[1] <- 1
                    tab1$val <-  ifelse(tab1$val > 1.000000e+20,1.000000e+20,tab1$val)
                }


                tabAn1 <- data.table(id,data=dn,espece=sp, nom_espece= nomSp ,year = tab1$year,
                                     abondance_relative=round(tab1$val,3),
                                     IC_inferieur = round(tab1$LL,3), IC_superieur = round(tab1$UL,3),
                                     erreur_standard = round(erreurannee1,4),
                                     p_value = round(tab1$pval,3),significatif = !is.na(tab1$catPoint),
                                     vif=vif1)


                tabAn1 <- inner_join(tabAn1,dDescri)

            } #END  if(method == "glmmTMB")

            dAn <- rbind(dAn,tabAn1)
            dgg <- rbind(dgg,tab1)#,tab2)



        cat("\nModèle tendance\n---------------\n")

        if(method == "GLM") {
            ## GLM tendance generale sur la periode
            formule <- as.formula("nb_contacts_strict~as.factor(num_site_txt)+I(julian^2) + expansion_direct + nb_Tron_strict +longitude + latitude + sample_cat + year")


            md2 <- try(glm(formule,data=d,family=quasipoisson),silent=TRUE)

            if(class(md2)[1] != "try-error") {
                smd2 <- summary(md2)
                smd2 <- coefficients(smd2)
                smd2 <- tail(smd2,1)

                ## tendences sur la periode
                coefan <- as.numeric(as.character(smd2[,1]))
                trend <- round(exp(coefan),3)
                ## pourcentage de variation sur la periode
                pourcentage <- round((exp(coefan*pasdetemps)-1)*100,2)
                pval <- as.numeric(as.character(smd2[,4]))

                erreuran <- as.numeric(as.character(smd2[,2]))
                ## erreur standard
                erreurannee2 <- erreuran*exp(coefan)


                ## calcul des intervalle de confiance
                if(assessIC) {
                    md2.sim <- sim(md2)
                    LL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.025),1)),3)
                    UL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.975),1)),3)
                } else {
                    LL <- NA
                    UL <- NA
                }

                ## tab1t table utile pour la realisation des figures
                tab1t <- data.frame(Est=trend,
                                    LL , UL,
                                    pourcent=pourcentage,signif=pval<seuilSignif,pval)


                trendsignif <- tab1t$signif
                pourcent <- round((exp(coefan*pasdetemps)-1)*100,3)
                ## surdispersion

                if(assessIC) dispTrend <- md2$deviance/md2$null.deviance else dispTrend <- md2$deviance/md2$nulldev



                ## classement en categorie incertain

                if(assessIC) {
                    if(dispTrend > 2 |dispAn > 2 |  median(tabAn1$occ_direct,na.rm=TRUE)<seuilOccu | median(tabAn1$occ_exp,na.rm=TRUE)<seuilOccu) catIncert <- "Incertain" else catIncert <-"bon"
                    vecLib <-  NULL
                    if(dispTrend > 2 |dispAn > 2| median(tabAn1$occ_direct,na.rm=TRUE)<seuilOccu | median(tabAn1$occ_exp,na.rm=TRUE)<seuilOccu) {
                        if(median( tabAn1$occ_direct,na.rm=TRUE)<seuilOccu | median(tabAn1$occ_exp,na.rm=TRUE)<seuilOccu) {
                            vecLib <- c(vecLib,"espece trop rare")
                        }
                        if(dispTrend > 2 |dispAn > 2) {
                            vecLib <- c(vecLib,"deviance")
                        }
                    }
                    raisonIncert <-  paste(vecLib,collapse=" et ")
                } else {
                    catIncert <- NA
                    raisonIncert <- NA
                }

                ## affectation des tendence EBCC
                catEBCC <- NA
                if(assessIC)  catEBCC <- affectCatEBCC(trend = tab1t$Est,pVal = tab1t$pval,ICinf=as.vector(tab1t$LL),ICsup=as.vector(tab1t$UL)) else catEBCC <- NA
                ## table complete de resultats

                tabTrend1 <- data.frame(
                    id,data=dn,espece=sp,nom_espece = nomSp ,indicateur = "",
                    nombre_annees = pasdetemps,premiere_annee = firstY,derniere_annee = lastY,
                    tendance = as.vector(tab1t$Est) ,  IC_inferieur=as.vector(LL) , IC_superieur = as.vector(UL),pourcentage_variation=as.vector(pourcent),
                    erreur_standard = as.vector(round(erreurannee2,4)), p_value = round(pval,3),
                    significatif = trendsignif,categorie_tendance_EBCC=catEBCC,
                    mediane_occurrence_direct=median(tabAn1$occ_direct,na.rm=TRUE) ,
                    mediane_occurrence_exp=median(tabAn1$occ_exp,na.rm=TRUE) ,
                    valide = catIncert,raison_incertitude = raisonIncert)

            } # END if(class(md2)[1] != "try-error")
        } # END if(method == "GLM")

        if(method == "glmmTMB") {
            repout <- paste("output/",id,"/",sep="")
            md2 <- Sp_GLM_short(dataFile=id,varInterest="nb_contacts_strict",listEffects=c("year","poly(julian,2)","sample_cat","nb_Tron_strict","temps_enr_strict","latitude","longitude","expansion_direct"),interactions=NA,formulaRandom="+(1|site)",selSample=1e10,tagModel=paste0("GLMalphatest_tendancesFY",id,"_",sp),family="nbinom2",asfactor=NA,data=d,repout=repout,checkRepout=TRUE,saveFig=TRUE,output=TRUE,doBeep=doBeep)
            smd2 <- md2[[2]]
            smd2 <- smd2[2,]


            coefan <- smd2[,2]
            trend <- round(exp(coefan),3)
            ## pourcentage de variation sur la periode
            pourcentage <- round((exp(coefan*pasdetemps)-1)*100,2)
            pval <- smd2[,5]

            erreuran <- smd2[,2]
            ## erreur standard
            erreurannee2 <- erreuran*exp(coefan)

            vif2 <- smd2[,6]


            md2IC <- confint(md2[[1]])
            md2IC <- md2IC[2,]
            ic_inf_sim <-  round(exp(md2IC[1]),3)
            ic_sup_sim <-  round(exp(md2IC[2]),3)


            ## tab1t table utile pour la realisation des figures
            tab1t <- data.frame(Est=trend,
                                LL=ic_inf_sim , UL=ic_sup_sim,
                                pourcent=pourcentage,signif=pval<seuilSignif,pval,vif2)


            trendsignif <- tab1t$signif
            pourcent <- round((exp(coefan*pasdetemps)-1)*100,3)
            ## surdispersion

            ## affectation des tendence EBCC
            catEBCC <- NA
            if(assessIC)  catEBCC <- affectCatEBCC(trend = tab1t$Est,pVal = tab1t$pval,ICinf=as.vector(tab1t$LL),ICsup=as.vector(tab1t$UL)) else catEBCC <- NA
            ## table complete de resultats


            vecLib <-  NULL
            if(vif2 > 2 | any(vif1 > 2) |  median(tabAn1$occ_direct,na.rm=TRUE)<seuilOccu | median(tabAn1$occ_exp,na.rm=TRUE)<seuilOccu) {
                catIncert <- "Incertain"
                if(median(tabAn1$occ_direct,na.rm=TRUE)<seuilOccu | median(tabAn1$occ_exp,na.rm=TRUE)<seuilOccu)
                    vecLib <- c(vecLib,"espece trop rare")

                if(vif2 > 2) vecLib <- c(vecLib,"vif tendance")
                if(any(vif1 > 2)) vecLib <- c(vecLib,"vif variation")

            } else {
                catIncert <-"bon"
            }


            raisonIncert <-  paste(vecLib,collapse=" et ")


            tabTrend1 <- data.frame(
                id,data=dn,espece=sp,nom_espece = nomSp ,indicateur = "",
                nombre_annees = pasdetemps,premiere_annee = firstY,derniere_annee = lastY,
                tendance = as.vector(tab1t$Est) ,  IC_inferieur=ic_inf_sim , IC_superieur = ic_sup_sim ,pourcentage_variation=as.vector(pourcent),
                erreur_standard = as.vector(round(erreurannee2,4)), p_value = round(pval,3),
                significatif = trendsignif,categorie_tendance_EBCC=catEBCC,
                mediane_occurrence_direct=median(tabAn1$occ_direct,na.rm=TRUE) ,
                mediane_occurrence_exp=median(tabAn1$occ_exp,na.rm=TRUE) ,
                valide = catIncert,raison_incertitude = raisonIncert)


        } # END if(method == "glmmTMB")

        dTrend <- rbind(dTrend,tabTrend1)

        } #END for(dn in donneesName)

    ##   if(assessIC)  listGLMsp <- list(list(glm1,glm1.sim,md2,md2.sim)) else  listGLMsp <- list(list(glm1,md2))
    ##   names(listGLMsp)[[1]] <-sp
    ##   fileSaveGLMsp <- paste(fileSaveGLMs,"_",sp,".Rdata",sep="")

    ##   save(listGLMsp,file=fileSaveGLMsp)
    ##   cat("--->",fileSaveGLMsp,"\n")
    ##   flush.console()



    ## les figures
        if(figure) {
            ## table complete pour la figure en panel par ggplot2
            ## table pour graphe en panel par ggplot2
            ## les figures
                                        #if(sp == "Eptser") browser()
            dggSp <- subset(dgg,espece == sp)
            if(!(is.null(dTrend)))
                dTrendSp <- subset(dTrend,espece==sp) else dTrendSp <- data.frame(espece=NULL,panel=NULL)
            theCatIncert <- "incertain"
            if(nrow(dTrendSp)>0)
                theCatIncert <- ifelse(any(dTrendSp$valide == "bon"),"bon","incertain")
            ggplot.espece(dgg=dggSp,dTrend=dTrendSp,id=id,sp=sp,valide=theCatIncert,hline.data="auto",tendanceSurFigure=tendanceSurFigure,seuilOccu=14,vpan = vpan)

        } # END  if(figure)


        if(ecritureStepByStep) {
            write.csv2(dgg,filesavedgg,row.names=FALSE,quote=FALSE)
            cat("--->",filesavedgg,"\n")
            write.csv2(dAn,filesaveAn,row.names=FALSE,quote=FALSE)
            cat("--->",filesaveAn,"\n")
            write.csv2(dTrend,filesaveTrend,row.names=FALSE,quote=FALSE)
            cat("--->",filesaveTrend,"\n")

            flush.console()

        }


    } # END for(sp in listSp)

    write.csv2(dgg,filesavedgg,row.names=FALSE,quote=FALSE)
    cat("--->",filesavedgg,"\n")
    write.csv2(dAn,filesaveAn,row.names=FALSE,quote=FALSE)
    cat("--->",filesaveAn,"\n")
    write.csv2(dTrend,filesaveTrend,row.names=FALSE,quote=FALSE)
    cat("--->",filesaveTrend,"\n")


    flush.console()



}



                                        # la figure realiser par ggplot

ggplot.espece <- function(dgg,dTrend,id,serie=NULL,sp,valide,hline.data="auto",nomSp=NULL,
                          tendanceSurFigure=TRUE,seuilOccu=14, vpan = c("Variation abondance","Abondances brutes","Proportion","Occurrences","Nombre de sites")) {

                                        #      ddgg=dgg;dgg=subset(dgg,espece==sp); ddTrend=dTrend;dTrend=subset(dTrend,espece==sp);hline.data="auto";serie=NULL;nomSp=NULL;description=TRUE;valide=catIncert; tendanceSurFigure=TRUE;seuilOccu=14;vpan = c("Variation abondance","Abondances brutes","Proportion","Occurrences","Nombre de sites")
    require(ggplot2)
    library(scales)
    figname<- paste("Output/",id,"/",ifelse(valide=="Incertain","Incertain/",""),
                    sp,"_",id,serie, ".png",
                    sep = "")
    ## coordonnée des ligne horizontal de seuil pour les abondances et les occurences

    varAbond <- TRUE
    vpanInData <- unique(dgg$panel)
    notAllowedVpan <- setdiff(vpan,vpanInData)
    if("Variation abondance" %in% notAllowedVpan) {
        dggvar <- dgg[1,]
        dggvar$val <- NA
        dggvar$courbe <- "abondance"
        dggvar$courbe2 <- "abondance"
        dggvar$panel <- "Variation abondance"
        dgg <- rbind(dggvar,dgg)
        vpanInData <- unique(dgg$panel)
        notAllowedVpan <- setdiff(vpan,vpanInData)
        varAbond <- FALSE

    }


    if(length(notAllowedVpan)>0) {
        cat("WARNINGS vpan not in panel column:", paste(notAllowedVpan,collapse=", "),"\n")
        cat("vpan item excluded\n")
        vpan <- intersect(vpan,vpanInData)
    }

    description <- length(vpan)>1
    dgg <- subset(dgg,panel %in% vpan)
    dgg$panel <- factor(dgg$panel, levels=vpan)

    if(hline.data == "auto") {

        hline.ref  <- data.table(z = c(1), pattern = "VAR",panel=vpan[grep("VAR",toupper(vpan))],type=c("base"))
        hline.ref <- rbind(hline.ref,data.table(z = c(0,seuilOccu), pattern = rep("OCCU",2),panel=vpan[grep("OCCU",toupper(vpan))],type=c("base","seuil")))
        hline.ref <- rbind(hline.ref,data.table(z = c(0,seuilOccu), pattern = rep("MBRE",2),panel=vpan[grep("MBRE",toupper(vpan))],type=c("base","seuil")))
        ##    hline.ref <- rbind(hline.ref,data.table(z = NA,pattern = "PROP",panel=vpan[grep("PROP",toupper(vpan))]))
        hline.ref$couleur <- hline.ref$panel
        ##    hline.ref$type <- hline.ref$panel



        hline.data <- data.table(panel = vpan)
        hline.data <- inner_join(hline.data,hline.ref)
        hline.data$panel <- factor(hline.data$panel, levels=vpan)


    }

    titre <- paste(dgg$nom_espece[1]," (",dgg$espece[1],")\n",min(dgg$year) ," - ",max(dgg$year),sep="")
    if(nrow(dTrend)>0) titre <- paste(titre,": ",paste(unique(as.character(dTrend$categorie_tendance_EBCC)),collapse=", "),sep="")

    ## texte de la tendance
    tab1 <- subset(dgg,panel =="Variation abondance")
    pasdetemps <- max(dgg$year) - min(dgg$year) + 1
    if(nrow(dTrend)>0) {
        txtPente <- paste(dTrend$tendance,
                          ifelse(dTrend$significatif," *",""),"  [",dTrend$IC_inf," , ",dTrend$IC_sup,"]",
                          ifelse(dTrend$significatif,paste("\n",ifelse(dTrend$pourcentage_variation>0,"+ ","- "),
                                                           abs(dTrend$pourcentage_variation)," % en ",pasdetemps," ans",sep=""),""),sep="")



        ## table du texte de la tendence

        tabTextPent <- data.table(data=dTrend$data,
                                  x=ifelse(dTrend$pourcentage_variation>0,-Inf,Inf),
                                  text_hjust= ifelse(dTrend$pourcentage_variation>0,-0.1,1.1),
                                  txt=txtPente,
                                  courbe=c(vpan[1]),panel=c(vpan[1]))
        tabTextPent$panel <- factor(tabTextPent$panel, levels=vpan)
        if(!tendanceSurFigure) tabTextPent$txt <- ""
    }


    ## les couleurs
    vecColPoint <- c("#ffffff","#eeb40f","#ee0f59")
    names(vecColPoint) <- c("significatif","infSeuil","0")
    vecColCourbe <- c("#3c47e0","#fc8d62","#66c2a5")
    names(vecColCourbe) <- c("abondance","exp","direct")
    vecColHline <- c("#ffffff","#e76060")
    names(vecColHline) <- c("Variation abondance","seuil")

    col <- c(vecColPoint,vecColCourbe,vecColHline)
    names(col) <- c(names(vecColPoint),names(vecColCourbe),names(vecColHline))

    vecLineType <- c("base"="solid","seuil"="dashed")

    ## si description graphique en 3 panels
    if(!description) {
        dgg <- subset(dgg,panel==hline.data$panel[1])
        hline.data <- hline.data[1,]
    }

    p <- ggplot(data = dgg, mapping = aes(x = year, y = val,colour=courbe,fill=courbe))
    p <- p + facet_grid(panel ~ data,scale="free") +
        theme(panel.grid.minor=element_blank(),
              panel.grid.major.y=element_blank())  +
        ylab("") + xlab("Année")+ ggtitle(titre) +
        scale_colour_manual(values=col, name = "" ,
                            breaks = names(col))+
        scale_fill_manual(values=col, name = "" ,
                          breaks = names(col),guide=FALSE)+
        scale_linetype_manual(values=vecLineType, name = "" ,
                              breaks = names(vecLineType),guide=FALSE)+
        scale_alpha_continuous(guide = FALSE) +
        scale_x_continuous(breaks=pretty_breaks())

    p <- p + geom_hline(data =hline.data,mapping = aes(yintercept=z,linetype=type ),colour = "white",alpha=1,size=1)

    if(varAbond)  p <- p + geom_ribbon(aes(ymin=LL,ymax=UL),colour=NA,alpha=.2)
    if(varAbond)  p <- p + geom_pointrange(aes(y=val,ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.5)
    p <- p + geom_line(size = 1.2,alpha=.8)
    p <- p + geom_point(size = 2)
    p <- p + geom_point(aes(colour=catPoint,alpha=ifelse(!is.na(catPoint),1,0)),size = 1.2)


    if(nrow(dTrend) > 0) p <- p + geom_text(data=tabTextPent, mapping=aes(x=x,y=Inf,label=txt,hjust=text_hjust),vjust=1.1,colour="black",parse=FALSE,fontface=2, size=3)

    if(description)  ggsave(figname, p,width=10,height=12)  else ggsave(figname, p,width=10,height=5.5)

}


