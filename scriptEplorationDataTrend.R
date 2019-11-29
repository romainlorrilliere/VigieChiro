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




    if(output == "wide")  return(dout_wide)    if(output == "length")   return(d)

}



