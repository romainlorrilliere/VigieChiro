
source("script_trend.r")


dt50 <- prepa_data(id= "DataRP_SpTron_50",d="data/DataRP_SpTron_50.csv",aggregate_site=FALSE,add_abs=TRUE)
#ddt50 <- dt50 %>% select(c("num_site_txt","Tron","sample_cat","date_format","year","julian","espece","expansion_direct","nb_contacts"))
#ddt50 <- ddt50 %>% arrange(num_site_txt,date_format,espece,expansion_direct)
#historic_tron(ddt50,id="DataRP_SpTron_50")
d50 <- prepa_data(id="DataRP_SpTron_50",d="data/DataRP_SpTron_50.csv",aggregate_site=TRUE,add_abs=TRUE)
#summary_sp(d50,id="DataRP_SpTron_50")



dt90 <- prepa_data(id= "DataRP_SpTron_90",d="data/DataRP_SpTron_90.csv",aggregate_site=FALSE,add_abs=TRUE)




#ddt90 <- dt90 %>% select(c("num_site_txt","Tron","sample_cat","date_format","year","julian","espece","expansion_direct","nb_contacts"))
#ddt90 <- ddt90 %>% arrange(num_site_txt,date_format,espece,expansion_direct)
#historic_tron(ddt90,id="DataRP_SpTron_90")

d90 <- prepa_data(id="DataRP_SpTron_90",d="data/DataRP_SpTron_90.csv",aggregate_site=TRUE,add_abs=TRUE)
 #summary_sp(d90,id="DataRP_SpTron_90")


main.glm(listSp="Hypsav")


main.glm()
