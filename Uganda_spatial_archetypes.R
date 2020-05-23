#************************************************************************************************
# Managing spatial data for UGANDA Archetypes project
#************************************************************************************************

# Packages installation and activation
# library(devtools)

library(rgdal)
library(rgeos)
library(dplyr)
library(fmsb)
library(cluster)
# library(stringr)
# library(tidyverse)
library(ggplot2)
# library(RODBC)
library(tidyr)
library(maptools)
library(raster)
library(sp)
# library(ggmap)
# library(maps)
library(maptools)
library(data.table)
# library("cluster")

#--------------------------------------------------------------------------------------
###### Command_shortcuts
# comment: ctrl+shift+c
# clear Console: ctrl+l
# clear Environment: rm(list=ls())
# breackpoint: fn+shift+f9
# Run: alt+cmd+R
# breackpoint step: fn+f10
# traceback()
#--------------------------------------------------------------------------------------

# Clearing workspace
rm(list=ls(all=TRUE)) ## Clear Environment
graphics.off()
# -------------------------------------------------------------------------------------

# Setting working directories
Ug_data = "~/Documents/Uganda/"
Ug_Admin_data = "~/Documents/Uganda/Other_spatial_data/UGA_adm/"
Ug_AidData = "~/Documents/Uganda/AidData/"
plots_dir<-"~/Desktop/"
Ug_spatial_data = "~/Documents/Uganda/Other_spatial_data/"
crs_WGS84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

setwd(Ug_Admin_data)
# Load shp of Admin2 Uganda
admin2_name="UGA_adm2.shp"
UG_admin_2<-readOGR(admin2_name)#, proj4string=CRS(as.character(crs_WGS84)))

#### Global social-ecological data to extract
# load global data for livestock units
setwd(paste0(Ug_spatial_data,"Global_Livestock_units/"))
global_livestock <- raster("AF_Cattle1km_AD_2010_v2_1.tif")
# load global data for land degradation
setwd(paste0(Ug_spatial_data,"Global_Land_degradation/"))
global_land_degradation <- raster("Fig 6.img")
# load global data for farm size
setwd(paste0(Ug_spatial_datay,"field_size_10_40_cropland/"))
global_farm_size <- raster("field_size_10_40_cropland.img")
# Rural poverty
setwd(paste0(Ug_spatial_data,"Ug_Rural-Poverty2005/"))
Rural_poverty<-readOGR("Ug_Rural-Poverty2005.shp")#, proj4string=CRS(as.character(crs_WGS84)))
r2 <- raster(ncol=4320, nrow=2160) # 0.00833333
projection(r2) <- CRS(crs_WGS84)
Rural_poverty_raster <- rasterize(Rural_poverty, r2, 'pov_dens') #poverty density (the number of poor persons per square kilometer)
# plot(Rural_poverty_raster)
# Urban indicator
# Urban_raster <- rasterize(Rural_poverty, r2, 'Urban')
# Education and gender disparity (2014)
setwd(Ug_spatial_data)
male_education <- raster("IHME_AFRICA_EDU_2000_2015_YEARS_MALE_15_49_MEAN_2014_Y2018M02D28.TIF")
female_education <- raster("IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN_2014_Y2018M02D28.TIF")
mean_education <- (male_education+female_education)/2
gendergap_education <- (male_education-female_education)/2
projection(mean_education) <- CRS(crs_WGS84)
projection(gendergap_education) <- CRS(crs_WGS84)
# setwd()
# writeRaster(mean_education, filename="mean_education.asc", format="ascii")
# writeRaster(gendergap_education, filename="gendergap_education.asc", format="ascii")

# Upload other social-ecological data at admin level 2
setwd(Ug_AidData)
allData_Uganda <- "Uganda_Indicators_Adm2_2.csv"
Aid_rowdata <- read.csv(allData_Uganda, header = T, sep = ";", row.names=NULL)

# CICLO FOR SUI DISTRETTI PER ESTRARRE VALORI DI LIVESTOCK E LAND DEGRADATION
# (ANCHE FARM E ALTRI?)
Aid_rowdata$Lstk <- NA
Aid_rowdata$Degr <- NA
Aid_rowdata$Educ <- NA
Aid_rowdata$Gend <- NA
# Aid_rowdata$Farm <- NA
Aid_rowdata$Pove <- NA
# Aid_rowdata$Urba <- NA
for (d in 1:nrow(Aid_rowdata)) {
  ID_district <- Aid_rowdata$ID_2[d]

  selected_district<-UG_admin_2[UG_admin_2$ID_2 == ID_district,]
  Aid_rowdata$Pove[UG_admin_2$ID_2 == ID_district]<-raster::extract(Rural_poverty_raster,selected_district,fun=mean,na.rm=TRUE)
  # Aid_rowdata$Urba[UG_admin_2$ID_2 == ID_district]<-raster::extract(Urban_raster,selected_district,fun=mean,na.rm=TRUE)
  # Aid_rowdata$Farm[UG_admin_2$ID_2 == ID_district]<-raster::extract(global_farm_size,selected_district,fun=mean,na.rm=TRUE)
  Aid_rowdata$Lstk[UG_admin_2$ID_2 == ID_district]<-raster::extract(global_livestock,selected_district,fun=mean,na.rm=TRUE)
  Aid_rowdata$Degr[UG_admin_2$ID_2 == ID_district]<-raster::extract(global_land_degradation,selected_district,fun=mean,na.rm=TRUE)
  Aid_rowdata$Educ[UG_admin_2$ID_2 == ID_district]<-raster::extract(mean_education,selected_district,fun=mean,na.rm=TRUE)
  Aid_rowdata$Gend[UG_admin_2$ID_2 == ID_district]<-raster::extract(gendergap_education,selected_district,fun=mean,na.rm=TRUE)
  }
# To include: FARM SIZE, LAND TENURE, EXTENSION SERVICES, FARMERS ASSOCIATIONS
Aid_rowdata1 <- Aid_rowdata[,c("ID_2","NAME_2","ENGTYPE_2","Shape_Area",
                               "Prec","Elev","Temp",#"Degr"
                               "Popd","Popc","Labo","GDPt","Pove","Mark",#"Road",#,"growth_rate",
                               "Gend","Educ","Lstk","Size","Cred","Exte","Orga")]
Aid_rowdata1$Gend[Aid_rowdata1$ID_2 == 97] <- 0.6446738
Aid_rowdata1$Educ[Aid_rowdata1$ID_2 == 97] <- 6.763845
# names(Aid_rowdata1)[names(Aid_rowdata1) == "Farm_Org"] <- "Orga"
# Claculate indicators of GDP, livestock and farmers' organization (Orga) relative to population
Aid_rowdata1$Lstp <- Aid_rowdata1$Lstk/Aid_rowdata1$Popc*1000
Aid_rowdata1$GDPp <- Aid_rowdata1$GDPt/Aid_rowdata1$Popc*1000
Aid_rowdata1$Orgp <- Aid_rowdata1$Orga/Aid_rowdata1$Popc*1000
# Add the column aids
Aids <- rowSums(Aid_rowdata[,c("worldbank","globalenvironmentfacility","ugandaaims")])
Socio_eco_data <- cbind(Aid_rowdata1,Aids)

setwd(Ug_data)
# write.csv(Socio_eco_data,'Socioeco_admin2_data_Uganda_2.csv')
# save(Socio_eco_data,file="Socioeco_admin2_data_Uganda_2.Rdata")
load("Socioeco_admin2_data_Uganda_2.Rdata")

# Socio_eco_data <- Aid_rowdata1[,1:15]
Socio_eco_data1 <- Socio_eco_data[complete.cases(Socio_eco_data), ]
# Exclude large urban areas
Socio_eco_data2 <- subset(Socio_eco_data1, Socio_eco_data1$ENGTYPE_2=="County")
# Exclude small sub-counties
Socio_eco_data3 <- subset(Socio_eco_data2, Socio_eco_data2$Shape_Area>0.02)
# Exclude Jinja sub-county because too urban (pop density and GDP)
Socio_eco_data4 <- subset(Socio_eco_data3, Socio_eco_data3$NAME_2!="Jinja")
# Select only pro capite data for clustering
dfco <- Socio_eco_data4[,c("Prec","Elev","Temp",#"Degr"
                           "Cred","Labo","Pove","Mark",#"Aids",#"Road",#,"growth_rate",
                           "Gend","Educ","Lstp","Size","Exte","Orgp")]
# dfco <- subset(Socio_eco_data2, select = -c(Shape_Area,NAME_2,ID_2,ENGTYPE_2))
#convert the data frame to numbers
dfco2 <- data.frame(sapply(dfco, function(x) as.numeric(as.character(x))))
df3 <- scale(dfco2, center = TRUE, scale = TRUE) #generate z-scores
# corr <- cor(dfco2, method = "pearson")
# pairs(dfco2)

# library(NbClust)
# nb <- NbClust(df3, distance = "euclidean", min.nc = 5,
# max.nc = 12, method = "average", index ="all")
# library(factoextra)
# fviz_nbclust(nb) + theme_minimal()
####  Hierarchical euclidian clustering ##### 
# library(factoextra)
d.euc <- dist(df3) #euclidean "manhattan", "canberra", "binary" or "minkowski"
hc_eucli <- hclust((d.euc)^2, method = "ward.D")
# fviz_dend(hc_eucli, k=7, k_colors = "jco",
#           as.ggplot = TRUE, show_labels = FALSE)
#### Kmeans
# km.res <- kmeans(df3, 10, nstart = 100)
# fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             # palette = "jco", ggtheme = theme_minimal())

# plot(hc_eucli, labels = NULL, hang = -1, cex = 0.6)

clust_nr <- cutree(hc_eucli, k=7)
df_barr_clust <- cbind(dfco, Clust = clust_nr)
df3_3 <- cbind(data.frame(df3), Clust = clust_nr)
df_all_clust <- cbind(Socio_eco_data4, Clust = clust_nr)

########### ANOVA test########### ########### ########### ########### ########### 
# df_Anova <- df_barr_clust[,c("Cred","Clust")]
# # Compute the analysis of variance
# res.aov <- aov(Cred ~ as.factor(Clust), data = df_Anova)
# # Summary of the analysis
# summary(res.aov)
# TukeyHSD(res.aov)
# plot(TukeyHSD(res.aov), las=1)
########### ########### ########### ########### ########### ########### ########### 
# Plotting the indicators
df_all_clust_plot <- df_all_clust[,c("ID_2","Prec","Elev","Temp",#"Degr"
                                        "Cred","Labo","Pove","Mark",#"Aids",#"Road",#,"growth_rate",
                                        "Gend","Educ","Lstp","Size","Exte","Orgp")]
# # Add value on covered disctricts for figure 1 (22,23,26,36,37,38,56,57,69,90,91)
# df_all_clust_plot$cases <- 0
# df_all_clust_plot$cases[df_all_clust_plot$ID_2==69] <- 1
# GEO_Clusters_Uganda_plot <- merge(UG_admin_2, df_all_clust_plot, by="ID_2")
# # writeOGR(GEO_Clusters_Uganda_plot, ".", "Uganda_7clust_fin", driver="ESRI Shapefile")
# 
# # Create raster of indicators for plotting
# r_model <- raster(ncol=4320, nrow=2160) # 0.00833333
# projection(r_model) <- CRS(crs_WGS84)
name_ind <- c("Prec","Elev","Temp",
              "Cred","Labo","Pove","Mark",
              "Gend","Educ","Lstp","Size","Exte","Orgp")
# Ras_plot_stack <- stack()
# extent_Uganda <- extent(GEO_Clusters_Uganda_plot[1])
# for (c in 1:length(name_ind)) {
#   ind <- 11+c
#   shp_ind <- GEO_Clusters_Uganda_plot[ind]
#   raster_ind <- rasterize(shp_ind, r_model, name_ind[c])
#   raster_ind_crop <- crop(raster_ind,extent_Uganda)
# 
#   Ras_plot_stack <- stack(Ras_plot_stack, raster_ind_crop)
#   rm(raster_ind,shp_ind,raster_ind_crop)
# }

# plot(Ras_plot_stack[[13]])
# 
# library(tmap)
# tm_shape(Ras_plot_stack) +
#   tm_raster() +
#   tm_legend(legend.position = c("right", "bottom"))
# # tm_basemap("Stamen.Watercolor")


# ###### Compute descriptive statistics for each group ######
# # calc MIN and MAX, df_barr_clust has real values and df3_3 has normalized values
dt <- data.table(df3_3)
# dt_GROUP <- dt[,list(GROUP_min=min(GROUP),GROUP_max=max(GROUP)),by=Clust]
dt_Prec <- dt[,list(Prec_min=min(Prec),Prec_max=max(Prec)),by=Clust]
dt_Temp <- dt[,list(Temp_min=min(Temp),Temp_max=max(Temp)),by=Clust]
dt_Elev <- dt[,list(Elev_min=min(Elev),Elev_max=max(Elev)),by=Clust]
dt_Size <- dt[,list(Size_min=min(Size),Farm_max=max(Size)),by=Clust]
dt_Cred <- dt[,list(Cred_min=min(Cred),Cred_max=max(Cred)),by=Clust]
dt_Exte <- dt[,list(Exte_min=min(Exte),Exte_max=max(Exte)),by=Clust]
# dt_Farm <- dt[,list(Farm_min=min(Farm),Farm_max=max(Farm)),by=Clust]
# dt_Popd <- dt[,list(Popd_min=min(Popd),Popd_max=max(Popd)),by=Clust]
dt_Labo <- dt[,list(Labo_min=min(Labo),Labo_max=max(Labo)),by=Clust]
# dt_Aids <- dt[,list(Aids_min=min(Aids),Aids_max=max(Aids)),by=Clust]
dt_Lstk <- dt[,list(Lstk_min=min(Lstp),Lstk_max=max(Lstp)),by=Clust]
dt_Educ <- dt[,list(Educ_min=min(Educ),Educ_max=max(Educ)),by=Clust]
dt_Gend <- dt[,list(Gend_min=min(Gend),Gend_max=max(Gend)),by=Clust]
dt_Mark <- dt[,list(Mark_min=min(Mark),Mark_max=max(Mark)),by=Clust]
dt_Pove <- dt[,list(Pove_min=min(Pove),Pove_max=max(Pove)),by=Clust]
dt_Orgp <- dt[,list(Orgp_min=min(Orgp),Orgp_max=max(Orgp)),by=Clust]
# dt_Degr <- dt[,list(Degr_min=min(Degr),Degr_max=max(Degr)),by=Clust]
# dt_Slop <- dt[,list(Slop_min=min(Slop),Slop_max=max(Slop)),by=Clust]
# dt_Soil <- dt[,list(Soil_min=min(Soil),Soil_max=max(Soil)),by=Clust]
# dt_Land <- dt[,list(Land_min=min(Land),Land_max=max(Land)),by=Clust]

dt_Prec_m <- dt[,list(Prec_m=mean(Prec),Prec_sd=sd(Prec),Prec_median=median(Prec)),by=Clust]
dt_Temp_m <- dt[,list(Temp_m=mean(Temp),Temp_sd=sd(Temp),Temp_median=median(Temp)),by=Clust]
dt_Elev_m <- dt[,list(Elev_m=mean(Elev),Elev_sd=sd(Elev),Elev_median=median(Elev)),by=Clust]
# dt_Popd_m <- dt[,list(Popd_m=mean(Popd),Popd_sd=sd(Popd),Popd_median=median(Popd)),by=Clust]
dt_Labo_m <- dt[,list(Labo_m=mean(Labo),Labo_sd=sd(Labo),Labo_median=median(Labo)),by=Clust]
# dt_Aids_m <- dt[,list(Aids_m=mean(Aids),Aids_sd=sd(Aids),Aids_median=median(Aids)),by=Clust]
dt_Lstk_m <- dt[,list(Lstk_m=mean(Lstp),Lstk_sd=sd(Lstp),Lstk_median=median(Lstp)),by=Clust]
dt_Educ_m <- dt[,list(Educ_m=mean(Educ),Educ_sd=sd(Educ),Educ_median=median(Educ)),by=Clust]
dt_Gend_m <- dt[,list(Gend_m=mean(Gend),Gend_sd=sd(Gend),Gend_median=median(Gend)),by=Clust]
dt_Mark_m <- dt[,list(Mark_m=mean(Mark),Mark_sd=sd(Mark),Mark_median=median(Mark)),by=Clust]
dt_Pove_m <- dt[,list(Pove_m=mean(Pove),Pove_sd=sd(Pove),Pove_median=median(Pove)),by=Clust]
dt_Orgp_m <- dt[,list(Orgp_m=mean(Orgp),Orgp_sd=sd(Orgp),Orgp_median=median(Orgp)),by=Clust]
dt_Size_m <- dt[,list(Size_m=mean(Size),Size_sd=sd(Size),Size_median=median(Size)),by=Clust]
dt_Cred_m <- dt[,list(Cred_m=mean(Cred),Cred_sd=sd(Cred),Cred_median=median(Cred)),by=Clust]
dt_Exte_m <- dt[,list(Exte_m=mean(Exte),Exte_sd=sd(Exte),Exte_median=median(Exte)),by=Clust]
# dt_Slop_m <- dt[,list(Slop_m=mean(Slop),Slop_sd=sd(Slop),Slop_median=median(Slop)),by=Clust]
# dt_Soil_m <- dt[,list(Soil_m=mean(Soil),Soil_sd=sd(Soil),Soil_median=median(Soil)),by=Clust]
# dt_Gove_m <- dt[,list(Gove_m=mean(Gove),Gove_sd=sd(Gove),Gove_median=median(Gove)),by=Clust]
# dt_Land_m <- dt[,list(Land_m=mean(Land),Land_sd=sd(Land),Land_median=median(Land)),by=Clust]

# Make table with descriptive statistics
table_DS <- cbind(dt_Prec,dt_Prec_m,dt_Temp,dt_Temp_m,dt_Elev,dt_Elev_m,
                  dt_Labo,dt_Labo_m,#dt_Aids,dt_Aids_m,
                  dt_Lstk,dt_Lstk_m,dt_Educ,dt_Educ_m,dt_Gend,dt_Gend_m,
                  dt_Mark,dt_Mark_m,dt_Pove,dt_Pove_m,dt_Orgp,dt_Orgp_m,
                  dt_Size,dt_Size_m,dt_Cred,dt_Cred_m,dt_Exte,dt_Exte_m)
# table_DS <- table_DS[,-("Clust_review")]
# write.csv(table_DS, file="Uganda_Cluster_ranges_5.csv")
# table_paper <- data.table(subset(dfcq, select=c(code,Country,GROUP,SUBGROUP)))
# write.csv(table_paper, file="table_paper.csv")

a <- cbind(dt_Prec_m,dt_Temp_m,dt_Elev_m,
           dt_Labo_m,#dt_Aids_m,
           dt_Lstk_m,dt_Educ_m,dt_Gend_m,
           dt_Mark_m,dt_Size_m,dt_Orgp_m,
           dt_Pove_m,dt_Cred_m,dt_Exte_m)
a <- a[,-("Clust")]
a <- a[,-c(3,6,9,12,15,18,21,24,27,30,33,36,39)]
a$Clust <- c(1,2,3,4,5,6,7)

df3_mean <- tidyr::gather(a, parameter, mean, 1,3,5,7,9,11,13,15,17,19,21,23,25)
df3_sd <- tidyr::gather(a, parameter, sd, 2,4,6,8,10,12,14,16,18,20,22,24,26)
df3_fin <- data.frame(Clust=df3_mean$Clust, Par=df3_mean$parameter, Mean=df3_mean$mean, SD=df3_sd$sd)

# df3_fin$Order <- as.numeric(as.character(df3_fin$Order))
df3_fin$Parameter[df3_fin$Par=="Prec_m"] <- "0_Prec_m"
df3_fin$Parameter[df3_fin$Par=="Temp_m"] <- "1_Temp_m"
df3_fin$Parameter[df3_fin$Par=="Elev_m"] <- "11_Elev_m"
df3_fin$Parameter[df3_fin$Par=="Cred_m"] <- "12_Cred_m"
df3_fin$Parameter[df3_fin$Par=="Labo_m"] <- "2_Labo_m"
df3_fin$Parameter[df3_fin$Par=="Pove_m"] <- "3_Pove_m"
# df3_fin$Parameter[df3_fin$Par=="Aids_m"] <- "4_Aids_m"
df3_fin$Parameter[df3_fin$Par=="Lstk_m"] <- "5_Lstk_m"
df3_fin$Parameter[df3_fin$Par=="Educ_m"] <- "6_Educ_m"
df3_fin$Parameter[df3_fin$Par=="Gend_m"] <- "7_Gend_m"
df3_fin$Parameter[df3_fin$Par=="Mark_m"] <- "8_Mark_m"
df3_fin$Parameter[df3_fin$Par=="Size_m"] <- "81_Size_m"
df3_fin$Parameter[df3_fin$Par=="Orgp_m"] <- "9_Orgp_m"
df3_fin$Parameter[df3_fin$Par=="Exte_m"] <- "91_Exte_m"

df3_fin <- df3_fin[order(df3_fin$Clust),]

#------- BARPLOTS ------------------------------------------------------
library(ggpubr) # gplots::col2hex("darkolivegreen3")
colors_clust <- c("#ce4b4d","#ffb400","#ffff01","#a6cee3","#995f3d","#b2df8a","#cc66cc")#,"#fdbf6f","#ffe6d2","#144bd5")

label_par <- c("Precipitation","Temperature","Elevation/Slope",
               "Access Credit","Houshold size",
               "Rural poverty","Livestock",#"Subsidies",
               "Education","Gender gap","Remoteness",
               "Farm size", "Farmers Organizations", "Extension services")
pd <- position_dodge(width = 0.8)

# pdf("Barplot_clusters.pdf")
p1<-ggplot(df3_fin, aes(x=Parameter, fill=as.factor(Clust), y=Mean, label=Mean))
p1 + theme(axis.text.x = element_text(face="bold", size=9),legend.title = element_blank()) +
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=label_par)+
  geom_col(stat='identity', aes(fill=as.factor(Clust)), position=pd, width=.8)  +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.4,
                position=pd) +
  theme(axis.text.y = element_text(colour="black",size=11,hjust=.5,vjust=.5,face="plain")) +
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(.1,"point"))+
  # guides(colour=guide_legend(nrow=8))+
  # scale_fill_manual(breaks=levels(df3_fin_A$Par[1:10]), values=colors)+
  scale_fill_manual(values=colors_clust)+ #, labels=label_SWHR
  # ylab(label_par) +
  # scale_fill_hue(limits = label_names)
  # scale_fill_manual(name="Parameters",
  # labels = df3_fin_A$Par[1:10],
  # values = colors) +
  theme(axis.line = element_line(colour = "black")) +
  coord_flip() +
  facet_wrap(~Clust, ncol=3) +
  # scale_y_continuous(limits = c(-2.5,6))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) #+
# ggsave('~/saved_image.png', width = 16, height = 9, dpi = 300)
# dev.off()
#---------------------------------------------------------------------------------------------------------------
##### BOXPLOT--------------------------------
df3_mean <- tidyr::gather(a, parameter, mean, 1,3,5,7,9,11,13,15,17,19,21,23,25)
df3_sd <- tidyr::gather(a, parameter, sd, 2,4,6,8,10,12,14,16,18,20,22,24,26)
df3_fin <- data.frame(Clust=df3_mean$Clust, Par=df3_mean$parameter, Mean=df3_mean$mean, SD=df3_sd$sd)

df3_box <- tidyr::gather(df3_3, key="Parameter", value="Value", 1:13)
df3_box$Parameter[df3_box$Par=="Prec"] <- "0_Prec"
df3_box$Parameter[df3_box$Par=="Temp"] <- "1_Temp"
df3_box$Parameter[df3_box$Par=="Elev"] <- "11_Elev"
df3_box$Parameter[df3_box$Par=="Cred"] <- "12_Cred"
df3_box$Parameter[df3_box$Par=="Labo"] <- "2_Labo"
df3_box$Parameter[df3_box$Par=="Pove"] <- "3_Pove"
df3_box$Parameter[df3_box$Par=="Lstp"] <- "5_Lstk"
df3_box$Parameter[df3_box$Par=="Educ"] <- "6_Educ"
df3_box$Parameter[df3_box$Par=="Gend"] <- "7_Gend"
df3_box$Parameter[df3_box$Par=="Mark"] <- "8_Mark"
df3_box$Parameter[df3_box$Par=="Size"] <- "81_Size"
df3_box$Parameter[df3_box$Par=="Orgp"] <- "9_Orgp"
df3_box$Parameter[df3_box$Par=="Exte"] <- "91_Exte"

df3_box <- df3_box[order(df3_box$Clust),]

p2 <- ggplot(df3_box, aes(x=Parameter, y=Value, fill=as.factor(Clust)))
p2 + geom_boxplot()+
  geom_hline(yintercept=00, color = "red")+
  theme(axis.text.x = element_text(face="bold", size=9),legend.title = element_blank()) +
  theme(legend.title = element_blank())+
  scale_x_discrete(labels=label_par)+
  geom_col(stat='identity', aes(fill=as.factor(Clust)), position=pd, width=0)  +
  theme(axis.text.y = element_text(colour="black",size=9,vjust=.5,face="plain")) +
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(.1,"point"))+
  scale_fill_manual(values=colors_clust)+
  theme(axis.line = element_line(colour = "black")) +
  coord_flip() +
  facet_wrap(~Clust, ncol=3) +
  # scale_y_continuous(limits = c(-2.5,6))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) #+
# ggsave('~/saved_image.png', width = 16, height = 9, dpi = 300)
# dev.off()


#---------------------------------------------------------------------------------------------------------------
## Test correlation between farmers perceptions (interviews) and macro social-ecological data
## of land fragmentation, gender, credit, povery and extension coverage.
setwd(Ug_data)
barriers_fieldwork_data <- read.csv("Barriers_quantitative_data.csv", header = T, sep = ";", row.names=NULL)
barr_data <- barriers_fieldwork_data[1:81,]

corr_data <- barr_data[,"Inputs"]
corr_data[corr_data==1] <- 10
corr_data[corr_data==2] <- 20
corr_data[corr_data==4] <- 2
corr_data[corr_data==5] <- 1
corr_data[corr_data==20] <- 4
corr_data[corr_data==10] <- 5

shape=GEO_Clusters_Uganda["ID_2"]

datapol <- data.frame(shape)
pointtoplot <- data.frame(x=barriers_fieldwork_data[,4], y=barriers_fieldwork_data[,5])
coordinates(pointtoplot) <- ~ x + y 
proj4string(pointtoplot) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#function over from package sp
test <- data.frame(xx=over(shape,pointtoplot)) # Extract geo clusters for case studies
# Plotting the indicators
combine_geoClust <- data.frame(barr_data, GeoClust=test) # bind the geo clusters with original data frame
# select only few columns
Practices_plot_all <- combine_geoClust[,c("SLM_practice_1","SLM_practice_2","SLM_practice_3",
                                      "SLM_practice_4","SLM_practice_5", "Clust")]

practices_plot <- dplyr::filter(Practices_plot_all, SLM_practice_1!="0")
practices_plot_b <-  gather(practices_plot, key="practice", value="practice", 8:25)


# #### Check for correlation
# x and y are numeric vectors
correlation_check <- cor.test(combine[,1], combine[,2], method = c("pearson", "kendall", "spearman"))

# library("ggpubr")
ggscatter(combine, x = "Interview", y = "Cred", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


