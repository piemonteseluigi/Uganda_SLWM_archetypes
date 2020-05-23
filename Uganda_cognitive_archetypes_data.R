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
library(vegan)
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

setwd(Ug_data)
barriers_fieldwork_data <- read.csv("Barriers_quantitative_data.csv", header = T, sep = ";", row.names=NULL)
barr_data <- barriers_fieldwork_data[1:81,]

df_barr <-  gather(barr_data, key="Barrier", value="Priority", 16:33)
df_barr <- arrange(df_barr, Priority)

# df_barr_Farmers <- subset(df_barr, df_barr$Stakeholder=="Farmer")
# df_barr_Region <- subset(df_barr, df_barr$Region=="Centre")

# df_barr_Farmers_1 <- subset(df_barr_Farmers, df_barr_Farmers$Priority<=3)

# BarPlot the barrier per district District Priority Barrier
df_barr$Barrier[df_barr$Barrier=="Land.fragmentation"] <- "01_Land_tenure"
df_barr$Barrier[df_barr$Barrier=="Population.growth"] <- "02_Pop_growth"
df_barr$Barrier[df_barr$Barrier=="awareness"] <- "03_Awareness"
df_barr$Barrier[df_barr$Barrier=="skills"] <- "04_Skills"
df_barr$Barrier[df_barr$Barrier=="interest"] <- "05_Interest"
df_barr$Barrier[df_barr$Barrier=="Resistance"] <- "06_Resistance"
df_barr$Barrier[df_barr$Barrier=="Weather"] <- "07_Weather"
df_barr$Barrier[df_barr$Barrier=="Drought"] <- "08_Drought"
df_barr$Barrier[df_barr$Barrier=="Storm"] <- "081_Storm"
df_barr$Barrier[df_barr$Barrier=="Bush.fire"] <- "082_Bush_fire"
df_barr$Barrier[df_barr$Barrier=="Pest"] <- "09_Pest"
df_barr$Barrier[df_barr$Barrier=="Poverty"] <- "1_Poverty"
df_barr$Barrier[df_barr$Barrier=="Inputs"] <- "2_Inputs"
df_barr$Barrier[df_barr$Barrier=="Extension"] <- "2_Extension"
df_barr$Barrier[df_barr$Barrier=="Laws"] <- "11_Laws"
df_barr$Barrier[df_barr$Barrier=="Conflicts"] <- "3_Conflicts"
df_barr$Barrier[df_barr$Barrier=="Women"] <- "4_Women"

Barriers_labels<-c("Land_tenure","Pop_growth","Awareness","Skills","Interest","Resistance",
"Weather","Drought","Storm","Bush_fire","Pest","Poverty","Inputs",
"Extension","Laws","Conflicts","Women")

# ggplot(df_barr, aes(x=reorder(Barrier, -Priority), y=Priority, fill=Priority)) +
#   geom_bar(width = 0.9, stat="identity") +
#   geom_hline(yintercept = 0,size=0.3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_grid(.~Region)
#   theme(axis.line = element_line(colour = "black")) +
#   # ylim(-100, 100) +
#   # coord_polar() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

# ## radarchart -----------------------------------------------------------------------------------------
# # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
# data_radarchart <- barr_data[,14:29]
# 
# a <- data_radarchart %>%
#   melt() %>%
#   dcast(value ~ variable, fun.aggregate=length)
# b <- a[1:5,2:17]
# data <- rbind(rep(10,16) , rep(0,16) ,b) #max(b, na.rm=T)
# plot_data <- data#[1:3,2:17]
# radarchart(data)

#################################################################################
######### CLUSTERING ############################################################
# Select only the relevant collumns
Main_data <- barr_data[,c("Interview_ID","X","Y","Gender","Stakeholder","District","Region",
                          "Poverty","Land.tenure","Land.fragmentation","Conflicts","Population.growth",
                          "awareness","skills","interest","Resistance","Weather","Laws",
                          "Bush.fire","Inputs","Extension","Drought","Storm","Pest","Women",
                          "Mulching","Trenches","Terraces","Agroforestry","Grass.bands","SWC",
                          "Zero.grazing", "Manure","Intercropping","Check.dam","Water.harvesting")]
# Select only the 16 factors
only_barr_data <- Main_data[,c("Poverty","Land.tenure", "Land.fragmentation","Conflicts","Population.growth",
                               "awareness","skills","interest","Resistance","Weather","Laws",
                               "Bush.fire","Inputs","Extension","Drought","Storm","Pest","Women")]
only_barr_data_priority <- only_barr_data
only_barr_data[only_barr_data==1] <- 10
only_barr_data[only_barr_data==2] <- 20
only_barr_data[only_barr_data==4] <- 2
only_barr_data[only_barr_data==5] <- 1
only_barr_data[only_barr_data==20] <- 4
only_barr_data[only_barr_data==10] <- 5
only_barr_data[is.na(only_barr_data)] <- 0

#-----------------------------------------------------------------------------
####  CLUSTERING #####
# # DISTANCE
library(ecodist)
# Bray-curtis distance
d.bray <- bcdist(only_barr_data, rmzero = FALSE)
# Jaccard distance
# d.jacc <- distance(only_barr_data, method = "jaccard")


# # Mhierarchical clustering
# ward.D
hc_eucli_BC <- hclust((d.bray)^2, method = "ward.D")
# hc_eucli_JA <- hclust((d.jacc)^2, method = "ward.D")

# Dendogram inspection
library(factoextra)
colors_clust <- c("#0015fe","#fb9a99","#ffff01","#00fe33","#12942e")
# fviz_dend(hc_eucli_BC, k=5, k_colors = colors_clust,
#           as.ggplot = TRUE, show_labels = FALSE)

# plot(hc_eucli_BC, labels = Main_data$Gender)
#--------------------------------------------
# # bind the clusters into the database
clust_nr <- cutree(hc_eucli_BC, k=5)
df_barr_clust <- cbind(only_barr_data, Clust = clust_nr)
df_all_clust <- cbind(Main_data, Clust = clust_nr)

# #### bar chart of cluster specific barriers
df_all_clust_b <-  gather(df_all_clust, key="Barrier", value="Priority", 8:25)
df_all_clust_b <- arrange(df_all_clust_b, Priority)
df_all_clust_b$Priority[is.na(df_all_clust_b$Priority)] <- 0

# # # Plotting all barriers
# df_all_clust_bc_n <- df_all_clust_b %>% group_by(Barrier,.drop = FALSE) %>% count(Priority)
# df_all_clust_bc_n$n[df_all_clust_bc_n$Priority==0] <- 0

# # Plotting the barriers per cluster (for spiderplot)
df_all_clust_n <- subset(df_all_clust_b, df_all_clust_b$Clust==5)
df_all_clust_bc_n <- df_all_clust_n %>% group_by(Barrier,.drop = FALSE) %>% count(Priority)
df_all_clust_bc_n$n[df_all_clust_bc_n$Priority==0] <- 0

# #### # #### # #### # #### # #### # #### # #### # #### # #### # #### # #### 
# Spiderplot of the barriers per cluster

# Order the barrier
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Land.tenure"] <- "01_Land_tenure"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Land.fragmentation"] <- "012_Land_fragm"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Population.growth"] <- "02_Pop_growth"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="awareness"] <- "03_Awareness"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="skills"] <- "04_Skills"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="interest"] <- "05_Interest"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Resistance"] <- "06_Resistance"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Weather"] <- "07_Weather"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Drought"] <- "08_Drought"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Storm"] <- "081_Storm"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Bush.fire"] <- "082_Bush_fire"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Pest"] <- "09_Pest"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Poverty"] <- "2_Poverty"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Inputs"] <- "21_Inputs"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Extension"] <- "22_Extension"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Laws"] <- "23_Laws"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Conflicts"] <- "3_Conflicts"
df_all_clust_bc_n$Barrier[df_all_clust_bc_n$Barrier=="Women"] <- "4_Gender"

df_all_clust_bc_n <- df_all_clust_bc_n[order(df_all_clust_bc_n$Barrier),]

Barriers_labels<-c("      Land tenure","    Land fragmentation"," Population growth","Awareness","Skills","Interest","Resistance",
                   "Weather","Drought","Storm","Bush fire","Pest","Poverty","Inputs",
                   "Extension","Laws","Conflicts","Gender")

Colour_priorities <- c("#ffffff","#581845","#900C3F","#C70039","#FF5733","#FFC30F")
# Colour_priorities <- c("#ffffff","#900C3F","#FF5733","#FF5733","#FFC30F","#FFC30F")
# library(ggthemes)
ggplot(df_all_clust_bc_n, aes(x=Barrier, y=n, fill=as.factor(Priority))) +
  geom_bar(stat="identity") +
  # geom_col(position="dodge", width=.8,col=1)+
  scale_fill_manual(values=Colour_priorities)+
  scale_x_discrete(labels=Barriers_labels)+
  # geom_hline(yintercept = 0,size=0.3) +
  # theme(axis.line = element_line(colour = "black")) +
  ylim(0, 20) +
  coord_polar() +
  theme_minimal()+
  # theme_economist()+ 
  # theme_light()+
  # theme_wsj()+
  theme(  axis.text.x = element_text(face="bold", size=0),legend.title = element_blank(),
    axis.line = element_blank(),axis.title = element_blank(),legend.position = "none",
        axis.text.y = element_blank(),axis.ticks.y = element_blank())


# #### merge spatial clustering with case studies -------------------------------
# load spatial cluster shapefile
setwd(plots_dir)
Spatiasl_SOCEC_cluster="Uganda_clust_Adm2_7c_NoAid.shp"
Spatial_clust<-readOGR(Spatiasl_SOCEC_cluster)#, proj4string=CRS(as.character(crs_WGS84)))
shape=Spatial_clust["Clust"]

datapol <- data.frame(shape)
pointtoplot <- data.frame(x=df_all_clust[,2], y=df_all_clust[,3])
coordinates(pointtoplot) <- ~ x + y 
proj4string(pointtoplot) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract GeoClusters from shapefile
test <- data.frame(xx=over(pointtoplot,shape)) #function over from package sp
Interviews_GeoClust_all <- data.frame(df_all_clust, GeoClust=test)
Interviews_GeoClust_all$Clust.1[Interviews_GeoClust_all$Clust.1==6] <- 3 # Only for 7 clusters
Interviews_GeoClust <- dplyr::filter(Interviews_GeoClust_all, !is.na(Clust.1)) # Remove NA
# Calculate sum per Geocluster
dt_int <- data.table(Interviews_GeoClust)
dt_int_Pove <- dt_int[,list(a_Pove_sum=sum(Poverty)),by=Clust.1]
dt_int_Cred <- dt_int[,list(b_Cred_sum=sum(Inputs)),by=Clust.1]
dt_int_Land <- dt_int[,list(c_Land_sum=sum(Land.fragmentation)),by=Clust.1]
dt_int_Exte <- dt_int[,list(z_Exte_sum=sum(Extension)),by=Clust.1]
dt_int_Gend <- dt_int[,list(x_Gend_sum=sum(Women)),by=Clust.1]
# Put the data togheter
table_interviews <- cbind(dt_int_Pove,-dt_int_Cred,dt_int_Land,
                          -dt_int_Exte,dt_int_Gend)
table_interviews <- table_interviews[order(table_interviews$Clust.1),]

table_interviews <- table_interviews[,-("Clust.1")]
# Scale the data by the number of interviews per cluster
table_interviews_s <- table_interviews
nc_1 <- nrow(dplyr::filter(Interviews_GeoClust, Clust.1==1))
nc_2 <- nrow(dplyr::filter(Interviews_GeoClust, Clust.1==2))
nc_3 <- nrow(dplyr::filter(Interviews_GeoClust, Clust.1==3))
nc_4 <- nrow(dplyr::filter(Interviews_GeoClust, Clust.1==4))

table_interviews_s[1,] <- table_interviews[1,]/nc_1
table_interviews_s[2,] <- table_interviews[2,]/nc_2 
table_interviews_s[3,] <- table_interviews[3,]/nc_3 
table_interviews_s[4,] <- table_interviews[4,]/nc_4 

table_interviews_s$GeoClust <- c(1,2,3,4)

# dfco2 is taken from the spatial analysis in "Uganda.data.R script"
dfco_PD <- dfco2[,c("Cred","Pove","Gend","Size","Exte")]
datapol2 <- na.omit(datapol)
SOCEC_GeoClust <- data.frame(dfco_PD, GeoClust=datapol2)
# Calculate mean and standard deviation per Geocluster
dt_Geo <- data.table(SOCEC_GeoClust)
dt_Pove_m <- dt_Geo[,list(a_Pove_m=mean(Pove)),by=Clust] #a_Pove_sd=sd(Pove)),
dt_Cred_m <- dt_Geo[,list(b_Cred_m=mean(Cred)),by=Clust]
dt_Land_m <- dt_Geo[,list(c_Land_m=mean(Size)),by=Clust]
dt_Exte_m <- dt_Geo[,list(z_Exte_m=mean(Exte)),by=Clust]
dt_Gend_m <- dt_Geo[,list(x_Gend_m=mean(Gend)),by=Clust]
# Put the data togheter
table_SOCEC_all <- cbind(dt_Pove_m,dt_Cred_m,-dt_Land_m,
                     dt_Exte_m,dt_Gend_m)
table_SOCEC_1 <- table_SOCEC_all[1:2,]
table_SOCEC_2 <- table_SOCEC_all[6,]
table_SOCEC_3 <- table_SOCEC_all[4,]
table_SOCEC <- rbind(table_SOCEC_1,table_SOCEC_2,table_SOCEC_3)
# Put the 2 tables togheter
table_all <- cbind(table_SOCEC,table_interviews_s)
table_all <- table_all[,-("GeoClust")]
table_all <- table_all[,-("Clust")]
# Rescale
table_all_scaled <- data.frame(scale(table_all, center = TRUE, scale = TRUE)) #generate z-scores
table_all_scaled$Clust <- c(1,2,3,4)
Table_plot <- tidyr::gather(table_all_scaled, parameter, Value, -Clust)
a<- Table_plot[grep("a_", Table_plot$parameter), ]
b<- Table_plot[grep("b_", Table_plot$parameter), ]
c<- Table_plot[grep("c_", Table_plot$parameter), ]
z<- Table_plot[grep("z_", Table_plot$parameter), ]
x<- Table_plot[grep("x_", Table_plot$parameter), ]
a$pair <- 1
b$pair <- 2
c$pair <- 3
z$pair <- 4
x$pair <- 5
Table_plot_fin <- rbind(a,b,c,z,x)
Table_plot_fin$Type <- "NA"
perception_type <- Table_plot_fin[grep("_sum", Table_plot_fin$parameter), ]
perception_type$Type <- "Perception"
data_type <- Table_plot_fin[grep("_m", Table_plot_fin$parameter), ]
data_type$Type <- "data"
table_fin_comp <- rbind(data_type,perception_type)
table_fin_comp$pair[table_fin_comp$pair==1] <- "4_Poverty"
table_fin_comp$pair[table_fin_comp$pair==2] <- "3_Credit"
table_fin_comp$pair[table_fin_comp$pair==3] <- "2_Land fragmentation"
table_fin_comp$pair[table_fin_comp$pair==4] <- "5_Extension services"
table_fin_comp$pair[table_fin_comp$pair==5] <- "1_Gender gap"

table_fin_comp <- table_fin_comp[order(table_fin_comp$Clust),]
table_fin_comp <- table_fin_comp[order(table_fin_comp$pair),]


# colors_clust <- c("#D4A338","#843C0C","#548235","#BE4ED5","#5B9BD5")
colors_clust <- c("black", "black")

p2 <- ggplot(table_fin_comp, aes(x=pair, fill=Type, y=Value, alpha=Type))
p2 + theme(axis.text.x = element_text(face="bold", size=9),legend.title = element_blank()) +
  theme(legend.title = element_blank())+
  scale_fill_manual(values=colors_clust)+
  geom_col(position="dodge", width=.8,col=1)+
  theme(axis.text.y = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain")) +
  theme(legend.position = "none")+
  theme(legend.position="bottom")+
  theme(legend.key=element_blank(), legend.key.size=unit(.1,"point"))+
  theme(axis.line = element_line(colour = "black")) +
  coord_flip() +
  facet_wrap(~Clust, ncol=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) #+




# ---------------# ---------------# ---------------# ---------------# ---------------
# Extract case studies in each GeoCluster and plot perception-SOCED data
# ---------------# ---------------# ---------------# ---------------# ---------------
Practices <-  gather(Interviews_GeoClust, key="Practice", value="Count", 26:36)
CL_Practices_NA <- dplyr::filter(Practices, !is.na(Count))
CL_Practices <- CL_Practices_NA %>% group_by(Clust) %>% count(Practice) # Clust for interviews and Clust.1 for regions
# Rename practices for the plot
CL_Practices$Practice[CL_Practices$Practice=="Trenches"] <- "00_Trenches"
CL_Practices$Practice[CL_Practices$Practice=="Terraces"] <- "01_Terraces"
CL_Practices$Practice[CL_Practices$Practice=="Grass.bands"] <- "02_Grass.bands"
CL_Practices$Practice[CL_Practices$Practice=="Check.dam"]<- "03_Check.dam"
CL_Practices$Practice[CL_Practices$Practice=="Water.harvesting"]<- "4_Water.harvesting"
CL_Practices$Practice[CL_Practices$Practice=="Zero.grazing"]<- "05_Zero.grazing"
CL_Practices$Practice[CL_Practices$Practice=="SWC"]<- "06_SWC"
CL_Practices$Practice[CL_Practices$Practice=="Mulching"]<- "07_Mulching"
CL_Practices$Practice[CL_Practices$Practice=="Manure"]<- "08_Manure"
CL_Practices$Practice[CL_Practices$Practice=="Intercropping"]<- "09_Intercropping"
CL_Practices$Practice[CL_Practices$Practice=="Agroforestry"]<- "091_Agroforestry"

CL_Practices<-arrange(CL_Practices, Practice)

colors_practices <- c("#71c7ec","#1ebbd7","#189ad3","#107dac","#005073",
                      "darkgoldenrod1","darkgoldenrod","darkgoldenrod3",
                      "#339966","#227755","#004422")
label_practeces<-c("Trenches","Terraces","Grass bands","Check Dams","Rainwater harvesting",
                   "SWC","Mulching","Manure",
                   "Zero grazing","Intercropping","Agroforestry")

ggplot(CL_Practices, aes(x = Clust,weight=n,fill = Practice)) +
  geom_bar(width = 0.9, position = 'fill') +
  scale_fill_manual(values=colors_practices, labels=label_practeces)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom",legend.spacing.x = unit(0, 'cm'))+
  guides(fill = guide_legend(label.position = "bottom"))


# ---------------# ---------------# ---------------# ---------------# ---------------
# Plotting perception-archetypes per GeoCluster
# ---------------# ---------------# ---------------# ---------------# ---------------
combine_all <- data.frame(SOCEC=test, Interview=df_all_clust[,"Clust"])
combine <- na.omit(combine_all)

# calculating frequency
combine_n <- combine %>% group_by(Clust,.drop = FALSE) %>% count(Interview)
combine_n <- combine_n[order(combine_n$Clust),]

label_par <- c("Poverty-driven","Over-populated","Farmers’ resistance",
               "Rural isolation","Post-conflict")
# Stacked + percent
ggplot(combine_n, aes(x=Interview,y=n,fill=as.factor(Interview))) + 
  geom_bar(position="dodge",stat="identity") + #
  scale_x_discrete(labels=label_par)+
  facet_wrap(~Clust) +
  theme(axis.text.x = element_text(face="bold", size=9),legend.title = element_blank()) +
  theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=colors_clust, labels=label_par) #+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # panel.background = element_blank(), axis.line = element_line(colour = "black"))


# ------------------------------- ------------------------------- -------------------------------
## Create spatial points with the case studies and clusters
Merge_clust <- subset(df_all_clust, select = c(Interview_ID,Clust))
# WH = WH[!is.na(WH$Wate),]
Cases_cluster_spatial <- merge(x = barr_data, y = Merge_clust, by = "Interview_ID", all = TRUE)
case_studies_clusters<-SpatialPointsDataFrame(Cases_cluster_spatial[,4:5], Cases_cluster_spatial, proj4string = CRS(as.character(crs_WGS84)))
setwd(plots_dir)
writeOGR(case_studies_clusters, ".", "Interviews_Uganda_5c_29Jan", driver="ESRI Shapefile")
