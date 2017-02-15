	#--------------------------------------------------------------------------------------------------
	#   
	#		GreenTech Competition: Prepare data
	#
	#					Script made by Yan Holtz (yan1166@hotmail.com / holtz@supagro.inra.fr)
	#
	#---------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------
# 	GOAL
#------------------------------

# This script takes data as provided by the website of the competition.
# It reorganize files to make them readable by R (for the shiny app)

# Please place the database of the competition in a folder named DATA
# And move in this folder:
cd ~/Dropbox/GreenTech_Challenge/DATA
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------








#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------
# 	PREPARATION DES FICHIERS UN PAR UN
#------------------------------


# Station data
#---------------
# --> nothing to do


# Pesticide data
#---------------
# --> nothing to do
 
 
# Shape files (heavy: ~173M)
#---------------
# --> nothing to do

 
# Moyenne Pesticide Historique
#---------------
cd Moyennes_analyses_pesticides\ dans\ eaux\ souterraines_HISTORIQUE/fichiers\ csv/
# A la main, JC change l'encodage du fichier en UTF
# Ensuite On va concaténer les différentes années?
# je le fais en R car problème d'encodage windows / mac
R
data2007=read.table("ma_qp_fm_ttres_pesteso_2007_utf.csv" , header=T , sep=";", quote="")
data2008=read.table("ma_qp_fm_ttres_pesteso_2008_utf.csv" , header=T , sep=";", quote="")
data2009=read.table("ma_qp_fm_ttres_pesteso_2009_utf.csv" , header=T , sep=";", quote="")
data2010=read.table("ma_qp_fm_ttres_pesteso_2010_utf.csv" , header=T , sep=";", quote="")
data2011=read.table("ma_qp_fm_ttres_pesteso_2011_utf.csv" , header=T , sep=";", quote="")
data2012=read.table("ma_qp_fm_ttres_pesteso_2012_utf.csv" , header=T , sep=";", quote="")
data=rbind(data2007, data2008, data2009, data2010, data2011, data2012)
data$ANNEE=c(rep(2007, nrow(data2007)) , rep(2008, nrow(data2008)) , rep(2009, nrow(data2009)) , rep(2010, nrow(data2010)), rep(2011, nrow(data2011)), rep(2012, nrow(data2012)) )
write.table(data, file="ma_qp_fm_ttres_pesteso.csv" , row.names=F , quote=F , sep=";")
# Je zipe tous les fichiers 
gzip ma*


# Moyenne Pesticide 
#---------------
# On va concaténer les données, comme ca, un seul fichier à charger par l'appli=plus rapide
cd Moyenne\ concentrations\ totale\ pesticides\ dans\ eaux\ souterraines/fichiers\ csv/
cat * | head -1 > moy_tot_quantif.csv
cat * | grep -v "^ANNEE" >> moy_tot_quantif.csv
cd ../..

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------














#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------
# 	CREATION ENVIRONNEMENT R
#------------------------------

#Path:
setwd("~/Dropbox/GreenTech_challenge/")

# ========= Moyenne Pesticide Historique
pest_station=read.table("DATA/Moyennes_analyses_pesticides\ dans\ eaux\ souterraines_HISTORIQUE/fichiers\ csv/ma_qp_fm_ttres_pesteso.csv.gz" , header=T , sep=";" , quote="" , dec=",")


# ======== Pesticide data
pesticide=read.table("DATA/Pesticides/pesticides.csv" , header=T , sep=";" , quote="")

# Guigui: On met une nouvelle colonne par fonction de pesticide, avec 1 si oui, 0 si non
pesticide$Acaricide <- 0
pesticide$Biocide <- 0
pesticide$Fongicide <- 0
pesticide$Herbicide <- 0
pesticide$Mollusticide <- 0
pesticide$Nematicide <- 0
pesticide$Insecticide <- 0
pesticide$Reg_croiss <- 0
pesticide$Rodenticide <- 0
pesticide$Repulsif <- 0
pesticide$Graminicide <- 0
pesticide$NO_FUNCTION <- 0

pesticide$Acaricide[grep("A",pesticide$CODE_FONCTION)] <- 1
pesticide$Biocide[grep("B",pesticide$CODE_FONCTION)] <- 1
pesticide$Fongicide[grep("F",pesticide$CODE_FONCTION)] <- 1
pesticide$Herbicide[grep("H",pesticide$CODE_FONCTION)] <- 1
pesticide$Mollusticide[grep("M",pesticide$CODE_FONCTION)] <- 1
pesticide$Nematicide[grep("N",pesticide$CODE_FONCTION)] <- 1
pesticide$Insecticide[grep("I",pesticide$CODE_FONCTION)] <- 1
pesticide$Rodenticide[grep("Ro",pesticide$CODE_FONCTION)] <- 1
pesticide$Reg_croiss[c(grep("Reg",pesticide$CODE_FONCTION),grep("reg",pesticide$CODE_FONCTION))] <- 1
pesticide$Repulsif[grep("Rep()",pesticide$CODE_FONCTION)] <- 1
pesticide$Graminicide[grep("G",pesticide$CODE_FONCTION)] <- 1
pesticide$NO_FUNCTION[grep("PP",pesticide$CODE_FONCTION)] <- 1
pesticide$NO_FUNCTION[which(pesticide$CODE_FONCTION=="")] <- 1





# ========== Station
# Changement accent de l'en tête à la main...
station=read.table("DATA/Stations/stations.csv" , header=T , sep=";" , na.strings="", dec="," , quote="")

# Il faut choper les long et lat a partir de lambert93. (Merci guigui)
library(rgdal)
library(rgeos)
tmp=station
coordinates(tmp) <- ~X_FICT_L93+Y_FICT_L93
proj4string(tmp) <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
tmp=spTransform(tmp , "+proj=longlat +datum=WGS84 +no_defs" ) 
station=cbind(station, coordinates(tmp))
colnames(station)[c(28,29)]=c("LONG","LAT")

# GUIGUI: On garde uniquement les stations qui ont au moins une année avec des relevés: --> Economise pas grand chose malheureusement
station <- station[which(is.element(station$CD_STATION, pest_station$CD_STATION)),]

# GUIGUI: On calcule la dernière concentration max pour chaque station pour afficher les stations avec un gradient de couleur
# Vraiment long --> NEEDS TO GO FASTER
#last_tot_conc <- sapply(unique(pest_station$CD_STATION), function(s){  
#  df <- pest_station[pest_station$CD_STATION==s,]
#  df <- df[df$ANNEE==max(df$ANNEE, na.rm=TRUE),]
#  sum(df$MA_MOY, na.rm=TRUE)  
#})
# On l'ajoute au tableau station
#station$last_tot_conc=last_tot_conc





# ============= Moyenne Pesticide
moy_pest=read.table("DATA/Moyenne\ concentrations\ totale\ pesticides\ dans\ eaux\ souterraines/fichiers\ csv/moy_tot_quantif.csv" , header=T , sep=";" , na.strings="", dec=",")
# ce fichier va pas servir a grand chose en fait.
# Il vaut mieux faire nous meme les aggrégats à partir du fichier détaillé.




# ============= Fichier polygone: masse d'eau + régions + départements
# On utilise la librairie RGDAL pour les lires, on prend les version allégés par GUIGUI
library(rgdal)
ME=readOGR( dsn= "DATA/Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/Polygones_ME_ultralight.shp" , layer="Polygones_ME_ultralight") 
regions=readOGR( dsn= "DATA/Regions/regions-20140306-100m.shp" , layer="regions-20140306-100m")
departements=readOGR( dsn= "DATA/Departements/" , layer="departements-20140306-100m")

# On joint les polygones par ID (problème des nouvelles régions)
regions <- regions[-which(regions@data$nom %in% c("Guadeloupe","Guyane","Martinique","La Réunion","Mayotte")),]
regions@data$nom <- as.character(regions@data$nom)
regions@data$Region <- regions@data$nom
regions@data$Region[regions@data$Region %in% c("Alsace","Champagne-Ardenne","Lorraine")] <- "Alsace-Champagne-Ardenne-Lorraine"
regions@data$Region[regions@data$Region %in% c("Aquitaine","Limousin","Poitou-Charentes")] <- "Aquitaine-Limousin-Poitou-Charentes"
regions@data$Region[regions@data$Region %in% c("Auvergne","Rhône-Alpes")] <- "Auvergne-Rhône-Alpes"
regions@data$Region[regions@data$Region %in% c("Haute-Normandie","Basse-Normandie")] <- "Normandie"
regions@data$Region[regions@data$Region %in% c("Bourgogne","Franche-Comté")] <- "Bourgogne-Franche-Comté"
regions@data$Region[regions@data$Region %in% c("Languedoc-Roussillon","Midi-Pyrénées")] <- "Languedoc-Roussillon-Midi-Pyrénées"
regions@data$Region[regions@data$Region %in% c("Nord-Pas-de-Calais","Picardie")] <- "Nord-Pas-de-Calais-Picardie"
regions@data$Region[regions@data$Region %in% "Île-de-France"] <- "Ile-de-France"
regions@data$Region[regions@data$Region %in% "Pays de la Loire"] <- "Pays-de-la-Loire"
regions <- gUnaryUnion(regions, id=regions@data$Region)
df <- data.frame("Region"=sapply(1:length(regions), function(i)regions@polygons[[i]]@ID))
row.names(df) <- df$Region
region <- SpatialPolygonsDataFrame(regions, data=df)
rm(regions)



# # ============= ajout d'une colonne région au fichier station
dep <- read.table("DATA/Departements/departements.csv", header=T, sep=";")
dep$Numéro=as.character(dep$Numéro)
shp.dep <- readOGR("DATA/Departements/departements-20140306-100m.shp","departements-20140306-100m")
shp.dep@data$code_insee <- as.character(shp.dep@data$code_insee)
shp.dep <- shp.dep[which(nchar(shp.dep@data$code_insee)<3),]
dep$Numéro[which(nchar(dep$Numéro)==1)] <- paste0("0",dep$Numéro[which(nchar(dep$Numéro)==1)])
shp.dep@data <- merge(shp.dep@data, dep[,c("Numéro","Nouvelle")], by.x="code_insee",by.y="Numéro")
shp.dep@data <- shp.dep@data[,c(1,2,5)]
colnames(shp.dep@data) <- c("Numero","Nom","Region")
#writeOGR(shp.dep, "DATA/Departements/departements-20140306-100m.shp","departements-20140306-100m",driver="ESRI Shapefile", overwrite_layer = TRUE)
station=merge(station, dep[,c("Numéro","Nouvelle")] , by.x="NUM_DEP", by.y="Numéro")
colnames(station)[ncol(station)]="Region"


# ========= pest_station: J'ajoute une colone avec les détails des stations
# En effet cette étape prend du temps, et il vaut mieux l'éviter dans l'appli
pest_station=merge(pest_station, station[ , c(1:4, 6,7,28,29,30)], by.x="CD_STATION", by.y="CD_STATION" , all.x=T)

# ========= pest_station: J'ajoute une coloone avec les détails des pesticides: fonction et famille
# En effet cette étape prend du temps, et il vaut mieux l'éviter dans l'appli
pest_station=merge(pest_station, pesticide[ , c(1,4,14:25)], by.x="CD_PARAMETRE", by.y="CD_PARAMETRE" , all.x=T)
pest_station=droplevels(pest_station)


# ========= Calcul moyenne de pesticide par région et ajout au fichier de département
# On réalise l'aggrégat ici et pas dans l'appli car ca permettra de faire la carte d'acceuil de la sheet3 plus rapidement
# Fait gagner ~ 5 secondes!
library(tidyr)
pest_station_filtered=pest_station
resum_dep=aggregate(pest_station_filtered$MA_MOY , by=list(pest_station_filtered$NUM_DEP, pest_station_filtered$ANNEE) , mean , na.rm=T)
resum_dep$Group.2=paste("y", resum_dep$Group.2,sep="")
resum_dep_quick=resum_dep


# ========= Calcul du barplot des pesticide les plus courant
# Calcul: pour chaque pesticide, combien de fois il a était vu au dessus du seuil de 0.1?
c=aggregate(pest_station$MA_MOY , by=list(pest_station$LB_PARAMETRE) , function(x){ length(x[x>0.1])}  )
# horizontal barplot
library(ggplot2)
my_barplot=ggplot(head(c[order(c$x , decreasing=T) , ]) , aes(y=x , x=Group.1, color=Group.1, fill=Group.1)) + geom_bar(stat = "identity", width=0.3) + coord_flip() + xlab("") + ylab("") + theme(legend.pos="none")




# ========= Ajout dangerosité fichier pesticide
# load csv file
alltox <- read.csv('pesticides_toxicity/ld50_values_list_matching.csv',sep=',', header=TRUE, na.strings=c("–", "-",""))
# change data type
alltox$LD50 <- as.numeric(levels(alltox$LD50))[alltox$LD50]
alltox$moleculename_fr <- as.character(alltox$moleculename_fr)
alltox$moleculename_en <- as.character(alltox$moleculename_en)
alltox$productname <- as.character(alltox$productname)
# remove NAs
alltox <- alltox[-which(is.na(alltox$LD50)==TRUE),]
# only pests that matches official pest list
tox <- subset.data.frame(alltox, hasmatch==TRUE)
# all those not in official pest list
othertox <- subset.data.frame(alltox, hasmatch==FALSE)
# Petit nettoyage de 3 petits problemes:
tox=tox[-which(tox$moleculename_fr=="Aldicarbe" & tox$fonction=="FongicidesandNematicides"), ]
tox=tox[-which(tox$moleculename_fr=="Terbuphos" & tox$fonction=="FongicidesandNematicides"), ]
tox=tox[-which(tox$moleculename_fr=="sulfate tribasique de cuivre" & tox$fonction=="FongicidesandNematicides"), ]
# On ajoute ces informations au fichier pesticide
pesticide=merge(pesticide , tox, by.x="LB_PARAMETRE", by.y="moleculename_fr" , all.x=T)
# On peux aggréger avec l'importance de chaque pesticide
colnames(c)[2]="importance_pest"
pesticide=merge(pesticide , c, by.x="LB_PARAMETRE", by.y="Group.1" , all.x=T)






##########################################
##### Modifs et ajouts Gui 25-26/01 ######
##########################################


# Nettoyage des colonnes useless
pest_station$NBANASPERTS1 <- NULL
pest_station$NUM_COM <- NULL
pest_station$NOM_COM <- NULL
pest_station$NO_FUNCTION <- NULL
pest_station$ALTITUDE <- NULL
pest_station$PROFONDEUR_MAXI_POINT <- NULL
pest_station$LONG <- NULL
pest_station$LAT <- NULL

# On rajoute le STATUT qui est important
pest_station <- merge(pest_station, pesticide[,c("CD_PARAMETRE","STATUT")], all.x=TRUE, all.y=FALSE)

# Petit nettoyage de 3 pesticides en double qui foutent la merde
pesticide <- pesticide[-which(pesticide$CD_PARAMETRE==1177 & pesticide$LB_PARAMETRE=="Diuron desmethyl"),]
pesticide <- pesticide[-which(pesticide$CD_PARAMETRE==1208 & pesticide$LB_PARAMETRE=="Isoproturon desmethyl"),]
pesticide <- pesticide[-which(pesticide$CD_PARAMETRE==1141 & pesticide$LB_PARAMETRE=="2,4-D methyl ester"),]
pest_station$Acaricide <- NULL
pest_station$Biocide <- NULL
pest_station$Fongicide <- NULL
pest_station$Herbicide <- NULL
pest_station$Mollusticide <- NULL
pest_station$Nematicide <- NULL
pest_station$Insecticide <- NULL
pest_station$Reg_croiss <- NULL
pest_station$Rodenticide <- NULL
pest_station$Repulsif <- NULL
pest_station$Graminicide <- NULL
pest_station <- merge(pest_station, pesticide[,c("CD_PARAMETRE","Acaricide","Biocide","Fongicide","Herbicide",
                                                 "Mollusticide","Nematicide","Insecticide","Reg_croiss",
                                                 "Rodenticide","Repulsif","Graminicide")])


# Nettoyage des valeurs aberrantes de concentration
pest_station <- pest_station[which(pest_station$MA_MOY<1),] # Vire 142 lignes sur les 2797956... et Q3 = 0.025ug/l...


# Calculs par département
pest_departement.mean <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+NUM_DEP, data=pest_station, mean,na.rm=TRUE)
colnames(pest_departement.mean) <- c("CD_PARAMETRE","ANNEE","NUM_DEP","MEAN")
pest_departement.max <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+NUM_DEP, data=pest_station, max,na.rm=TRUE)
colnames(pest_departement.max) <- c("CD_PARAMETRE","ANNEE","NUM_DEP","MAX")
pest_departement.min <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+NUM_DEP, data=pest_station, min,na.rm=TRUE)
colnames(pest_departement.min) <- c("CD_PARAMETRE","ANNEE","NUM_DEP","MIN")
pest_departement.med <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+NUM_DEP, data=pest_station, median,na.rm=TRUE)
colnames(pest_departement.med) <- c("CD_PARAMETRE","ANNEE","NUM_DEP","MEDIAN")

pest_departement <- merge(pest_departement.mean, pest_departement.min)
pest_departement <- merge(pest_departement, pest_departement.max)
pest_departement <- merge(pest_departement, pest_departement.med)

other <- unique(pest_station[,c("CD_PARAMETRE","NORME_DCE","CODE_FAMILLE")])
other2 <- unique(pest_station[,c("CD_PARAMETRE","Acaricide","Biocide","Fongicide","Herbicide","Mollusticide","Nematicide","Insecticide","Reg_croiss","Rodenticide","Repulsif","Graminicide")])
other <- merge(other,  other2)
pest_departement <- merge(pest_departement, other , all.x=T, all.y=F)

rm(pest_departement.mean, pest_departement.max, pest_departement.min, pest_departement.med)

# Calculs par masse d'eau
pest_station.ME <- merge(pest_station, station[,c("CD_STATION","CD_ME_v2")],all.x=TRUE, all.y=FALSE)
pest_ME.mean <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+CD_ME_v2, data=pest_station.ME, mean,na.rm=TRUE)
colnames(pest_ME.mean) <- c("CD_PARAMETRE","ANNEE","CD_ME_v2","MEAN")
pest_ME.max <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+CD_ME_v2, data=pest_station.ME, max,na.rm=TRUE)
colnames(pest_ME.max) <- c("CD_PARAMETRE","ANNEE","CD_ME_v2","MAX")
pest_ME.min <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+CD_ME_v2, data=pest_station.ME, min,na.rm=TRUE)
colnames(pest_ME.min) <- c("CD_PARAMETRE","ANNEE","CD_ME_v2","MIN")
pest_ME.med <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+CD_ME_v2, data=pest_station.ME, median,na.rm=TRUE)
colnames(pest_ME.med) <- c("CD_PARAMETRE","ANNEE","CD_ME_v2","MEDIAN")

pest_ME <- merge(pest_ME.mean, pest_ME.min)
pest_ME <- merge(pest_ME, pest_ME.max)
pest_ME <- merge(pest_ME, pest_ME.med)
pest_ME <- merge(pest_ME, other, all.x=T, all.y=F)

rm(pest_ME.mean, pest_ME.max, pest_ME.min, pest_ME.med, pest_station.ME)

# Calculs par régions
pest_region.mean <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+Region, data=pest_station, mean,na.rm=TRUE)
colnames(pest_region.mean) <- c("CD_PARAMETRE","ANNEE","Region","MEAN")
pest_region.max <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+Region, data=pest_station, max,na.rm=TRUE)
colnames(pest_region.max) <- c("CD_PARAMETRE","ANNEE","Region","MAX")
pest_region.min <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+Region, data=pest_station, min,na.rm=TRUE)
colnames(pest_region.min) <- c("CD_PARAMETRE","ANNEE","Region","MIN")
pest_region.med <- aggregate(MA_MOY~CD_PARAMETRE+ANNEE+Region, data=pest_station, median,na.rm=TRUE)
colnames(pest_region.med) <- c("CD_PARAMETRE","ANNEE","Region","MEDIAN")

pest_region <- merge(pest_region.mean, pest_region.min)
pest_region <- merge(pest_region, pest_region.max)
pest_region <- merge(pest_region, pest_region.med)
pest_region <- merge(pest_region, other, all.x=T, all.y=F)

rm(pest_region.mean, pest_region.min, pest_region.max, pest_region.med, other, other2)

#############
# On prépare les tableaux de données rapides pour les stations, départements et régions :
#############
# 1) CtotMax : Concentration totale maximale sur la zone
# 2) NbSupMax: Nombre max de molécules supérieures à leur seuil
# 3) NbMolQuantMax : Nombre max de molécules quantifiées = détectées
# 4) PropMolQuant : Proportion max de molécules quantifiées=détectées sur les molécules dosées
# 5) NbPNAMax: Nombre max de molécules non autorisées détectées

#### Par région
# 1) CtotMax 
pest_region.quick.CtotMax <- aggregate(MA_MOY~CD_STATION+ANNEE+Region, data=pest_station, sum,na.rm=TRUE)
pest_region.quick.CtotMax <- aggregate(MA_MOY~ANNEE+Region, data=pest_region.quick.CtotMax, max,na.rm=TRUE)
colnames(pest_region.quick.CtotMax) <- c("ANNEE","Region","CtotMax")

# 2) NbSupMax
pest_station.TEMP <- pest_station
pest_station.TEMP$SupSeuil <- 0
pest_station.TEMP$SupSeuil[which(pest_station.TEMP$MA_MOY > pest_station.TEMP$NORME_DCE)] <- 1
pest_region.quick.NbSupMax <- aggregate(SupSeuil~CD_STATION+ANNEE+Region, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_region.quick.NbSupMax <- aggregate(SupSeuil~ANNEE+Region, data=pest_region.quick.NbSupMax, max,na.rm=TRUE)
colnames(pest_region.quick.NbSupMax) <- c("ANNEE","Region","NbSupMax")

# 3) NbMolQuantMax et 4) PropMolQuant
pest_station.TEMP <- pest_station
pest_station.TEMP$MolQuant <- 1
pest_station.TEMP$MolQuant[which(pest_station.TEMP$MA_MOY == 0.05)] <- 0
pest_station.TEMP$Count <- 1
pest_region.quick.NbMolQuantMax <- aggregate(MolQuant~CD_STATION+ANNEE+Region, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_region.quick.PropMolQuantMax <- aggregate(Count~CD_STATION+ANNEE+Region, data=pest_station.TEMP, sum,na.rm=TRUE)

pest_region.quick.NbMolQuantMax <- aggregate(MolQuant~ANNEE+Region, data=pest_region.quick.NbMolQuantMax, max,na.rm=TRUE)
colnames(pest_region.quick.NbMolQuantMax) <- c("ANNEE","Region","NbMolQuantMax")

pest_region.quick.PropMolQuantMax <- aggregate(Count~ANNEE+Region, data=pest_region.quick.PropMolQuantMax, max,na.rm=TRUE)
pest_region.quick.PropMolQuantMax$Count <- pest_region.quick.NbMolQuantMax$NbMolQuantMax / pest_region.quick.PropMolQuantMax$Count
colnames(pest_region.quick.PropMolQuantMax) <- c("ANNEE","Region","PropMolQuantMax")

# 5) NbPNAMax
pest_station.TEMP$PNA <- 0
pest_station.TEMP$PNA[which(pest_station.TEMP$STATUT == "PNA")] <- 1

pest_region.quick.NbPNAMax <- aggregate(PNA~CD_STATION+ANNEE+Region, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_region.quick.NbPNAMax <- aggregate(PNA~ANNEE+Region, data=pest_region.quick.NbPNAMax, max,na.rm=TRUE)
colnames(pest_region.quick.NbPNAMax) <- c("ANNEE","Region","NbPNAMax")

pest_region.quick <- merge(pest_region.quick.CtotMax, pest_region.quick.NbSupMax)
pest_region.quick <- merge(pest_region.quick, pest_region.quick.NbMolQuantMax)
pest_region.quick <- merge(pest_region.quick, pest_region.quick.PropMolQuantMax)
pest_region.quick <- merge(pest_region.quick, pest_region.quick.NbPNAMax)

rm(pest_region.quick.CtotMax,pest_region.quick.NbSupMax,pest_region.quick.NbMolQuantMax,pest_region.quick.PropMolQuantMax,pest_region.quick.NbPNAMax, pest_station.TEMP)


#### Par département
# 1) CtotMax 
pest_departement.quick.CtotMax <- aggregate(MA_MOY~CD_STATION+ANNEE+NUM_DEP, data=pest_station, sum,na.rm=TRUE)
pest_departement.quick.CtotMax <- aggregate(MA_MOY~ANNEE+NUM_DEP, data=pest_departement.quick.CtotMax, max,na.rm=TRUE)
colnames(pest_departement.quick.CtotMax) <- c("ANNEE","NUM_DEP","CtotMax")

# 2) NbSupMax
pest_station.TEMP <- pest_station
pest_station.TEMP$SupSeuil <- 0
pest_station.TEMP$SupSeuil[which(pest_station.TEMP$MA_MOY > pest_station.TEMP$NORME_DCE)] <- 1
pest_departement.quick.NbSupMax <- aggregate(SupSeuil~CD_STATION+ANNEE+NUM_DEP, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_departement.quick.NbSupMax <- aggregate(SupSeuil~ANNEE+NUM_DEP, data=pest_departement.quick.NbSupMax, max,na.rm=TRUE)
colnames(pest_departement.quick.NbSupMax) <- c("ANNEE","NUM_DEP","NbSupMax")

# 3) NbMolQuantMax et 4) PropMolQuant
pest_station.TEMP <- pest_station
pest_station.TEMP$MolQuant <- 1
pest_station.TEMP$MolQuant[which(pest_station.TEMP$MA_MOY == 0.05)] <- 0
pest_station.TEMP$Count <- 1
pest_departement.quick.NbMolQuantMax <- aggregate(MolQuant~CD_STATION+ANNEE+NUM_DEP, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_departement.quick.PropMolQuantMax <- aggregate(Count~CD_STATION+ANNEE+NUM_DEP, data=pest_station.TEMP, sum,na.rm=TRUE)

pest_departement.quick.NbMolQuantMax <- aggregate(MolQuant~ANNEE+NUM_DEP, data=pest_departement.quick.NbMolQuantMax, max,na.rm=TRUE)
colnames(pest_departement.quick.NbMolQuantMax) <- c("ANNEE","NUM_DEP","NbMolQuantMax")

pest_departement.quick.PropMolQuantMax <- aggregate(Count~ANNEE+NUM_DEP, data=pest_departement.quick.PropMolQuantMax, max,na.rm=TRUE)
pest_departement.quick.PropMolQuantMax$Count <- pest_departement.quick.NbMolQuantMax$NbMolQuantMax / pest_departement.quick.PropMolQuantMax$Count
colnames(pest_departement.quick.PropMolQuantMax) <- c("ANNEE","NUM_DEP","PropMolQuantMax")

# 5) NbPNAMax
pest_station.TEMP$PNA <- 0
pest_station.TEMP$PNA[which(pest_station.TEMP$STATUT == "PNA")] <- 1

pest_departement.quick.NbPNAMax <- aggregate(PNA~CD_STATION+ANNEE+NUM_DEP, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_departement.quick.NbPNAMax <- aggregate(PNA~ANNEE+NUM_DEP, data=pest_departement.quick.NbPNAMax, max,na.rm=TRUE)
colnames(pest_departement.quick.NbPNAMax) <- c("ANNEE","NUM_DEP","NbPNAMax")

pest_departement.quick <- merge(pest_departement.quick.CtotMax, pest_departement.quick.NbSupMax)
pest_departement.quick <- merge(pest_departement.quick, pest_departement.quick.NbMolQuantMax)
pest_departement.quick <- merge(pest_departement.quick, pest_departement.quick.PropMolQuantMax)
pest_departement.quick <- merge(pest_departement.quick, pest_departement.quick.NbPNAMax)

rm(pest_departement.quick.CtotMax,pest_departement.quick.NbSupMax,pest_departement.quick.NbMolQuantMax,pest_departement.quick.PropMolQuantMax,pest_departement.quick.NbPNAMax, pest_station.TEMP)


#### Par masse d'eau
# 1) CtotMax 
pest_station.ME <- merge(pest_station, station[,c("CD_STATION","CD_ME_v2")],all.x=TRUE, all.y=FALSE)
pest_ME.quick.CtotMax <- aggregate(MA_MOY~CD_STATION+ANNEE+CD_ME_v2, data=pest_station.ME, sum,na.rm=TRUE)
pest_ME.quick.CtotMax <- aggregate(MA_MOY~ANNEE+CD_ME_v2, data=pest_ME.quick.CtotMax, max,na.rm=TRUE)
colnames(pest_ME.quick.CtotMax) <- c("ANNEE","CD_ME_v2","CtotMax")

# 2) NbSupMax
pest_station.TEMP <- pest_station.ME
pest_station.TEMP$SupSeuil <- 0
pest_station.TEMP$SupSeuil[which(pest_station.TEMP$MA_MOY > pest_station.TEMP$NORME_DCE)] <- 1
pest_ME.quick.NbSupMax <- aggregate(SupSeuil~CD_STATION+ANNEE+CD_ME_v2, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_ME.quick.NbSupMax <- aggregate(SupSeuil~ANNEE+CD_ME_v2, data=pest_ME.quick.NbSupMax, max,na.rm=TRUE)
colnames(pest_ME.quick.NbSupMax) <- c("ANNEE","CD_ME_v2","NbSupMax")

# 3) NbMolQuantMax et 4) PropMolQuant
pest_station.TEMP <- pest_station.ME
pest_station.TEMP$MolQuant <- 1
pest_station.TEMP$MolQuant[which(pest_station.TEMP$MA_MOY == 0.05)] <- 0
pest_station.TEMP$Count <- 1
pest_ME.quick.NbMolQuantMax <- aggregate(MolQuant~CD_STATION+ANNEE+CD_ME_v2, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_ME.quick.PropMolQuantMax <- aggregate(Count~CD_STATION+ANNEE+CD_ME_v2, data=pest_station.TEMP, sum,na.rm=TRUE)

pest_ME.quick.NbMolQuantMax <- aggregate(MolQuant~ANNEE+CD_ME_v2, data=pest_ME.quick.NbMolQuantMax, max,na.rm=TRUE)
colnames(pest_ME.quick.NbMolQuantMax) <- c("ANNEE","CD_ME_v2","NbMolQuantMax")

pest_ME.quick.PropMolQuantMax <- aggregate(Count~ANNEE+CD_ME_v2, data=pest_ME.quick.PropMolQuantMax, max,na.rm=TRUE)
pest_ME.quick.PropMolQuantMax$Count <- pest_ME.quick.NbMolQuantMax$NbMolQuantMax / pest_ME.quick.PropMolQuantMax$Count
colnames(pest_ME.quick.PropMolQuantMax) <- c("ANNEE","CD_ME_v2","PropMolQuantMax")

# 5) NbPNAMax
pest_station.TEMP$PNA <- 0
pest_station.TEMP$PNA[which(pest_station.TEMP$STATUT == "PNA")] <- 1

pest_ME.quick.NbPNAMax <- aggregate(PNA~CD_STATION+ANNEE+CD_ME_v2, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_ME.quick.NbPNAMax <- aggregate(PNA~ANNEE+CD_ME_v2, data=pest_ME.quick.NbPNAMax, max,na.rm=TRUE)
colnames(pest_ME.quick.NbPNAMax) <- c("ANNEE","CD_ME_v2","NbPNAMax")

pest_ME.quick <- merge(pest_ME.quick.CtotMax, pest_ME.quick.NbSupMax)
pest_ME.quick <- merge(pest_ME.quick, pest_ME.quick.NbMolQuantMax)
pest_ME.quick <- merge(pest_ME.quick, pest_ME.quick.PropMolQuantMax)
pest_ME.quick <- merge(pest_ME.quick, pest_ME.quick.NbPNAMax)

rm(pest_ME.quick.CtotMax,pest_ME.quick.NbSupMax,pest_ME.quick.NbMolQuantMax,pest_ME.quick.PropMolQuantMax,pest_ME.quick.NbPNAMax, pest_station.TEMP)


#### Par station
# 1) CtotMax 
pest_station.quick.CtotMax <- aggregate(MA_MOY~CD_STATION+ANNEE, data=pest_station, sum,na.rm=TRUE)
colnames(pest_station.quick.CtotMax) <- c("CD_STATION","ANNEE","CtotMax")

# 2) NbSupMax
pest_station.TEMP <- pest_station
pest_station.TEMP$SupSeuil <- 0
pest_station.TEMP$SupSeuil[which(pest_station.TEMP$MA_MOY > pest_station.TEMP$NORME_DCE)] <- 1
pest_station.quick.NbSupMax <- aggregate(SupSeuil~CD_STATION+ANNEE, data=pest_station.TEMP, sum,na.rm=TRUE)
colnames(pest_station.quick.NbSupMax) <- c("CD_STATION","ANNEE","NbSupMax")

# 3) NbMolQuantMax et 4) PropMolQuant
pest_station.TEMP <- pest_station
pest_station.TEMP$MolQuant <- 1
pest_station.TEMP$MolQuant[which(pest_station.TEMP$MA_MOY == 0.05)] <- 0
pest_station.TEMP$Count <- 1
pest_station.quick.NbMolQuantMax <- aggregate(MolQuant~CD_STATION+ANNEE, data=pest_station.TEMP, sum,na.rm=TRUE)
pest_station.quick.PropMolQuantMax <- aggregate(Count~CD_STATION+ANNEE, data=pest_station.TEMP, sum,na.rm=TRUE)
colnames(pest_station.quick.NbMolQuantMax) <- c("CD_STATION","ANNEE","NbMolQuantMax")

pest_station.quick.PropMolQuantMax$Count <- pest_station.quick.NbMolQuantMax$NbMolQuantMax / pest_station.quick.PropMolQuantMax$Count
colnames(pest_station.quick.PropMolQuantMax) <- c("CD_STATION","ANNEE","PropMolQuantMax")

# 5) NbPNAMax
pest_station.TEMP$PNA <- 0
pest_station.TEMP$PNA[which(pest_station.TEMP$STATUT == "PNA")] <- 1

pest_station.quick.NbPNAMax <- aggregate(PNA~CD_STATION+ANNEE, data=pest_station.TEMP, sum,na.rm=TRUE)
colnames(pest_station.quick.NbPNAMax) <- c("CD_STATION","ANNEE","NbPNAMax")

pest_station.quick <- merge(pest_station.quick.CtotMax, pest_station.quick.NbSupMax)
pest_station.quick <- merge(pest_station.quick, pest_station.quick.NbMolQuantMax)
pest_station.quick <- merge(pest_station.quick, pest_station.quick.PropMolQuantMax)
pest_station.quick <- merge(pest_station.quick, pest_station.quick.NbPNAMax)

rm(pest_station.quick.CtotMax,pest_station.quick.NbSupMax,pest_station.quick.NbMolQuantMax,pest_station.quick.PropMolQuantMax,pest_station.quick.NbPNAMax, pest_station.TEMP)





# ========= On sauvegarde l'environnement:
# A t'on vraiment besoin de station & pesticide?
save( my_barplot, resum_dep_quick, pest_station, pesticide,region,station,departements,ME,
pest_ME, pest_ME.quick, pest_region, pest_region.quick, pest_departement, pest_departement.quick,
pest_station.quick, file="DATA/env_greentech.R")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------








# --------------------------------------------------------------------------

# Note: on peut maintenant charger tous les fichiers en qq (3) secondes:
#R
#setwd("~/Dropbox/GreenTech_challenge/")
#load("DATA/env_greentech.R")

# --------------------------------------------------------------------------
