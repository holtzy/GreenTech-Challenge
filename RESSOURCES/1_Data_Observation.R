	#--------------------------------------------------------------------------------------------------
	#   
	#		GreenTech Competition: Data Observation
	#
	#					Script made by Yan Holtz (yan1166@hotmail.com / holtz@supagro.inra.fr)
	#
	#---------------------------------------------------------------------------------------------------


# Goal
#------

# This script aims to understand data. We take tables one by one and try to understand what is into it.
# most of the charts of the app are also present in this file.









#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- LOAD THE CSV FILE OF THE DATABASE & LIBRARIES
#-----------------------------------------------------------------------------

# Library
library(ggplot2)
library(plotly)

# Un environnement R a été préparé dans le fichier 0_Prepare_data. On peux donc tout charger en qq secondes:
setwd("~/Dropbox/GreenTech_challenge/")
load("DATA/env_greentech.R")
ls()

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#










#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# --- Observe Pesticide data
#-----------------------------------------------------------------------------

# Donne des infos pesticides par pesticide. 
# 1046 lignes, mais je ne vois pas d'ID unique..
# Je sais pas trop ce que l'on va en faire..

# Correction des é dans les nomes:


# Dimension? --> 1046 pesticide répertoriés
dim(pesticide)

# Combien de famille? --> 32 familles
nlevels(pesticide$CODE_FAMILLE)

# Effectif par famille?
dat=data.frame(table(pesticide$CODE_FAMILLE))
colnames(dat)=c("famille" , "nbr_pest")
dat=dat[which(dat$famille!="") , ]

# Représentation en barplot
barplot(table(pesticide$CODE_FAMILLE) , las=2 , xaxt="n")

# Repésentation en circular plot?
library(circlize)
my_factor=sort(rep(c("a","b"),nrow(dat) ))
circos.clear()
# General parameters
circos.par("track.height" = 0.6 ,"start.degree"=-180,  "canvas.xlim" = c(-1, 1), "canvas.ylim" = c(0, 1))
# Initialize chart
circos.initialize(factors=my_factor, x = rep( c(1:nrow(dat)),2) )
# Build the regions with no border
circos.trackPlotRegion(factors = my_factor, y=rep(dat$nbr_pest,2), bg.border = NA)
# update region 1
circos.updatePlotRegion(sector.index = "a", bg.border = "grey" , bg.lwd=0.8)
# Add line
circos.trackLines( rep("a",nrow(dat)), c(1:nrow(dat)), dat$nbr_pest, col = rgb(0.1,0.5,0.8,0.3), lwd=7, type="h")
--> A creuser

# Ensuite il faudrait ajouter à ce tableau les explications des codes fonctions. A--> Acaricide
# Il faudrait aussi ajouter les données d'agriculture si on les trouve.

# Quelle famille a quel type de fonction:
table(pesticide$CODE_FAMILLE , pesticide$CODE_FONCTION)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#








#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# --- Station data:
#-----------------------------------------------------------------------------

# On a 13039 stations, une ligne par station
nlevels(station$CD_STATION)
dim(station)
#id unique="CD_STATION"

# On a les infos pour toutes les stations: position, département, altitude, en quelle année elles ont servies.

# Distribution des altitudes?
hist(station$ALTITUDE , border=F , col=rgb(0.4,0.1,0.9,0.5) , xlab="altitude" , main="distribution de l'altitude des stations")
# --> Beaucoup de stations de basse altitude, et quelques unes assez haute

# Position en coordonnées:
# On a qq stations avec des coordonnées à 0. Il faut donc nettoyer.
station$X_FICT_L93[station$X_FICT_L93==0]=NA
station$Y_FICT_L93[station$Y_FICT_L93==0]=NA
# On peut regarder la position des stations assez facilement:
ggplot(station , aes(x=X_FICT_L93, y=Y_FICT_L93)) + geom_point() + xlab("") + ylab("")

# Et colorer en fonction de l'altitude:
ggplot(station , aes(x=X_FICT_L93, y=Y_FICT_L93, colour=ALTITUDE)) + geom_point() + xlab("") + ylab("")

# Colorer en fonction de la profondeur des prélèvements:
station$PROFONDEUR_MAXI_POINT=as.numeric(as.character(station$PROFONDEUR_MAXI_POINT))
ggplot(station , aes(x=X_FICT_L93, y=Y_FICT_L93, colour=PROFONDEUR_MAXI_POINT )) + geom_point() + xlab("") + ylab("")

# En interactif?
p=ggplot(station , aes(x=X_FICT_L93, y=Y_FICT_L93, colour=PROFONDEUR_MAXI_POINT )) + geom_point() + xlab("") + ylab("")
ggplotly(p)
#--> c'est un peu trop lourd, faudra peut etre passer par leaflet plutot

# Combien de relevé ont été fait par station? Y a t"il des stations inutiles?
nb_data_per_station=rowSums(is.na(station[ ,19:24]))
hist(nb_data_per_station)
# --> beaucoup de station avec 5 relevé à priori
# Station avec 0 relevé: --> 1296
summary(as.factor(nb_data_per_station))
# Station avec 1 relevé: 380
# 13039 stations en tout.

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#







#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# --- Moyenne Pesticide data:
#-----------------------------------------------------------------------------

# Ce fichier donne les stats de chaque station chaque année. Donc une ligne par station utilisée par an.
# Nbrepel= nbr de prélevement d'eau effectués
# Ensuite les stats données concernent tous les pesticides confondus
# pour avoir les détails par pesticide et par station, il faut aller voir le fichier "historique"
# 29398 lignes et 9 colonnes.
dim(moy_pest)

#Entre 1600 et 2000 mesures par année
table(moy_pest$ANNEE)

# Boxplot de la concentration des pesticides pour chaque année, toutes stations confondues:
par(mfrow=c(1,2))
boxplot(moy_pest$MOYPTOT ~ moy_pest$ANNEE  )
boxplot(moy_pest$MOYPTOT ~ moy_pest$ANNEE , ylim=c(0,1) )
# on a des points très haut = qq stations qui ont vraiment pris lourd

# Evolution des stations qui ont déja dépassé le seuil acceptable?
depasse=unique(moy_pest[ which(moy_pest$MOYPTOT>1) , "CD_STATION"])
# 949 station ont déja dépassé le seuil au moins une fois! C'est énorme !
tmp=moy_pest[moy_pest$CD_STATION%in%depasse , ]
ggplot(tmp , aes(x=ANNEE , y=MOYPTOT, group=CD_STATION, color=CD_STATION)) + geom_line() + theme(legend.pos="") + scale_y_log10()
#--> Ca va pas de montrer autant de station.. On voit plus rien.. 

# Show one station only:
tmp=moy_pest[which(moy_pest$CD_STATION=="04352X0006/SOURCE") , ]
ggplot(tmp , aes(x=ANNEE , y=MOYPTOT, color=CD_STATION)) + geom_line() + theme(legend.pos="") + geom_point()

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#







#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- Moyenne Pesticide historique:
#-----------------------------------------------------------------------------

Voir 02381X0035/P1 --> évolution + ou moins?

# Gros fichier de 2 779 684 lignes
# Une ligne par pesticide par station par an!
dim(pest_db)

# nbr d'échantillon par an:
table(pest_db$ANNEE)
  2007   2008   2009   2010   2011   2012 
604829 489240 949198 222735 227720 285962 
barplot(table(pest_db$ANNEE))

# Observons l'évolution de tous les pesticides d'une station au cours de toutes les années:
tmp=pest_db[which(pest_db$CD_STATION=="00053X0002/SO1") , ] 
ggplot(tmp , aes(x=ANNEE , y=MA_MOY, color=LB_PARAMETRE, group=LB_PARAMETRE)) + geom_line() + theme(legend.pos="") + geom_point() + ylim(0,1)
#--> seulement 23 pesticides observés en 2007, pas très rigolo

# Une autre
tmp=pest_db[which(pest_db$CD_STATION=="06991X0001/S") , ] 
#--> beaucoup mieux, 529 échantillons observés en tout sur 3 ans. Observons l'évolution de chaque pesticide chaque année:
ggplot(tmp , aes(x=ANNEE , y=MA_MOY, color=LB_PARAMETRE, group=LB_PARAMETRE)) + geom_line() + theme(legend.pos="") + geom_point() + ylim(0,1)

# Une autre
tmp=pest_db[which(pest_db$CD_STATION=="04352X0006/SOURCE") , ] 
#--> beaucoup mieux, 529 échantillons observés en tout sur 3 ans. Observons l'évolution de chaque pesticide chaque année:
ggplot(tmp , aes(x=ANNEE , y=MA_MOY, color=LB_PARAMETRE, group=LB_PARAMETRE)) + geom_line() + theme(legend.pos="") + geom_point() + scale_y_log10()

# Même chose en streamgraph? --> Y peut y a voir de l'idée.
streamgraph(tmp, key="LB_PARAMETRE", value="MA_MOY", date="ANNEE" , offset="zero") %>%
       sg_colors(axis_color = "black", tooltip_color = "black", label_color = "black") %>%
       sg_axis_x(tick_interval = 1, tick_units = "yoyoyoyoyoyoyoyoyo")#, tick_format = ‘sprintf’)

streamgraph(tmp, key="LB_PARAMETRE", value="MA_MOY", date="ANNEE" , offset="silouhette")
streamgraph(tmp, key="LB_PARAMETRE", value="MA_MOY", date="ANNEE" , offset="expand")
streamgraph(tmp, key="LB_PARAMETRE", value="MA_MOY", date="ANNEE" , offset="zero", interpolate="linear")
streamgraph(tmp, key="LB_PARAMETRE", value="MA_MOY", date="ANNEE" , offset="zero", interpolate="step")


# Même chose, mais en aggrégeant par fonction de pesticide? --> cool, on peut ajouter un widget pour dire en fonction de quoi on agggrege
myvar="CODE_FAMILLE"
v=merge(tmp , pesticide , by.x="CD_PARAMETRE" , by.y="CD_PARAMETRE" , all.x=T)
v=v[ , c("MA_MOY" , "ANNEE", myvar)]
v=aggregate(v$MA_MOY, by=list(v$ANNEE, v[,myvar] ) , mean)
colnames(v)=c("ANNEE" , "FONCTION" , "MOY")
streamgraph(v, key="FONCTION", value="MOY", date="ANNEE" , offset="zero")


#--> Par contre c relou, la pluparts des 255 pesticides ne sont pas observés souvent
# Observons l'évolution de ce observés les 3 années?
my_list=names(table(tmp$LB_PARAMETRE)[table(tmp$LB_PARAMETRE)>=3])
tmp=tmp[which(tmp$LB_PARAMETRE%in%my_list) , ]
ggplot(tmp, aes(x=ANNEE , y=MA_MOY), colour=LB_PARAMETRE) + geom_line()
#--> ok j'ai pas compris enfait

# Carte des stations colorées selon c pesticide par an?
		resum_station=aggregate(pest_db$MA_MOY , by=list(pest_db$CD_STATION, pest_db$ANNEE) , mean , na.rm=T)
		resum_station$Group.2=paste("y", resum_station$Group.2,sep="")
		resum_station=resum_station %>% spread(Group.2, x)
		
		# Merge au fichier station
		inFile=station[ , c("CD_STATION","LONG","LAT")]
		inFile= merge(resum_station , inFile , by.x=1 , by.y=1 , all.x=T)
		
		# Select one year:
		choix="2008"
		inFile=inFile[ , c("Group.1","LONG","LAT",paste("y",choix,sep=""))]
		inFile=na.omit(inFile)
		
		# Carto leaflet:
		leaflet(data=inFile) %>% 
		setView(lng = 2,34, lat = 47, zoom = 5) %>% 
  		#addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
		addCircleMarkers(~LONG, ~LAT, radius=3 , color=colorQuantile("YlOrRd", inFile[,4])(inFile[,4]), stroke = TRUE, fillOpacity = 0.5 , popup = "" )
	
	
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#







#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- Données des polygones
#-----------------------------------------------------------------------------

#
departements=readOGR( dsn= "DATA/Departements/" , layer="departements-20140306-100m")

# voir les données associées au polygone de masse d'eau:
head(ME@data)
# --> 1103 masses d'eau.

# Distribution des niveau des masses d'eau:
table(ME@data$Niveau)
  1   2   3   4   5   6   7   8   9  10 
578 292 115  51  28  17  12   6   3   1 

# On peut ploter ces polygones, mais c'est long a tourner:
plot(ME)
plot(regions)
plot(departements) --> bug

# Avec leaflet --> carte chloropleth en fonction de la surface.
library(leaflet)
leaflet(regions) %>%
   setView(lng = 2,34, lat = 47, zoom = 5) %>%
   addPolygons(
     stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
     color = ~colorQuantile("YlOrRd", regions$surf_km2)(surf_km2)
   )
   
   
# == Carte chloropleth de la concentration moyenne en pesticide par départements?
	# aggregation par département & par années
	resum_dep=aggregate(pest_db$MA_MOY , by=list(pest_db$NUM_DEP, pest_db$ANNEE) , mean , na.rm=T)
	resum_dep$Group.2=paste("y", resum_dep$Group.2,sep="")
	resum_dep=resum_dep %>% spread(Group.2, x)
		
	# Merge au fichier département:
	departements@data=departements@data[ , 1:4]
	departements@data= merge(departements@data , resum_dep , by.x=1 , by.y=1 , all.x=T)

# Plot it
leaflet(departements) %>%
   setView(lng = 2,34, lat = 47, zoom = 5) %>%
   addPolygons(
     stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
     color = colorQuantile("YlOrRd", departements$y2010)(departements$y2010)
   )



# 1- Je calcule la concentration moyenne par départements:
# 17 secondes pour le merge...
data=merge(pest_db, station, by.x="CD_STATION", by.y="CD_STATION" , all.x=T)
# 3 secondes pour l'aggregate
resum_dep=aggregate(data$MA_MOY , by=list(data$NUM_DEP) , mean , na.rm=T)
# add to the departement dataframe
departements@data= merge(departements@data , resum_dep , by.x=1 , by.y=1 , all.x=T)
# Plot it
leaflet(departements) %>%
   setView(lng = 2,34, lat = 47, zoom = 5) %>%
   addPolygons(
     stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
     color = ~colorQuantile("YlOrRd", departements$x.x)(x.x)
   )


# Idem mais que en 2006?
resum_dep=aggregate(data$MA_MOY , by=list(data$NUM_DEP, data$ANNEE) , mean , na.rm=T)
departements@data= merge(departements@data , resum_dep[which(resum_dep$Group.2=="2008"),] , by.x=1 , by.y=1 , all.x=T)
# Plot it
leaflet(departements) %>%
   setView(lng = 2,34, lat = 47, zoom = 5) %>%
   addPolygons(
     stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
     color = ~colorQuantile("YlOrRd", departements$x)(x)
   )

# Du coup par région c'est facile
# Faire le curseur par année c'est facile aussi.

# Par masse d'eau? --> Marche as

leaflet(ME) %>%
   setView(lng = 2,34, lat = 47, zoom = 5) %>%
   addPolygons(
     stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
   )

Plot évolution pesticide pour chaque réion?

 
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#








#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- BARPLOT MOST COMMON PESTICIDES
#-----------------------------------------------------------------------------

# Calcul: pour chaque pesticide, combien de fois il a était recherché = combien de ligne il a dans le fichier pest_db
a=aggregate(pest_db$MA_MOY , by=list(pest_db$LB_PARAMETRE) , length)
a=head(a[order(a$x , decreasing=T) , ])
barplot(a[,2])

# Calcul: pour chaque pesticide, dans combien de station il a été quantifié=au dessus du seuil de détection de la machine
b=aggregate(pest_db$NBQUANTIF , by=list(pest_db$LB_PARAMETRE) , function(x){length(x[x>0])} )
b=head(b[order(b$x , decreasing=T) , ])
barplot(b[,2])

# Calcul: pour chaque pesticide: % de fois ou il a été décelé par rapport au nombre de fois ou il a été recherché?

# Calcul: pour chaque pesticide, combien de fois il a était vu au dessus du seuil de 0.1?
c=aggregate(pest_db$MA_MOY , by=list(pest_db$LB_PARAMETRE) , function(x){ length(x[x>0.1])}  )
c=head(c[order(c$x , decreasing=T) , ])
barplot(c[,2])

# Calcul: pour chaque pesticide, % de fois ou il a était vu au dessus du seuil de 0.1?
#d=aggregate(pest_db$MA_MOY , by=list(pest_db$LB_PARAMETRE) , function(x){ length(x[x>0.1])/length(x) * 100 } )
#d=head(d[order(d$x , decreasing=T) , ])
#barplot(d[,2])

# --> donc plein de manière de voir le problème..
# --> regarder les moyennes de MA = pas intéressant je pense en fait.

# Barplot propre?
ggplot(c , aes(y=x , x=reorder(Group.1,x,decreasing=T), label=reorder(Group.1,x,decreasing=T))) + 
	geom_bar(stat = "identity", width=0.6, color="transparent",fill=rgb(0.3,0.5,0.9,0.8) ) + 
	scale_fill_brewer(palette = "YlOrRd") +
	coord_flip() + 
	xlab("") + 
	ylab("") + 
	theme(
		legend.pos="none", axis.ticks.y = element_blank(), axis.text.y = element_blank() , 
		panel.background=element_rect(color="grey"), panel.grid.major.y=element_blank(), 
		panel.grid.major.x=element_line(color="grey", size=0.3) , panel.grid.minor=element_blank(),
		plot.margin=unit(c(0,0,50,50),"mm") , 
		legend.box.spacing = unit(0, "mm")
	)+
	geom_text(aes(y=0+50 , size=log(x+4) ), hjust = 0 , col="white", fontface='bold')

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#









#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- BUBBLE PLOT BIBLIO PESTICIDE
#-----------------------------------------------------------------------------

# JC a fait la prépa des fichiers.
# Du coup le fichier pesticide contient toutes les infos nécessaires!
set.seed(123)

# PREPARE DATA FROM PESTICIDE FILE:
don=pesticide[which(pesticide$LB_PARAMETRE%in%pest_db$LB_PARAMETRE) , ]
don=pesticide[!is.na(pesticide$LD50) , ]
don=don[ , c(1,4,6,11,12,13,14:24,29,31)]
don=don %>% gather(fonction, value, 7:17) 
don=don[which(don$value==1 & don$importance_pest>0) , ]
don$fonction[don$fonction=="Reg_croiss"]="Rég. croissance"
 
# On invente un x:
don$posx=sample(seq(1,100) , nrow(don), replace=T)
# Si l'importance du pesticide est NA, c'est qu'on ne l'a pas observé. Donc je lui mets 0.1
#don$importance_pest[is.na(don$importance_pest)]=0.1
don=droplevels(don)


# C'est parti pour plotly
# GROSSE LIMITATION: ce con de plotly ne permet pas de gérer la taille des points des légendes...
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
don$size=13*log(log(don$importance_pest+1)+1)
don$my_text=paste("Nom: ",don$LB_PARAMETRE,"<br>","Frequence: ",don$importance_pest,"<br>","Famille: ",don$CODE_FAMILLE,"<br>","Formule: ",don$FORMULEB,"<br>","Norme: ",don$NORME_DCE,"<br>","Statut: ",don$STATUT, sep="")

plot_ly(don, 

	# général
	x= ~posx, y=~LD50, color= ~fonction, size = ~size, colors = colors,  type = 'scatter', mode = 'markers',	
	# aspect des marqueurs
	marker = list(symbol = 'circle', sizemode = 'diameter', line = list(width = 2, color = '#FFFFFF')),	
	# hover text
	text=~my_text, hoverinfo="text",
	# size of plot
	width = 1000, height = 600
	)  %>%
  	# value of risk
  	add_annotations(x = rep(-5,4),y = c(0.9,2.3,3.4,4.5),text = c("Haute","Modéré","Légère","Faible"),xref = "x",yref = "y",showarrow = FALSE) %>%
  	# Layout
  	layout(
  		 title = '',
         xaxis = list(title = '',gridcolor = 'rgb(255, 255, 255)',range = c(-10,100),type = 'linear',zeroline=F ,showticklabels=F,ticklen = 0,gridwidth = 2),
         yaxis = list(title = '',gridcolor = 'rgb(255, 255, 255)',zerolinewidth = 1,showticklabels=F,ticklen = 0,type="log",autorange = "reversed",gridwith = 2,range=c(0,100000) ),
         legend = list(orientation = 'h', x=0.1, y=0),
         paper_bgcolor = '',
         plot_bgcolor = '',
         autosize = F, margin = list(l=0 , r=0,b=0,t=0,pad=0),
         shapes = list(
            list(type = "rect", fillcolor = "grey", line = list(color = "grey"), opacity = 0.2,  x0 = -15, x1 = 100, xref = "x", y0 = 0, y1 = 50, yref = "y"),
            list(type = "rect", fillcolor = "grey", line = list(color = "grey"), opacity = 0.05,  x0 = -15, x1 = 100, xref = "x", y0 = 50, y1 = 500, yref = "y"),
            list(type = "rect", fillcolor = "grey", line = list(color = "grey"), opacity = 0.2,  x0 = -15, x1 = 100, xref = "x", y0 = 500, y1 = 5000, yref = "y"),
            list(type = "rect", fillcolor = "grey", line = list(color = "grey"), opacity = 0.05,  x0 = -15, x1 = 100, xref = "x", y0 = 5000, y1 = 100000, yref = "y")
            )
        )

     
# En ggplot2?
ggplot(don, aes(x=posx, y=log(LD50), text=my_text, color=CODE_FONCTION )) + 
	geom_point(aes(size=13*log(log(don$importance_pest+1)+1)) , alpha=0.6) +
	ylab("") + xlab("")  +  ylim(log(max(don$LD50)),log(1))+  xlim(-15,105) +
	scale_size(range=c(1,20), guide="none") + 
	annotate("rect", xmin = -15, xmax = 105, ymin = log(1), ymax = log(50), alpha = .2) +
	annotate("rect", xmin = -15, xmax = 105, ymin = log(500), ymax = log(5000), alpha = .2) +
	annotate("text", x=-14,  y = (log(1)+log(50))/2, label = "Haute", hjust = 0, size=3) + 
	annotate("text", x=-14,  y = (log(50)+log(500))/2, label = "Modérée", hjust = 0, size=3) + 
	annotate("text", x=-14,  y = (log(500)+log(5000))/2, label = "Légère", hjust = 0, size=3) + 
	annotate("text", x=-14,  y = (log(5000)+log(max(don$LD50)))/2, label = "Faible", hjust = 0, size=3) + 
	theme(
		axis.ticks = element_blank(), axis.text = element_blank(),
		panel.background = element_blank(),
		legend.title=element_blank(), legend.position="bottom", legend.key = element_blank(),
		plot.margin=unit(c(0,-20,0,-20),"mm") , 
		legend.box.spacing = unit(0, "mm")
	) +
	guides(colour = guide_legend(override.aes=list(size=8) , nrow=1))

ggplotly() 



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#













#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- TREEMAP
#-----------------------------------------------------------------------------

# load libraries
library(treemap)
library(d3treeR)
library(tidyr)

# PREPARE DATA FROM PESTICIDE FILE:
don=pesticide[which(pesticide$LB_PARAMETRE%in%pest_db$LB_PARAMETRE) , ]
don=don[ , c(1,14:24,31)]
don=don %>% gather(fonction, value, 2:(ncol(don)-1) )
don=don[don$value==1 & don$importance_pest>0 , ]

# ======  Treemap en fonction des fonctions. Taille = nbr de molécule de la fonction?
# Si une molécule apparait dans plusieurs fonction, alors on la fait apparaitre plusieurs fois.
static=treemap(don,
            index=c("fonction", "LB_PARAMETRE"),
            vSize="value",
            type="index", 
            fontsize.labels=19,
			fontcolor.labels="black",
			fontface.labels=2,
			bg.labels="white"
)
# Puis on le transforme en dynamique
d3tree2( static , rootname = "Les grandes familles de Pesticides")


# ======  Treemap en fonction des fonctions. Taille = nbr de molécule de la fonction?
# Si une molécule apparait dans plusieurs fonction, alors on la fait apparaitre plusieurs fois.
static=treemap(don,
            index=c("fonction", "LB_PARAMETRE"),
            vSize="importance_pest",
            type="index", 
            fontsize.labels=19,
			fontcolor.labels="black",
			fontface.labels=2,
			bg.labels="white"
)
# Puis on le transforme en dynamique
d3tree2( static , rootname = "Les grandes familles de Pesticides")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#









#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- BOUTON NEUNEU
#-----------------------------------------------------------------------------

# get a station
my_station="03812X0025/S1"
val_station=pest_db[which(pest_db$CD_STATION==my_station) , ] 

# nbr of years with prelevement
length(unique(val_station$ANNEE))

# Cumul nbr de molécules cherchées:
nrow(val_station)

# Nbr de pesticide au dessus du seuil détectés + libélé
names=unique(val_station[val_station$MA_MOY>val_station$NORME_DCE , "LB_PARAMETRE"])
number=length(names)

# Nbr de pesticide quantifiés = détecté
names2=unique(val_station[val_station$NBQUANTIF>0 , "LB_PARAMETRE"])
number2=length(names2)
names=unique(as.factor(c(names,names2)))

# Evolution?
last2=sort(unique(val_station$ANNEE), decreasing=T)[c(1,2)]
tmp=val_station[which(val_station$ANNEE%in%last2) , ]
res=aggregate(tmp$MA_MOY , by=list(tmp$ANNEE) , mean, na.rm=T)
res=res[order(res$Group.1) , ]
evol=(res[2,2]-res[1,2])/res[1,2]*100
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#









#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# --- BARPLOT pesticide au dessus du seuil
#-----------------------------------------------------------------------------

	# Ou plutot chope la sur la carte:
	#val_station=pest_db[which(pest_db$CD_STATION=="03812X0025/S1") , ] 
	val_station=pest_db[which(pest_db$CD_STATION=="04084X1004/S") , ] 
	val_station=pest_db[which(pest_db$CD_STATION=="05836X0001/S") , ] 
	
	 
	 
	 
	# On récupère la dernière année seulement & on garder les valeurs supérieurs à 0.01 seulement.
	val_station=val_station[val_station$ANNEE==max(val_station$ANNEE) , ]
	val_station=val_station[val_station$MA_MOY>0.025, ]
	
	# Si une molécule à plusieurs fonction, on va lui donner une fonction seulement, au hasard. Code un peu ghetto mais il est tard..
	for(i in c(1:nrow(val_station))){ 
		to_change=colnames(val_station)[16:26][which(val_station[i,c(16:26)]==1)][-1]
		val_station[i,to_change]=0
		}
		
	# Construction colonne fonction
	library(tidyr)
	val_station=val_station %>% gather(fonction, value, 16:26)
	val_station=val_station[val_station$value==1 , ]
	
	# petit bug dans le fichier: certains pesticide sont répétés
	val_station=unique(val_station[,-which(colnames(val_station)=="STATUT")])
	
	# reorder the levels of LB_PARAMETRE en fonction de fonction
	val_station$LB_PARAMETRE <- factor(val_station$LB_PARAMETRE, levels = val_station$LB_PARAMETRE[order(val_station$fonction)])
	
	# On réalise le barplot - GGPLOT2
	library(ggplot2)
	library(RColorBrewer)
	ggplot(val_station , aes(x=LB_PARAMETRE , y=MA_MOY, fill=fonction)) + geom_bar(stat = "identity") + 
		ylim(0,0.2)+
		xlab("")+
		theme() + 
		#scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(nlevels(droplevels(val_station$LB_PARAMETRE)))) +
	  	annotate("segment", x = 0, xend = nrow(val_station), y = 0.1, yend = 0.1, colour = "red") +
		ylab("Concentration en pesticide (Microgrammes / L)")+
		annotate("text", x = 1.6, y = 0.12, label = "Seuil autorisé", col="red")+
		theme( axis.text.x = element_text(angle = 75, hjust = 1))
	ggplotly()
	
	# Test en plotly
	my_text=paste(val_station$LB_PARAMETRE,"<br>Concentration observée: ",val_station$MA_MOY,"Mg/L","<br>Famille: ",val_station$CODE_FAMILLE,sep="")
	plot_ly(val_station, x = ~LB_PARAMETRE, y = ~MA_MOY, type = 'bar', color=~fonction, text=~my_text, hoverinfo="text",
		marker = list(line = list(color = 'white', width = 2))
		)  %>%
  	# value of risk
  	add_annotations(x=2, y=0.12, text = c("<br>Seuil autorisé: 0.1 Mg/L"), xref = "x",yref = "y",showarrow = FALSE, align="left",font=list(color="red")) %>%
  	# Layout
  	layout(
  		 title = '',
         xaxis = list(title = ''), 
         yaxis = list(title = 'Concentration observée (Mg/L)' , range=c(0,0.26) ),
         margin = list(b=150),
         barmode = 'stack',
         paper_bgcolor = 'rgba(245, 246, 249, 1)',
         plot_bgcolor = 'rgba(245, 246, 249, 1)',
         shapes = list(
            list(type = "line", fillcolor = "red", line = list(color = "red"), opacity = 0.8,  x0 = 0, x1 = nrow(val_station), xref = "x", y0 = 0.1, y1 = 0.1, yref = "y")
            )
        )
	
 #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#








#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# --- CARTE CHLOROPLETH
#-----------------------------------------------------------------------------

	library(tidyr)
	pest_db_filtered=pest_db
	departements@data=departements@data[ , 1:4]
	resum_dep_quick=resum_dep_quick %>% spread(Group.2, x)
	departements@data=merge(departements@data , resum_dep_quick , by.x=1 , by.y=1 , all.x=T)
	inFile=departements


		p=leaflet(inFile) %>%
			setView(lng = 2,34, lat = 47, zoom = 5) %>% 
			addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity =0.5)) %>% 
			addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = colorNumeric( palette = "YlOrRd", domain = inFile@data$y2007)(inFile@data$y2007) , group="2007") %>%
			addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = colorNumeric( palette = "YlOrRd", domain = inFile@data$y2008)(inFile@data$y2008) , group="2008") %>%
			addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = colorNumeric( palette = "YlOrRd", domain = inFile@data$y2009)(inFile@data$y2009) , group="2009") %>%
			addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = colorNumeric( palette = "YlOrRd", domain = inFile@data$y2010)(inFile@data$y2010) , group="2010") %>%
			addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = colorNumeric( palette = "YlOrRd", domain = inFile@data$y2011)(inFile@data$y2011) , group="2011") %>%
			addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = colorNumeric( palette = "YlOrRd", domain = inFile@data$y2012)(inFile@data$y2012) , group="2012") %>%
			addLayersControl(baseGroups = c("2007","2008","2009","2010","2011","2012") , options = layersControlOptions(collapsed = FALSE) )
		p

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#










#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# --- LINE PLOT SHEET3
#-----------------------------------------------------------------------------



	# on récupère le fichier département:
	my_data=pest_departement
	
	#
	my_data=my_data[which(my_data$Niveau=="Ctot"), ] 
	
	# Ajout nom de la région
	link=unique(departements@data[,c(1,3)])
	AA=merge(my_data, link , by.x="NUM_DEP" , by.y="Numero" , all.x=T)
	
	# Racourcissement noms de régions:
	#AA$Region=gsub("Alsace-Champagne-Ardenne-Lorraine","Als.-Champ.-Ard.-Lor.",AA$Region)
	#AA$Region=gsub("Aquitaine-Limousin-Poitou-Charentes","Aqu.-Limou.-P. Charentes",AA$Region)
	#AA$Region=gsub("Languedoc-Roussillon-Midi-Pyrénées","L. Roussilon - M. Pyrénées",AA$Region)
	
	# Graphique en plotly
	p=ggplot(AA , aes(x=ANNEE , y=VALEUR , color=NUM_DEP)) + geom_line() + facet_wrap(~Region, ncol=3) + geom_hline(aes(yintercept=0.1), col="red") + theme(legend.position="none" , axis.text.x = element_text(size=6)) + xlab("") + ylab("")
	ggplotly(p)
	
	})





#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#




