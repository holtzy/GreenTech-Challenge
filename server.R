

		################################################
		#
		#		THE GREEN-TECH CHALLENGE
		#
		###############################################



# OPEN THE SHINY SERVER
shinyServer(function(input, output, session) {


#-----------------------------------------------------------------------------
# --- PART0 : PERMETTRE LE PASSAGE DU HOME VERS UNE DES 4 SHEETS
#-----------------------------------------------------------------------------

  
# Lien vers tab accueil
observeEvent(input$link_to_tabpanel_accueil, {
    newvalue <- "accueil"
    updateTabItems(session, "menu", newvalue)
})
  
# Lien vers tab station
observeEvent(input$link_to_tabpanel_station, {
    newvalue <- "station"
    updateTabItems(session, "menu", newvalue)
})
  
# Lien vers tab evolution
observeEvent(input$link_to_tabpanel_evolution, {
    newvalue <- "evolution"
    updateTabItems(session, "menu", newvalue)
})

# Lien vers tab pesticide
observeEvent(input$link_to_tabpanel_pesticide, {
    newvalue <- "pesticide"
    updateTabItems(session, "menu", newvalue)
})
  
# Geoloc a l'accueil
observeEvent(input$geoloc_accueil, {
	newvalue <- "station"
    updateTabItems(session, "menu", newvalue)
    js$geoloc()
})



#-----------------------------------------------------------------------------
# --- SHEET2 : CARTE LEAFLET DES STATIONS?
#-----------------------------------------------------------------------------


# J'initialise un objet: il va récupérer les positions et infos des cliques des gens:
data_of_click <- reactiveValues(clickedMarker=NULL)

# Je fais ma carte leaflet avec les stations:
output$map <- renderLeaflet({
 
	# create text for popup window:
	my_text=paste("<b>Station de <b>", station$NOM_COM, " (",  station$NUM_COM ,")"  ,    "<br/>"    ,   "Altitude: "   ,    station$ALTITUDE  ,  "<br/>"   ,  "Nom: "  ,  station$CD_STATION  ,  sep="")
	
	# Je récupère la zonne sélectionnée par un utilisateur? et je chope ses coord GPS:
	if(input$target_zone!="Ex: France"){
		target_pos=geocode(input$target_zone)
		}
	
	# plot
	leaflet(data=station) %>% 
  	setView(lng = ifelse(input$target_zone=="Ex: France" , 2.398 , target_pos$lon) , lat = ifelse(input$target_zone=="Ex: France" , 47.08 , target_pos$lat), zoom = ifelse(input$target_zone=="Ex: France" , 5 , 12) ) %>% 
  	addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
  	addCircleMarkers(~LONG, ~LAT, layerId = ~CD_STATION, radius=8 , color="black",  fillColor=~colorQuantile("YlOrRd", station$ALTITUDE)(ALTITUDE), stroke = TRUE, fillOpacity = 0.8 , popup = my_text, clusterOptions = markerClusterOptions() )
 })


# Observer qui permettra de récupérer la valeur du click et de la réutiliser dans n'importe quel graphique par la suite
observeEvent(input$map_marker_click,{
	data_of_click$clickedMarker <- input$map_marker_click
	})

  # Zoom in on user location if given
observe({
	if(!is.null(input$lat)){
		map <- leafletProxy("map")
		dist <- 0.05
		lat <- input$lat
		lng <- input$long
		map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
	}
})
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#







#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET2 : OBJET REACTIF AVEC LES RELEVES DE LA STATION CLIQUEE
#-----------------------------------------------------------------------------

current_station=reactive({

	# Ou plutot chope la sur la carte:
	my_station=data_of_click$clickedMarker$id
	if(is.null(my_station)){my_station="03812X0025/S1"}
	val_station=pest_db[which(pest_db$CD_STATION==my_station) , ] 
	return(val_station)
	
})

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#









#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET2 : EVOLUTION PESTICIDE : STREAMGRAPH
#-----------------------------------------------------------------------------

# Ce chunk permet de fabriquer un streamgraph qui représente l'évolution des concentrations en pesticide pour une station choisie.
# On peut Observer le streamgraph en absolut ou en relatif
# On peut aussi aggréger en fonction de la famille de pesticide, ou de la fonction du pesticide.
# TODO: ajouter un système pour que le user puisse chercher par commune / dep /
output$graph2 <- renderStreamgraph({

	
 	# Récupération des infos de la station d intéret via la réactive de ci dessus
 	val_station=current_station()
 	
 	# Si je n ai pas au moins 2 années, alors pas de StreamGraph.
	validate(
      need(length(unique(val_station$ANNEE)) > 1, "Au moins 2 années d'observation sont nécessaires pour analyser l'évolution des concentrations!")
    ) 	
    	
	# récupération de l'input d'aggrégation:
	var_to_aggregate=switch(input$agrege, "pesticide" = "pesticide", "familles de pesticide" = "CODE_FAMILLE", "fonctions de pesticide" = "CODE_FONCTION")

	# récupération de l'input absolue / relatif
	my_offset=switch(input$propor, "absolu" = "zero", "relatif" = "expand")
	      
	# Si on aggrege pas -> on regarde l'évolution de chaque pesticide
	if( var_to_aggregate == "pesticide"){
		val_station$MA_MOY=round(val_station$MA_MOY,2)
		return(streamgraph(val_station, key="LB_PARAMETRE", value="MA_MOY", date="ANNEE" , offset=my_offset) %>% sg_axis_x(tick_interval = 1))
		}
		
	# Si on aggrege par famille:
	if( var_to_aggregate == "CODE_FAMILLE"){
		v=aggregate(val_station$MA_MOY, by=list(val_station$ANNEE, val_station[,var_to_aggregate]) , max)
		colnames(v)=c("ANNEE" , "TYPE" , "MOY")		
		v$MOY=round(v$MOY , 2)	
		# Ya plus qua ploter
		return(streamgraph(v, key="TYPE", value="MOY", date="ANNEE" , offset=my_offset) %>% sg_axis_x(tick_interval = 1))
		}
		
	# Si on aggrege par fonction:	
	if( var_to_aggregate == "CODE_FONCTION"){
		# On transforme les colonnes de fonction en une seule colonne de fonction
		colnames(val_station)[which(colnames(val_station)=="Reg_croiss")]="Régulateur de croissance"
		val_station=val_station %>% gather(fonction, value, 12:22)
		val_station=val_station[val_station$value==1 , ]
		
		# On aggrege = moyenne pour chaque année et chaque famille / fonction
		v=aggregate(val_station$MA_MOY, by=list(val_station$ANNEE, val_station$fonction ) , max)
		colnames(v)=c("ANNEE" , "TYPE" , "MOY")		
		v$MOY=round(v$MOY , 2)	
		# Ya plus qua ploter
		return(streamgraph(v, key="TYPE", value="MOY", date="ANNEE" , offset=my_offset) %>% sg_axis_x(tick_interval = 1))
		}

})


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#











#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET2 : BARPLOT VALEUR SEUIL
#-----------------------------------------------------------------------------

output$barplot_val_seuil <- renderPlotly({

 	# Récupération des infos de la station d intéret via la réactive de ci dessus
 	val_station=current_station()
 		
	# On récupère la dernière année seulement & on garder les valeurs supérieurs à 0.025 seulement.
	last_year=max(val_station$ANNEE)
	val_station=val_station[val_station$ANNEE==last_year , ]
	val_station=val_station[val_station$MA_MOY>0.025, ]
	
   # je retourne pas de graphique si pas de mol:
   #if(nrow(val_station)<1){p=ggplot()+geom_blank() ; return(ggplotly(p))}

	# Si pas de molécule au dessus de ce seuil, alors on met un message d'explication:
	validate(
      need(nrow(val_station) > 1, paste("La dernière année de prélèvement est ",last_year,". Ces prélèvements n'ont donnés aucun relevé supérieur à 0.025 Mg / L","\n","\n","\n","\n","-",sep=""))
    ) 
    	     
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
		
	#  plotly
	my_text=paste(val_station$LB_PARAMETRE,"<br>Concentration observée: ",val_station$MA_MOY,"Mg/L","<br>Famille: ",val_station$CODE_FAMILLE,sep="")
	p=plot_ly(val_station, x = ~LB_PARAMETRE, y = ~MA_MOY, type = 'bar', color=~fonction, text=~my_text, hoverinfo="text",
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
        return(p)		

	})
 

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#






#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET2 : INFOBOX POUR LES NEUNEU
#-----------------------------------------------------------------------------


# nbr année et nbr mol testés
output$infobox1 <- renderInfoBox({
 	val_station=current_station()
	nb_year=length(unique(val_station$ANNEE))
	nb_mol=nrow(val_station)
	infoBox("Relevés effectués", paste(nb_year," Années",sep=""), paste(nb_mol," Molécules testées",sep=""), icon=icon("info"), color = "teal", fill = TRUE )
	})

# nbr de mol au dessus du seuil
output$infobox2 <- renderInfoBox({
 	val_station=current_station()
 	names=unique(val_station[val_station$MA_MOY>val_station$NORME_DCE , "LB_PARAMETRE"])
	number=length(names)
	if(number==0){
		infoBox("Pesticide en excès", number, names, "rer", icon = icon("thumbs-up"), color = "green", fill = TRUE )
	}else{
		infoBox("Pesticide en excès", number, paste(names, collapse=', '), "rer", icon = icon("warning"), color = "red", fill = TRUE )	
	}
	
	})
	
# evolution des quantités:
output$infobox3 <- renderInfoBox({

 	val_station=current_station()

 	if(length(unique(val_station$ANNEE))<2){
 		infoBox("Une seule année de mesure", "Pas de données d'évolution", icon = icon("question"), color = "light-blue", fill = TRUE )
 	}
 	else{

		last2=sort(unique(val_station$ANNEE), decreasing=T)[c(1,2)]
		tmp=val_station[which(val_station$ANNEE%in%last2) , ]
		res=aggregate(tmp$MA_MOY , by=list(tmp$ANNEE) , sum, na.rm=T)
		res=res[order(res$Group.1) , ]
		evol=(res[2,2]-res[1,2])/res[1,2]*100
		evol=round(evol,2)		

		if(evol>5){ return(infoBox("En hausse", paste(evol," % d'augmentation"), icon = icon("arrow-up"), color = "red", fill = TRUE ))}
		if(evol<(-5)){ return(infoBox("En baisse", paste(evol," % de baisse",sep=""), icon = icon("arrow-down"), color = "green", fill = TRUE ))}
		if(evol>=(-5) & evol<=5){ return(infoBox("Stable", "Très peu de variation observée", icon = icon("window-minimize"), color = "light-blue", fill = TRUE ))}

		}
	})

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#








#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET3 : CARTE CHLOROPLETH DE FRANCE!
#-----------------------------------------------------------------------------

# RECUPERATION DES DONNES CHOISIES
inFile=reactive({

	# Selon l unité de surface choisie, on récupère un fichier
	my_data=switch(input$geo_unit, "Station"=pest_station, "Departement"=pest_departement, "Région"=pest_region, "Masse d'eau"=pest_ME)
	
	# Ensuite, on va récupérer le sous ensemble de pesticides choisis.
	# Attention, pas le meme widget pour le mode neuneu que pour le mode experts
	#if(input$details_visibility==TRUE){
		if(input$choix_aggregat=="Pesticide"){ my_data=my_data[which(my_data$LB_PARAMETRE==input$choix_pesticide) , ]}
		if(input$choix_aggregat=="Famille"){ my_data=my_data[which(my_data$CODE_FAMILLE==input$choix_famille) , ]}
		if(input$choix_aggregat=="Tous"){ my_data=my_data[which(my_data$Niveau=="Ctot"), ] }
		if(input$choix_aggregat=="Fonction"){ my_data=my_data[which(my_data$FONCTION==input$choix_fonction), ]}
	#	}
#~ 	if(input$details_visibility==FALSE){
#~ 		if(input$choix_aggregat_neuneu=="Tous"){ my_data=my_data[which(my_data$Niveau=="Ctot"), ] }
#~ 		if(input$choix_aggregat_neuneu=="Atrazine"){ my_data=my_data[which(my_data$LB_PARAMETRE=="Atrazine") , ]}
#~ 		if(input$choix_aggregat_neuneu=="Glyphosate"){ my_data=my_data[which(my_data$LB_PARAMETRE=="Glyphosate") , ]}
#~ 		if(input$choix_aggregat_neuneu=="AMPA"){ my_data=my_data[which(my_data$LB_PARAMETRE=="AMPA") , ]}
#~ 		if(input$choix_aggregat_neuneu=="Bentazone"){ my_data=my_data[which(my_data$LB_PARAMETRE=="Bentazone") , ]}
#~ 		}
		
	# Et il y a plus qu a merger ca avec le shape file associé
	if(input$geo_unit=="Station"){ 
		return(my_data)
		}
	if(input$geo_unit=="Departement"){	
		my_data$ANNEE=paste("y",my_data$ANNEE,sep="")
		my_data=my_data %>% spread(ANNEE,VALEUR)
		departements@data=departements@data[ , 1:3] ; 
		departements@data=merge(departements@data , my_data , by.x=1 , by.y=1 , all.x=T)
		return(departements)
		}
	if(input$geo_unit=="Région"){	
		my_data$ANNEE=paste("y",my_data$ANNEE,sep="")
		my_data=my_data %>% spread(ANNEE,VALEUR)
		#region@data=region@data[ , 1] ; 
		region@data=merge(region@data[,1] , my_data , by.x=1 , by.y=1 , all.x=T)
		return(region)
		}
	if(input$geo_unit=="Masse d'eau"){	
		my_data$ANNEE=paste("y",my_data$ANNEE,sep="")
		my_data=my_data %>% spread(ANNEE,VALEUR)
		ME@data=ME@data[ , 1:26] ; 
		ME@data=merge(ME@data , my_data , by.x="CdMssDE" , by.y="ME" , all.x=T) 
		ME=ME[which(ME@data$SrfcAff>2000),]
		return(ME)
		}
	
	})



# FABRICATION CARTE CHLOROPLETH
output$map2 <- renderLeaflet({
	
	# Je récupère le tableau de calcul d aggrégat
	inFile=inFile()
	
	# 1/ ---- Cas du département / Région / ME:
	if(input$geo_unit!="Station"){
    
    # Create text for popup window:
	  if(input$geo_unit=="Departement"){
	    
	    my_text.2007=paste("<b>", inFile@data$Nom, "</b>", "<br/>", round(inFile@data$y2007,2), " &micro;g/l", sep="")
	    my_text.2008=paste("<b>", inFile@data$Nom, "</b>", "<br/>", round(inFile@data$y2008,2), " &micro;g/l", sep="")
	    my_text.2009=paste("<b>", inFile@data$Nom, "</b>", "<br/>", round(inFile@data$y2009,2), " &micro;g/l", sep="")
	    my_text.2010=paste("<b>", inFile@data$Nom, "</b>", "<br/>", round(inFile@data$y2010,2), " &micro;g/l", sep="")
	    my_text.2011=paste("<b>", inFile@data$Nom, "</b>", "<br/>", round(inFile@data$y2011,2), " &micro;g/l", sep="")
	    my_text.2012=paste("<b>", inFile@data$Nom, "</b>", "<br/>", round(inFile@data$y2012,2), " &micro;g/l", sep="")
	    
	  }else{
	    
	    my_text.2007=paste("<b>", inFile@data$x, "</b>", "<br/>", round(inFile@data$y2007,2), " &micro;g/l", sep="")
	    my_text.2008=paste("<b>", inFile@data$x, "</b>", "<br/>", round(inFile@data$y2008,2), " &micro;g/l", sep="")
	    my_text.2009=paste("<b>", inFile@data$x, "</b>", "<br/>", round(inFile@data$y2009,2), " &micro;g/l", sep="")
	    my_text.2010=paste("<b>", inFile@data$x, "</b>", "<br/>", round(inFile@data$y2010,2), " &micro;g/l", sep="")
	    my_text.2011=paste("<b>", inFile@data$x, "</b>", "<br/>", round(inFile@data$y2011,2), " &micro;g/l", sep="")
	    my_text.2012=paste("<b>", inFile@data$x, "</b>", "<br/>", round(inFile@data$y2012,2), " &micro;g/l", sep="")
	    
	  }
	  
		# Détermination du threshold pour la palette de couleur:
		#if(input$details_visibility==TRUE){ 
			my_threshold=ifelse(input$choix_aggregat=="Pesticide",0.1,0.5)
		#}
#~ 		if(input$details_visibility==FALSE){ 
#~ 			my_threshold=ifelse(input$choix_aggregat_neuneu=="Tous",0.5,0.1)
#~ 		}
    
    # On crée la légende avec la fonction chargée dans l'environement global
	  legend <- getPalette(inFile=inFile, threshold=my_threshold) 
	
		# Je créé la carte
		p=leaflet(inFile) %>%
		  setView(lng = 2,34, lat = 47, zoom = 5) %>% 
		  addProviderTiles("Esri.WorldShadedRelief", options = providerTileOptions(opacity =0.5)) %>%  # Thunderforest.Pioneer
		  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = ~legend$palette(inFile@data$y2007) , group="2007", popup = my_text.2007) %>%
		  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = ~legend$palette(inFile@data$y2008) , group="2008", popup = my_text.2008) %>%
		  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = ~legend$palette(inFile@data$y2009) , group="2009", popup = my_text.2009) %>%
		  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = ~legend$palette(inFile@data$y2010) , group="2010", popup = my_text.2010) %>%
		  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = ~legend$palette(inFile@data$y2011) , group="2011", popup = my_text.2011) %>%
		  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5, color = ~legend$palette(inFile@data$y2012) , group="2012",popup = my_text.2012) %>%
		  addLegend(position = 'bottomleft', ## choose bottomleft, bottomright, topleft or topright
		            colors = legend$colourCodes, 
		            labels = legend$bins,  ## legend labels (only min and max)
		            opacity = 0.6,      ##transparency again
		            title = "Concentration<br>(en &micro;g/l)")  %>%  ## title of the legend
		  addLayersControl(baseGroups = c("2007","2008","2009","2010","2011","2012") , options = layersControlOptions(collapsed = FALSE))
	return(p)
	}
		

	# 2/ --- Cas de la station
	if(input$geo_unit=="Station"){

		data2007=inFile[which(inFile$ANNEE=="2007" & !is.na(inFile$VALEUR) ), ]
		data2008=inFile[which(inFile$ANNEE=="2008" & !is.na(inFile$VALEUR) ), ]
		data2009=inFile[which(inFile$ANNEE=="2009" & !is.na(inFile$VALEUR) ), ]
		data2010=inFile[which(inFile$ANNEE=="2010" & !is.na(inFile$VALEUR) ), ]
		data2011=inFile[which(inFile$ANNEE=="2011" & !is.na(inFile$VALEUR) ), ]
		data2012=inFile[which(inFile$ANNEE=="2012" & !is.na(inFile$VALEUR) ), ]
		
		# Carto leaflet:
		q=leaflet() %>% 
			setView(lng = 2,34, lat = 47, zoom = 5) %>% 
		  	addProviderTiles("Esri.WorldShadedRelief", options = providerTileOptions(opacity =0.5)) %>%  # Thunderforest.Pioneer
			addCircleMarkers(data=data2007 , ~LONG, ~LAT, radius=2 , color=colorNumeric( palette ="YlOrRd", domain=data2007$VALEUR)(data2007$VALEUR), stroke = FALSE, fillOpacity = 0.8 , popup = "" , group="2007" ) %>%
			addCircleMarkers(data=data2008 , ~LONG, ~LAT, radius=2 , color=colorNumeric( palette ="YlOrRd", domain=data2008$VALEUR)(data2008$VALEUR), stroke = FALSE, fillOpacity = 0.8 , popup = "" , group="2008" ) %>%
			addCircleMarkers(data=data2009 , ~LONG, ~LAT, radius=2 , color=colorNumeric( palette ="YlOrRd", domain=data2009$VALEUR)(data2009$VALEUR), stroke = FALSE, fillOpacity = 0.8 , popup = "" , group="2009" ) %>%
			addCircleMarkers(data=data2010 , ~LONG, ~LAT, radius=2 , color=colorNumeric( palette ="YlOrRd", domain=data2010$VALEUR)(data2010$VALEUR), stroke = FALSE, fillOpacity = 0.8 , popup = "" , group="2010" ) %>%
			addCircleMarkers(data=data2011 , ~LONG, ~LAT, radius=2 , color=colorNumeric( palette ="YlOrRd", domain=data2011$VALEUR)(data2011$VALEUR), stroke = FALSE, fillOpacity = 0.8 , popup = "" , group="2011" ) %>%
			addCircleMarkers(data=data2012 , ~LONG, ~LAT, radius=2 , color=colorNumeric( palette ="YlOrRd", domain=data2012$VALEUR)(data2012$VALEUR), stroke = FALSE, fillOpacity = 0.8 , popup = "" , group="2012" ) %>%
			addLayersControl(baseGroups = c("2007","2008","2009","2010","2011","2012") , options = layersControlOptions(collapsed = FALSE) )
	return(q)	
	}
		
})

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#










#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET3: COURBE EVOLUTION DES PESTICIDES PAR REGION ET DEP
#-----------------------------------------------------------------------------

output$graph_evol_dep <- renderPlotly({

	# on récupère le fichier département:
	my_data=pest_departement
	
	# Ensuite, on va récupérer le sous ensemble de pesticides choisis
	if(input$choix_aggregat=="Pesticide"){ my_data=my_data[which(my_data$LB_PARAMETRE==input$choix_pesticide) , ]}
	if(input$choix_aggregat=="Famille"){ my_data=my_data[which(my_data$CODE_FAMILLE==input$choix_famille) , ]}
	if(input$choix_aggregat=="Tous"){ my_data=my_data[which(my_data$Niveau=="Ctot"), ] }
	if(input$choix_aggregat=="Fonction"){ my_data=my_data[which(my_data$FONCTION==input$choix_fonction), ]}

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








#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET3: BARPLOT DES PESTICIDES LES PLUS COURANT
#-----------------------------------------------------------------------------

# il va falloir en calculer un dans le 0_prepare_data
output$barplot_most_important_pesticides <- renderPlot({
	
	c=head(c[order(c$importance_pest , decreasing=T) , ])
	ggplot(c , aes(y=importance_pest , x=reorder(Group.1,importance_pest,decreasing=T), label=reorder(Group.1,importance_pest,decreasing=T))) + 
	geom_bar(stat = "identity", width=0.8, color="transparent",fill=rgb(0.3,0.5,0.9,0.8) ) + 
	scale_fill_brewer(palette = "YlOrRd") +
	coord_flip() + 
	xlab("") + 
	ylab("") + 
	theme(
		legend.pos="none", axis.ticks.y = element_blank(), axis.text.y = element_blank() , 
		panel.background=element_rect(color="grey"), panel.grid.major.y=element_blank(), 
		panel.grid.major.x=element_line(color="grey", size=0.3) , panel.grid.minor=element_blank(),
		plot.margin=unit(c(0,0,0,-7),"mm") , 
		legend.box.spacing = unit(0, "mm")
	)+
	geom_text(aes(y=0+50) , size=4, hjust = 0 , col="black")
	
	})

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#









#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET4: BUBBLE CHART DANGEROSITE DES PESTICIDES
#-----------------------------------------------------------------------------

output$bubblechart=renderPlotly({

	set.seed(123)

	# PREPARE DATA FROM PESTICIDE FILE:
	don=pesticide[which(pesticide$LB_PARAMETRE%in%pest_db$LB_PARAMETRE) , ]
	don=pesticide[!is.na(pesticide$LD50) , ]
	don=don[ , c(1,4,6,11,12,13,14:22,27,29)]
	don=don %>% gather(fonction, value, 7:15) 
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
		width = 800, height = 500
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


})


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#






#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------
# --- SHEET4: TREEMAP SHEET 4
#-----------------------------------------------------------------------------

output$treemap <- renderD3tree2({

	# prepare data (really quick)
	don=pesticide[which(pesticide$LB_PARAMETRE%in%pest_db$LB_PARAMETRE) , ]
	don=don[ , c(1,14:22,29)]
	don=don %>% gather(fonction, value, 2:(ncol(don)-1) )
	don=don[don$value==1 & don$importance_pest>0 , ]

	d3tree2(treemap(don,
				index=c("fonction", "LB_PARAMETRE"),
				vSize=switch(input$treemapchoice, "nombre de pesticides" = "value", "quantité mesurée" = "importance_pest"),
				type="index", 
				fontsize.labels=30,
				fontcolor.labels="black",
				fontface.labels=2,
				bg.labels="white",
				draw=FALSE
	) , rootname = "Les grandes familles de Pesticides")

})



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#







# Close the shiny app:
})


  
  

