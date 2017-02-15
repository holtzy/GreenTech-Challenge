
		################################################
		#
		#		THE GREENTECH CHALLENGE: access the app
		#
		###############################################


# The Shiny app can be used using several ways !
	
	# 1: en local sur son ordi (attention, il faut installer la librairie Shiny) :
	cd ~/Dropbox ; R ; library(shiny) ; runApp("GreenTech_Challenge")	 

	# 2: en ligne à l'adresse web suivante:
	www.agap-sunshine.inra.fr/holtz-apps/GreenTech_Challenge/

	# 3: via le compte github
	TODO





# Pour mettre l'appli en ligne sur le serveur de l'équipe ge2pop de l'inra:

	# Connection au serveur:
	ssh holtz@147.100.164.72
	?LOF@L~$QPt=diTIhXg5u<EA3

	# Mise en ligne de l'appli:
	cd ~/Dropbox
	scp -r GreenTech_Challenge/* holtz@147.100.164.72:/srv/shiny-server/holtz-apps/GreenTech_Challenge


