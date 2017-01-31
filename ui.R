

		################################################
		#
		#		SUPERVIZ
		#
		###############################################




# On réalise le ui.R avec un format Shiny Dashboard.Séparé en 3 grandes parties
shinyUI(dashboardPage(skin="blue",
	
	
# -- 1: header
dashboardHeader(
	title = "Superviz",
	tags$li(class = "dropdown",
		#tags$a(href="http://snap.uaf.edu", target="_blank", 
			img(height = "50px", alt="logo Superviz", src="logo.png")
		#)
	)
),



	
	
# -- 2: Sidebar
dashboardSidebar(
	
	# Menu
	sidebarMenu(id="menu",
		menuItem("Accueil", tabName = "accueil", icon = icon("home")),
		menuItem("Près de chez moi", tabName = "station", icon = icon("map-marker")),
		menuItem("Ailleurs en France", tabName = "evolution", icon = icon("map-o")),
		menuItem("Types de pesticides", tabName = "classification", icon = icon("book")),
		menuItem("Toxicité", tabName = "danger", icon = icon("warning")),
		menuItem("A propos", tabName = "apropos", icon = icon("info")),		

		# Boutton du mode expert
#~ 		div(style="text-align : -webkit-center",
#~ 			switchButton(inputId = "details_visibility", label = "Souhaitez-vous voir plus de graphiques experts?", value = FALSE, col = "GB", type = "ON")
#~ 		),
		
		# Logo et texte superviz		
		br(),br(),br(),br(),
		div(style="text-align : center; font-size: 9px",
			br(),
			img(src="logo_greentech.png", class='img-rounded', style="width: 40%"),
			br(),br(),
			HTML("Made with &hearts; by the Superviz team !"),
			br(),br(),br(),
			p("Partager ce site"),
			HTML("<div class='ssba ssba-wrap'>
				 <script>(function(d, s, id) {
					var js, fjs = d.getElementsByTagName(s)[0];
					if (d.getElementById(id)) return;
					js = d.createElement(s); js.id = id;
					js.src = '//connect.facebook.net/fr_FR/sdk.js#xfbml=1&version=v2.8';
					fjs.parentNode.insertBefore(js, fjs);
					}(document, 'script', 'facebook-jssdk'));
				 </script>
				 <script>
					window.onload = function() {
						fb_share.href ='http://www.facebook.com/share.php?url=' + encodeURIComponent(location.href);
						google_share.href = 'https://plus.google.com/share?url=' + encodeURIComponent(location.href);
						twitter_share.href = 'http://twitter.com/share?url=' + encodeURIComponent(location.href);
						linkedin_share.href = 'http://www.linkedin.com/shareArticle?mini=true&amp;url=' + encodeURIComponent(location.href);
						email_share.href = 'mailto:?subject=&amp;body=' + encodeURIComponent(location.href);
					}  
				</script>
				<a id='fb_share' data-site='' class='ssba_facebook_share' href='' target='_blank'>
				<img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/facebook.png' title='Facebook' class='ssba ssba-img' alt='Share on Facebook'>
				</a>
				<a id='google_share' data-site='' class='ssba_google_share' href='https://plus.google.com/share?url= + encodeURIComponent(location.href) target='_blank'>
				<img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/google.png' title='Google+' class='ssba ssba-img' alt='Share on Google+'>
				</a>
				<a id='twitter_share' data-site='' class='ssba_twitter_share' href='http://twitter.com/share?url=http://www.r-graph-gallery.com/&amp;text=+' target='_blank'>
				<img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/twitter.png' title='Twitter' class='ssba ssba-img' alt='Tweet about this on Twitter'>
				</a>
				<a id='linkedin_share' ata-site='linkedin' class='ssba_linkedin_share ssba_share_link' href='http://www.linkedin.com/shareArticle?mini=true&amp;url=http://eneo.fr/fr/home/' target='_blank'>
				<img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/linkedin.png' title='LinkedIn' class='ssba ssba-img' alt='Share on LinkedIn'>
				</a>
				<a id='email_share' data-site='email' class='ssba_email_share' href='mailto:?subject=&amp;body=%20http://www.r-graph-gallery.com/'>
				<img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/email.png' title='Email' class='ssba ssba-img' alt='Email this to someone'>
				</a>
				</div>"
			)
		)
	)
),
	
	
# --3: Body
dashboardBody(
	useShinyjs(),
	extendShinyjs(text = jsCode),
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "GreenTech_Challenge.css"),
		tags$link(rel = "stylesheet", type = "text/css", href = "button.css")
	),
	tabItems(
#-----------------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 1 : HOME PAGE
# ----------------------		
tabItem(tabName = "accueil",
	h2(align = "center", "Bienvenue sur ce portail d'informations concernant la qualité des masses d'eau souterraines
	en France et leur pollution par les pesticides"),
	br(),
	p(align="justify", "Un pesticide est une substance chimique utilisée pour lutter contre des organismes considérés
	comme nuisibles. C'est un terme générique qui rassemble les insecticides, les fongicides, les herbicides, les parasiticides.
	Ils s'attaquent respectivement aux insectes ravageurs, aux champignons, aux « mauvaises herbes » et aux vers parasites (Wikipédia).
	Sur ce site nous vous présentons de manière interactive la pollution des masses d'eau par ces pesticides."),
	br(),
	fluidRow(
		box(title = actionLink("link_to_tabpanel_station", "Quelle est la qualité de l'eau chez moi? Chez mes proches?"),
			width = 12, solidHeader = TRUE, collapsible = FALSE, background = NULL, status = "primary",
			helpText("Vous souhaitez connaître la qualité de l'eau dans une région précise de la France? C'est par ici !"),
			column(width=4,
				style="text-align : center",
				valueBox(1891, "stations de mesure en 2012", icon = icon("map-marker"), color="light-blue", width=12),
				actionButton("geoloc_accueil", "Voir ma ville", class="btn btn-primary center", style="width:90% ; color: white; font-weight: bold")
			),
			column(width=8,
				img(src="img_stations.png", class='img-rounded', style="width: 100%")
			)
		)
	),
	fluidRow(
		box(title = actionLink("link_to_tabpanel_evolution", "Quel est l'état des nappes d'eau en France?"),
			width = 6, solidHeader = TRUE, collapsible = FALSE, background = NULL, status = "warning",
			helpText("Venez découvrir la qualité de l'eau en France, année après année."),
			valueBox(6, "ans d'historique (2007-2012)", icon = icon("map-o"), color="yellow"),
			img(src="img_evolution.png", class='img-rounded', style="width: 60%")
		),
		box(title = actionLink("link_to_tabpanel_pesticide", "Que sont les pesticides? Est-ce dangereux?"),
			width = 6, solidHeader = TRUE, collapsible = FALSE, background = NULL, status = "success",
			helpText("Tout ce que vous avez toujours voulu savoir sur les pesticides !"),
			valueBox(1043, "pesticides répertoriés", icon = icon("book"), color="green"),
			img(src="img_bubblechart.png", class='img-rounded', style="width: 60%")
		)
	)
),
#-----------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 2 : STATIONS
# ----------------------
tabItem(tabName = "station",
	h2(align = "center", "Quel est l'état de l'eau prêt de chez vous ?"),
	br(),
	
	# ---- Row avec carte + controle widget
	fluidRow(
		tabBox(title = NULL, width=12,
			tabPanel("Stations",
				h3("Sélectionnez une station sur la carte pour visualiser la qualité de l'eau."),
				fluidRow(
					column(width=4,
						actionButton("geoloc", "Voir ma ville", class="btn btn-primary", style="width:100% ; color: white; font-weight: bold", onClick="shinyjs.geoloc()"),
						textInput("target_zone", "ou recherchez une ville..." , "Ex: France"),
						br(),
						infoBoxOutput("infobox2"),
						infoBoxOutput("infobox3")
					),
					column(width=8,
						leafletOutput("map")
					)
				)
			),
			tabPanel("Historique",
				h3("Evolution des pesticides sur la station sélectionnée"),
				fluidRow(
					column(width=4,
						infoBoxOutput("infobox1"),
						selectInput("agrege", "Evolution des ...", choices = c("pesticide","familles de pesticide","fonctions de pesticide") , selected ="fonctions de pesticide"),
						radioButtons("propor", "Valeur absolue ou proportion?", choices = c("absolu","relatif"), selected ="absolu" , inline = TRUE)
					),
					column(width=8,
						streamgraphOutput("graph2", width = "100%" )
					)
				)
			),
			tabPanel("Dernières mesures",
				h3("Mesure de la dernière année"),
				fluidRow(
					plotlyOutput("barplot_val_seuil")
				)
			)
		)
	)
),
#-----------------------------------------------------------------------------------------------











#-----------------------------------------------------------------------------------------------

# ----------------------
# SHEET 3 : EVOLUTION
# ----------------------
tabItem("evolution",
	h2(align = "center", "Observez l'état des pesticides en France"),
	br(),
	fluidRow(
		tabBox(title = NULL,
			width = 9,
			tabPanel("Carte de France",
				p(align="justify", "La carte ci-dessous présente les concentrations en pesticides observées dans chaque département français. Les couleurs permettent d'évaluer la gravité de la situation pour chacun d'entre eux : plus la couleur est foncée, plus les concentrations observées sont fortes."),
				br(),
				leafletOutput("map2")
			),
			tabPanel("Graphiques",
				plotlyOutput("graph_evol_dep" , height="600px")
			)
		),
		# mode expert
		#conditionalPanel(condition = "input.details_visibility == true",
		box(title = NULL,
			width = 3, solidHeader = FALSE, collapsible = FALSE, background = NULL, status = NULL,
			selectInput( "geo_unit", "Unité géographique:" , choices=c("Station", "Departement", "Région", "Masse d'eau") , selected="Departement"),
			p(align="justify", "Filtrer les données afin d'observer uniquement une fonction, une famille ou un pesticide 
			en particulier. Pour information, voici les 5 pesticides les plus répandus en France :"),
			plotOutput("barplot_most_important_pesticides" , height="194px"),
			selectInput( "choix_aggregat", "Types de pesticides:" , choices=c("Tous", "Famille","Fonction","Pesticide") , selected="Tous" ),
			conditionalPanel("input.choix_aggregat == 'Famille'", selectInput( "choix_famille", "Choisissez une famille", choices = levels(droplevels(pest_db$CODE_FAMILLE)) , selected ="Amides"  )),
			conditionalPanel("input.choix_aggregat == 'Fonction'", selectInput( "choix_fonction", "Choisissez une fonction", choices = colnames(pest_db)[18:28] , selected ="Insecticide"  )),
			conditionalPanel("input.choix_aggregat == 'Pesticide'", selectInput( "choix_pesticide", "Choisissez un pesticide", choices = levels(pest_db$LB_PARAMETRE) , selected ="Tritosulfuron"  ))
		)
		#),
		# mode neuneu
		#conditionalPanel(condition = "input.details_visibility == false",
#~ 		box(title = NULL,
#~ 			width = 3, solidHeader = FALSE, collapsible = FALSE, background = NULL, status = NULL,
#~ 			p(align="justify", "Visualisez les concentrations de tous les pesticides ou choisissez parmi les 5 principaux pesticides observés en France."),
#~ 			plotOutput("barplot_most_important_pesticides" , height="194px"),
#~ 			selectInput( "choix_aggregat", "Types de pesticides:" , choices=c("Tous", "Atrazine","Glyphosate","AMPA") , selected="Tous")
#~ 		)
		#)
	)
),
#-----------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 4 : CLASSIFICATION
# ----------------------
tabItem("classification",
	h2("Quels sont les différents types de pesticides ?", align = "center"),
	br(),
	p(align="justify", "Le graphique ci-dessous présente les types de pesticides observés en France.
	En fonction de l'option choisie, la taille de chaque bloc est proportionnelle soit au nombre de pesticides 
	dans cette famille (plus un bloc est gros, 	plus il y a de pesticides de ce type) soit à la quantité mesurée
	de ce type de pesticides dans les nappes (plus un bloc est gros, plus ce type de pesticides a été retrouvé
	dans les nappes)."),
	fluidRow(
		box(width=9,
			helpText("Zommer sur une famille pour voir les molécules qui la composent."),
			radioButtons("treemapchoice", "Voir les types de pesticides par", choices=c("nombre de pesticides","quantité mesurée"), selected="nombre de pesticides", inline=T),
			d3tree2Output("treemap")
		),
		#conditionalPanel(id="details_treemap", condition = "input.treemapchoice == 'quantité mesurée'",
		box(width=3,
			p(align="justify", "Essayez de trouver la quantité relative de ces pesticides dans le graphique...vous risquez d'être étonné !"),
			p(align="justify", "Le thiaméthoxam, la clothianidine et l'imidaclopride ont été suspendus en 2013. source: http://www.lemonde.fr/planete/article/2013/05/24/abeilles-interdiction-de-trois-pesticides-dans-l-ue-a-compter-du-1er-decembre_3416897_3244.html"),
			p(align="justify", "L’atrazine a été interdite dans l'Union européenne depuis 2003. Avec 76 millions de livres répandues chaque année, l'atrazine est le second herbicide le plus utilisé au Etats-Unis après le Roundup.source
			L’atrazine est donc suspectée d’être une des causes du phénomène récent d’explosion des infections fongiques et virales qui déciment les amphibiens sur toute la planète."),
			p(align="justify", "L’oxadixyl, fongicide interdit depuis 2003. Il était utilisé principalement pour son action contre les mildious."),
			p(align="justify", "La substance active du Cruiser, le thiamétoxam, intoxique les abeilles même à faible dose. source:http://www.actu-environnement.com/ae/news/ministere-agriculture-cruiser-osr-etude-inra-acta-cruiser-thiamethoxam-abeilles-15340.php4"),
			p(align="justify", "Le Piperonyl Butoxide est un synergisant c'est à dire un produit chimique ajouté aux pesticides pour accroître la toxicité des ingrédients actifs.")
		)
	)
),
#-----------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 5 : TOXICITE
# ----------------------
tabItem("danger",
	h2(align = "center", "Toxicité des pesticides"),
	br(),
	p(align="justify", "La toxicité est mesurée par le LD50, elle correspond à la quantité de substance ingérée qui 
	tue 50% de l'échantillon test. L'unité est le mg/kg, milligrames de substances par kilogrammes de poids de l'individu."),
	br(),
	fluidRow(id="toxicite_row",
		tabBox(title=NULL, width=12,
			tabPanel("Lesquels sont les plus toxiques?",
				fluidRow(
					p(align="justify", "Sur ce graphique vous pouvez zoomer sur une zone (cliquer-glisser). Cliquez sur la légende pour faire 
					apparaitre ou disparaitre les familles."),
					br(),
					plotlyOutput("bubblechart")
				)
			),
			tabPanel("Effets sur la santé humaine",
				fluidRow(
					column(width=6,
						img(src = "PesticideEffectsOnBody_graph.png", class='img-rounded', style="width: 100%")
					),
					column(width=6,
						p(align="justify", "Les personnes exposées aux pesticides ont plus de risque de développer de nombreuses 
						maladies que les autres : cancer, malformations congénitales, problèmes d’infertilité, problèmes neurologiques 
						ou encore système immunitaire affaibli sont plus fréquent chez eux !"),
						p(align="justify", "75% des études menées indiquent une relation positive entre l'exposition aux pesticides et l'atteinte 
						par un lymphome (cancer des lymphocytes). source"),
						p(align="justify", "> 200 000 décès sont causés par des organophosphates"),
						p(align="justify", ">90 substances cancérigènes probables ou possibles recensées par l'UE. source"),
						valueBox("Top 5 des cancérigènes", HTML("
							Glyphosate (Roundup)<br>
							Malathion<br>
							Diazinon (Spectracide)<br>
							Parathion<br>
							Tétrachlorvinphos<br>
							Epoxiconazole<br>
							DDT, interdit en France<br>
							"),
							icon = icon("book"), color="red"
						),
						valueBox("Top 4 des perturbateurs endocriniens", HTML("
							Chlorotoluron (Pestanal)<br>
							Dimoxystrobin<br>
							Epoxiconazole<br>
							ProfoxydimChloro<br>
							"),
							icon = icon("book"), color="orange"
						),
						valueBox("Top 5 des immunotoxiques", HTML("
							Atrazine (AAtrex, Spectracide)<br>
							2,4-D (Une des deux molécules de l'Agent Orange)<br>
							de nombreux organophosphates (gaz sarin par exemple)<br>
							de nombreux organochlorés (gaz moutarde par exemple)<br>
							de nombreux carbamates<br>
							"),
							icon = icon("book"), color="yellow"
						)
					)
				)
			),
			tabPanel("Autres effets sur l'environnement",
				fluidRow(
					column(width=5,
						img(src = "abeilles_dead.png", class='img-rounded', style="width: 50%")
					),
					column(width=7,
						p(align="justify", "Les organophosphates, neonicotinoïdes et les carbamates provoquent des décès massifs parmi les abeilles."),
						p(align="justify", "De nombreux insecticides récents sont des néonicotinoïdes:
						Imidacloprid (Confidor, Admire), Thiamethoxam (Cruiser, Actara), Clothianidin (Dantop)
						Acetamiprid  (Mospilan), Thiacloprid (Calypso), Dinotefuran (Venom), Nitenpyram (Capstar, Guardian)")
					)
				),
				fluidRow(
					column(width=5,
						img(src = "oeuf.jpg", class='img-rounded', style="width: 30%")
					),
					column(width=7,
						p(align="justify", "Le DDT provoque une réduction de l'épaisseur de la coquille des oiseaux jusqu'à 90%. source.")
					)
				)
			)
		)
	)
),
#~ 	valueBox("Top des organophosphastes", HTML("
#~ 		malathion<br>
#~ 		parathion<br>
#~ 		diazinon<br>
#~ 		fenthion<br>
#~ 		dichlorvos<br>
#~ 		chlorpyrifos (Lorsban, Durap)<br>
#~ 		ethion<br>
#~ 		trichlorfon<br>
#~ 		"),
#~ 		icon = icon("book"), color="green"
#~ 	),
#~ 	valueBox("Top des organochlorés", HTML("
#~ 		DDT<br>
#~ 		aldrin<br>
#~ 		dieldrin<br>
#~ 		endrin<br>
#~ 		heptachlor<br>
#~ 		chlordane<br>
#~ 		endosulfan (Paser, Thiodan)<br>
#~ 		"),
#~ 		icon = icon("book"), color="green"
#~ 	)

			
			

#-----------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 5 : A PROPOS
# ----------------------
tabItem("apropos",
	h2("A propos"),
	br(),
	p(align="justify", "Ce site a été réalisé dans le cadre d'un appel à projets organisé par le Ministère de l'Environnement.
	Les données présentées sont  disponibles en OpenData à cette adresse."),
	hr(),br(),
	h2("Qui sommes nous"),
	p(align="justify","JCex, Charlouz, Guiguix, Yano"),
	hr(),br(),
	h2("Utilisation"),
	p(align="justify", "L'app peux etre utilisée sur le web. Lien :"),
	p(align="justify", "Le code est disponible sur github"),
	hr(),br(),
	h2("Les données"),
	p(align="justify", "Disponible sur le site du ministère de l'environnement")
)

#-----------------------------------------------------------------------------------------------






) # end tabItems
) # end Dashboard body
)) #end Dashboard page and shinyUI








































