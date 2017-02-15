

		################################################
		#
		#		SUPERVIZ
		#
		###############################################




# On réalise le ui.R avec un format Shiny Dashboard.
shinyUI(dashboardPage(skin="red",
	
	
# -- 1: header
dashboardHeader(
	title = div("Superviz", br(), img(height = "40px", alt="logo Superviz", src="logo.png")),
	tags$li(class = "dropdown",
		img(height = "50px", alt="logo Superviz", src="logo.png")
	)
),



	
# -- 2: Sidebar
dashboardSidebar(
	
	# Menu
	sidebarMenu(id="menu",
		menuItem("Accueil", tabName = "accueil", icon = icon("home")),
		menuItem("Près de chez moi", tabName = "station", icon = icon("map-marker")),
		menuItem("Ailleurs en France", tabName = "evolution", icon = icon("map-o")),
		menuItem("Origine et types de pesticides", tabName = "classification", icon = icon("book")),
		menuItem("Toxicité", tabName = "danger", icon = icon("medkit")),
		menuItem("A propos", tabName = "apropos", icon = icon("info")),		
		
		# Logo et texte superviz		
		br(),br(),br(),br(),
		div(style="text-align : center; font-size: 9px",
			br(),
			img(src="logo_greentech.png", class='img-rounded', style="width: 40%"),
			br(),br(),
			HTML("Made with &hearts; by the Superviz team !"),
			br(),br(),br(),
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
				<img src='facebook.png' title='Facebook' class='ssba ssba-img' alt='Share on Facebook'>
				</a>
				<a id='google_share' data-site='' class='ssba_google_share' href='' target='_blank'>
				<img src='google.png' title='Google+' class='ssba ssba-img' alt='Share on Google+'>
				</a>
				<a id='twitter_share' data-site='' class='ssba_twitter_share' href='' target='_blank'>
				<img src='twitter.png' title='Twitter' class='ssba ssba-img' alt='Tweet about this on Twitter'>
				</a>
				<a id='linkedin_share' ata-site='linkedin' class='ssba_linkedin_share ssba_share_link' href='' target='_blank'>
				<img src='linkedin.png' title='LinkedIn' class='ssba ssba-img' alt='Share on LinkedIn'>
				</a>
				<a id='email_share' data-site='email' class='ssba_email_share' href=''>
				<img src='email.png' title='Email' class='ssba ssba-img' alt='Email this to someone'>
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
	h2(align = "center", "Portail d'information sur les pesticides en France"),
	br(),
	p(align="justify", "Un pesticide est une substance chimique utilisée pour lutter contre des organismes considérés
	comme nuisibles. C'est un terme générique qui rassemble les insecticides, les fongicides, les herbicides, les parasiticides.
	Ils s'attaquent respectivement aux insectes ravageurs, aux champignons, aux « mauvaises herbes » et aux vers parasites.",
	a("(Wikipédia)", href="https://fr.wikipedia.org/wiki/Pesticide", target="_blank")),
	br(),
	fluidRow(
		box(title = actionLink("link_to_tabpanel_station", "Quelle est la qualité de l'eau chez moi? Chez mes proches?"),
			width = 12, height = "300px", solidHeader = TRUE, collapsible = FALSE, background = NULL, status = NULL,
			helpText("Vous souhaitez connaître la qualité de l'eau dans une région précise de la France? C'est par ici !"),
			column(width=4,
				style="text-align : center",
				br(),
				valueBox(length(unique(pest_db$CD_STATION[pest_db$ANNEE==max(pest_db$ANNEE)])), 
                 paste0("stations de mesure en ",max(pest_db$ANNEE)), icon = icon("map-marker"), color="red", width=12),
				actionButton("geoloc_accueil", "Voir ma ville", style="width:90% ; color: white; font-weight: bold; font-size:20px")
			),
			column(width=8,
				img(src="img_stations.png", class='img-rounded', style="width: 70%")
			)
		)
	),
	br(),
	fluidRow(
		box(title = actionLink("link_to_tabpanel_evolution", "Où trouve-t-on des pesticides en France?"),
			width = 4, height = "300px", solidHeader = TRUE, collapsible = FALSE, background = NULL, status = NULL,
			helpText("Venez découvrir la qualité de l'eau en France, année après année."),
			valueBox(length(unique(pest_db$ANNEE)), 
               paste0("ans d'historique (",min(pest_db$ANNEE),"-",max(pest_db$ANNEE),")"), icon = icon("map-o"), color="red"),
			img(src="img_evolution.png", class='img-rounded', style="width: 60%")
		),
		box(title = actionLink("link_to_tabpanel_classification", "Quels pesticides trouve-t-on en France?"),
			width = 4, height = "300px", solidHeader = TRUE, collapsible = FALSE, background = NULL, status = NULL,
			helpText("Tout ce que vous avez toujours voulu savoir sur les pesticides !"),
			valueBox(length(unique(pesticide$CD_PARAMETRE)), "pesticides répertoriés", icon = icon("book"), color="red"),
			img(src="img_treemap.png", class='img-rounded', style="width: 60%")
		),
		box(title = actionLink("link_to_tabpanel_danger", "Les pesticides sont-ils dangereux?"),
			width = 4, height = "300px", solidHeader = TRUE, collapsible = FALSE, background = NULL, status = NULL,
			helpText("On parle souvent des effets néfastes des pesticides, qu'en est-il réellement?"),
			valueBox(90, "substances cancérigènes", icon = icon("medkit"), color="red"),
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
	h2(align = "center", "Quel est l'état de l'eau près de chez vous ?"),
	br(),
	
	# ---- Row avec carte + controle widget
	fluidRow(
		tabBox(title = NULL, width=12,
			tabPanel("Stations",
				h3(align = "center", "Sélectionnez une station sur la carte pour visualiser la qualité de l'eau."),
				fluidRow(
					column(width=4,
						actionButton("geoloc", "Voir ma ville", style="width:100% ; color: white; font-weight: bold", onClick="shinyjs.geoloc()"),
						textInput("target_zone", "ou recherchez une ville..." , "Ex: France"),
						infoBoxOutput("infobox4"),
						infoBoxOutput("infobox2"),
						infoBoxOutput("infobox3")
					),
					column(width=8,
						leafletOutput("map")
					)
				)
			),
			
			tabPanel("Historique",
				h3(align = "center", "Historique des mesures sur la station sélectionnée."),
				fluidRow(
					column(4,align="justify",
						br(),
						"Le streamgraph ci-contre présente l'évolution des pesticides pour la station 
						sélectionnée. Chaque couche représente au choix un pesticide, une famille de pesticide 
						ou bien une fonction. Toutes les années sont représentées, et vous pouvez obtenir les
						 concentrations exactes en passant la souris sur une couche.",
						br(),br(),
						infoBoxOutput("infobox1"),
						br(),
						selectInput("agrege", "Evolution des ...", choices = c("pesticide","familles de pesticide","fonctions de pesticide") , selected ="fonctions de pesticide"),
						br(),
						radioButtons("propor", "Valeur absolue ou proportion?", choices = c("absolu","relatif"), selected ="absolu" , inline = TRUE)
					),
					column(width=8,
						br(),
						streamgraphOutput("graph2", width = "100%" )
					)
				)
			),
			tabPanel("Dernières mesures",
				h3(align = "center", "Mesures de la dernière année sur la station sélectionnée."),
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
tabItem(tabName = "evolution",
	h2(align = "center", "Observez l'état des pesticides en France"),
	br(),
	fluidRow(
		tabBox(title = NULL,
			width = 9,
			tabPanel("Carte de France",
				p(align="justify", "La carte ci-dessous présente les concentrations en pesticides observées dans chaque département français."),
				br(),
				leafletOutput("map2")
			),
			tabPanel("Graphiques",
				plotlyOutput("graph_evol_dep" , height="600px")
			)
		),
		box(title = NULL,
			width = 3, solidHeader = FALSE, collapsible = FALSE, background = NULL, status = NULL,
			selectInput("geo_unit", "Unité géographique:" , choices=c("Station", "Departement", "Région") , selected="Departement"),
			p(align="justify", "Filtrer les données afin d'observer uniquement une fonction, une famille ou un pesticide 
			en particulier. Pour information, voici les 5 pesticides les plus répandus en France :"),
			plotOutput("barplot_most_important_pesticides" , height="194px"),
			selectInput( "choix_aggregat", "Types de pesticides:" , choices=c("Tous", "Famille","Fonction","Pesticide") , selected="Tous" ),
			conditionalPanel("input.choix_aggregat == 'Famille'", selectInput( "choix_famille", "Choisissez une famille", choices = levels(droplevels(pest_db$CODE_FAMILLE)) , selected ="Amides"  )),
			conditionalPanel("input.choix_aggregat == 'Fonction'", selectInput( "choix_fonction", "Choisissez une fonction", choices = colnames(pest_db)[18:28] , selected ="Insecticide"  )),
			conditionalPanel("input.choix_aggregat == 'Pesticide'", selectInput( "choix_pesticide", "Choisissez un pesticide", choices = sort(unique(pest_db$LB_PARAMETRE)) , selected ="Atrazine déséthyl"  ))
		)
	)
),
#-----------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 4 : CLASSIFICATION
# ----------------------
tabItem(tabName = "classification",
	h2("Quels sont les différents types de pesticides ?", align = "center"),
	br(),
	fluidRow(
		column(width=8,
			p(align="justify", "Les pesticides sont des molécules utilisées en agriculture pour lutter contre les ravageurs. 
			On compte 4 grandes familles : les herbicides, les fongicides, les insecticides, les acaricides. 
			Le problème des pesticides est qu'ils sont lessivés (emmenés en profondeur) et contaminent les masses d'eau 
			profondes qui sont souvent nos ressources en eau potable et posent un problème de santé publique."),
			helpText(align="justify", "Le graphique ci-dessous présente les types de pesticides observés en France.
				Cliquez sur une famille pour voir les molécules qui la composent.
				Cliquez dans la barre du haut pour revenir à l'état initial."
			),
			box(width=12,
				radioButtons("treemapchoice", "Voir les types de pesticides par", choices=c("nombre de pesticides","quantité mesurée"), selected="nombre de pesticides", inline=T),
				conditionalPanel(id="details_treemap", condition = "input.treemapchoice == 'nombre de pesticides'",
					helpText(
						"La taille des blocs est proportionnelle au nombre de pesticides dans cette famille."
					)
				),
				conditionalPanel(id="details_treemap", condition = "input.treemapchoice == 'quantité mesurée'",
					helpText(
						"La taille des blocs est proportionnelle au nombre de détection de ces pesticides en station."
					)
				),
				d3tree2Output("treemap", height="500px", width="100%")
			)
		),
		column(width=4,
			box(title="Herbicide atrazine", width= 12, solidHeader = TRUE, status = "danger", 
				p(align="justify", "2e herbicide le plus utilisé aux USA. Suspectée d’être une des causes du phénomène récent 
				qui décime les amphibiens sur toute la planète."),
				infoBox( width=12,
					NULL,
					a("interdiction UE en 2003", href="https://www.senat.fr/rap/l02-215-2/l02-215-241.html", target="_blank"),
					icon = icon("remove"), color ="red"
				)
			),
			box(title="Fongicide oxadixyl", width= 12, solidHeader = TRUE, status = "warning",
				p(align="justify", "Fongicide utilisé pour combattre le mildiou."),
				infoBox( width=12,
					NULL,
					"interdiction UE en 2003",
					icon = icon("remove"), color ="red"
				)
			),
			box(title="Insecticide thiamethoxam", width= 12, solidHeader = TRUE, status = "warning",
				"La substance active du Cruiser ",
				a("intoxique les abeilles", href="http://www.actu-environnement.com/ae/news/ministere-agriculture-cruiser-osr-etude-inra-acta-cruiser-thiamethoxam-abeilles-15340.php4", target="_blank"),
				" même à faible dose.",
				infoBox( width=12,
					NULL,
					a("interdiction UE en 2013", href="http://www.lemonde.fr/planete/article/2013/05/24/abeilles-interdiction-de-trois-pesticides-dans-l-ue-a-compter-du-1er-decembre_3416897_3244.html", target="_blank"),
					icon = icon("remove"), color ="red"
				)
			),
			box(title="Herbicide piperonyl butoxide", width= 12, solidHeader = TRUE, status = "primary",
				p(align="justify", "Un synergisant, un produit chimique ajouté aux pesticides pour accroître la toxicité des ingrédients actifs")
			)
		)
	)
),
#-----------------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 5 : TOXICITE
# ----------------------
tabItem(tabName = "danger",
	h2(align = "center", "Toxicité des pesticides"),
	br(),
	p(align="justify", "La toxicité est mesurée par le LD50, elle correspond à la quantité de substance ingérée qui 
	tue 50% de l'échantillon test. L'unité est le mg/kg, milligrames de substances par kilogrammes de poids de l'individu."),
	br(),
	fluidRow(id="toxicite_row",
		tabBox(title=NULL, width=12,
			tabPanel("Lesquels sont les plus toxiques?",
				fluidRow(align="center",
					column(width=10, offset=1, 
						p(align="justify", "Ce graphique décrit la toxicité des pesticides retrouvés en france: de très toxique (en haut),
						à une toxicité modérée (en bas). La taille des bulles est proportionnelle au nombre de fois où la molécule a été 
						détectée en ",max(pest_db$ANNEE),". Vous pouvez zoomer sur une zone (cliquer-glisser) et faire 
						apparaitre / disparaitre les familles en cliquant sur la légende."
						),
						plotlyOutput("bubblechart", width="100%"),
						br(),br(),br(),br(),br()
					)
				)
			),
			tabPanel("Effets sur la santé humaine",
				fluidRow(
					column(width=6,
						img(src = "PesticideEffectsOnBody_graph.png", class='img-rounded', style="width: 100%")
					),
					column(width=6,
						p(align="justify", "Les personnes exposées aux pesticides ont plus de risque de développer de nombreuses 
						maladies que les autres : cancer, malformations congénitales, problèmes de fertilité, problèmes neurologiques 
						ou encore système immunitaire affaibli sont plus fréquent chez eux !"),
						p(align="justify", "75% des études menées indiquent une relation positive entre l'exposition aux pesticides et l'atteinte 
						par un lymphome (cancer des lymphocytes).", a("source", href="http://www.lymphomahelp.org/rr_pesticides.pdf", target="_blank")),
						valueBox("Top 7 des cancérigènes", HTML("
							Glyphosate (Roundup)<br>
							Malathion<br>
							Diazinon (Spectracide)<br>
							Parathion<br>
							Tétrachlorvinphos<br>
							Epoxiconazole<br>
							DDT, interdit en France<br>
							<br>
							<strong>Au total plus de 90 substances cancérigènes ont été recencées (UE)</strong>
							"),
							icon = icon("medkit"), color="red"
						),
						valueBox("Top 4 des perturbateurs endocriniens", HTML("
							Chlorotoluron (Pestanal)<br>
							Dimoxystrobin<br>
							Epoxiconazole<br>
							ProfoxydimChloro<br>
							"),
							icon = icon("medkit"), color="orange"
						),
						valueBox("Top 5 des immunotoxiques", HTML("
							Atrazine (AAtrex, Spectracide)<br>
							2,4-D (Une des deux molécules de l'Agent Orange)<br>
							Organophosphates (gaz sarin par exemple)<br>
							Composés organochlorés (gaz moutarde par exemple)<br>
							Carbamates<br>
							<br>
							<strong>Plus de 200 000 décès causés par des organophosphates</strong>
							"),
							icon = icon("medkit"), color="yellow"
						)
					)
				)
			),
			tabPanel("Autres effets sur l'environnement",
				br(),br(),
				fluidRow(
					column(offset=1,width=5,
						box(width=4,align="center",
							title= "Disparition des abeilles", solidHeader = TRUE, status="warning",br(),
							img(src = "abeilles_dead.png", class='img-rounded', style="width: 60%"), br(),br(),br(),
							p(align="justify", "Les organophostphates, neonicotinoïdes et les carbamates en cause. De nombreux insecticides récents sont des néonicotinoïdes:
							Imidacloprid (Confidor, Admire), Thiamethoxam (Cruiser, Actara), Clothianidin (Dantop)
							Acetamiprid  (Mospilan), Thiacloprid (Calypso), Dinotefuran (Venom), Nitenpyram (Capstar, Guardian)")
						)
					),
					column(width=5,
						box(width=4,align="center",
							title="Epaisseur de la coquille d'oeuf", solidHeader = TRUE, status="info",
							img(src = "oeuf.jpg",class='img-rounded', style="width: 50%"),br(),br(),
							p(align="justify", "Le DDT a un impact fort sur l'épaisseur des coquilles d'oeuf. La réduction de l'épaisseur de celles ci peut aller jusqu'à 90%!", a("(source)", href="http://www.iewonline.be/IMG/pdf/4_pesticides-substancesactives.pdf", target="_blank"))					
						)
					)
				)
			)
		)
	)
),
		
			

#-----------------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------------
# ----------------------
# SHEET 5 : A PROPOS
# ----------------------
tabItem("apropos",
	br(),
	fluidRow(
		column(offset=3, width=6, 
			box(align="justify", width=12, solidHeader = TRUE, status="info", title=h3("Le GreenTech Challenge"),
				"Le ",
				a("GreenTech challenge", href="http://www.developpement-durable.gouv.fr/Concours-de-data-visualisation-sur.html",target="_blank"),
				" est un concours de data visualisation mis en place par le ",
				a("Ministère de l'environnement", href="http://www.developpement-durable.gouv.fr/", target="_blank"),
				". L'objectif est de d'améliorer la connaissance du grand public en ce qui concerne les pesticides.",
				"Pour ce faire, un important ",
				a("jeu de données", href="http://www.donnees.statistiques.developpement-durable.gouv.fr/dataviz_pesticides/", target="blank()"),
				" a été mis à la disposition des candidats, comprenant plus de 2 millions de relevés dans plus de 2000 stations de mesures."
			)
		)
	),
	br(),
	fluidRow(
		box(align="justify", offset=1, width=6, solidHeader = TRUE, status="info", title=h3("Team Superviz"),
			"Cette application a été développé par 4 ingénieurs de ",
			a("Montpellier Supagro",href="https://www.google.fr/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0ahUKEwjth7nooe3RAhWEExoKHaIfDS4QFggeMAA&url=https%3A%2F%2Fwww.supagro.fr%2F&usg=AFQjCNHi91pguw_LzmDeFjPozMyu7kX2FQ&sig2=kDf7phZ0b7Dsr0nPIP2V3Q", target="_blank()"),
			". Amis de longue date et tous geek dans l'âme, il ne nous a pas fallu plus de 5 minutes pour nous décider à nous lancer dans cette 
			aventure après avoir (tardivement) entendu parler de ce challenge.",
			"Vous pouvez en apprendre plus sur nos personnalités via les pages web suivantes:" ,
			br(),br(),
			"Charles Moszkowicz:", a("linkedin",href="https://www.linkedin.com/in/charles-moszkowicz", target="_blank"),br(),   
			"Jean-Charles Simonin:", a("company",href="http://eneo.fr/fr/contact/", target="_blank"),br(),
			"Guilhem Marre:", a("linkedin",href="https://www.linkedin.com/in/guilhem-marre-42132b28", target="_blank"),  br(), 
			"Yan Holtz:", a("homepage",href="https://holtzyan.wordpress.com/", target="_blank")  ,br()
		),
		box(align="justify", width=6, solidHeader = TRUE, status="info", title=h3("L'application"),
			"Cette application repose sur la library",
			a("Shiny",href="https://shiny.rstudio.com/",target="_blank()"),
			" du langage R.",
			"Le code est entièrement reproductible et disponible librement sur:",
			a("Github",href="https://github.com/holtzy/GreenTech-Challenge", target="_blank"),
			"Vous pouvez donc accéder à cet outil en local, ou via ",
			a("l'URL suivant.", href="www.agap-sunshine.inra.fr/holtz-apps/GreenTech_Challenge/", target="_blank()"),
			br(),br(),
			"En espérant que vous y trouverez toutes les informations que vous recherchez, l'équipe Superviz vous 
			souhaite une bonne navigation!"
		)	
 	)
)

#-----------------------------------------------------------------------------------------------






) # end tabItems
) # end Dashboard body
)) #end Dashboard page and shinyUI
