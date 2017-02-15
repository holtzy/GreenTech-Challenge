   The [GreenTech Challenge](www.agap-sunshine.inra.fr/holtz-apps/GreenTech_Challenge/)
==============================

  
Overview  
--------
The [GreenTech challenge](http://www.developpement-durable.gouv.fr/Concours-de-data-visualisation-sur.html) is a Dataviz competition proposed by the [French Ministery of environment](http://www.developpement-durable.gouv.fr/). A huge [dataset](http://www.donnees.statistiques.developpement-durable.gouv.fr/dataviz_pesticides/) concerning pesticides in France has been released, and the goal was to propose [innovative visualizations](www.r-graph-gallery.com) to sensibilize and inform citizen concerning the french situation over the past 6 Years.  

We decided to make a team of 4 friends and built a [ShinyApp](https://shiny.rstudio.com/) that allows to efficiently explore this dataset. The app is composed by several sheets, each targeting a specific theme. This tool should allow you to understand what happens in your neighbourhood and in the entire country. It is also studied to improve your genereal knowledge concerning [pesticides](https://en.wikipedia.org/wiki/Pesticide).  

Do not hesitate, try it online [here](http://www.agap-sunshine.inra.fr/holtz-apps/GreenTech_Challenge/)!  

Here is a screenshot of the first sheet of the app:  
   
![fig1](www/ScreenShotApp.png)


  
  
Input
--------
The dataset used for this study is available online [here](http://www.developpement-durable.gouv.fr/Concours-de-data-visualisation-sur.html). Note that the app already charged this dataset and transformed it in a R environnement, so you do not really need it.  



Local use
--------
The best way to consult the application is [on the web](http://www.agap-sunshine.inra.fr/holtz-apps/GreenTech_Challenge/). However, you can easily use it locally, on your own computer.
  
Follow these steps:  
-1/ Download this whole repository  
-2/ Open R ([install it](http://https://www.r-project.org/) if needed)
-3/ Install some libraries running the code below:  
```
# Packages available on CRAN:
to_install=c( "shiny", "sp", "plotly", "ggplot2", "DT", "RColorBrewer", "devtools", "leaflet", "ggmap", "tidyr", "shinydashboard", "shinyjs")
install.packages(to_install)

# Packages comming from Github
library(devtools)
install_github("hrbrmstr/streamgraph")
install_github("mtennekes/treemap", subdir="pkg")
install_github("timelyportfolio/d3treeR")
```

-4/ Start the app from R (add your proper path to the folder):  
```
library(shiny)
runApp("This/is/my/path/GreenTech-Challenge")
```


  
Members
--------
This application has been developped by 4 friends from [Montpellier Supagro](www.supagro.fr/):    
Charles Moszkowicz: [company](http://eneo.fr/fr/contact/)   
Jean-Charles Simonin: [company](http://eneo.fr/fr/contact/)   
Guilhem Marre: [linkedin](https://www.linkedin.com/in/guilhem-marre-42132b28)   
Yan Holtz: [homepage](https://holtzyan.wordpress.com/)    
  









