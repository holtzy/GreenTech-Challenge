library(shiny)

runApp(launch.browser = TRUE, 
       list(
         ui = shinyUI(navbarPage(
           title=" ", fluid=FALSE,
           header = HTML("<div class='ssba ssba-wrap'>
                            <div style='text-align:right'>
                         <code>
                         'Share the Gallery !'
                         <a data-site='' class='ssba_facebook_share' href='http://www.facebook.com/sharer.php?u=http://cocofanfareclub.fr/' target='_blank'>
                         <img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/facebook.png' title='Facebook' class='ssba ssba-img' alt='Share on Facebook'>
                         </a>
                         <script>(function(d, s, id) {
                            var js, fjs = d.getElementsByTagName(s)[0];
                            if (d.getElementById(id)) return;
                            js = d.createElement(s); js.id = id;
                            js.src = '//connect.facebook.net/fr_FR/sdk.js#xfbml=1&version=v2.8';
                            fjs.parentNode.insertBefore(js, fjs);
                            }(document, 'script', 'facebook-jssdk'));
                         </script>
                         <a data-site='' class='ssba_google_share' href='https://plus.google.com/share?url=http://www.r-graph-gallery.com/' target='_blank'>
                         <img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/google.png' title='Google+' class='ssba ssba-img' alt='Share on Google+'>
                         </a>
                         <a data-site='' class='ssba_twitter_share' href='http://twitter.com/share?url=http://www.r-graph-gallery.com/&amp;text=+' target='_blank'>
                         <img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/twitter.png' title='Twitter' class='ssba ssba-img' alt='Tweet about this on Twitter'>
                         </a>
                         <a data-site='linkedin' class='ssba_linkedin_share ssba_share_link' href='http://www.linkedin.com/shareArticle?mini=true&amp;url=http://eneo.fr/fr/home/' target='_blank'>
                         <img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/linkedin.png' title='LinkedIn' class='ssba ssba-img' alt='Share on LinkedIn'>
                         </a>
                         <a data-site='email' class='ssba_email_share' href='mailto:?subject=&amp;body=%20http://www.r-graph-gallery.com/'>
                         <img src='http://www.r-graph-gallery.com/wp-content/plugins/simple-share-buttons-adder/buttons/somacro/email.png' title='Email' class='ssba ssba-img' alt='Email this to someone'>
                         </a>
                         </code>
                         </div>
                         </div>"),
           tabPanel(
             title = "Data",
             h1("Data"),
             br(),
             tabsetPanel(
               type = "tabs", 
               tabPanel("Selection"),
               tabPanel("View")
             )
           ),
           tabPanel(
             title = "Plots"
           )
           )),
         server = function(input, output) {
         }
       ))