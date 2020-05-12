library(shiny)
library(shinythemes)
library(xtable)
library(shinyjs)
library(V8)
library(dplyr)
library(readxl)
options(shiny.sanitize.errors = FALSE)

#### Change these

DAYS = c("May 4", "May 5", "May 6")
MONTHSTR <- "May"
WSTC <- "WSTC6"
############


Z <- readRDS("WSTC6_keywords.rds")
Z$keywords <- as.character(Z$keywords)


X <- readRDS("WSTC6_Abstracts.rds")
#X <- enc2utf8(X)
X$ids <-paste(as.character(X$Last),', ',as.character(X$First),sep='')
X$Affiliation <- as.character(X$Affiliation)
X$Country <- as.character(X$Country)



jscode <- "shinyjs.refresh = function() { history.go(0); }"


shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode),
  extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory")),
  tags$head(
    tags$script(src="countdown.js"),
    tags$link(rel="stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto:100,200,300,500")
  ),
  windowTitle = paste(WSTC,'schedule'),
  selected = 'Home',
  theme = shinytheme('spacelab'),
  includeCSS("style.css"),
  includeCSS("countdown.css"),
  tabsetPanel(
    id = "tabs",
    
    tabPanel('Home',
             fluidRow(class='homrow1',
                      column(12,
                             tags$a(img(src='WSTC6_logo_white_text.png',width='65%'),href='https://twitter.com/search?f=tweets&vertical=default&q=%23WSTC5&src=typd&lang=en')
                      ),
                      column(4,offset=8,
                             fluidRow(
                               column(12,class='timer',
                                      h1('Countdown to #WSTC6')
                               ),
                               column(12,HTML("<div class='countdown' data-date='2020-05-04-07-00-0'></div>"))     
                             )
                             
                      )
             ),
             
             
             
             
             fluidRow(class='homrow3',
                      column(12,
                             #tags$a("Download the past WSTC report",href='http://seabirds.net/documents/6/Past_WSTCs.pdf'),
                             #HTML("<h1 style='font-size:30pt;text-align:center;font-weight:500'>Over 180 Registrants! <a href='http://seabirds.net/documents/6/Past_WSTCs.pdf'>Download the past WSTC report</a> </h1>")
                             h1("OVER 180 REGISTRANTS!",style='font-size:30pt;text-align:center;font-weight:500')
                      ),    
                      
                      column(3,class='col-xs-6 col-md-3 col-lg-3',
                             tags$button(
                               id = "info",
                               type="button",
                               class = "btn action-button shiny-bound-input",
                               img(src="BrownBoob.png",width='100%')
                             )),
                      column(3,class='col-xs-6 col-md-3 col-lg-3',
                             tags$a(
                               img(src="img-thing.png",width='100%'),
                               href='https://twitter.com/seabirders'
                             )),
                      column(3,class='col-xs-6 col-md-3 col-lg-3',
                             tags$a(
                               img(src="Bass.png",width='100%'),
                               href='http://www.seabirds.net'
                             ))
                      
                      
                      
             ),
             
             
             
             fluidRow(class='footer',
                      column(2,class='col-xs-3 col-sm-2 col-lg-1 col-md-2',
                             tags$a(img(src='Blackbawks.png',height='90px'),href='http://www.blackbawks.net')
                      ),
                      column(2,style='margin-top:40px;',class='col-xs-12 col-sm-12 col-lg-2 col-md-12',
                             fluidRow(
                               column(12,HTML('<p style="vertical-align:bottom">site created by <a href="http://www.blackbawks.net">Black bawks data science</a></p>')),
                               column(12,HTML('<p>WebMaster: <a href="mailto:grwhumphries@blackbawks.net">Grant Humphries</a>')),
                               column(12,HTML('<p>General contact: <a href="mailto:wstc.seabirds@gmail.com">WSTC6 Information</a>'))
                             )
                      ),
                      column(12,class='col-xs-12 col-sm-12 col-md-12 col-lg-7',
                             column(12, h1(style='text-align:center','Sponsored by:')),
                             column(2, class='col-md-2 col-lg-offset-2',tags$a(img(src='TSGlogo.png',height='90px'),href='http://www.seabirdgroup.org.uk/')),
                             column(2, class='col-md-2',tags$a(img(src='BOU.jpg',height='90px'),href='https://www.bou.org.uk/')),
                             column(2, class='col-md-2',tags$a(img(src='wsulogo3.png',height='90px'),href='https://www.seabirds.net')),
                             column(2, class='col-md-2',tags$a(img(src='PSG_logo.jpg',height='90px'),href='https://www.pacificseabirds.org')),
                             column(2, class='col-md-2',
                                    HTML('<p style="font-size:20pt"><a href="https://www.instagram.com/jodicel/?hl=en">@jodicel</a></p>')
                                    
                             )
                      )
                      
             )
             
             
             
             
    ),
    
    
    tabPanel("Search by Presenter",
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 useShinyjs(), 
                 selectInput("Presenter", label = "Presenter:",
                             choices = c(as.character(X$ids))),
                 
                 hr(),
                 p('Once your filter has been selected, choose a presenter from the list above'),
                 h3('Filter presenter by:'),
                 
                 radioButtons("filter", label = NULL, choices = c("Country", "Affiliation","Session","Plenaries","Keyword","Show all"), inline=TRUE, selected='Show all'),
                 
                 
                 selectInput("Country", label = "Country:",selected="Canada ",
                             choices = c(sort(as.character(X$Country)))),
                 
                 selectInput("Affiliation", label = "Affiliation:",selected =X$Affiliation[1],
                             choices = c(sort(as.character(X$Affiliation)))),
                 
                 selectInput("Session", label = "Session:",
                             choices = c(sort(unique(as.character(X$Session))))),
                 
                 selectInput("Keyword", label = "Keyword:",
                             choices = c((Z$keywords))),
                 
                 hr(),
                 
                 h3('Try out our timezone converter'),
                 selectInput("TimeZone",label= "Select a time zone (UTC offset):", selected = 0,
                             choices = c(-12:14)),
                 HTML('<h3><a href="https://www.timeanddate.com/time/map/" target="_blank">INTERACTIVE TIME ZONE MAP</a></h3>'),
                 
                 hr(),
                 img(src='Blackbawks.png',width = '25%'),
                 HTML('<span>Created by: <a href="http://blackbawks.net">Black Bawks Data Science Ltd.</a></span>'),
                 HTML('<p><a href="mailto:grwhumphries@blackbawks.net">Click here</a> to report a bug</p>')
                 
                 
                 
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tableOutput('Person'),
                 tableOutput('Info'),
                 htmlOutput('CurrentTimeZone'),
                 tableOutput('Time'),
                 HTML('<h2> Title </h2>'),
                 htmlOutput('TitleDat'),
                 HTML('<h2> Abstract </h2>'),
                 htmlOutput('AbstractDat'),
                 hr()
               )
               
             )
    ),  ## tabPanel END
    
    tabPanel("Schedule",
             
             sidebarLayout(
               sidebarPanel(
                 useShinyjs(), 
                 h3('Filter:'),
                 
                 radioButtons("filter2", label = NULL, choices = c("Show Overview","Day","Session"), inline=TRUE, selected='Show Overview'),
                 
                 
                 selectInput("Day", label = "Day:", selected = DAYS[1],
                             choices = DAYS),
                 
                 selectInput("Session2", label = "Session:", selected = as.character(X$Session[1]),
                             choices = c(sort(as.character(X$Session)))),
                 hr(),
                 htmlOutput('CurrentTimeZone2'),
                 
                 h3('Try out our timezone converter'),
                 selectInput("TimeZone2",label= "Select a time zone (UTC offset):", selected = 0,
                             choices = c(-12:14)),
                 HTML('<h3><a href="https://www.timeanddate.com/time/map/" target="_blank">INTERACTIVE TIME ZONE MAP</a></h3>'),
                 
                 hr(),
                 
                 img(src='Blackbawks.png',width = '25%'),
                 HTML('<span>Created by: <a href="http://blackbawks.net">Black Bawks Data Science Ltd.</a></span>'),
                 HTML('<p><a href="mailto:grwhumphries@blackbawks.net">Click here</a> to report a bug</p>')
                 
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tableOutput("Schedule")
               )
               
             )
             
    ),
    
    
    
   
    tabPanel("Rules and Guidelines",value='information',
             fluidRow(class='inforow1',
                      column(4,offset=4,class='inforow1col',
                             img(src='WSTC6_logo_white_text_NODATE.png',width='100%')
                      )),
             fluidRow(
               column(8,offset=2,
                      h1('We are excited to announce the 6th annual World 
                         Seabird Twitter Conference #WSTC6! This is an opportunity to learn about 
                         seabird research and conservation around the world from the comfort of your 
                         home, office, or field station. #WSTC6 will take place 4-6 May 2020. ',
                         style='font-weight:500;line-height:25pt'),
                      hr(),
                      h1('Past WSTCs',style='font-size:24pt;font-weight:600'),
                      tags$a("Download the past WSTC report",href='http://seabirds.net/documents/6/Past_WSTCs.pdf'),
                      HTML("<h2>
                           The first five years of the World Seabird Twitter Conference were a great success.
                           During last year's edition the hashtag #WSTC5 was used 3,400 times by 700 
                           contributors, with a potential reach of over 1.3 million users 
                           (try fitting that into a conference hall!). 
                           We had over 100 presenters tweeting about their work from over 20 countries, 
                           some even presenting in multiple languages, and people got involved from all 
                           around the world by asking questions, re-tweeting and following the presentations, 
                           making it a truly global event. You can have a look at the map of hashtag use
                           <a href='https://www.trendsmap.com/v/FqgG'>here</a></h2>"),
                      hr()
                      ),
               
               column(8, offset=2,
                      h1('#WSTC6',style='font-weight:600'),
                      h2('What is a Twitter conference?',style='font-weight:400'),
                      HTML("<h3>A Twitter conference, as the name suggests, <strong>takes place on Twitter</strong>. During the conference, 
                           each presenter gets <strong>15 minutes</strong> in which to tweet out four tweets about their topic. 
                           By following the hashtag specific to the conference (#WSTC6) the tweets can then be seen by 
                           people from all over the world. Just like at a normal academic conference it allows you to 
                           present your work, catch up on the work of others and network with people in your field – but 
                           with the added benefits that it is all completely cost- and carbon-free and you reach a much 
                           wider audience.
                           </h3>"),
                      
                      h2('How do I participate?',style='font-weight:400'),
                      HTML("<h3>If you are interested in being a spectator only, you just need to search for the Twitter hashtag
                           #WSTC6 to see all posts related to the conference. There will also be <strong>session-specific hashtags</strong> 
                           allowing you to follow topics you are particularly interested in and a programme will be published 
                           where you can find the times and titles of all presentations. You are also encouraged to get 
                           involved and interact with the presenters and other spectators! In this case, you need to get a 
                           Twitter account if you don't already have one – which can be easily done at 
                           <a target='_blank' href='https://www.twitter.com'>www.twitter.com</a>  
                           (see next section for how to get started with Twitter). <br/><br/>
                           
                           <strong>Interacting by asking questions, re-tweeting and liking the tweets is an incredibly important 
                           part of the conference.</strong> You can either do this live during the presentation times or use the breaks between sessions 
                           to reach out if you didn't manage to do so during the session. 
                           When asking a question, please direct the tweet at the individual using their handle and use the 
                           hashtag #WSTC6. <br/><br/>
                           Example: @Seabirders #WSTC6 How easy is it to use Twitter to participate 
                           in this great event?<br/><br/>
                           
                           If you want to present, <strong>please register and submit your abstract before 22 March 2020</strong> 
                           (abstract submission can be found under the “Registration” tab on this website). 
                           To present you need to have a Twitter account. More rules, tips and tricks for 
                           presenting can be found below.</h3>"),
                      
                      h2("What if I don't know how to use Twitter?",style='font-weight:400'),
                      
                      HTML("<h3>There are a lot of online resources to help you learn how to use and get the most 
                           out of Twitter. We find the “masterclasses” produced by the BOU particularly helpful.<br/><br/>
                           
                           This is a good one for getting to terms with the basics:<br/>
                           <a target='_blank' href='https://www.bou.org.uk/twitter-masterclass-9/'>https://www.bou.org.uk/twitter-masterclass-9/</a><br/><br/>
                           
                           Here are instructions on how to set up a Twitter account:<br/>
                           <a target='_blank' href='https://help.twitter.com/en/create-twitter-account'>
                           https://help.twitter.com/en/create-twitter-account</a> <br/><br/>
                           
                           Once you have set up an account it is a good idea to follow some of the larger seabird groups,
                           which will help you make sure that you get content that you are interested in. 
                           (Suggestions: @Seabirders @PacificSeabirds @TheSeabirdGroup
                           @AUS_NZ_Seabirds.) From here you can then find and follow other accounts 
                           you are interested in.<br/><br/>
                           
                           It is a good idea to make sure that your profile shows who you are and what you do. 
                           Here is a helpful BOU masterclass on how to get your profile sorted:<br/> 
                           
                           <a target='_blank' href='https://www.bou.org.uk/twitter-masterclass-13/'>
                           https://www.bou.org.uk/twitter-masterclass-13/</a> <br/><br/>
                           
                           If you’re still confused about how to navigate Twitter – 
                           please get in touch at 
                           <a href='mailto:wstc.seabirds@gmail.com'>wstc.seabirds@gmail.com</a></h3>"),
                      hr(),
                      h1('Information for Presenters',style='font-weight:400'),
                      HTML('<h3>Once you have had your abstract accepted and the programme has been set, 
                           you will be allocated <strong>15 min in which to present your four tweets</strong> (each of 
                           which is 280 characters). We will be assigning these times based on your geographic 
                           location so please <strong>make sure you give us the correct time zone information</strong> when you 
                           register. If you are unable to be present during your allocated time slot, you can 
                           schedule your tweets so that they get posted automatically without you having to be 
                           online (see below for details on how to do this). 
                           However, we encourage all presenters to try to be available during their slot to answer questions.</h3>'),
                      
                      h3("Below is an example presentation from Rowan Mott from #WSTC4 to get an idea what the 
                         presentations may look like:"),
                      
                      
                      
                      HTML('<div class="col-md-6 col-xs-12"><div style="width:100%"><blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">1/4 <a href="https://twitter.com/hashtag/WSTC4?src=hash&amp;ref_src=twsrc%5Etfw">#WSTC4</a> <a href="https://twitter.com/hashtag/ManPolPlan1?src=hash&amp;ref_src=twsrc%5Etfw">#ManPolPlan1</a>. <a href="https://twitter.com/hashtag/InvasiveSpecies?src=hash&amp;ref_src=twsrc%5Etfw">#InvasiveSpecies</a> decimate <a href="https://twitter.com/hashtag/seabird?src=hash&amp;ref_src=twsrc%5Etfw">#seabird</a> colonies globally. Rats, cats, and mice are the main culprits, but <a href="https://twitter.com/hashtag/ants?src=hash&amp;ref_src=twsrc%5Etfw">#ants</a> can also be a threat. At Ashmore Reef (eastern Indian Ocean), invasive Tropical <a href="https://twitter.com/hashtag/FireAnts?src=hash&amp;ref_src=twsrc%5Etfw">#FireAnts</a> kill hatchling seabirds and disturb incubating adults. <a href="https://t.co/mvvBHf02vq">pic.twitter.com/mvvBHf02vq</a></p>&mdash; Rowan Mott (@roamingmoth) <a href="https://twitter.com/roamingmoth/status/986401493438816256?ref_src=twsrc%5Etfw">April 18, 2018</a></blockquote>
                           <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script></div></div>
                           <div class="col-md-6 col-xs-12"><div style="width:100%">
                           <blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">2/4 <a href="https://twitter.com/hashtag/WSTC4?src=hash&amp;ref_src=twsrc%5Etfw">#WSTC4</a> <a href="https://twitter.com/hashtag/ManPolPlan1?src=hash&amp;ref_src=twsrc%5Etfw">#ManPolPlan1</a> We studied Red-tailed <a href="https://twitter.com/hashtag/Tropicbird?src=hash&amp;ref_src=twsrc%5Etfw">#Tropicbird</a> nest site fidelity &amp; its potential to inform localised Tropical <a href="https://twitter.com/hashtag/FireAnt?src=hash&amp;ref_src=twsrc%5Etfw">#FireAnt</a> control until eradication can be achieved. We measured distance between successive <a href="https://twitter.com/hashtag/nest?src=hash&amp;ref_src=twsrc%5Etfw">#nest</a> sites for individual <a href="https://twitter.com/hashtag/birds?src=hash&amp;ref_src=twsrc%5Etfw">#birds</a> across three <a href="https://twitter.com/hashtag/breeding?src=hash&amp;ref_src=twsrc%5Etfw">#breeding</a> seasons. <a href="https://t.co/SYe4oyMT2H">pic.twitter.com/SYe4oyMT2H</a></p>&mdash; Rowan Mott (@roamingmoth) <a href="https://twitter.com/roamingmoth/status/986402228297654272?ref_src=twsrc%5Etfw">April 18, 2018</a></blockquote>
                           <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
                           </div></div>
                           
                           <div class="col-md-6 col-xs-12"><div style="width:100%"><blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">3/4 <a href="https://twitter.com/hashtag/WSTC4?src=hash&amp;ref_src=twsrc%5Etfw">#WSTC4</a> <a href="https://twitter.com/hashtag/ManPolPlan1?src=hash&amp;ref_src=twsrc%5Etfw">#ManPolPlan1</a>. Individual <a href="https://twitter.com/hashtag/RedtailedTropicbirds?src=hash&amp;ref_src=twsrc%5Etfw">#RedtailedTropicbirds</a> nested a median of 13.4m from their previous year’s nest. As Tropical <a href="https://twitter.com/hashtag/FireAnts?src=hash&amp;ref_src=twsrc%5Etfw">#FireAnts</a> range &gt;9m from their colony, periodic <a href="https://twitter.com/hashtag/baiting?src=hash&amp;ref_src=twsrc%5Etfw">#baiting</a> within 13.4m of <a href="https://twitter.com/hashtag/tropicbird?src=hash&amp;ref_src=twsrc%5Etfw">#tropicbird</a> nest sites would benefit 69% of future tropicbird nesting attempts. <a href="https://t.co/shL8hVE71v">pic.twitter.com/shL8hVE71v</a></p>&mdash; Rowan Mott (@roamingmoth) <a href="https://twitter.com/roamingmoth/status/986402997751070720?ref_src=twsrc%5Etfw">April 18, 2018</a></blockquote>
                           <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script></div></div>
                           
                           <div class="col-md-6 col-xs-12"><div style="width:100%"><blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">4/4 <a href="https://twitter.com/hashtag/WSTC4?src=hash&amp;ref_src=twsrc%5Etfw">#WSTC4</a> <a href="https://twitter.com/hashtag/ManPolPlan1?src=hash&amp;ref_src=twsrc%5Etfw">#ManPolPlan1</a> The Ashmore Reef Red-tailed <a href="https://twitter.com/hashtag/Tropicbird?src=hash&amp;ref_src=twsrc%5Etfw">#Tropicbird</a> population is small (20 nests) so effort &amp; cost for this interim <a href="https://twitter.com/hashtag/management?src=hash&amp;ref_src=twsrc%5Etfw">#management</a> action is low. Only 0.85ha would need <a href="https://twitter.com/hashtag/baiting?src=hash&amp;ref_src=twsrc%5Etfw">#baiting</a> and this can occur during the non-breeding season to remove disturbance to breeding <a href="https://twitter.com/hashtag/birds?src=hash&amp;ref_src=twsrc%5Etfw">#birds</a>. <a href="https://t.co/5OeoLTrxGY">pic.twitter.com/5OeoLTrxGY</a></p>&mdash; Rowan Mott (@roamingmoth) <a href="https://twitter.com/roamingmoth/status/986403706240360448?ref_src=twsrc%5Etfw">April 18, 2018</a></blockquote>
                           <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script></div></div>
                           
                           '),
                      
                      
                      HTML("<div class='col-xs-12'><h2 style='font-weight:400'>Drafting your tweets</h3></div>"),
                      column(12,
                             HTML('<h3>It is best to draft your tweets ahead of time to avoid issues during the event. 
                                  <strong>Again, a reminder that you are allowed FOUR tweets. Each tweet needs to be numbered (1-4) and needs to start 
                                  with the conference hashtag #WSTC6 and the hashtag specific to your session</strong> (e.g. 1 #WSTC6 
                                  #ClimateChange1). Beyond that, the content of the tweets is up to yourself. Many choose to 
                                  follow a traditional introduction-methods-results-discussion format, but this is not necessary.</h3>'),
                             h3('Some tips:')
                             
                             ),
                      
                      
                      HTML("
                           <div class='col-xs-12'>
                           <ol>
                           <li>Get creative! The strict character limit forces you to be creative 
                           with how you present your work. Use graphics, photos, GIFs, videos, 
                           infographics and anything else you can think of to get people's attention. 
                           In 2018 this video from @ArcticEcology got a lot of attention: </li>
                           
                           </ol>
                           </div>
                           <div class='col-xs-12 col-md-offset-3 col-md-6'>
                           <div style='width:100%'>
                           <blockquote class='twitter-tweet' data-lang='en'><p lang='en' dir='ltr'>(1/4) 
                           Murre on a Mission: Some seabirds are essentially solitary foragers with little evidence of competition 
                           with conspecifics <a href='https://twitter.com/hashtag/ForgEco2?src=hash&amp;ref_src=twsrc%5Etfw'>#ForgEco2</a> 
                           <a href='https://twitter.com/hashtag/WSTC4?src=hash&amp;ref_src=twsrc%5Etfw'>#WSTC4</a> 
                           <a href='https://t.co/U33ZXlpT9D'>pic.twitter.com/U33ZXlpT9D</a></p>&mdash; Kyle Elliott 
                           (@ArcticEcology) <a href='https://twitter.com/ArcticEcology/status/986320062893187072?ref_src=twsrc%5Etfw'>
                           April 17, 2018</a></blockquote>
                           <script async src='https://platform.twitter.com/widgets.js' charset='utf-8'></script>
                           
                           </div></div>
                           <div class='col-xs-12'>
                           <ol start='2'>
                           <li>You can help draw in a very large audience if you use hashtags in a clever way. 
                           Many people follow hashtags such as #ornithology and #seabirds and depending on your presentation topic
                           you may be able to draw in a much larger audience than just the conference attendees. Here is an example 
                           from Kat Keogan’s presentation from 2018:</li>
                           </ol></div>
                           
                           <div class='col-xs-12 col-md-offset-3 col-md-6'>
                           <div style='width:100%'>
                           <blockquote class='twitter-tweet' data-lang='en'><p lang='en' dir='ltr'>3. <a href='https://twitter.com/hashtag/WSTC4?src=hash&amp;ref_src=twsrc%5Etfw'>#WSTC4</a> hugely <a href='https://twitter.com/hashtag/collaborative?src=hash&amp;ref_src=twsrc%5Etfw'>#collaborative</a> global <a href='https://twitter.com/hashtag/phylogenetic?src=hash&amp;ref_src=twsrc%5Etfw'>#phylogenetic</a> <a href='https://twitter.com/hashtag/metaanalysis?src=hash&amp;ref_src=twsrc%5Etfw'>#metaanalysis</a> of <a href='https://twitter.com/hashtag/seabird?src=hash&amp;ref_src=twsrc%5Etfw'>#seabird</a> breeding <a href='https://twitter.com/hashtag/phenology?src=hash&amp;ref_src=twsrc%5Etfw'>#phenology</a> indicated NO trends over TIME or SST. High between year variance = <a href='https://twitter.com/hashtag/shags?src=hash&amp;ref_src=twsrc%5Etfw'>#shags</a> &amp; <a href='https://twitter.com/hashtag/gannets?src=hash&amp;ref_src=twsrc%5Etfw'>#gannets</a>; low = <a href='https://twitter.com/hashtag/albatross?src=hash&amp;ref_src=twsrc%5Etfw'>#albatross</a> &amp; <a href='https://twitter.com/hashtag/petrels?src=hash&amp;ref_src=twsrc%5Etfw'>#petrels</a>. More results here! <a href='https://t.co/fNeGWQl5Bg'>https://t.co/fNeGWQl5Bg</a> <a href='https://twitter.com/hashtag/ClimCha?src=hash&amp;ref_src=twsrc%5Etfw'>#ClimCha</a> <a href='https://twitter.com/hashtag/ornithology?src=hash&amp;ref_src=twsrc%5Etfw'>#ornithology</a> <a href='https://t.co/NYYyDrdyMh'>pic.twitter.com/NYYyDrdyMh</a></p>&mdash; Kat Keogan (@KatharineKeogan) <a href='https://twitter.com/KatharineKeogan/status/986209660226146305?ref_src=twsrc%5Etfw'>April 17, 2018</a></blockquote>
                           <script async src='https://platform.twitter.com/widgets.js' charset='utf-8'></script>
                           </div></div>
                           
                           <div class='col-xs-12'>
                           <h3>For useful hashtags for ornithologists, check out the list below. </h3>
                           
                           
                           <table class='table table-responsive'>
                           <tr>
                           <td>#Seabirds</td>
                           <td>#SeabirderSaturday</td>                
                           <td>#Birdbanding</td>
                           <td>#BirdMigration</td>
                           </tr>
                           <tr>
                           <td>#Migration</td>
                           <td>#MigratorySpecies</td>
                           <td>#Ornithology</td>
                           <td>#Petrelhead</td>
                           </tr>
                           <tr>
                           <td>#TeamAuk</td>
                           <td>#TeamGull</td>	
                           <td>#TeamPetrel</td>
                           <td>#TeamSkua</td>
                           </tr>
                           </table>
                           </div>
                           <div class='col-xs-12'>
                           <ol start='3'>
                           <li>If you formulate short, simple sentences you can get 
                           a lot of information into a single tweet. Spend some time editing your tweet to increase 
                           clarity and avoid complex sentences and you can get a lot of information across in a very small space.
                           The tweet below from Mark Miller from #WSTC4 is a good example of how this can be done well using some imagination:</li>              
                           </ol>       
                           
                           <div class='col-xs-12 col-md-offset-3 col-md-6'>
                           <div style='width:100%'>
                           <blockquote class='twitter-tweet' data-lang='en'><p lang='en' dir='ltr'>3 <a target='_blank' href='https://twitter.com/hashtag/WSTC4?src=hash&amp;ref_src=twsrc%5Etfw'>#WSTC4</a> <a href='https://twitter.com/hashtag/ForgEcol1?src=hash&amp;ref_src=twsrc%5Etfw'>#ForgEcol1</a> Heron pop. consistently dual-foraged, Lord Howe switched between unimodal and dual-foraging. GPS and Isotopes showed Lord Howe 🐣 and 🐦 both normally sustained by Tasman Sea resources whereas Heron 🐣 sustained by reef and 🐦 by Coral Sea resources <a href='https://t.co/c9OJIsVDE0'>pic.twitter.com/c9OJIsVDE0</a></p>&mdash; Mark Miller (@lark_gorilla) <a href='https://twitter.com/lark_gorilla/status/986180802110197760?ref_src=twsrc%5Etfw'>April 17, 2018</a></blockquote>
                           <script async src='https://platform.twitter.com/widgets.js' charset='utf-8'></script>
                           
                           </div></div>
                           
                           "),
                      HTML("<div class='col-xs-12'>"),
                      h2('Tweeting your tweets',style='font-weight:400'),
                      HTML('<h3>You can either tweet out your tweets as individual tweets or you can thread them together. 
                           Threading them together means that your four tweets stick together, which ensures that 
                           your tweets are always seen together and in order. This WikiHow page shows you how to thread
                           your tweets together: <a target="_blank" href="https://www.wikihow.com/Create-a-Twitter-Thread">https://www.wikihow.com/Create-a-Twitter-Thread</a>'),
                      
                      h3("While threading helps to keep your tweets together it is not necessary – you can also 
                         tweet them out individually. You can either tweet them all at once in the beginning or 
                         space them out – it is up to you – but remember to leave some space after your last tweet 
                         for questions before the next presentation starts."),
                      
                      h3("Again, ideally you should be present during the presentation to make sure it all works out 
                         OK and you are available to answer any questions. However, if it turns out that this is not possible, you can schedule your tweets in advance. Scheduling can be done using services 
                         such as TweetDeck, Hootsuite, or Buffer. We suggest that if you do need to schedule your tweets in advance, you practise using a similar type of content (images, GIFs, videos) to make sure that it works before the conference comes around. 
                         It is also important that you make sure your account is public and tweets are not protected so they are visible to everyone. Extra-check this before the conference! 
                         "),
                      
                      
                      h1('Still have questions?',style='font-size:24pt;font-weight:400'),
                      HTML("<h2>You can always get in touch with us on <a href='mailto:wstc.seabirds@gmail.com'>
                           wstc.seabirds@gmail.com</a> if you have questions at any point.<h2></div>")
                      
                      )
               
                      )
             
                      )
    
    
    
                      )
                      )
  
                      )