library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(dygraphs)
library(xts)
library(leaflet)
library(shinydashboard)

shinyUI( fluidPage(
          navbarPage(
          "Amazon Movie Reviews",
          #theme= 'style.css',
###################overview part###########################          
          tabPanel("Overview",icon=icon("film"),
              box(width=6,
                       img(src="movie2.jpg", width = 400)   
                   ),     
              box(width=6,
                  h5("Analysis of Amazon movie reviews",align="center"),
                  hr(),
                  p("We focused on the analysis of Amazon movie reviews. The date sets contain 110000 reviews of 409 movies on Amazon.
                    The analysis mainly develops into three parts. First we summarized the most popular movies and then we explored the time trend. Finally we computed the similarity 
                    of the movies based on the review scores."),
                  p("Data Source:",
                    a(href = 'http://snap.stanford.edu/data/web-Movies.html', 'Amazon movie review'),
                    br(),"Repo here: ",
                    a(href = "https://github.com/TZstatsADS/project4-team-7", icon("github")),
                    br(),
                    "Copyright 2016 By",
                    span("STAT4249 Project4 Group 7", style = "color:blue"))  
                  )
                  ),

 ############################################################         







 #################### xiaoyu s Menu Item ####################
  tabPanel("Timelines",icon=icon("line-chart"),
           sidebarLayout(position="left",
             sidebarPanel(width = 2,
                        conditionalPanel(condition = "input.conditionedPanels == 0",
                                           selectInput("rank", "Rank Status:",
                                                       choices = list( "Rank by Movie Type" = 1, 
                                                                       "Rank by Director" = 2), selected = 1)
                                         ),
                          
                        conditionalPanel(condition = "input.conditionedPanels == 1",
                                           selectInput("review", "Customer Reviews:",
                                                       choices = list( "Most Popular movies" = 1,
                                                                       "Most Active Users" = 2), selected = 1),
                                           
                                           selectInput("more", "More Details:",
                                                       choices = list("Overall Ranking" = 1,
                                                                      "Summary by Movie Type" = 2), selected = 1),
                                           
                                           textInput("text_id", label = h5("Input Product ID"), value = "")

                                         ),
                          
                        conditionalPanel(condition = "input.conditionedPanels == 2 ",
                                           selectInput("time", "Timeline by MovieType:", 
                                                       choices = list("All Types" = 1,
                                                                      "Comedy " = 2,
                                                                      "Animation" = 3,
                                                                      "Action" = 4,
                                                                      "Drama" = 5,
                                                                      "Horror&Thriller" = 6,
                                                                      "Crime" = 7),selected = 1),
                                           
                                           selectInput("compare", "Between-Types Comparison:", 
                                                       choices = list("NONE" = 1,
                                                                      "Comedy " = 2, 
                                                                      "Animation" = 3,
                                                                      "Action" = 4,
                                                                      "Drama" = 5,
                                                                      "Horror&Thriller" = 6,
                                                                      "Crime" = 7),selected = 1),
                          
                                           textInput("text_year", label = h5("Input Year"), value = ""))
                          
                          
                      ),
           
              mainPanel(width = 10,tabsetPanel(id = "conditionedPanels", type="pill",
                                  tabPanel("Ranking",br(),
                                           plotlyOutput("case0", width="900px",height="600px"),value = 0),
                                  
                                  tabPanel("Popularity",br(),
                                           fluidRow(
                                             column(6,plotlyOutput("review0", width="550px",height="300px")),
                                             column(6,plotlyOutput("review1", width="550px",height="300px")),
                                             column(12,dygraphOutput("review2", width="900px",height="300px"))),value = 1),
                                            
                                  tabPanel("Timeline",br(),
                                           fluidRow(
                                             dygraphOutput("dygraph", width="900px",height="300px"),
                                             plotlyOutput("case3", width="1000px",height="300px")),value = 2)
                                               
                                            ))
    )),
 #################### end of  xiaoyu s Menu Item ####################  
  
 
 #################### ---- 's Menu Item ####################
 navbarMenu("Similarity",icon=icon("area-chart"),

            tabPanel("Factor analysis",
                     sidebarLayout(position="left",
                       sidebarPanel(
                        h3("2D mapping of the movies"),
                         helpText("We filtered out the movies with > 100 reviews and projected them into 2 dimensional space according to 
                                  users' review scores. Please choose the group you are interested in."),
                         
                         
                         sliderInput("topmovies", label = h4("Top movies"), min = 10, 
                                     max = 327, value = 327),
                         
                         radioButtons("factor", label = h4("Group by"),
                                      choices = list("Year" = 1,"Genre" = 2, "Popularity" = 3,
                                                     "Countries"=4,"Recommendation"=5),selected =1)),
                       
                      
                      mainPanel(width=8,                       
                       plotlyOutput("gmap")))
            ),
            
            tabPanel("Correlation analysis",
                     sidebarLayout(position="left",
                     sidebarPanel(width=3,
                     textInput("interestid", label = h4("Please enter the product id that you are interested in"), value = "B001UV4XI8"),
                     helpText("Example:B001UV4XI8 for Harry Potter and the Deathly Hallows, Part 1 [Blu-ray]"),
                     textOutput("interestmoviename"),
                     br(),
                     uiOutput("interestmovieimg")
                     ),
                     
                     mainPanel(
                       fluidRow(
                         column(6,uiOutput("interestmovieinfo")
                         ),
                         column(6,
                                plotlyOutput("interestmoviehist",width="400px",height="400px"))),
                       fluidRow(
                         h3("Top related movies"),
                         dataTableOutput("interestmovietable"))
                         
                       )
    
                       
                     )
            )

          
            
            
 ),
 #################### end of ---- Menu Item  ####################








###################About us #############################
tabPanel("About us",icon=icon("reddit-alien"),
         
         tabItem(
      tabName = "about",
        box(width = 12,
      h3(icon("group"),"Contact information"),
      hr(),
      #title = "Team Members", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(width = 6,
             p("Ho, Eric Ming-Yin: emh2213@columbia.edu"),
             p("Liu, Mengying: ml3810@columbia.edu"),
             p("Shi, Ruixiong: rs3569@columbia.edu"),
             p("Wang, Xiaoyu: xw2419@columbia.edu"),
             p("Zhao, Yuan: yz2885@columbia.edu"))

)))

##########################End##################################

 )))



