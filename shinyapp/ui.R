library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(jsonlite)
library(giphyr)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(shinyMatrix)



#m <- matrix(runif(12), 6, 3, dimnames = list(c("a","b","c","d","e","f"), c("x", "y","z"))) 


shinyUI(fluidPage(useShinyjs(),theme = shinytheme("cosmo"),
                  tags$head(
                          tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                  ),
                  
        #titlePanel("Learn Machine Learning on your own Facebook Data"),
        navbarPage("",id="thenav",
        tabPanel(icon("home"),
                 
                 fluidRow(column(tags$img(src="lookingdata.png",width="100px",height="100px"),width=2),
                          column(
                                  
                                  br(),
                                  h2(" What does Facebook think you're interested in?",style="color:black;background-color:white;padding:15px;border-radius:10px"),
                                  
                                  
                                  h4("This lesson will help you learn machine learning principles on your own Facebook data.",style="color:black;background-color:white;padding:15px;border-radius:10px"),
                                  
                                  width=7),
                          ),
                 hr(),
                 
                 fluidRow(column(
                         br(),
                         
                         p("You can download a zip file of your data by following these instructions:",
                           br(),
                           a(href="https://www.facebook.com/help/212802592074644", "How to Download Your Facebook Data",target="_blank",style="color:#4287f5"),style="color:black"),
                           br(),
                         p("Download your data in 'JSON' format:"),
                        img(src="download.png",width="70%"),
                        br(),
                        br(),
                        p("There are lots of things available for download:"),
                        img(src="all.png",width="70%"),
                        br(),
                        br(),
                        p("But for this tutorial you only need Ads and Businesses:"),
                        img(src="ads_and_bus.png",width="70%"),
                        br(),br(),
                        p("Extract the zip file and look for the folder 'ads_and_businesses'. Don't look at anything yet."),
                        img(src="ads_interests.png",width="50%"),
                        br(),br(),
                        p("We do not save any of your Facebook data for this research study. It is for you to look at."),
                        br(),
                        actionButton("homenext", "Next",style="margin-bottom:4%;font-size:17pt;"),
                        br(),
                         width=11,offset=1),
                         
                          
                      
                 
                        
                                     
                             ),
                 ),
        ###############################################################################################
        tabPanel(title="Intro",value="intro",
         column(
                h4("When you download your Facebook Data, you get", em(" a lot "), "of data. This lesson will help you to explore:",align="left"),
                br(),
                tags$ul(
                        tags$li(p("What Facebook thinks you're interested in")), 
                        tags$li(p("What kinds of computations can be done on that data")),
                        tags$li(p("A common algorithm called 'collaborative filtering' used for generating recommendations")), 
                        align="left"
                ),
               
                
                br(),
                actionButton("intronext", "Next",style="margin-bottom:4%;font-size:17pt;"),
                br(),
                
                width=8,offset=2)
        ),
        
        ###############################################################################################
        tabPanel(title="Pre-Survey",value="presurvey",
                 htmlOutput("pre")
        ),
        
        
        
        
        #####################################################################################################################################################################
                             tabPanel(title="Step 1",value="step1",
                                      
                                     
                                      
                                    
                                      column(
                                               
                                               h1("What Facebook Thinks You're Interested In",style="color:black;text-align:center;"),
                                                       
        
                                               #div(img(src="collabfilter_realpython.webp",width="500px"),style="text-align:center;"),
                                               width=12
                                               
                                      ),
                                                
                                      
                                      column(
                                              
                                              br(),br(),
                                              h3("Upload the file titled ", strong('ads_and_businesses/ads_interests.json') ,br()," from the Facebook Download folder:",
                                              align="left"),
                                              br(),
                                              fileInput("adsfile", "Choose File",
                                                        multiple = FALSE,
                                                        accept = c(".json"))
                                              ,
                                              width=8,offset=2),
        
                                        
                                              fluidRow(
                                                      column(5,offset=1, DT::dataTableOutput('maketable')),
                                                      column(4, offset=1, h3("Choose the interests that actually apply to you by clicking on them. We will use this data for the rest of the tutorial. Choose 7."),verbatimTextOutput('x4'),
                                                      hidden(actionButton("done","Done")))
                                              )
                                      
                                                
                                              
                                                
                                               
                                                
                                        
                             ),
        
                                      
                             
        
        
        ############################################
        ########################################################################################
                        
        ###################################################################################################################################################
                             
                             tabPanel(title="Step 2",value="step2",
                                      
                                      fluidRow(column(width=2),
                                               column(
                                                       h1("Let's Make Friends",style="color:black;text-align:center"),
                                                       width=8),
                                      ),
                                      br(),
                                      fluidRow(column(width=2,align="center"),
                                               column(width=8,
                                                      div(img(src="youarehere.png",height="100px"),br(),
                                                          
                                                          p("My Selected Interests:"),style="text-align:center;"),
                                                      
                                                      verbatimTextOutput("collect"),br(),
                                                      p("Imagine these people are some of your friends on Facebook. ",style="color:black;text-align:center"),
                                                      br(),
                                                      div(actionButton("generate_avatar","Regenerate Avatars",style="margin-bottom:2%;margin-top:1%;font-size:12pt;"),style="text-align:center"),
                                                      p(strong("Write in the names of your 'friends' below."),style="text-align:center;"),br(),
                                                      p("(it helps you to remember who is who)",style="text-align:center;"),
                                                      div(
                                                              div(column(3,
                                                                         div(textInput("user", "", "Me"),
                                                                             textOutput("username"),
                                                                             uiOutput(outputId = "renderuserimage"),
                                                                             br(),
                                                                             #div(verbatimTextOutput("friend1"),style="margin-top:10px;text-align:left;"),
                                                                             style="text-align:center;"),
                                                                         style="text-align:center;")),
                                                              div(column(3,
                                                                         div(textInput("name1", "", "Friend1"),
                                                                         textOutput("name1value"),
                                                                         uiOutput(outputId = "friend1_image"),
                                                                         br(),
                                                                         #div(verbatimTextOutput("friend1"),style="margin-top:10px;text-align:left;"),
                                                                         style="text-align:center;"),
                                                                         style="text-align:center;")),
                                                              div(column(3,
                                                                         div(textInput("name2", "", "Friend2"),
                                                                             textOutput("name2value"),
                                                                             uiOutput(outputId = "friend2_image"),
                                                                             br(),
                                                                             #div(verbatimTextOutput("friend2"),style="margin-top:10px;text-align:left;"),
                                                                             style="text-align:center;"),
                                                                         style="text-align:center;")),
                                                              div(column(3,
                                                                         div(textInput("name3", "", "Friend3"),
                                                                             textOutput("name3value"),
                                                                             uiOutput(outputId = "friend3_image"),
                                                                             br(),
                                                                             #div(verbatimTextOutput("friend3"),style="margin-top:10px;text-align:left;"),
                                                                             style="text-align:center;"),
                                                                         style="text-align:center;")),
                                                              
                                                              br(),
                                                              br(),
                                                              
                                                              
                                                             
                                                             
                                                              
                                                              style="text-align: center;"),
                                                      
                                                      actionButton("step2next", "Next",style="margin-bottom:4%;margin-top:2%;font-size:17pt;"),
                                                      br(),
                                                      style="text-align: center;" ),
                                               
                                      ),
                                      
                                      
                                      
                                ),
        ####################################################################################################################
        
        tabPanel(title="Step 3",value="step3",
                 
                 fluidRow(column(width=2),
                          column(
                                  h1("Your Friends' Generated Interests",style="color:black;text-align:center"),
                                  p("These friends each have some interests similar to yours. Here, we have randomly generated some interests from your selected interests that you and these friends might have in common. On Facebook, they would have access to the real interest information about your friends.",style="color:black;text-align:justify"),
                                  br(),
                                  width=8),
                 ),
                 br(),
                 
                          column(width=12,
                                 div(
                                         div(column(3,
                                                    
                                                    div(textOutput("username_2"),style="text-align:center;",
                                                        uiOutput("renderuserimage2"),),
                                                    br(),
                                                    div(verbatimTextOutput("collect2"),style="margin-top:10px;text-align:left;"),
                                                    style="text-align:left;")),
                                         div(column(3,
                                                   
                                                    div(textOutput("name1value_2"),style="text-align:center;",
                                                    uiOutput("friend1_image2"),),
                                                    br(),
                                                    div(verbatimTextOutput("friend1_2"),style="margin-top:10px;text-align:left;"),
                                                    style="text-align:left;")),
                                         div(column(3,
                                                    
                                                    div(textOutput("name2value_2"),style="text-align:center;",
                                                    uiOutput(outputId = "friend2_image2"),),
                                                    br(),
                                                    div(verbatimTextOutput("friend2_2"),style="margin-top:10px;text-align:left;"),
                                                    style="text-align:left;")),
                                         div(column(3,
                                                   
                                                    div(textOutput("name3value_2"),style="text-align:center;",
                                                    uiOutput(outputId = "friend3_image2"),),
                                                    br(),
                                                    div(verbatimTextOutput("friend3_2"),style="margin-top:10px;text-align:left;"),
                                                    br(),br(),
                                                    style="text-align:left;")),
                                         
                                         
                                         
                                         
                                         
                                         style="text-align: center;"),
                                 br(),
                                 
                                 h3("All friends share common interests in:"),
                                 br(),
                                 tags$style(type='text/css', '#incommon {background-color: rgba(144,238,144,0.40); color: black;}'),
                                 verbatimTextOutput("incommon"),
                                 br(),
                                 actionButton("step3next", "Next",style="margin-bottom:4%;font-size:17pt;"),
                                 br(),
                                 
                                
                                 style="text-align: center;"  )
                          
                        
                 
                 
                 
        ),
        ###########################################################
        
        tabPanel(title="Step 4",value="step4",
                 
                 
                 column(width = 8,offset=2, 
                          column(
                                  h1("What You're All Interested In",style="color:black;text-align:center"),
                                  br(),
                                  p("This is how data often gets represented so that machines can read it. It is a chart with the Friends across the top and the Interests along the side. A ",strong("0")," means that the friend in that column has", strong("NOT"),"shown interest in that thing. A ",strong("1")," means the friend ",strong("IS"), "interested in that thing."),
                                 br(),br()
                                  ,width=12),
                 
                 br(),
                 br(),
                 
                 
                      
                h3("Chart representation"),   
                br(),
                
                DT::dataTableOutput("matrix"),
                br(),
                h3("How would you determine which of these friends are the most similar?"),
                column(textAreaInput("aftermatrix","","",height="200px"),
                br(),
                br(),
                align="center",width=12),
                br(),
                actionButton("step4next", "Next",style="margin-bottom:4%;margin-top:2%;font-size:17pt;"),
                br(),align="center",style="text-align:center;")
                 
                 
                 
        ),
        #####################################################
        
        tabPanel(title="Step 5",value="step5",
                 
                 
                 column(width = 12, 
                       
                                h1("Network Graph",style="color:black;text-align:center"),
                                br(),
                                p("Interpet this graph"),
                        
                        visNetworkOutput("network",width="95%",height = "100vh"),
                        br(),
                        actionButton("step5next", "Next",style="margin-bottom:4%;margin-top:2%;font-size:17pt;"),
                        br(),align="center",style="text-align:center;")
                 
                 
                 
        ),
        ####################################################
        
        
        tabPanel(title="Step 6",value="step6",
                 
                 fluidRow(column(width=12,align="center"),
                          column(
                                  h1("Who is Most Similar to You?",style="color:black;text-align:center"),
                                  br(),
                                  p("We can actually count up the similarity between each pair of friends. If you want to know more about that metric, it's called Cosine Similarity. But basically, friends that have more things in common will get rated as more similar.",style="margin-left:20%;margin-right:20%;"),
                                  br(),
                                  br(),
                                  width=12),
                          column(
                                   width=12, align="center",tableOutput("combos"),
                                   br(),
                                   br(),
                                   tags$head(tags$style("#mostsimilar{color: #4287f5;
                                        font-size: 17px;
                                        }"
                                   )
                                   ),
                                   textOutput("mostsimilar"),
                                   
                          ),
                          
                          br(),
                          actionButton("step6next", "Next",style="text-align:center,margin-bottom:4%;margin-top:2%;font-size:17pt;"),
                          br(),
                align="center" ),
               
                 
                 
                 
                 
                 
        ),
        #####################################################
        #####################################################
        
        
        tabPanel(title="Step 7",value="step7",
                 
                 fluidRow(column(width=12,align="center"),
                          column(
                                  h1("New Recommendations for You",style="color:black;text-align:center"),
                                  br(),
                                  div(htmlOutput("explainsimilar"),style="font-size:14pt;"),
                                  br(),
                                  br(),
                                  width=12),
                          column(
                                  width=6, align="center",
                                  textOutput("similar1"),
                                  br(),
                                  uiOutput(outputId = "similarimage1"),
                                  br(),
                                  verbatimTextOutput("displaysimilarity1"),
                                  br()
                                  
                          ),
                          column(
                                  width=6, align="center",
                                  textOutput("similar2"),
                                  br(),
                                  uiOutput(outputId = "similarimage2"),
                                  br(),
                                  verbatimTextOutput("displaysimilarity2"),
                                  br()
                                  
                          ),
                          column(
                                  width=12, align="center",
                                  div(htmlOutput("recommend"),style="font-size:14pt;"),
                                  br(),
                                  tags$style(type='text/css', '#antijoin {background-color: rgba(144,238,144,0.40); color: black;}'),
                                 verbatimTextOutput("antijoin"),
                                  br()
                                  
                          ),
                          
                          br(),
                          actionButton("step7next", "Next",style="text-align:center,margin-bottom:4%;margin-top:2%;font-size:17pt;"),
                          br(),
                          
                          width=12,align="center")
                    
                 
                 
                 
                 
                 
        ),
        #####################################################
        
        tabPanel(title="Post-Survey",value="postsurvey",
                 htmlOutput("post")
        )
              
                  
)))