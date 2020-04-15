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
library(tidyverse)
library(lubridate)
library(mongolite)
library(data.table)
library(visNetwork)




options(mongodb = list(
  "host" = "yimdata-t0gd6.mongodb.net",
  "username" = "Yim",
  "password" = "yimregister"
)
)

databaseName <- "shinydatabase"
collectionName <- "responses"

saveData <- function(data) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Insert the data into the mongo collection as a data.frame
  data <- as.data.frame(data)
  db$insert(data)
}

loadData <- function() {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
}

OPTIONS = 7



cosine_similarity <- function(a,b){
  crossprod(a,b)/sqrt(crossprod(a) * crossprod(b))
}

shinyServer(function(input, output,session) {
 
values <- reactiveValues( presurvey_data=NULL
  
)

images_and_names <- reactiveValues( imagesdf=NULL
                          
)

 makedata <- reactive({
    req(input$adsfile)
    tryCatch(
      {
        
        thedata <- fromJSON(input$adsfile$datapath)
        
        thedata <-as.data.frame(thedata)
        names(thedata)<-c("Interest")
        #set.seed(42)
        rows <- sample(nrow(thedata))
        shuffle <- as.data.frame(thedata[rows,])
        names(shuffle)<-c("Interest")
       return(shuffle)
        
        
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
      
      
    )
  })
 

    
grab_answer_from_matrx <- reactive({
  
   print(input$aftermatrix)
   print(typeof(input$aftermatrix))
   values$presurvey_data$HowSimilarity <-as.character(input$aftermatrix)
   
 })

output$maketable <- DT::renderDataTable(
  
  {makedata()},server=TRUE,options=list(pageLength=15),
  
  
)


output$renderuserimage <- output$renderuserimage2 <- renderUI({
  srcstring = "youarehere.png"
  
  if(!is.null(input$userphoto)){
    req(input$userphoto)
    print(input$userphoto$datapath)
    srcstring = gsub("\\\\", "/", input$userphoto$datapath)
    print(srcstring)
    
  }
  else{
    srcstring="youarehere.png"
  }
  
  
  tags$img(src = srcstring,width="100px")
  
})



regenerate_avatar <- function(){
  friend_imgs <- c("friend1.png","friend2.png","friend3.png","friend4.png","friend5.png","friend6.png","friend7.png","friend8.png","friend9.png","friend10.png",
                   "friend11.png","friend12.png","friend13.png","friend14.png","friend15.png","friend16.png","friend17.png","friend18.png","friend19.png","friend20.png",
                   "friend21.png","friend22.png","friend23.png","friend24.png")
  
  choices <- sample(friend_imgs,3)
  labels <- c("Friend1","Friend2","Friend3")
  
  images_and_names$imagesdf$Name = c(input$name1,input$name2,input$name3)
  images_and_names$imagesdf$Image = choices
  View(images_and_names$imagesdf)
  

  output$friend1_image <- output$friend1_image2 <-renderUI({
    srcstring = as.character(images_and_names$imagesdf$Image[images_and_names$imagesdf$Name==input$name1])
    
    tags$img(src = srcstring,width="100px")
  })
  output$friend2_image <- output$friend2_image2 <- renderUI({
    
    srcstring = as.character(images_and_names$imagesdf$Image[images_and_names$imagesdf$Name==input$name2])
    
    tags$img(src = srcstring,width="100px")
  })
  
  output$friend3_image <- output$friend3_image2 <- renderUI({
    
    srcstring = as.character(images_and_names$imagesdf$Image[images_and_names$imagesdf$Name==input$name3])
    
    tags$img(src = srcstring,width="100px")
  })
  
}




# print the selected indices
output$x4 = renderPrint({
  s = input$maketable_rows_selected
    
    cat('The interests you chose:\n\n')
    cat(as.character(makedata()$Interest[s]),sep="\n",fill=TRUE)
    
    shinyjs::show("done")
    shinyjs::disable("done")
    
    if(length(s)==OPTIONS){
      shinyjs::enable("done")
      
    }
  
})

observeEvent(input$done,
             {
               
               updateTabsetPanel(session, "thenav",
                                 selected = "step2")
               
             })


collect <- reactive({
  return(as.character(makedata()$Interest[input$maketable_rows_selected]))
  
})
output$collect <-output$collect2<- renderPrint({
                  cat(sort(collect()),sep="\n")
          })


observeEvent(input$name1,
             { 
               images_and_names$imagesdf$Name[1] = input$name1
               View(images_and_names$imagesdf)
             })
observeEvent(input$name2,
             { 
               images_and_names$imagesdf$Name[2] = input$name2
               View(images_and_names$imagesdf)
             })

observeEvent(input$name3,
             { 
               images_and_names$imagesdf$Name[3] = input$name3
               View(images_and_names$imagesdf)
             })


RollDie <- reactive({
   return(sample(2:OPTIONS-2,3,replace=F))
  })

sample1 <- reactive({
  N = RollDie()[1]
  rest = OPTIONS - N
  sameasyou <- sample(as.character(makedata()$Interest[input$maketable_rows_selected]),N,replace=F)
  therest <- sample(as.character(makedata()$Interest),rest,replace=F)
  sample <- c(sameasyou,therest)
  return(sort(sample))
})


sample2 <- reactive({
  N = RollDie()[2]
  rest = OPTIONS - N
  sameasyou <- sample(as.character(makedata()$Interest[input$maketable_rows_selected]),N,replace=F)
  therest <- sample(as.character(makedata()$Interest),rest,replace=F)
  sample <- c(sameasyou,therest)
  return(sort(sample))
})

sample3 <- reactive({
  N = RollDie()[3]
  rest = OPTIONS - N
  sameasyou <- sample(as.character(makedata()$Interest[input$maketable_rows_selected]),N,replace=F)
  therest <- sample(as.character(makedata()$Interest),rest,replace=F)
  sample <- c(sameasyou,therest)
  return(sort(sample))
  })

output$friend1 <- output$friend1_2 <- renderPrint({
  
  cat("\n")
  cat(sample1(),sep="\n")
})



output$friend2 <- output$friend2_2 <-renderPrint({
  
  cat("\n")
  cat(sample2(),sep="\n")
})


output$friend3 <- output$friend3_2 <-renderPrint({
  
  cat("\n")
  cat(sample3(),sep="\n")
})

#literally a github issue of duplicate bindings not my fault
output$name1value <- output$name1value_2 <- renderText({ input$name1 })
output$name2value <- output$name2value_2 <- renderText({ input$name2 })
output$name3value <- output$name3value_2 <- renderText({ input$name3 })
output$username <- output$username_2 <- renderText({ input$user })

common <- reactive({
  common1_2 <- Reduce(intersect, list(sample1(),sample2()))
  common1_3 <- Reduce(intersect, list(sample1(),sample3()))
  common2_3 <- Reduce(intersect, list(sample2(),sample3()))
  common1_2_3 <- Reduce(intersect, list(sample1(),sample2(),sample3()))
  
  return(c(common1_2,common1_3,common2_3,common1_2_3))
  
  
})

common1_2_3 <- reactive({
  return(Reduce(intersect, list(sample1(),sample2(),sample3())))
})

output$incommon <- renderPrint({
  if(length(common1_2_3())){
 
  cat(common1_2_3(),sep="\n")
  }
  
  else{
    cat("Nothing")
  }
  
  
})

make_matrix <- reactive({
  
  friends<-c(rep(input$name1,length(sample1())),
             rep(input$name2,length(sample2())),
             rep(input$name3,length(sample3())),
             rep(input$user,length(collect()))
             
             )
  print(friends)
  prefs <-c(sample1(),sample2(),sample3(),collect())

  
  data <- data.frame(friends,prefs)

 
  data<-as.data.frame(table(friends,prefs))
  data <- data %>% 
    rename(
      Ad_Preferences = prefs
    )
  
  d <- spread(data,friends,Freq)
  
  
  
  return (d)
})

flip_data <- reactive({
  friends<-c(rep(input$name1,length(sample1())),
             rep(input$name2,length(sample2())),
             rep(input$name3,length(sample3())),
             rep(input$user,length(collect()))
             
  )
  prefs <-c(sample1(),sample2(),sample3(),collect())
  data <- data.frame(friends,prefs)
  
  data<-as.data.frame(table(friends,prefs))
  data <- data %>% 
    rename(
      Ad_Preferences = prefs
    )
  
  d <- spread(data,Ad_Preferences,Freq)
  
  return(d)
  
})

get_cosine_similarity <- reactive({
  
  
  names <- levels(flip_data()$friends)
  
  combos<- as.data.frame(combn(names,2))
  
  #we have our combos and we need to get the cosine similarity between each of them
  newframe <- list()
  
  
  for (c in 1:length(combos)){
    
    
    #c[1],c[2]
    firstname <- as.character(combos[[c]][1])
    
    secondname <- as.character(combos[[c]][2])
    #print(paste(firstname,secondname))
    #we need to extract the vectors from the matrix
    first_data <- as.matrix(flip_data()[flip_data()$friend==firstname,])
    
    first_data <-first_data[-(1:2)]
    
    second_data <- as.matrix(flip_data()[flip_data()$friend==secondname,])
    second_data <-second_data[-(1:2)]
    cs <- cosine_similarity(as.numeric(first_data),as.numeric(second_data))
    #print(cs[1])
    temp <-data.frame(name1=firstname,name2=secondname,cossim=cs)
    newframe[[c]] <- temp
    
  }
  data = do.call(rbind, newframe)
  data <- as.data.frame(data)
  data <- data %>%
    arrange(desc(cossim))
  colnames(data) <- c("Name1", "Name2", "Similarity (0-1)")
  
  #rank by cosine similarity
 
  
  return(data)
  
})

output$combos <-renderTable(
  get_cosine_similarity()
)

output$mostsimilar <- renderPrint({
  d<- get_cosine_similarity()[1,]
  
  
  name <- str_extract(d$Name1,"(\\w+)") 
  othername <- str_extract(d$Name2,"(\\w+)") 
  similarity <-round( d[3],2)
  
  
  cat(
    paste("The friends with the most in common are " ,name, " and ", othername, " with a similarity of: ", similarity)
  )
})

output$explainsimilar <- renderPrint({
  d<- get_cosine_similarity()[1,]
  
 
  name <- str_extract(d$Name1,"(\\w+)") 
  othername <- str_extract(d$Name2,"(\\w+)") 
  similarity <-round( d[3],2)
 
  
  cat(
    paste("Because <strong>" ,name, "</strong> and <strong>", othername, "</strong> are most similar, now we can recommend Ads to <strong>", name, "</strong> based on <strong>", othername,"</strong>'s Ad Preferences. That is a very simple explanation of the collaborative filtering algorithm.")
  )
})

output$recommend <- renderPrint({
  d<- get_cosine_similarity()[1,]
  
  
  name <- str_extract(d$Name1,"(\\w+)") 
  othername <- str_extract(d$Name2,"(\\w+)") 
  similarity <-round( d[3],2)
  
  cat(
    paste("Recommend to <strong>" ,othername, "</strong><br> from <strong>", name, "'s</strong> Ad Preferences:<br>")
    
  )
  
})

makedataagain <- reactive({
  friends<-c(rep(input$name1,length(sample1())),
             rep(input$name2,length(sample2())),
             rep(input$name3,length(sample3())),
             rep(input$user,length(collect()))
             
  )
  prefs <-c(sample1(),sample2(),sample3(),collect())
  data <- data.frame(friends,prefs)
  
  data<-as.data.frame(table(friends,prefs))
  
  
  return(data)
  
})

similarity1<-reactive({data<-makedataagain()
d<- get_cosine_similarity()[1,]
name <- str_extract(d$Name1,"(\\w+)") 
othername <- str_extract(d$Name2,"(\\w+)") 
similarity <-round( d[3],2)


interests1 <-  data %>%
  filter(friends==name,Freq==1)

return(interests1)
  
})

output$displaysimilarity1 <- renderPrint({
  
  
  interests1<-similarity1()
  interests1<-as.character(interests1$prefs)
  
  cat(interests1,sep="\n")
  
})


output$similar1 <- renderPrint({
  d<- get_cosine_similarity()[1,]
  
  
  name <- str_extract(d$Name1,"(\\w+)") 
  cat(name)
})

output$similar2 <- renderPrint({
  d<- get_cosine_similarity()[1,]
  
  
  othername <- str_extract(d$Name2,"(\\w+)") 
  cat(othername)
})

makenetwork <- reactive({
  
  
})

highlight <- function(text, search) {
  x <- unlist(strsplit(text, split = " ", fixed = T))
  x[tolower(x) == tolower(search)] <- paste0("<mark>", x[tolower(x) == tolower(search)], "</mark>")
  paste(x, collapse = " ")
}

similarity2<-reactive({
  data<-makedataagain()
  
  d<- get_cosine_similarity()[1,]
  
  
  name <- str_extract(d$Name1,"(\\w+)") 
  othername <- str_extract(d$Name2,"(\\w+)") 
  similarity <-round( d[3],2)
  
  interests2 <-  data %>%
    filter(friends==othername,Freq==1)
  
  return(interests2)
  
})

output$displaysimilarity2 <- renderPrint({

  interests2<-similarity2()
  interests2<-as.character(interests2$prefs)
  cat(interests2,sep="\n")
  
  
})

#display the antijoin of the two friends

output$antijoin <- renderPrint({
  not_in_common <- anti_join(similarity1(),similarity2(),by="prefs")
  not_in_common <-as.character(not_in_common$prefs)
  cat(not_in_common,sep="\n")
  
  
})

#the visNetwork
output$network <-renderVisNetwork({


  
  incommon <- merge(similarity2(),similarity1(),by="prefs")
  View(incommon)
  
  nodes <- data.frame(id = 1:length(incommon$prefs), label = incommon$prefs, 
                      group = incommon$friends.x) #me
  
  length <- (length(similarity2()$friends)+length(similarity1()$friends))
  ids<- seq(length(similarity2()$friends)+1,length)
  
  nodes2<-data.frame(id = ids, label = similarity1()$prefs, 
                     group = similarity1()$friends) #friend
  
  nodes <- rbind(nodes,nodes2)

  nodes$label <- as.character(nodes$label)
  nodes$group <- as.character(nodes$group)
  
  friend1 <- c(length+1,as.character(similarity1()$friends[1]),"firstfriend") #friend
  nodes <- rbind(nodes,friend1)
  friend2 <- c(length+2,as.character(similarity2()$friends[1]),"secondfriend") #me
  nodes <- rbind(nodes,friend2)
  View(nodes)
  
  
  
  #for all the edges in list 1, go to me
  edges <- data.frame(from = 1:length(incommon$prefs), to = OPTIONS*2+2,label="",color="black",arrows="") #me
  
  #for all the edges in list 2, go to friend
  edges2 <- data.frame(from=ids, to=OPTIONS*2+1,label="",color="black",arrows="") #friend
  edges <-rbind(edges,edges2)
  
  
  #for overlap, suggest from friend to me
  
  suggest<- anti_join(nodes[nodes$group==as.character(similarity1()$friends[1]),],nodes[nodes$group==as.character(similarity2()$friends[1]),],by="label")
  View(suggest)
  edges3<-data.frame(from=suggest$id,to=OPTIONS*2+2,label="Suggest",color="#39D11A",arrows="to")
  edges <-rbind(edges,edges3)
  
  edges4<-data.frame(from=OPTIONS*2+1,to=OPTIONS*2+2,label="Similar",color="red",arrows="to;from")
  edges <-rbind(edges,edges4)
  
  
  imageforfriend <- images_and_names$imagesdf$Image[images_and_names$imagesdf$Name == similarity1()$friends[1]]
  
  
  visNetwork(nodes,edges) %>%
    visGroups(groupname = as.character(similarity1()$friends[1]), shape = "box",color="lightgray") %>%
    visGroups(groupname = as.character(similarity2()$friends[1]), shape = "box",color="lightgray") %>%
    visGroups(groupname = "firstfriend", shape = "circularImage", image=imageforfriend) %>%
    visGroups(groupname = "secondfriend", shape = "circularImage", image="youarehere.png") %>%
    #visHierarchicalLayout()%>%
    visNodes(shapeProperties = list(useBorderWithImage = TRUE))%>%
    visOptions(selectedBy = "group")%>%
    visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
    visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
    visLayout(randomSeed = 5) %>%
    addFontAwesome()%>%
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -100))
  
  
})

# the matrix of Friends and their interests
output$matrix <- DT::renderDataTable(
  DT::datatable({
    make_matrix()
  },options=list(pageLength=18,sDom  = '<"top">lrt<"bottom">ip'),class = 'cell-border stripe',rownames = FALSE))

#get the similar people's images


output$similarimage1 <- renderUI({
  srcstring = images_and_names$imagesdf$Image[images_and_names$imagesdf$Name == similarity1()$friends[1]]
  tags$img(src = srcstring,width="100px")
  
})


output$similarimage2 <- renderUI({
  srcstring = "youarehere.png"
  tags$img(src = srcstring,width="100px")
  
})
# the choosing which pair they think is the most similar, needs to be populated with the names and correct answer
observe({
  
  x <- c()
  # Can also set the label and select items
  updateRadioButtons(session, "choosepair2",
                     label = paste(""),
                     choices = x,
                     selected = x
  )
})


question_one <- function(...) {
  renderUI({ source("question1.R", local = TRUE)$value })
}

question_two <- function(...) {
  renderUI({ source("question2.R", local = TRUE)$value })
}

question_three <- function(...) {
  renderUI({ source("question3.R", local = TRUE)$value })
}

thanks <- function(...) {
  renderUI({ source("thanks.R", local = TRUE)$value })
}




render_page <- function(...,f, title = "test_app") {
  page <- f(...)
  renderUI({
    fluidPage(page, title = title)
  })
}

presurvey <- function(...) {
  args <- list(...)
  div(class = 'container', id = "presurvey",
      div(class = 'col-sm-2'),
      div(class = 'col-sm-8',
          h1("Survey"),
          p("Before exploring your data, please complete the following survey. "),
          br(),
          actionButton("block_one", "Start",style="font-size:17pt;")
      ))
  
}
  
  ## render the presurvey
  output$pre <- render_page(f = presurvey)
  
  observeEvent(input$block_one, {
    output$pre <- render_page(f = question_one)
   
  })
  
  observeEvent(input$block_two, {
    
    output$pre <- render_page(f = question_two)
    
  })
  
  observeEvent(input$block_three, {
    
    output$pre <- render_page(f = question_three)
    
    
  })
  
  observeEvent(input$block_four, {
    
    
    output$pre <- render_page(f = thanks)
    
    
    
  })
 
  create_unique_ids <- function( char_len = 7){
   
    pool <- c(letters, LETTERS, 0:9)
    
     # pre-allocating vector is much faster than growing it
    
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
      
      
    
    return(this_res)
  }
  
  
  
  observeEvent(input$completed, {
    id <- create_unique_ids()
    print(id)
    values$presurvey_data <- data.frame(matrix(ncol = 5, nrow = 0))
    names <- c(
      'Subject', 'WillingtoShare' ,'AnonymizedUSD' ,'DeanonymizedUSD' ,'PreWritein')
    colnames(values$presurvey_data) <- names
    
    
    row <- c(id,input$question1,input$question2,input$question2b,input$question3)
    values$presurvey_data[1, ] <-row
    
    updateTabsetPanel(session, "thenav",
                      selected = "step1")
 
    
    
    
  })
  
postquestion_one <- function(...) {
    renderUI({ source("postquestion1.R", local = TRUE)$value })
  }
  postsurvey <- function(...) {
    args <- list(...)
    div(class = 'container', id = "postsurvey",
        div(class = 'col-sm-2'),
        div(class = 'col-sm-8',
            h1("Please complete this post survey."),
            p("In order to be compensated for this study, please fill out a few questions after completing the tutorial. "),
            br(),
            actionButton("postblock_one", "Start",style="font-size:17pt;")
        ))
    
  }
  
  ## render the postsurvey
  output$post <- render_page(f = postsurvey)
  
  observeEvent(input$postblock_one, {
    output$post <- render_page(f = postquestion_one)
    
  })
  
  
  
  
  

  
  observeEvent(input$homenext, {
    regenerate_avatar()
    updateTabsetPanel(session, "thenav",
                      selected = "intro")
    
    
    
  })
  observeEvent(input$intronext, {
    updateTabsetPanel(session, "thenav",
                      selected = "presurvey")
    
    
    
  })
  
  observeEvent(input$step2next, {
    updateTabsetPanel(session, "thenav",
                      selected = "step3")
    
    
    
  })
  
  observeEvent(input$step3next, {
    updateTabsetPanel(session, "thenav",
                      selected = "step4")
    
    
    
  })
  
  
  observeEvent(input$step4next, {
    updateTabsetPanel(session, "thenav",
                      selected = "step5")
    
    
    
  })
  
  observeEvent(input$step5next, {
    updateTabsetPanel(session, "thenav",
                      selected = "step6")
    
    
    
  })

  
  observeEvent(input$step6next, {
    updateTabsetPanel(session, "thenav",
                      selected = "step7")
    
    
    
  })
  
  
  observeEvent(input$step7next, {
    updateTabsetPanel(session, "thenav",
                      selected = "postsurvey")
    
    
    
  })
  
  
  observeEvent(input$step4next,{
    grab_answer_from_matrx()
  })


  observeEvent(input$generate_avatar,{
    regenerate_avatar()
    
    
  })
  
  

    
  })
