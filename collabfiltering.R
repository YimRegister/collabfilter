

data <-as.data.frame(data)
names(data)<-c("AdInterest")

#from the ad data get a sample. 
mydata <- sample(data$AdInterest,25)
View(mydata)

SAMPLE_LENGTH=7
#then sample from THAT data 3 times for your "friends"
friend1 <- as.character(sample(mydata,SAMPLE_LENGTH))
friend2 <- as.character(sample(mydata, SAMPLE_LENGTH))
friend3 <- as.character(sample(mydata, SAMPLE_LENGTH))

name1 <- "Amanda"
name2 <- "Lisa"
name3 <- "Tom"

#need to make sure they do have similarities

# put into a matrix

friends<-c(rep(name1,SAMPLE_LENGTH),
           rep(name2,SAMPLE_LENGTH),
           rep(name3,SAMPLE_LENGTH)
           
)
prefs <-c(friend1,friend2,friend3)

#friends <-c("tim","tim","abby","abby","kimmy","kimmy")
#prefs <- c("cats","bats","cats","chickens","cats","icecream")

data <- data.frame(friends,prefs)

data<-as.data.frame(table(friends,prefs))
data <- data %>% 
  rename(
    Ad_Preferences = prefs
  )

d <- spread(data,Ad_Preferences,Freq)
library(tidyverse)
cosine_similarity <- function(a,b){
  crossprod(a,b)/sqrt(crossprod(a) * crossprod(b))
}


#for each pair of friends, compute their cosine similarity, 
#then find the highest one



View(d)



names <- levels(d$friends)
combos<- as.data.frame(combn(names,2))

# we want to get the cosine similarity for each user to each other
calc_cos_sim <- function(combos, matrix){
  #we have our combos and we need to get the cosine similarity between each of them
  newframe <- list()
 
 
  for (c in 1:length(combos)){
    
    
    #c[1],c[2]
    firstname <- as.character(combos[[c]][1])
    
    secondname <- as.character(combos[[c]][2])
    #print(paste(firstname,secondname))
    #we need to extract the vectors from the matrix
    first_data <- as.matrix(matrix[matrix$friend==firstname,])
    
    first_data <-first_data[-(1:2)]
 
    second_data <- as.matrix(matrix[matrix$friend==secondname,])
    second_data <-second_data[-(1:2)]
    cs <- cosine_similarity(as.numeric(first_data),as.numeric(second_data))
    #print(cs[1])
    temp <-data.frame(name1=firstname,name2=secondname,cossim=cs)
    newframe[[c]] <- temp
    
  }
  data = do.call(rbind, newframe)
  return(data)
  
}

dd <- calc_cos_sim(combos,d)
dd <- as.data.frame(dd)
#rank by cosine similarity
ranked <- dd %>%
        arrange(desc(cossim))
View(ranked)


dd<-dd[1,]
name <- str_extract(dd$name1,"(\\w+)") 
othername <- str_extract(dd$name2,"(\\w+)") 
similarity <-round( dd[3],2)


#d is our flipdata


list1 <- data %>%
        filter(friends==name,Freq==1)
list1$Ad_Preferences

list2 <- data %>%
  filter(friends==othername,Freq==1)
list2$Ad_Preferences

not_in_common <-anti_join(list1,list2,by="Ad_Preferences")
print(as.character(not_in_common$Ad_Preferences),sep="\n")





presurvey_data <- data.frame(matrix(ncol = 5, nrow = 0))
names <- c(
  'Subject', 'WillingtoShare' ,'AnonymizedUSD' ,'DeanonymizedUSD' ,'PreWritein')
colnames(presurvey_data) <- names
row <- c("9cd87","No","100","500","stuff,stuff, other stuff")
presurvey_data[1, ] <-row
View(presurvey_data)

presurvey_data$CorrectPair = FALSE
presurvey_data$HowSimilar = "you could count up the 1s"
View(presurvey_data)
#question to self: can users identify the cold start problem?


nodes <- read.csv("Datafiles/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Datafiles/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(nodes)
head(links)
library('igraph')
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net
plot(net)
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)




library(visNetwork)

set.seed(124)


incommon <- merge(list1,list2,by="Ad_Preferences")

nodes <- data.frame(id = 1:length(incommon$Ad_Preferences), label = incommon$Ad_Preferences, 
                    group = incommon$friends.x)

length <- (length(list2$friends)+length(list1$friends))
ids<- seq(length(list2$friends)+1,length)

nodes2<-data.frame(id = ids, label = list2$Ad_Preferences, 
                 group = list2$friends)

nodes <- rbind(nodes,nodes2)
nodes$label <- as.character(nodes$label)
nodes$group <- as.character(nodes$group)ststr
friend1 <- c(length+1,as.character(list1$friends[1]),"Friend1")
nodes <- rbind(nodes,friend1)
friend2 <- c(length+2,as.character(list2$friends[1]),"Friend2")
nodes <- rbind(nodes,friend2)
View(nodes)

nodes$image <- "friend1.png"

#for all the edges in list 1, go to friend1

edges <- data.frame(from = 1:length(incommon$Ad_Preferences), to = 15,label="",color="black",arrows="")
#for all the edges in list 2, go to friend2
edges2 <- data.frame(from=ids, to=16,label="",color="black",arrows="")
edges <-rbind(edges,edges2)


#for overlap, go to friend2

suggest<- anti_join(nodes[nodes$group=="Lisa",],nodes[nodes$group=="Amanda",],by="label")
edges3<-data.frame(from=suggest$id,to=15,label="Suggest",color="#39D11A",arrows="to")
edges <-rbind(edges,edges3)

edges4<-data.frame(from=15,to=16,label="Similar",color="red",arrow="")
edges <-rbind(edges,edges4)





visNetwork(nodes,edges) %>%
  visGroups(groupname = as.character(list1$friends[1]), shape = "box",color="lightgray") %>%
  visGroups(groupname = as.character(list2$friends[1]), shape = "box",color="lightgray") %>%
  visGroups(groupname = "Friend1", shape = "circularImage", image="friend1.png") %>%
  visGroups(groupname = "Friend2", shape = "circularImage", image="friend2.png") %>%
  #visHierarchicalLayout()%>%
  visNodes(shapeProperties = list(useBorderWithImage = TRUE))%>%
  visOptions(selectedBy = "group")%>%
  visInteraction(dragNodes = F, dragView = FALSE, zoomView = FALSE) %>%
  visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
  visLayout(randomSeed = 5) %>%
  addFontAwesome()%>%
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -100))

path_to_images <- "www/friend"
nodes <- data.frame(id = 1:4, 
                    shape = c("image", "circularImage"),
                    image = paste0(path_to_images, 1:4, ".png"),
                    label = "I'm an image")

edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  

