library(visNetwork)
classlist.df <- data.frame(read.csv("bs-cs-tracking-2016.csv",stringsAsFactors = FALSE,strip.white = TRUE,blank.lines.skip = TRUE))

#get list of all classes
stackedClasses <- stack(classlist.df)
allClasses = sort(unique(stackedClasses[1])[,1],decreasing = FALSE)
allClasses = allClasses[-1] #remove empty string at top of list
numClasses <- length(allClasses)
numSubplans <- ncol(classlist.df)
edges.df <- data.frame(from=integer(), to=integer()) #so we can connect subplans to classes

for(r in 1:numClasses){
  for(c in 1:numSubplans){
    if(is.element(allClasses[r],classlist.df[,c])){
#      cat(allClasses[r]," is in ",(colnames(classlist.df))[c],"\n")
      edges.df <-rbind(edges.df, c(r,c+numClasses))
    }
  }
} 

#what we need
#list of nodes:  nodes include:
#tracks and classes
#list of edges:  contain from and to

colnames(edges.df) <- c("from","to")

nodeList <- c(allClasses,colnames(classlist.df))
sizes <- rep(5,numClasses)
sizes <- c(sizes, rep(15,numSubplans))
nodes.df <- data.frame(1:length(nodeList),nodeList)
colnames(nodes.df) <- c("id","label") #add group
nodes.df$size <- sizes
nodes.df$sizeLabels <- factor(nodes.df$size, labels = c("small", "large"))
nodes.df$color.highlight.background <- "orange"
nodes.df$color.highlight.border <- "darkred"
nodes.df$color.background <- c("dodgerblue3", "gold")[nodes.df$sizeLabels]
nodes.df$font.size <- 18
#todo:
#add search feature
#add selector checkbox for subplans on the side
visNetwork(main = "Subplans and Connecting Classes", nodes = nodes.df, edges = edges.df)


