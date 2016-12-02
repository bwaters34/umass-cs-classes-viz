library(cluster)
library(fpc)
library(ggvis)

classlist.df <- data.frame(read.csv("bs-cs-tracking-2016.csv",stringsAsFactors = FALSE,strip.white = TRUE,blank.lines.skip = TRUE))

#get list of all classes
stackedClasses <- stack(classlist.df)
allClasses = sort(unique(stackedClasses[1])[,1],decreasing = FALSE)
allClasses = allClasses[-1] #remove empty string at top of list

for(i in 1:length(allClasses)){
  allClasses[i] = strsplit(allClasses[i]," ")[[1]][1]
}

kMeansMatrix = matrix(0,nrow=length(allClasses),ncol=ncol(classlist.df))
rownames(kMeansMatrix) <- allClasses
for(r in 1:nrow(kMeansMatrix)){
  for(c in 1:ncol(kMeansMatrix)){
    if(is.element(allClasses[r],classlist.df[,c])){
      cat(allClasses[r]," is in ",(colnames(classlist.df))[c],"\n")
      kMeansMatrix[r,c] <-1
    }
  }
} 

coordinateClasses <- cmdscale(dist(kMeansMatrix))

dist_test <- dist(kMeansMatrix)
plot(coordinateClasses)

colnames(coordinateClasses) <- c("component1", "component2")
rownames(coordinateClasses) <- rownames(kMeansMatrix)

coordinateClasses <- data.frame(coordinateClasses)
coordinateClasses$classNames <- allClasses
coordinateClasses$id <- 1:nrow(coordinateClasses) #used as a key for tooltips

className <- function(x){
    if(is.null(x)) return(NULL)
    row <- x$id
    correspondingClasses <- which((round(coordinateClasses$component1[row],digits = 2) == round(coordinateClasses$component1,digits =2))&(round(coordinateClasses$component2[row],digits = 2) == round(coordinateClasses$component2,digits =2)))
    output <- ""
    for(i in 1:length(correspondingClasses)){
    output <-  paste0(output,"\nClass:", coordinateClasses[correspondingClasses[i],]$classNames)
    }
    paste0(output)
}

plot <- coordinateClasses %>%
  ggvis(x = ~component1, y = ~component2, fill := "black", opacity := 0.4, key:= ~id) %>%
  layer_points()

plot %>% add_tooltip(className, "hover")
plot %>% add_tooltip(className, "click")
sortedClasses <- kMeansMatrix[do.call(order, lapply(1:NCOL(kMeansMatrix), function(i) kMeansMatrix[, i])), ]

sorted <- sort(rowSums(kMeansMatrix))




# - - - OLD STUFF - - -

#create matrix, where each row is a class and each col is a subplan
# if the subplan contains that class, set the class value to 1, otherwise to 0
# i.e. if a class is only taken by the first two subplans, it's corresponding row is:
# [1,1,0,0,0,0,0,0,0,0,0,0,0]



set.seed(1234)
fit.km8 <- kmeans(kMeansMatrix, 11, nstart = 25)

#plot(allClasses, type='n')
#text(allClasses, labels=fit.km8$cluster, col=fit.km8$cluster)
  

plotcluster(kMeansMatrix, fit.km8$cluster)
plotWithLabels <- clusplot(kMeansMatrix, fit.km8$cluster, color = TRUE, labels = 1)
plotWithLabels

distances <- princomp(kMeansMatrix)

plot(plotWithLabels$Distances)
library(NbClust)
nc <- NbClust(kMeansMatrix, min.nc=2, max.nc=11, method="kmeans")
table(nc$Best.n[1,])
