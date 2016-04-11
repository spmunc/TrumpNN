##########################################
##PLOT HEXAGONAL U-MATRIX 
##from "kohnonen" library output
##
##
##BY SETH E. SPIELMAN, UNIVERSITY OF COLORADO
##
##NEEDS SOME LOVE.  
##RUDIMENTRARY BUT FUNCTIONAL
##
##BORROWS CODE FROM
##http://nbremer.blogspot.nl/2013/11/how-to-create-hexagonal-heatmap-in-r.html
################################################

library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors
library(network)
library(deldir)

plotUmat <- function(som_obj, type="Equal Interval"){
  if (som_obj$grid$topo != "hexagonal"){
    stop("function assumes hexgonal SOM")
  }
  
  #CALCULATE U-MATRIX
  
  #Delaunay Triangulation to form network of neurons 
  d <- deldir(x=som_obj$grid$pts[,1], y=aSom$grid$pts[,2])
  
  #Build network
  n <- network(x=unique(d$delsgs[,5:6]), directed=FALSE, matrix.type="edgelist")
  
  #calculate u-matrix: the average eudlidean distance between each vertex and its neighbors
  neigh.dists <- NA
  for(vert in network.vertex.names(n)){
    neighs <- get.neighborhood(x=n, v=vert)
    neigh.dists[vert] <- (sum(dist(aSom$codes[c(vert, neighs),][,1]))/length(neighs))
  }
  
  
  #Function to create the polygon for each hexagon
  Hexagon <- function (x, y, unitcell = 1, col = "grey", border=NA) {
    polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
              x + unitcell/2), c(y + unitcell * 0.125, y + unitcell * 
                                   0.875, y + unitcell * 1.125, y + unitcell * 0.875, 
                                 y + unitcell * 0.125, y - unitcell * 0.125), 
            col = col, border=border)
  }
  
  
  plot(0, 0, type = "n", axes = FALSE, xlim=c(0, som_obj$grid$xdim), 
       ylim=c(0, som_obj$grid$ydim), xlab="", ylab= "", asp=1, main="U-Matrix")
  
  ColRamp <- rev(designer.colors(n=9, col=brewer.pal(9, "Spectral")))
  
  #color code for each neuron
  ColorCode <- rep("#FFFFFF", length(neigh.dists)) #default is all white
  
  if(type == "Equal Interval") {
    #Equal interval bins
    Bins <- seq(min(neigh.dists), max(neigh.dists), length=length(ColRamp))
  }
  
  if(type == "Quantile") {
    #Quantile colorbins
    Bins <- quantile(x=neigh.dists, probs=cumsum(rep(1/length(ColRamp), length(ColRamp))))
  }
  
  
  for (i in 1:length(neigh.dists))
    if (!is.na(neigh.dists[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-neigh.dists[i]))] 
  
  offset <- 0.5 #offset for the hexagons when moving up a row
  ind <- 1
  for (row in 1:som_obj$grid$ydim) {
    for (column in 0:(som_obj$grid$xdim - 1)){ 
      Hexagon(column + offset, row - 1, col = ColorCode[ind])
      ind <- ind +1}
    offset <- ifelse(offset, 0, 0.5)
  }  
}





##########################################
##PLOT HEXAGONAL COMPONENT PLANES
##from "kohnonen" library output
##
##
##BY SETH E. SPIELMAN, UNIVERSITY OF COLORADO
##
##NEEDS SOME LOVE.  
##RUDIMENTRARY BUT FUNCTIONAL
##
##BORROWS CODE FROM
##http://nbremer.blogspot.nl/2013/11/how-to-create-hexagonal-heatmap-in-r.html
##kohnonen library source
################################################

library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors

##PLOTTING FUNCTION
plotCplane <- function(som_obj, variable=sample(colnames(som_obj$data), 1), type="Quantile", legend=FALSE){
  if (is.numeric(variable)){
    variable <- colnames(som_obj$data)[variable]
  }
  
  if (som_obj$grid$topo != "hexagonal"){
    stop("function assumes hexgonal SOM")
  }
  #xCoord <- rep(1:som_obj$grid$xdim, som_obj$grid$ydim)
  #yCoord <- rep(1:som_obj$grid$ydim, each=som_obj$grid$xdim)
  #plane_codebook <- data.frame(xCoord, yCoord, som_obj$codes) #contains vector codebook and position
  
  component_plane_matrix <- function(data=plane_codebook, variable_index_or_name=variable){
    cp <- matrix(nrow=som_obj$grid$ydim, ncol=som_obj$grid$xdim, data=data[,variable_index_or_name]) #, byrow=TRUE)
    return(cp)
  }
  
  #Function to create the polygon for each hexagon
  #from http://nbremer.blogspot.nl/2013/11/how-to-create-hexagonal-heatmap-in-r.html
  Hexagon <- function (x, y, unitcell = 1, col = "grey", border=NA) {
    polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
              x + unitcell/2), c(y + unitcell * 0.125, y + unitcell * 
                                   0.875, y + unitcell * 1.125, y + unitcell * 0.875, 
                                 y + unitcell * 0.125, y - unitcell * 0.125), 
            col = col, border=border)
  }
  
  hm <- component_plane_matrix(data=som_obj$codes, variable_index_or_name=variable)
  
  plot(0, 0, type = "n", axes = FALSE, xlim=c(0, som_obj$grid$xdim), 
       ylim=c(0, som_obj$grid$ydim), xlab="", ylab= "", asp=1, main=substr(variable, 1, 10))
  
  ColRamp <- rev(designer.colors(n=50, col=brewer.pal(9, "Spectral")))
  
  ColorCode <- rep("#FFFFFF", length(hm)) #default is all white
  
  if(type == "Equal Interval") {
    #Equal interval bins
    Bins <- seq(-1.5, 1.5, length=length(ColRamp))
  }
  
  if(type == "Quantile") {
    #Quantile colorbins
    Bins <- quantile(x=som_obj$codes, probs=cumsum(rep(1/length(ColRamp), length(ColRamp))))
  }
  
  
  for (i in 1:length(hm))
    if (!is.na(hm[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-hm[i]))] 
  
  
  offset <- 0.5 #offset for the hexagons when moving up a row
  ind <- 1
  for (row in 1:som_obj$grid$ydim) {
    for (column in 0:(som_obj$grid$xdim - 1)) {
      Hexagon(column + offset, row - 1, col = ColorCode[ind])
      ind <- ind +1}
    offset <- ifelse(offset, 0, 0.5)
  }  
  if (legend==TRUE){
    image.plot(legend.only=TRUE, col=ColRamp, zlim=c(-1.5,1.5))
  }
}









##CODE FOR BRUNDSON AND SINGLETON BOOK CHAPTER.

##LIBRARIES
library(kohonen)

##Code for Plots
source("somComponentPlanePlottingFunction.R")
source("Map_COUNTY_BMU.R")
source("plotUMatrix.R")


#Load Data
##DATA FOR ALL BLOCKGROUPS IN THE US
##SOURCE ACS 2006-2010 AND CENSUS 2010
load("somInput.rdata")

#Build SOM
aGrid <- somgrid(xdim = 16, ydim = 10, topo="hexagonal")

##NEXT LINE IS SLOW!!!
##Rlen is arbitrarily low
aSom <- som(data=as.matrix(base::scale(na.omit(usa.bg.som[1:1000,1:7]))), grid=aGrid, rlen=1, alpha=c(0.05, 0.01), keep.data=TRUE)

##VISUALIZE RESULTS
##COMPONENT PLANES
dev.off()
par(mar = rep(1, 5))
cplanelay <- layout(matrix(1:10, nrow=5))
vars <- colnames(aSom$data)
for(p in vars) {
  plotCplane(som_obj=aSom, variable=p, legend=FALSE, type="Quantile")
}
plot(0, 0, type = "n", axes = FALSE, xlim=c(0, 1), 
     ylim=c(0, 1), xlab="", ylab= "")
par(mar = c(0, 0, 0, 6))
image.plot(legend.only=TRUE, col=rev(designer.colors(n=50, col=brewer.pal(9, "Spectral"))), zlim=c(-1.5,1.5))
##END PLOT

##PLOT U-MATRIX
dev.off()
plotUmat(aSom)
