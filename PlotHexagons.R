

## plot hexagon things

# as defined in somNew
somm <- somm

# function to plot
library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors

##PLOTTING FUNCTION
plotCplane <- function(som_obj, variable=sample(colnames(som_obj$data), 1), type="Quantile", legend=TRUE){
  if (is.numeric(variable)){
    variable <- colnames(som_obj$data)[variable]
  }
  
  if (som_obj$grid$topo != "hexagonal"){
    stop("function assumes hexgonal SOM")
  }
  
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
  
  hm <- component_plane_matrix(data=som_obj$codes$X, variable_index_or_name=variable)
  
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
    Bins <- quantile(x=som_obj$codes$X, probs=cumsum(rep(1/length(ColRamp), length(ColRamp))))
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
  
  if(legend==TRUE){
    image.plot(legend.only=TRUE, col=ColRamp, zlim=c(-1.5,1.5))
  }
}

dev.off()
vars <- colnames(somm$codes$X)
for(p in vars){
  plotCplane(som_obj=somm, variable=p, legend=TRUE, type="Quantile")
}





#change somm X code map
scale <- function(vector_to_scale, goal_min, goal_max){ 
  new_vector <- (vector_to_scale - min(vector_to_scale))*(goal_max - goal_min)/(max(vector_to_scale)-min(vector_to_scale)) + goal_min
  return(new_vector)
}
winners <- scale(apply(somm$codes$Y,1,which.max), -1,1)
type = 'Quantile'
variable = 'Winner'

# hm
hm <- matrix(nrow=somm$grid$xdim, ncol=somm$grid$ydim, data=winners) #, byrow=TRUE)


# this function
Hexagon <- function (x, y, unitcell = 1, col = "grey", border=NA) {
  polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
            x + unitcell/2), c(y + unitcell * 0.125, y + unitcell * 
                                 0.875, y + unitcell * 1.125, y + unitcell * 0.875, 
                               y + unitcell * 0.125, y - unitcell * 0.125), 
          col = col, border=border)
}

plot(0, 0, type = "n", axes = FALSE, xlim=c(0, somm$grid$xdim), 
     ylim=c(0, somm$grid$ydim), xlab="", ylab= "", asp=1, main=substr(variable, 1, 10))

ColRamp <- rev(designer.colors(n=50, col=brewer.pal(9, "Spectral")))

ColorCode <- rep("#FFFFFF", length(hm))

if(type == "Quantile") {
  #Quantile colorbins
  Bins <- quantile(x=somm$codes$X, probs=cumsum(rep(1/length(ColRamp), length(ColRamp))))
}



for (i in 1:length(hm))
  if (!is.na(hm[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-hm[i]))] 


offset <- 0.5 #offset for the hexagons when moving up a row
ind <- 1
for (row in 1:somm$grid$ydim) {
  for (column in 0:(somm$grid$xdim - 1)) {
    Hexagon(column + offset, row - 1, col = ColorCode[ind])
    ind <- ind +1}
  offset <- ifelse(offset, 0, 0.5)
}  

image.plot(legend.only=TRUE, col=ColRamp, zlim=c(-1,1))


