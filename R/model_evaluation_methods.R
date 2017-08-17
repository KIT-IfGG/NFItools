

plot_2d_response <- function(gbm_model, xlevels, ylevels, ncolors=10, color_palette = heat.colors, n.trees, main_txt="", threshold=NULL) {
  
  nx <- length(xlevels)
  ny <- length(ylevels)
  
  newdata <- expand.grid(x=xlevels, y=ylevels)
  names(newdata) <- gbm_model$var.names
  newdata <- cbind.data.frame(newdata, expand.grid(xcoords = 1:nx,  ycoords = 1:ny))
  newdata$pred <- predict(gbm_model, newdata=newdata, n.trees=5000, type="response")
  
  ints <- classInt::classIntervals(newdata$pred, ncolors-1, style = "quantile")  ### This can also be done based on more than pred of one single model
  newdata$colors <- findInterval(newdata$pred,  sort(unique(ints$brks)))
  newdata$colors <-  color_palette(ncolors)[newdata$colors]
  
  if(!is.null(threshold)) newdata$colors[newdata$pred<threshold] <- 0
  
  plot(xcoords ~ ycoords, data=newdata, type="n", asp=1, axes=F, xlab="", ylab="", xlim=c(0,nx) + 0.5, ylim=c(0,ny) + 0.5)
  symbols(newdata$xcoords, newdata$ycoords, squares=rep(0.9, nrow(newdata)), bg=newdata$colors,  fg=newdata$colors, inches=F, add=T)
  axis(1, at=1:nx, labels=(xlevels))
  axis(2, at=1:ny, labels=(ylevels))
  mtext(main_txt, 3, 1)
  box()
}


