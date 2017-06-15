editExposure <- function(exposures, image.path, which) {
  for (j in which) {
    name <- exposures[j,'image']
      to.load <- paste0(image.path,name, '.jpg')
        image.target <- brick(to.load)
        # image.target <- .binaryConvert(image.target)
        # cut.image <- image.target[coords['y1']:coords['y2'],
        # coords['x1']:coords['x2']]
        # cut.image.binary <- round(cut.image)
        par(mfrow=c(2,1))
        plotRGB(image.target)
        plot(0, type='n')
        act.value <- exposures[j,'numerics']
        text(1,0, act.value, cex=5)
        correct.value <- ask('type in the correct exposure value or press enter to keep it as it is: ')
        if(nchar(correct.value)==0) {
                  exposures$exposure[j] <- exposures$numerics[j]
        } else {
        exposures$exposure[j] <- as.numeric(correct.value)
    }
  }
  return(exposures)
}
