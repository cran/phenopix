CutSeason <-
function(data, plot=FALSE) {
    if (!is.null(dim(data))) stop('provide an univariate zoo object')
    x.vect <- index(data)
    y.vect <- as.vector(data)
    data2 <- data.frame(x=x.vect, y=y.vect)
    cat('left-click to set a season break, to finish job right click (on x11 device) \nsee ?locator for how to finish job on other devices \nNote that you should click on beginning and end of time series\n')
    # if (length(x)==1) x.vect <- data[,x] else x.vect <- x
    # if (class(x.vect)[1]=='POSIXct') x.vect <- as.numeric(format(x.vect, '%j'))
    # if (!is.null(y)) y.vect <- data[,y] else y.vect <- as.vector(data)
    plot(x.vect, y.vect, type='l')
    point.cut <- locator()
    breaks <- floor(point.cut$x)
    if (class(x.vect)[1]=='POSIXct') breaks <- as.POSIXct(round(as.POSIXct(breaks, origin='1970-01-01'), 'day'))
    matches <- which(x.vect %in% breaks == TRUE)
    # if (add.tails==TRUE) matches <- c(0, matches, 365)
    cuts <- cut(x.vect, breaks)
    splitted <- split(data2, cuts)
    if (plot==TRUE) {
        plot(x.vect, y.vect, type='l')
        act.palette <- rainbow(length(splitted))
        for (a in 1:length(splitted)) {
            lines(splitted[[a]]$x, splitted[[a]]$y, col=act.palette[a], pch=20)
        }
    }
    print(paste('cut in', length(splitted), 'chunks: done'))
    ## zoo conversion
    for (a in 1:length(splitted)) {
        act.obj <- splitted[[a]]
        act.obj <- zoo(act.obj$y, order.by=act.obj$x)
        splitted[[a]] <- act.obj
    }
    return(splitted)
}
