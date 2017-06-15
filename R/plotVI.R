plotVI <- function(VI.data, VI.path) {
	roi.name <- names(VI.data)
	for (roi in 1:length(roi.name)) {
		VI.data.roi <- VI.data[[roi]]
      png(filename=paste(VI.path,roi.name[roi],'_roi_VI_plot.png',sep=''), width=800, height=5*400, pointsize=30)
      par(mfrow=c(5,1))
      par(mar=c(3,4,2,0.5))
      plot(VI.data.roi$date,VI.data.roi$r.av,col='red',pch=20,xlab='',ylab='R-G-B',main=paste('ROI: ',roi.name[roi],sep=''))
      points(VI.data.roi$date,VI.data.roi$g.av,col='green',pch=20)  
      points(VI.data.roi$date,VI.data.roi$b.av,col='blue',pch=20)
      par(mar=c(3,4,0.5,0.5))
      plot(VI.data.roi$date,VI.data.roi$ri.av,col='red',pch=20,xlab='',ylab='RI')
      par(mar=c(3,4,0.5,0.5))  
      plot(VI.data.roi$date,VI.data.roi$gi.av,col='green',pch=20,xlab='',ylab='GI')
      par(mar=c(3,4,0.5,0.5))  
      plot(VI.data.roi$date,VI.data.roi$bi.av,col='blue',pch=20,xlab='',ylab='BI')
      par(mar=c(4,4,0.5,0.5)) 
      plot(VI.data.roi$date,VI.data.roi$bri.av,col='grey',pch=20,xlab='doy',ylab='BRI')
      dev.off()
  }
}