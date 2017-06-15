NDVI <- function(exposure.matched, RGB.VI, IR.VI, spatial=FALSE) {
	if (!spatial) {
		pos.rgb <- which(RGB.VI$date %in% exposure.matched$RGB.timestamp == TRUE)
		pos.ir <- which(IR.VI$date %in% exposure.matched$IR.timestamp == TRUE)
		exit.rgb <- RGB.VI[pos.rgb, c('r.av', 'g.av', 'b.av')]
		exit.ir <- IR.VI[pos.ir, c('r.av', 'g.av', 'b.av')]
		names(exit.rgb) <- paste0('RGB.', names(exit.rgb))
		names(exit.ir) <- paste0('IR.', names(exit.ir))
		exit.df <- data.frame(date=exposure.matched$matching, exit.rgb, exit.ir, 
			RGB.exposure=exposure.matched$RGB.exposure, IR.exposure=exposure.matched$IR.exposure)
		ey <-exposure.matched$RGB.exposure       
		ez <-exposure.matched$IR.exposure
		rdn <- exit.df$RGB.r.av
		gdn <- exit.df$RGB.g.av
		bdn <- exit.df$RGB.b.av
		Rdn <-(rdn/sqrt(ey))
		Bdn <- (bdn/sqrt(ey)) 
		ydn <-((0.3*rdn)+(0.59*gdn)+(0.11*bdn))
		zdn <- exit.df$IR.r.av
      # xdn <- exit.df$IR.r.av
      # zdn <- ydn + xdn
		Zdn<-(zdn/sqrt(ez))
		Ydn<-(ydn/sqrt(ey))
		Xdn<-(Zdn-Ydn)           
		ndvi<-((Xdn-Rdn)/(Xdn+Rdn))
		## evi formulation used by MODIS
		evi <- 2.5 * (Xdn-Rdn)/(Xdn + 6*Rdn - 7.5* Bdn + 1)
      # ndvi <- 0.53*ndvi+0.84 # scaling factor from Andrew
      # ndvi[ndvi<0]<-NA
      # ndvi[ndvi>1]<-NA
		exit.df$NDVI <- ndvi
		exit.df$NIR <- Xdn
		exit.df$RED_corr <- Rdn	
		s <- lm(NIR~RED_corr, na.omit(exit.df))$coefficients[[2]]
		L <- 1 - (2*s*(Xdn-Rdn)*(Xdn-s*Rdn)) / (Xdn + Rdn)
		msavi <- ((Xdn-Rdn)*(1+L) /(Xdn+Rdn+L))
		msavi2 <- (2*Xdn+1-sqrt(((2*Xdn+1)^2)-8*(Xdn-Rdn))) / 2
		exit.df$slope <- s
		exit.df$L.savi <- L
		exit.df$MSAVI <- msavi
		exit.df$MSAVI2 <- msavi2
		exit.df$EVI <- evi
		return(exit.df)
	} else {
		pos.rgb <- which(as.POSIXct(names(RGB.VI)) %in% exposure.matched$RGB.timestamp == TRUE)
		pos.ir <- which(as.POSIXct(names(IR.VI)) %in% exposure.matched$IR.timestamp == TRUE)
		matched.RGB <- RGB.VI[pos.rgb]	
		matched.IR <- IR.VI[pos.ir]
		# if (ncores=='all') cores <- detectCores() else cores <- ncores
		# cl <- makeCluster(cores)
		# registerDoParallel(cl)
		# a <- NULL
		exit.list <- list()
		for (a in 1:length(matched.RGB)) {
			act.date <- as.POSIXct(names(matched.RGB)[a])
			act.RGB <- matched.RGB[[a]]
			act.IR <- matched.IR[[a]]
			names(act.RGB) <- c('r.av', 'g.av', 'b.av')
			names(act.IR) <- c('r.av', 'g.av', 'b.av')
		# exit.rgb <- RGB.VI[pos.rgb, c('r.av', 'g.av', 'b.av')]
		# exit.ir <- IR.VI[pos.ir, c('r.av', 'g.av', 'b.av')]
			names(act.RGB) <- paste0('RGB.', names(act.RGB))
			names(act.IR) <- paste0('IR.', names(act.IR))
			pos.match <- which(exposure.matched$RGB.timestamp == act.date)
			matching.date <- exposure.matched$matching[pos.match]
			act.RGB.exposure <- exposure.matched$RGB.exposure[pos.match]
			act.IR.exposure <- exposure.matched$IR.exposure[pos.match]
			exit.df <- data.frame(act.RGB, act.IR, 
				RGB.exposure=act.RGB.exposure, IR.exposure=act.IR.exposure)
			ey <-act.RGB.exposure       
			ez <- act.IR.exposure
			rdn <- exit.df$RGB.r.av
			gdn <- exit.df$RGB.g.av
			bdn <- exit.df$RGB.b.av
			Rdn <-(rdn/sqrt(ey)) 
			ydn <-((0.3*rdn)+(0.59*gdn)+(0.11*bdn))
			zdn <- exit.df$IR.r.av
      # xdn <- exit.df$IR.r.av
      # zdn <- ydn + xdn
			Zdn<-(zdn/sqrt(ez))
			Ydn<-(ydn/sqrt(ey))
			Xdn<-(Zdn-Ydn)           
			ndvi<-((Xdn-Rdn)/(Xdn+Rdn))
      # ndvi <- 0.53*ndvi+0.84 # scaling factor from Andrew
      # ndvi[ndvi<0]<-NA
      # ndvi[ndvi>1]<-NA
			exit.list[[a]] <- data.frame(ndvi)
			print(round(a/length(matched.RGB)*100))
		}
		names(exit.list) <- names(matched.RGB)
		return(exit.list)	
	}
}
