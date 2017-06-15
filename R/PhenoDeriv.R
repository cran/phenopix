PhenoDeriv <-
function(
	x, 
	formula=NULL, 
	uncert=FALSE, 
	params=NULL, 
	breaks,
	...
) {
	if (all(is.na(x))) return(c(sos=NA, eos=NA, los=NA, pop=NA, mgs=NA, rsp=NA, rau=NA, peak=NA, msp=NA, mau=NA))
	n <- index(x)[length(x)]
	avg <- mean(x, na.rm=TRUE)
	x2 <- na.omit(x)
	# avg2 <- mean(x2[x2 > min.mean], na.rm=TRUE)
	peak <- max(x, na.rm=TRUE)
	mn <- min(x, na.rm=TRUE)
	ampl <- peak - mn
	
	# get peak of season position
	pop <- median(index(x)[which(x == max(x, na.rm=TRUE))])	
	# return NA if amplitude is too low or time series has too many NA values
	# if (!calc.pheno) {
	# 	if (avg < min.mean) { # return for all metrics NA if mean is too low
	# 		return(c(sos=NA, eos=NA, los=NA, pop=NA, mgs=NA, rsp=NA, rau=NA, peak=NA, msp=NA, mau=NA))
	# 	} else { # return at least annual average if annual mean > min.mean
	# 		return(c(sos=NA, eos=NA, los=NA, pop=pop, mgs=avg2, rsp=NA, rau=NA, peak=peak, msp=NA, mau=NA))
	# 	}
	# }
		
	# calculate derivative
	xd <- c(NA, diff(x))
	
	# get SOS and EOS 
	soseos <- index(x)
	rsp <- max(xd, na.rm=TRUE)
	rau <- min(xd, na.rm=TRUE)
	sos <- median(soseos[xd == rsp], na.rm=TRUE)
	eos <- median(soseos[xd == rau], na.rm=TRUE)
	los <- eos - sos
	los[los < 0] <- n + (eos[los < 0] - sos[los < 0])
	
	# get MGS
	if (sos < eos) {
		mgs <- mean(x[index(x) %in% sos:eos], na.rm=TRUE)
	} else {
		cut1 <- as.vector(window(x, end=eos))
		cut2 <-  as.vector(window(x, start=sos))
		mgs <- mean(c(cut1, cut2), na.rm=TRUE)
	}
		
	# get MSP, MAU
	msp <- mau <- NA
	if (!is.na(sos)) {
		id <- (sos-10):(sos+10)
		id <- id[(id > 0) & (id < n)]
		msp <- mean(x[which(index(x) %in% id==TRUE)], na.rm=TRUE)
	}
	if (!is.na(eos)) {
		id <- (eos-10):(eos+10)
		id <- id[(id > 0) & (id < n)]
		mau <- mean(x[which(index(x) %in% id==TRUE)], na.rm=TRUE)
	}
	metrics <- c(sos=sos, eos=eos, los=los, pop=pop, mgs=mgs, rsp=rsp, rau=rau, peak=peak, msp=msp, mau=mau)
	
	# if (plot) {
	# 	PlotPhenCycle(x, metrics=metrics, ...)
	# }
		
	return(metrics)
	### The function returns a vector with SOS, EOS, LOS, POP, MGS, RSP, RAU, PEAK, MSP and MAU. }
}
