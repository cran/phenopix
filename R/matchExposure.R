matchExposure <- function(exposure.df=NULL, exposure.RGB=NULL, exposure.IR=NULL, pattern.RGB="RGB", pattern.IR="IR", tol=1, 
	matching.column='timestamp'){
	if (!is.null(exposure.df)){
		exposure.RGB <- exposure.df[grepl(pattern.RGB,exposure.df$image[]),]
		exposure.IR <- exposure.df[grepl(pattern.IR,exposure.df$image[]),]
	} else if(is.null(exposure.RGB) || is.null(exposure.IR)){
		stop("Something is wrong with your Input! Enter 1 OR 2 lists.")
	}
  # matchNIR exposure.df
  ## check and remove duplicates from both datasets
	dupl.rgb <- which(duplicated(exposure.RGB[,matching.column]))
	if (length(dupl.rgb)!=0) exposure.RGB <- exposure.RGB[-dupl.rgb,]
	dupl.ir <- which(duplicated(exposure.IR[,matching.column]))
	if (length(dupl.ir)!=0) exposure.IR <- exposure.IR[-dupl.ir,]
  ## extract maching column
	time.rgb <- exposure.RGB[,matching.column]
	time.ir <- exposure.IR[,matching.column]
	sec.rgb <- round(as.numeric(time.rgb)/60)
	sec.ir <- round(as.numeric(time.ir)/60)
  ## round to tolerance
	rounding.value <- round(-log(tol, 10))
	rounded.rgb <- round(sec.rgb, rounding.value)
	rounded.ir <- round(sec.ir, rounding.value)
  ## put it again into the original dataframes
	exposure.RGB$matching <- as.POSIXct(rounded.rgb*60, origin='1970-01-01')
	exposure.IR$matching <- as.POSIXct(rounded.ir*60, origin='1970-01-01')
  ## check for duplicates 
	dupl.rgb <- which(duplicated(exposure.RGB$matching))
	if (length(dupl.rgb)!=0) exposure.RGB <- exposure.RGB[-dupl.rgb,]
	dupl.ir <- which(duplicated(exposure.IR$matching))
	if (length(dupl.ir)!=0) exposure.IR <- exposure.IR[-dupl.ir,]  
	exposure.matched <- merge(exposure.RGB,exposure.IR, by="matching", all=FALSE)
	pos.RGB <- grep('.x', names(exposure.matched), fixed=TRUE)
	pos.IR <- grep('.y', names(exposure.matched), fixed=TRUE)
	names.RGB <- names(exposure.matched)[pos.RGB]
	names.RGB <- substr(names.RGB, 0, nchar(names.RGB)-2)
	names.RGB <- paste0('RGB.', names.RGB)
	names(exposure.matched)[pos.RGB] <- names.RGB
	names.IR <- names(exposure.matched)[pos.IR]
	names.IR <- substr(names.IR, 0, nchar(names.IR)-2)
	names.IR <- paste0('IR.', names.IR)
	names(exposure.matched)[pos.IR] <- names.IR  
	print(paste0("Matched ",nrow(exposure.matched)," images."))
	return(exposure.matched)
}
