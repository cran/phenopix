extractDateFilename <- function(filename, date.code){
	n.underscores <- length(gregexpr('_', filename)[[1]])
	n.underscores.date.code <- length(gregexpr('_', date.code)[[1]])
	if (n.underscores.date.code==1 & gregexpr('_', date.code)[[1]][1]==-1) n.underscores.date.code <- 0
	pos.underscores <- gregexpr('_', filename)[[1]] 	  	
 	pos.underscore <- gregexpr('_', filename)[[1]][n.underscores-n.underscores.date.code]	
 	filename.cleaned <- substr(filename, pos.underscore+1, nchar(filename))
 	# separated <- str_split(filename.cleaned, '')[[1]][-1]
 	separated <- str_split(filename.cleaned, '')[[1]]	
 	year.char <- ifelse(length(gregexpr('y', date.code)[[1]]) == 2, '%y', '%Y')
 	year.pos <- unlist(gregexpr('y', date.code)[[1]])
 	year.val <- paste(separated[year.pos], collapse='')
 	month.pos <- unlist(gregexpr('m', date.code)[[1]])
 	month.val <- paste(separated[month.pos], collapse='')
 	day.pos <- unlist(gregexpr('d', date.code)[[1]])
 	day.val <- paste(separated[day.pos], collapse='')
 	hour.pos <- unlist(gregexpr('H', date.code)[[1]])
 	hour.val <- ifelse(hour.pos[1] > 0, paste(separated[hour.pos], collapse=''), '12')
 	min.pos <- unlist(gregexpr('M', date.code)[[1]])
 	min.val <- ifelse(min.pos[1] > 0, paste(separated[min.pos], collapse=''), '00')
 	final.date <- paste(year.val, month.val, day.val, sep='-')
 	final.time <- paste(hour.val, min.val, sep=':')
 	final.datetime <- paste(final.date, final.time)
 	final.format <- paste0(year.char,'-%m-%d %H:%M')
 	date <- as.POSIXct(strptime(final.datetime, format=final.format))
 #    string_length <- nchar(filename.cleaned)
	# string_split <-suppressWarnings(as.numeric(str_split_fixed(filename.cleaned,"", string_length+1)))
	# sub_string <- string_split[which(is.na(string_split)==FALSE)]
 #    yyyy <- str_c(as.character(sub_string[1]),as.character(sub_string[2]),as.character(sub_string[3]),as.character(sub_string[4]))
 #    MM <- str_c(as.character(sub_string[5]),as.character(sub_string[6]))
 #    DD <- str_c(as.character(sub_string[7]),as.character(sub_string[8]))  
	# yyyyMMDD<-paste(yyyy,"-",MM,"-",DD,sep="")  
	# if (is.na(as.character(sub_string[9])))  HH <- '00' else {
	# HH<- str_c(as.character(sub_string[9]),as.character(sub_string[10]))
	# 	}
	# if (is.na(as.character(sub_string[11])))  mm <- '00' else {
	# 	mm<- str_c(as.character(sub_string[11]),as.character(sub_string[12]))	
	# 	}
 #    date<-as.POSIXct(strptime(paste(yyyyMMDD,' ',HH,':',mm,sep=''),"%Y-%m-%d %H:%M"),'GMT')
 	if (is.na(date)) stop(paste('Date extraction from',filename,'failed'))
	return(date)	
}
