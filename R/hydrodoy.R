hydrodoy <- function(x, year, reverse=FALSE) {
if (reverse) year <- as.numeric(year) -1	
act.oct.first <- as.numeric(format(as.POSIXct(paste0(as.numeric(year)-1, '-10-01')), '%j'))
# complement <- ifelse(act.oct.first==274, 92, 92)
if (reverse) doys  <- ifelse(x >= 93,  x - 92, x + act.oct.first -1) else {
doys  <- ifelse(x >= act.oct.first,  x - (act.oct.first - 1), x + 92) 
}
return(doys)
}