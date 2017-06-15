getExposure <- function(ipath, coords, train.data=FALSE, date.code, sample=NULL) {
	## define number converter function
	.number.converted <- function(x) {
		num.conv <- x
		for (a in 1:length(x)) {
			act.num <- x[a]
			if (is.na(act.num)) next()
				if (act.num=='0') num.conv[a] <- 0
			if (act.num=='1') num.conv[a] <- 1
			if (act.num=='2') num.conv[a] <- 2
			if (act.num=='3') num.conv[a] <- 3
			if (act.num=='4') num.conv[a] <- 4
			if (act.num=='5') num.conv[a] <- 5
			if (act.num=='6') num.conv[a] <- 6
			if (act.num=='7') num.conv[a] <- 7
			if (act.num=='8') num.conv[a] <- 8
			if (act.num=='9') num.conv[a] <- 9
		}
		pos.na.first <- which(is.na(num.conv))[1]
		if (is.na(pos.na.first)) pos.na.first <- numeric(0)
		if (length(pos.na.first!=0)) num.conv <- num.conv[1:(pos.na.first-1)]
		# if (length(pos.na)==1 && pos.na==3) num.conv <- num.conv[1:2]
		final.value <- as.numeric(paste(na.omit(t(num.conv)), collapse=''))
		return(final.value)
	}
		.binaryConvert <- function(img) {
		grey.image <- 0.2126*img[,,1] + 0.7152*img[,,2] + 0.0722*img[,,3]
		binary <- round(grey.image, 0)
		rev.binary <- ifelse(binary==1, 0, 1)
		return(rev.binary)
    }
#####
	## get train data	
	binary.number.samples <- train.data 
	## ref columns and rows are needed to check matrix dimension
	ref.ncol <- max(sapply(binary.number.samples, ncol))	
	ref.nrow <- max(sapply(binary.number.samples, nrow))
	## provide possibility to only get a sample of images and not the whole folder
	to.sample <- length(list.files(ipath, full.names=TRUE, recursive=TRUE))	
	if (!is.null(sample)) the.sample <- sample(to.sample, sample) else the.sample <- 1:to.sample
	## get image file names 
	all.jpeg.files.full <- list.files(ipath, full.names=TRUE, recursive=TRUE)[the.sample]
	all.jpeg.files <- list.files(ipath, recursive=TRUE)[the.sample]	
	## loop of exposure
	exposure.final <- data.frame(image=all.jpeg.files, exposure=NA)
	for (tt in 1:nrow(exposure.final)) {

		image.target <- readJPEG(all.jpeg.files.full[tt])
		image.target <- .binaryConvert(image.target)
		image.width <- dim(image.target)[2]
		image.height <- dim(image.target)[1]
		counter <- 0
		while(counter<6) {
			counter <- counter+1

		## cut according to coords
		cut.image <- image.target[coords['y1']:coords['y2'], coords['x1']:coords['x2']]
		cut.image.binary <- round(cut.image)
		target.img <- cut.image.binary
		target.ncol <- ncol(target.img)
		target.nrow <- nrow(target.img)
		## find exposure in you image by comparing to sample E
		exposure.matrix <- binary.number.samples$E
		## loop for moving rows and columns 
		row.difference <- target.nrow - nrow(exposure.matrix)
		col.difference <- target.ncol - ncol(exposure.matrix)
		response.matrix <- matrix(nrow=row.difference, ncol=col.difference)
		for (j in 1:row.difference) {
			moving.row.matrix <- target.img[j:(j+nrow(exposure.matrix)-1),] 
			for (a in 1:col.difference) {
				image.moving.matrix <- moving.row.matrix[,a:(a+ncol(exposure.matrix)-1)]
				nwhites <- length(which(image.moving.matrix==1))
				nblacks <- length(which(image.moving.matrix==0))
				if (nwhites>nblacks) to.remove <- 1 else to.remove <- 0
				if (length(image.moving.matrix)==0) next()
					na.matrix <- image.moving.matrix
				na.matrix[image.moving.matrix==to.remove] <- NA						
				ntrue <- length(which(exposure.matrix==na.matrix))
				response.matrix[row.difference +1 - j,a] <- ntrue
			}
		}
		## get row with maximum agreement between sample E and your image
		row.max.pos <- which.max(apply(response.matrix,1, max))
		## same with columns
		col.max.pos <- which.max(response.matrix[row.max.pos,])-1
		## cut image accordingly
		cutted.left.down <- target.img[1:(target.nrow-row.max.pos),col.max.pos:ncol(target.img)]
		cut.up <-  nrow(cutted.left.down) - nrow(exposure.matrix) + 1
		cutted <- cutted.left.down[cut.up:nrow(cutted.left.down),]
		## in first column, get first black (0) value to decide where to start
		colsums <- apply(cutted, 2, sum)
		pos0 <- which(colsums==ref.nrow)
		## identify region with 3 consecutive whites, it is between : and numbers
		median.value <- 0
		ind <- 1
		breaker <- FALSE
		while (median.value!=ref.nrow) {
			median.value <- try(median(colsums[ind:(ind+3)], na.rm=TRUE))
			if (is.na(median.value)) {
				median.value <- ref.nrow ## to exit the while loop
				breaker <- TRUE ## to break the loop 
			}
			ind <- ind +1
		}
		if (breaker) next()
		beg.point <- ind +1
		## identify white spaces to split single numbers (max number of figures allowed:4)
		colsum.cut <- colsums[beg.point:length(colsums)]
		first.no.white <- beg.point-1 + which(colsum.cut!=ref.nrow)[1]
		second.cut <- try(colsums[first.no.white:length(colsums)], silent=TRUE)
		second18 <- first.no.white -1 + which(second.cut==ref.nrow)[1]
		letter1 <- try(cutted[,first.no.white:second18], silent=TRUE)
		### adjust single letters if they don't match the dimension of the sample
		if (class(letter1)!='try-error') {
		## erase white
			colsums1 <- apply(letter1, 2, sum)
			pos.white <- which(colsums1==ref.nrow)
			if(length(pos.white)!=0) letter1 <- as.matrix(letter1[,-pos.white])	
				col.difference <- ref.ncol - ncol(letter1)
			if (col.difference>0) {
				replace <- ifelse(ncol(letter1)<col.difference, TRUE, FALSE)
				cols.to.replicate <- sample(1:ncol(letter1), col.difference, replace=replace)
				cols.vector.to.resample <- sort(c(1:ncol(letter1), cols.to.replicate))
				new.letter <- letter1[,cols.vector.to.resample]
				letter1 <- new.letter
			}
		}
		third.cut <- try(colsums[second18:length(colsums)], silent=TRUE)
		second.no.white <- second18 -1 + which(third.cut!=ref.nrow)[1]
		third.cut <- try(colsums[second.no.white:length(colsums)], silent=TRUE)
		third18 <- second.no.white -1 + which(third.cut==ref.nrow)[1] 
		letter2 <- try(cutted[,second.no.white:third18], silent=TRUE)
		if (class(letter2)!='try-error') {
			colsums2 <- apply(letter2, 2, sum)
			pos.white <- which(colsums2==ref.nrow)
			if(length(pos.white)!=0) letter2 <- as.matrix(letter2[,-pos.white])	
				col.difference <- ref.ncol - ncol(letter2)
			if (col.difference>0) {
				replace <- ifelse(ncol(letter2)<col.difference, TRUE, FALSE)
				cols.to.replicate <- sample(1:ncol(letter2), col.difference, replace=replace)
				cols.vector.to.resample <- sort(c(1:ncol(letter2), cols.to.replicate))
				new.letter <- letter2[,cols.vector.to.resample]
				letter2 <- new.letter
			}
		}
		forth.cut <- try(colsums[third18:length(colsums)], silent=TRUE)
		third.no.white <- third18 -1 + which(forth.cut!=ref.nrow)[1]
		forth.cut <- try(colsums[third.no.white:length(colsums)], silent=TRUE)
		forth18 <- third.no.white -1 + which(forth.cut==ref.nrow)[1] 
		letter3 <- try(cutted[,third.no.white:forth18], silent=TRUE)
		if (class(letter3)!='try-error') {
			colsums3 <- apply(letter3, 2, sum)
			pos.white <- which(colsums3==ref.nrow)
			if(length(pos.white)!=0) letter3 <- as.matrix(letter3[,-pos.white])	
				col.difference <- ref.ncol - ncol(letter3)
			if (col.difference>0) {
				replace <- ifelse(ncol(letter3)<col.difference, TRUE, FALSE)
				cols.to.replicate <- sample(1:ncol(letter3), col.difference, replace=replace)
				cols.vector.to.resample <- sort(c(1:ncol(letter3), cols.to.replicate))
				new.letter <- letter3[,cols.vector.to.resample]
				letter3 <- new.letter
			}
		}
		fifth.cut <- try(colsums[forth18:length(colsums)], silent=TRUE)
		forth.no.white <- forth18 -1 + which(fifth.cut!=ref.nrow)[1]
		fifth.cut <- try(colsums[forth.no.white:length(colsums)], silent=TRUE)
		fifth18 <- forth.no.white -1 + which(fifth.cut==ref.nrow)[1] 
		if (is.na(fifth18)) letter4 <- try(cutted[,forth.no.white:ncol(cutted)], silent=TRUE) else {
			letter4 <- try(cutted[,forth.no.white:fifth18], silent=TRUE)
		}
		if (class(letter4)!='try-error') {
			colsums4 <- apply(as.matrix(letter4), 2, sum)
			pos.white <- which(colsums4==ref.nrow)
			if(length(pos.white)!=0) letter4 <- as.matrix(letter4[,-pos.white])	
				col.difference <- ref.ncol - ncol(letter4)
			if (length(col.difference)==0) col.difference <- 0
			if (col.difference>0) {
				replace <- ifelse(ncol(letter4)<col.difference, TRUE, FALSE)
				cols.to.replicate <- sample(1:ncol(letter4), col.difference, replace=replace)
				cols.vector.to.resample <- sort(c(1:ncol(letter4), cols.to.replicate))
				new.letter <- letter4[,cols.vector.to.resample]
				letter4 <- new.letter
			}
		}
		if (class(letter4)!='try-error') {
			if (dim(as.matrix(letter4))[2]>(ref.ncol+5) | dim(as.matrix(letter4))[2]==1) {
				letter4 <- NA
				class(letter4) <- 'try-error'
			}
		}
		## match separated figures and sample numbers in a loop for each number
		
		choosen.numbers <- data.frame(n1=NA, n2=NA, n3=NA, n4=NA)
		for (l in 1:4) {
			act.letter <- get(paste0('letter',l))
			if (class(act.letter)=='try-error' || nrow(act.letter)/ncol(act.letter)<1) next()
				responses <- list()
				for (a in 2:length(binary.number.samples)) {
					response.tmp <- try(all(act.letter==binary.number.samples[[a]]), silent=TRUE)
					if (class(response.tmp)=='try-error' || !response.tmp) response.tmp <- try(all(act.letter[,-c(1:2)]==binary.number.samples[[a]]), silent=TRUE)
					if (class(response.tmp)=='try-error' || !response.tmp) response.tmp <- try(all(act.letter[,-c(1)]==binary.number.samples[[a]]), silent=TRUE)
					if (class(response.tmp)=='try-error' || !response.tmp) response.tmp <- try(all(act.letter[,-c(1, ncol(act.letter))]==binary.number.samples[[a]]), silent=TRUE)
					if (class(response.tmp)=='try-error' || !response.tmp) response.tmp <- try(all(act.letter[,-c(ncol(act.letter)-1, ncol(act.letter))]==binary.number.samples[[a]]), silent=TRUE)						
					if (class(response.tmp)=='try-error' || !response.tmp) response.tmp <- try(all(act.letter[,-c(ncol(act.letter))]==binary.number.samples[[a]]), silent=TRUE)
					if (class(response.tmp)=='try-error' || !response.tmp) response.tmp <- try(all(act.letter[,-c(6, ncol(act.letter))]==binary.number.samples[[a]]), silent=TRUE)
					if (class(response.tmp)=='try-error' || !response.tmp) response.tmp <- try(all(act.letter[,-c(6, 1)]==binary.number.samples[[a]]), silent=TRUE)

					length(which(act.letter==0))
					length(which(binary.number.samples[[a]]==0))						
					responses[a] <- response.tmp
				}
				classes <- sapply(responses, class)
				numbers <- c('E','0', '1', '2', '3', '4', 
					'5', '6', '7', '8', '9')
				responses[which(classes!='logical')] <- FALSE
				pos.good <- which(responses==TRUE)
		choosen.numbers[l] <- as.numeric(numbers[pos.good])
		}
		exposure.tmp <- .number.converted(choosen.numbers)
		if(!is.na(exposure.tmp)) counter <- 10
		exposure.final[tt,2] <- exposure.tmp
		# print(paste(tt, tot.true, sep='-----'))
			print(paste(tt, counter, sep='----'))
		}
	}
	## convert date stamp
	date.stamp <- sapply(as.character(exposure.final$image), FUN=extractDateFilename, date.code=date.code)
	date.stamp <- as.POSIXct(date.stamp, origin='1970-01-01')
	exposure.final$timestamp <- date.stamp
	nfail <- length(which(is.na(exposure.final$exposure)))/dim(exposure.final)[1]*100
	if (nfail>10) warning(paste0('You failed ', round(nfail), '% of exposure retrieval, consider shifting \n your x1 coord 5 pixels on the left'))
		return(exposure.final)
}
