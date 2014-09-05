##	This code is radically more efficient than the original code that inspired it:
##		http://aschinchon.wordpress.com/2014/01/14/cellular-automata-the-beauty-of-simplicity/

require(raster)

##	Create the field size:
width <- 2^10
depth <- width/2

##	Create a data.frame object large enough to hold the data:
gridded <- matrix(0, nrow=depth, ncol=width)

##	Initialized to zeroes above, we could sample 0:1 to create a random
##		starting row or keep it 0:0 to keep it null and assign 1s to the
##		center of the field:
gridded[1, 1:width] <- sample(0:0, width, replace=T)
gridded[1, width/2] <- 1
gridded[1, width/2+1] <- 1

gridded[1, 1:width] <- sample(0:1, width, replace=T)


##	Create indices for our field width that correspond to left, right and up
##		for the row above as we loop through.  First we create a shift() function
##		that can rotate a vector around (so that rows wrap, creating a continuous
##		2D world):
##	First is good for data.frame objects, second for vectors:
#shiftx <- function(d, k) { rbind( tail(d,k), head(d,-k), deparse.level = 0 ) }
shiftx <- function(d, k) { c( tail(d,k), head(d,-k)) }

##	Loop over rows, checking left, above, and right above every item in the rows
##		and assigning a new value to the new row based on its sum:
for (i in 2:depth) {
	row <- i - 1

	z <- shiftx(gridded[row,1:width], 1) + gridded[row,1:width] + shiftx(gridded[row,1:width], -1)
	
	gridded[i,] <- as.numeric( (z > 0) & (z < 3) )
}

image(raster(gridded), col=c("white", "black"), axes=FALSE, xlab="", ylab="")
