
# calcOBP
# calculate the OBP for a given dataset
# inputs
# 	data - data for all the pitches a batter has seen
#		fields needed:
#			pitch_result - see codebook
calcOBP <- function(data) {
  # get atbats as the number of times the batter has seen 0 - 0 count
  # this may not be the best way to do it
  atbats <- dim(filter(data, data$balls==0, data$strikes==0))[1]
  t <- table(data$pitch_result)
  sum(c(t['S'],t['2B'],t['TR'],t['HR'],t['W'],t['HBP']), na.rm = T) /
    sum(c(atbats,t['W'],t['HBP'],t['SacF']), na.rm=T)
}

# calcSLG
# calculate the slugging percentage for given dataset
# inputs
#	data - data for all pitches a batter has seen (see calcOBP)
calcSLG <- function(data) {
  	atbats <- dim(filter(data, data$balls==0, data$strikes==0))[1]
 	t <- table(data$pitch_result)
	sum(c(t['S'],2*t['2B'],3*t['TR'],4*t['HR']), na.rm=T) / atbats
}


# calcOPS
# calculate OPS (On base plus slugging)
# inputs
#	data - data for all pitches a batter has seen (see calcOBP)
calcOPS <- function(data) {
	calcSLG(data) + calcOBP(data)
}
