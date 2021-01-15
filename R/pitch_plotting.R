
#########################################################################
### File containing functions for plotting pitches
#########################################################################


# plotPitches
# funciton will plot all the pitches supplied in the input, given the df has the 
# necessary columns
# inputs  
#   data - df with defined columns:
#     x
#     y
#     gen_code (general result code) - one of these things:
#       Strike/Foul, Ball, Bunt, Hit, Ball In Play Out
#   dotSize - the size of the dot (int, default 5) 
# dependencies: 
#   ggplot2
plotPitches <- function(data, dotSize=3, includeLegend=TRUE) {
  
  color_values = c('Red', 'Blue', 'Green', 'Yellow', 'Brown')
  color_breaks = c('Strike/Foul','Ball','Bunt','Hit','Ball In Play Out')
  
  ggplot(data=data) + 
    # box
    geom_segment(aes(x=0, y=0, xend=100, yend=0)) +
    geom_segment(aes(x=0, y=0, xend=0, yend=150)) +
    geom_segment(aes(x=0, y=150, xend=100, yend=150)) +
    geom_segment(aes(x=100, y=0, xend=100, yend=150)) +
    scale_x_continuous(limits = c(-50, 150)) +
    scale_y_reverse(limits = c(200, -50)) +
    scale_color_manual(values=color_values, breaks=color_breaks) +
    coord_fixed() +
    geom_point(aes(x=x, y=y, color=gen_code), size=dotSize) +
    labs(color='') + 
    theme_void() +
    theme(
      legend.position = ifelse(includeLegend, 'right', 'none')
    )
}

# genCodeLegend
# generate a plot containing the general code legend
genCodeLegend <- function() {
  color_values = c('Red', 'Blue', 'Green', 'Yellow', 'Brown')
  color_breaks = c('Strike/Foul','Ball','Bunt','Hit','Ball In Play Out')
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", title="Legend", legend = color_breaks, lty=1, lwd=2, cex=1.25,
         bty='n', col = color_values)
}

# getGenCode
# add the generic pitch result codes to the inputted data frame
# inputs
#   code - code from codebook to be generalized
getGenCode <- function(code) {
  
  # define subsets of the codes that relate to a generic code
  strikefoul_codes = c('STRIKE', 'KL', 'KS', 'S/M', 'FOUL', 'FOUL/O', 'SSB')
  ball_codes = c('BALL', 'W','HBP', 'WP')
  bunt_codes = c('B', 'SacB','B/O', 'B/FOUL', 'B/M')
  hit_codes = c('S', 'S/FC', '2B', 'TR', 'HR')
  ballinplayout_codes = c('GO', 'PO', 'FO', 'LO', 'SacF', 'DP', 'TP', 'IH', 'FP', 'OBR')
  
  if (code %in% strikefoul_codes ) {return('Strike/Foul')}
  if (code %in% ball_codes) {return('Ball')}
  if (code %in% bunt_codes) {return('Bunt')}
  if (code %in% hit_codes) {return('Hit')}
  if (code %in% ballinplayout_codes) {return('Ball In Play Out')}
  return('Other')
}


# plotPitchesByCount
# create and save a plot for each possible count
# plots will be saved as png files in the given directory
# NOTE the directory must already exist
# inputs
#   data - the data to be plotted
#	   see plotPitches function docs for variable reqs
#   directory - the directory where the images to be saved to
#	        default: the current working directory	
#   name - name of the pitcher (used in the title)
plotPitchesByCount <- function(data, directory='.', name='') {
  for (b in 0:3) {
    for (s in 0:2) {
      # loop through all possible counts
      # create spray chart filtered by count
      p <- plotPitches(filter(data, data$balls == b, data$strikes == s), 
                       includeLegend = FALSE) +
        labs(title=sprintf('%s Pitches on Count %s - %s',name, b, s))
      ggsave(filename = paste0(directory, sprintf('/count%s-%s.png',b,s)),
             plot = p,
             device = 'png',
             width=3)
    }
  }
}













