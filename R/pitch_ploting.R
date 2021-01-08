
#########################################################################
### File containing functions for plotting pitches
#########################################################################


# plotPitches
# funciton will plot all the pitches supplied in the input, given the df has the 
# necessary columns
# inputs - the data to plot
#   df with defined columns:
#     x
#     y
#     gen_code (general result code) - one of these things:
#       Strike/Foul, Ball, Bunt, Hit, Ball In Play Out
# dependencies: 
#   ggplot2
plotPitches <- function(data) {
  
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
    geom_point(aes(x=x, y=y, color=gen_code), size=5) +
    labs(color='') + 
    theme_void() 
}

