
######################################## Select Columns ########################################

select_columns = function(games){
  selection = c('Name','Pitcher','Batter','Count','Outs','Inning','Strike.Type',
                'Pitch.Type','Event.Type','Pitch.Result','Hit.Result',
                'Hit.Type','Velocity','RBI','Team.at.Bat','Attempted.Bunt',
                'Ball.Type','Bunt','Batter.Strength','Hit.Location',
                'Hit.Location.X','Hit.Location.Y','Pitch.Location',
                'Pitch.Location.X','Pitch.Location.Y','Pitcher.Strength',
                'Team.in.Field','Base.Runner.1st.Base','Base.Runner.2nd.Base',
                'Base.Runner.3rd.Base','Wild.Pitch','Stolen.Base.Lead','Stolen.Base.Lead.Runner',
                'Caught.Stealing')
  return(games[,selection])
}

######################################### Clean Data #########################################

clean_data = function(games){
  
  # Remove pitch count from game name
  games$Name = matrix(unlist(strsplit(games$Name, ',')), ncol = 2, byrow = TRUE)[,1]
  
  # Create new variables - Plot Result and Spray Result
  games$Plot.Result = games$Pitch.Result
  games$Spray.Result = games$Pitch.Result

  # For strikes, use strike type variable
  strikes = which(games$Pitch.Result == 'Strike')
  games$Plot.Result[strikes] = games$Strike.Type[strikes]
  games$Spray.Result[strikes] = games$Strike.Type[strikes]
  
  # For bunts, use event type variable
  bunts = which(games$Event.Type %in% c('Sac Bunt','bunt') | games$Attempted.Bunt != '' |
                  games$Bunt == 'Bunt')
  games$Plot.Result[bunts] = 'BUNT ATTEMPT'
  games$Spray.Result[bunts] = 'BUNT ATTEMPT'
  
  # Potentially add this in later
  #ball_in_play = which(games$Plot.Result == 'Ball in Play')
  #games$Plot.Result[ball_in_play] = games$Hit.Type[ball_in_play]
  
  # Combine all types of pop ups
  #pop_ups = which(games$Plot.Result %in% c('Pop up catcher','Pop up infield','Pop up outfield'))
  #games$Plot.Result[pop_ups] = 'Pop up'
  
  # Convert to upper case to avoid duplicates
  games$Plot.Result = toupper(games$Plot.Result)
  games$Spray.Result = toupper(games$Spray.Result)
  
  # STEALS
  # success when runner goes from first to second and batter doesn't change
  # fail when runner on first gets out and batter doesn't change
  
  
  return(games)
}

###################################### Changeup Barplot ######################################

changeup_barplot = function(games, team){
  
  subset = games[which(games$Team.in.Field == team),]
  subset$Count = as.factor(subset$Count)
  players = unique(subset$Pitcher)
  
  for (p in 1:length(players)){
    psubset = subset[which(subset$Pitcher == players[p] & subset$Pitch.Type != ''),]
    changeups = psubset[which(psubset$Pitch.Type == 'Changeup'),]
    
    t = table(changeups$Count)
    t = t[order(t,decreasing = TRUE)]
    barplot(t, horiz = TRUE, las = 2, xaxt = 'n', cex.names = 1, 
            main = paste0(players[p], " Change-Up by Count \n", nrow(changeups), ' changeups per ', nrow(psubset),
                          ' pitches \n (', round(nrow(changeups)/nrow(psubset)*100, 2), '%)'), 
            col = rgb(0,0,0,.8), xlab = 'Frequency', ylab='Pitch Count', cex.main = 1, cex.lab = 1)
    axis(1, at = seq(0, max(t), 1), labels = as.character(seq(0, max(t), 1)), cex.axis = 1) 
  }
}

####################################### Spray Charts #######################################

spray_chart = function(right, left, pitcher, view_type){
  
  leg = c('BALL','SWING','TAKE','FOUL','BUNT ATTEMPT','BALL IN PLAY','NO CHANGEUP','CHANGEUP')
  color = c('grey','red','firebrick4','goldenrod2','blue','forestgreen','black','black')
  
  type = c(NA, '','Dropball','Rise','Changeup')
  symbol = c(16, 16, 16, 16, 17)
  
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3), widths = c(3, 1, 3))
  
  # Right handed hitters
  par(mar=c(5, 3, 4, 2), xpd=TRUE)
  plot(right$Pitch.Location.X, right$Pitch.Location.Y, col = color[match(right$Spray.Result, leg)],
       xlab = " ", ylab = " ", xlim = c(0, 250), ylim = c(0, 250),
       main = paste0(pitcher,'\n','Right - ', view_type,' [Pitcher View]'),
       cex = 1.1, pch = symbol[match(right$Pitch.Type, type)])
  mtext("HITTER", side = 4, at = 50, col = c('maroon'), line = 1.3, cex = 1.5, font = 3)
  par(xpd = FALSE);   grid(col = "Dark Grey")
  rect(xleft = 50, xright = 200, ytop = 200, ybottom = 50, border = "Red")
  
  par(mar = c(0,0,5,0))
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("top", inset = c(0, 0), legend = leg, col = color, pch = c(rep(16,length(leg)-1),17), cex = 1.2)
  
  # Left handed hitters
  par(mar=c(5, 3, 4, 2), xpd=TRUE)
  plot(left$Pitch.Location.X, left$Pitch.Location.Y, col = color[match(left$Spray.Result, leg)],
       xlab = " ", ylab = " ", xlim = c(0, 250), ylim = c(0, 250),
       main = paste0(pitcher,'\n','Left - ', view_type,' [Pitcher View]'),
       cex = 1.1, pch = symbol[match(left$Pitch.Type, type)])
  mtext("HITTER", side = 2, at = 50, col = c('maroon'), line = 2, cex = 1.5, font = 3)
  par(xpd = FALSE);   grid(col = "Dark Grey")
  rect(xleft = 50, xright = 200, ytop = 200, ybottom = 50, border = "Red")
}

###################################### Pitch Scatterplot ######################################

pitch_scatterplot = function(games, pitcher){
  
  # All pitches
  subset = games[which(games$Team.in.Field == team & games$Pitcher == pitcher),]
  right = subset[which(subset$Batter.Strength == 'Right'),]
  left = subset[which(subset$Batter.Strength == 'Left'),]
  
  spray_chart(right, left, pitcher, 'Overall')

  # Two strike pitches  
  curr = subset[which(subset$Count %in% c('0-2','1-2','2-2','3-2')),]
  right = curr[which(curr$Batter.Strength == 'Right'),]
  left = curr[which(curr$Batter.Strength == 'Left'),]
  
  spray_chart(right, left, pitcher, '2 Strikes')
  
  # All counts
  counts = c('0-0','0-1','0-2','1-0','1-1','1-2','2-0','2-1','2-2','3-0','3-1','3-2')
  for (c in 1:length(counts)){
    curr = subset[which(subset$Count == counts[c]),]
    right = curr[which(curr$Batter.Strength == 'Right'),]
    left = curr[which(curr$Batter.Strength == 'Left'),]
    
    spray_chart(right, left, pitcher, counts[c])
  }
}

####################################### Hit Result Plot #######################################

hit_result_plot = function(subset, player, type = 'pitcher', min_atbat = 5){
  
  leg = c('BALL','SWING','TAKE','FOUL','BUNT ATTEMPT','BALL IN PLAY')
  color = c('grey','red','firebrick4','goldenrod2','blue','forestgreen')
  
  if (type == 'pitcher') strength = subset$Pitcher.Strength[1]
  if (type == 'batter') strength = subset$Batter.Strength[1]
  at_bats = length(which(subset$Count == '0-0'))
  
  if (at_bats >= min_atbat){
    par(mar=c(8, 2, 4, 3), xpd=TRUE)
  
    # Tabulate and plot results
    t = table(subset$Plot.Result, subset$Count)
    row_order = match(leg, rownames(t))
    t = t[row_order[!is.na(row_order)],]
    xx = barplot(t, beside = TRUE, xlab = ' ', col = color[match(rownames(t), leg)], 
               main = paste('\n', player, ' --- ' , strength, 'handed --- ', at_bats, 'at bats'),
               legend.text=TRUE, args.legend = list(legend = leg[match(rownames(t), leg)], 
                                                    x = "topright", inset = c(-0.05,0), cex = 0.8),
               yaxt = 'n', ylim = c(0, max(t)+2))
    axis(2, at = seq(0, (max(t)+2),2))  
    t[which(t == 0)] = NA
    text(x = xx, y = t, label = t, pos = 3, cex = 0.8, col = "black")
  }
}

######################################### Barplots #########################################

make_barplot = function(games, team, type = 'pitcher'){
  
  if (type == 'pitcher'){
    # Select only rows where the team was pitching
    subset = games[which(games$Team.in.Field == team & games$Plot.Result != ''),]
    players = unique(subset$Pitcher)
  
    # For each pitcher, produce barplot of hit results
    for (p in 1:length(players)){
      # Select only rows where the pitcher was pitching
      psubset = subset[which(subset$Pitcher == players[p]),]
      hit_result_plot(psubset, players[p], type)
    } 
  } else if (type == 'batter'){
    # Select only rows where the team was batting
    subset = games[which(games$Team.at.Bat == team & games$Plot.Result != ''),]
    players = unique(subset$Batter)
    
    # For each batter, produce barplot of hit results
    for (p in 1:length(players)){
      psubset = subset[which(subset$Batter == players[p]),]
      hit_result_plot(psubset, players[p], type)
    }
  }
}
