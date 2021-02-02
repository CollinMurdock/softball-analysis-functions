

# load in the necessary functions

library(tidyverse)
source_path = 'C:/Users/colli/Desktop/mu_softball/dev/analysis/R/' # REPLACE THIS WITH YOUR PATH
source(paste0(source_path,'pitch_plotting.R'))
source(paste0(source_path,'data_merging.R'))
source(paste0(source_path,'util.R'))

setwd("C:/Users/colli/Desktop/mu_softball/analysis/pitcher_analysis") #REPLACE WITH YOUR PATH

############# LOAD DATA
path = 'C:/Users/colli/Desktop/mu_softball/analysis/pitcher_analysis/' # REPLACE WITH YOUR PATH
# read every file in games/
file_names <- list.files(paste0(path, 'games'), pattern = '*.csv')
file_names <- list.files('C:/Users/colli/Desktop/mu_softball/analysis/pitcher_analysis/games', pattern="*.csv")
file_names <- paste0(path, 'games/', file_names)
games <- do.call(rbind, lapply(file_names,read.csv))


# read every file in rosters/
file_names <- list.files(paste0(path,'rosters'), pattern = '*.csv')
file_names <- paste0(path, 'rosters/', file_names)
rosters <- do.call(rbind,lapply(file_names,read.csv))

##################################### FULL DATA MANIPULATION

#create full dataset of both games and roster data
full <- games %>% 
  left_join(rosters, by=c("batter"="Player.Number", "at_bat"="Team.Code"))

# add swing variable
swing_codes = c('FOUL', 'S/M', 'KS', 'GO', 'S', 'LO' ,'FOUL/O', '2B', 'S/FC', 'FO', 'PO', 'SSB',
                'HR', 'SacF', 'DP', 'FP')
full <- mutate(full, Swing=ifelse(pitch_result %in% swing_codes, 1, 0))
  
# add generic codes
full$gen_code <- sapply(full$pitch_result, FUN=getGenCode)


################# VIERSTRA

# subset data for the pitcher we're interested in
vierstra <- filter(full, full$pitcher == 13 & full$at_bat != 'MU')

library(ggplot2)
plotPitches(vierstra)


# create plots and save them
removeDirContents('mu/vierstra/vs_left', pattern='*.png')

vierstra %>% filter(Bat.Handedness == 'L') %>% 
  
  plotPitchesByCount(func=plotPitches, directory='mu/vierstra/vs_left', name='Vierstra')

removeDirContents('mu/vierstra/vs_right', pattern='*.png')

vierstra %>% filter(Bat.Handedness == 'R') %>% 
  plotPitchesByCount(func = plotPitches, directory='mu/vierstra/vs_right', name='Vierstra')



vierstra %>% filter(base_position == 'Empty') %>% dim()
  mutate(count = sprintf('%s-%s', balls, strikes)) %>% 
  group_by(count) %>% 
  summarize(n = n())  #14 out of 248
  
vierstra %>% filter(base_position != 'Empty') %>% dim()
  mutate(count = sprintf('%s-%s', balls, strikes)) %>% 
  group_by(count) %>% 
  summarize(n = n()) # 20 out of 251
  
pitchResultBarGraph(vierstra)


################# PRATT

# subset data for the pitcher we're interested in
pratt <- filter(full, full$pitcher == 9 & full$at_bat != 'MU')

# create plots and save them
removeDirContents('mu/pratt/vs_left', pattern='*.png')
pratt %>% filter(Bat.Handedness == 'L') %>% 
  plotPitchesByCount(func=plotPitches, directory='mu/pratt/vs_left', name='Pratt')
removeDirContents('mu/pratt/vs_right', pattern='*.png')
pratt %>% filter(Bat.Handedness == 'R') %>% 
  plotPitchesByCount(func=plotPitches, directory='mu/pratt/vs_right', name='Pratt')


pitchResultBarGraph(pratt)













