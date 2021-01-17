# Combining data sets

# Must install these package: purrr, tidyverse
# Can install by either going to the tools tab and clicking "install packages" and searching or 
# type "install.packages("purrr") in the console

# Then load the package
library(purrr)
library(tidyverse)

# Must set your working directory by either Session -> Set working directory -> Choose working directory
# or setwd("")
# This is mine, you will have to change yours to wherever you downloaded the data files
setwd("C:/Users/doria/OneDrive/Misc/CADS Internship/data")

# In this case, we are creating a list of all of the data sets with Miami, so we set the pattern to "MU"
full.data.list <- list.files(pattern = "MU", full.names = TRUE)

# This function iterates through the list of files and pulls the data out of each
# Initialize the Pitch Location to 0
read_fun = function(path) {
  test = read.csv(path)
  test$PitchLocation = 0
  test
}

# This takes our list of data sets and applies the function to each one, combining the data
combined.data <- map_dfr(full.data.list, read_fun)

# Swing Codes
swing_codes = c('FOUL', 'S/M', 'KS', 'GO', 'S', 'LO' ,'FOUL/O', '2B', 'S/FC', 'FO', 'PO', 'SSB',
                'HR', 'SacF', 'DP', 'FP')


# This creates a pitch location for each pitch using the x any y coordinates
final <- combined.data %>% 
  mutate(PitchLocation = ifelse((x <= 50 & y <= 75), 11, PitchLocation),
         PitchLocation = ifelse((x >= 50 & y <= 75), 12, PitchLocation),
         PitchLocation = ifelse((x <= 50 & y >= 75), 13, PitchLocation),
         PitchLocation = ifelse((x >= 50 & y >= 75), 14, PitchLocation),
         
         PitchLocation = ifelse((x >= 0 & x <= 33 & y >= 0 & y <= 50), 1, PitchLocation),
         PitchLocation = ifelse((x > 33 & x <= 66 & y >= 0 & y <= 50), 2, PitchLocation),
         PitchLocation = ifelse((x > 66 & x <= 100 & y >= 0 & y <= 50), 3, PitchLocation),
         
         PitchLocation = ifelse((x >= 0 & x <= 33 & y > 50 & y <= 100), 4, PitchLocation),
         PitchLocation = ifelse((x > 33 & x <= 66 & y > 50 & y <= 100), 5, PitchLocation),
         PitchLocation = ifelse((x > 66 & x <= 100 & y > 50 & y <= 100), 6, PitchLocation),
         
         PitchLocation = ifelse((x >= 0 & x <= 33 & y > 100 & y <= 150), 7, PitchLocation),
         PitchLocation = ifelse((x > 33 & x <= 66 & y > 100 & y <= 150), 8, PitchLocation),
         PitchLocation = ifelse((x > 66 & x <= 100 & y > 100 & y <= 150), 9, PitchLocation)) %>% 
  
  mutate(Swing = ifelse(pitch_result %in% swing_codes, 1, 0))
                         

head(final)

# Now we can save the final combined data set and download it to our pc
# You can choose the file name, make sure to include ".csv"
# It will save to the same location where you pulled the individual data sets from
write.csv(final, file = "MU_combined_data.csv")

