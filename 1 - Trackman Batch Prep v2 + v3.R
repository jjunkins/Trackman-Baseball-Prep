library(lubridate)
library(chron)
library(plyr)


#THRU 10/2

# Set directory of files

setwd("/Users/jerodjunkins/Desktop/NDSAP Baseball/Trackman Data Sources/Game Files/Batch v2")
# Get all filenames
filenames  <- list.files(path = "/Users/jerodjunkins/Desktop/NDSAP Baseball/Trackman Data Sources/Game Files/Batch v2",
                         full.names = TRUE, pattern = "*.csv")

# Read in all csv's
read_fun <- function(x){
  read.csv(file = x, row.names = NULL, header = TRUE, stringsAsFactors = FALSE)
}   

list.files <- lapply(filenames, FUN = read_fun)

# function to prep
TM_prep <- function(trackman.data){
  # Set up filler field for previous pitch
  trackman.data$`Previous.Pitch` <- 1
  
  # Replace filler data with previous pitch info
  for(i in 1:nrow(trackman.data)){
    if(trackman.data$PitchofPA[i] == 1){
      trackman.data$`Previous.Pitch`[i] <- "First Pitch"
    }
    else(trackman.data$`Previous.Pitch`[i] <- trackman.data$TaggedPitchType[(i-1)])
  }
  #----Combine Date and Time Fields----
  # Use Lubridate package to combine Date and Time
  trackman.data$Date <- parse_date_time(paste(trackman.data$Date, trackman.data$Time), '%m/%d/%y %I:%M:%S %p')
  
  # Rearrange columns to fit Previous Pitch field with old setup (before TFP)
  trackman.data <- trackman.data[,c(1:75, 87, 76:86)]
  
  # Return the finished df
  return(trackman.data)
}

# Run function to prep all
files.prepped <- lapply(list.files, FUN = TM_prep) 

# rbind
# Combine all df's into one
comb.df <- ldply(files.prepped,rbind)
# Write to csv

setwd("/Users/jerodjunkins/Desktop/NDSAP Baseball/Trackman Data Sources")
TM.Data <- read.csv("Trackman - Master.csv", stringsAsFactors = FALSE)

comb.df$Date <- format(comb.df$Date, "%m/%d/%Y %I:%M:%S")

# Delete existing games (mainly for ND home games)
TM.Data <- TM.Data[!(TM.Data$GameID %in% comb.df$GameID), ]

#Need to add in new v3 columns to the v2 files as NA rows
comb.df[setdiff(names(TM.Data), names(comb.df))] <- NA

# Combine existing df with new data
TM.Data <- rbind(TM.Data, comb.df)

# Start v3
setwd("/Users/jerodjunkins/Desktop/NDSAP Baseball/Trackman Data Sources/Game Files/Batch v3")
# Get all filenames
filenames  <- list.files(path = "/Users/jerodjunkins/Desktop/NDSAP Baseball/Trackman Data Sources/Game Files/Batch v3",
                         full.names = TRUE, pattern = "*.csv")

# Read in all csv's
read_fun <- function(x){
  read.csv(file = x, row.names = NULL, header = TRUE, stringsAsFactors = FALSE)
}   

list.files <- lapply(filenames, FUN = read_fun)

# function to prep
TM_prep <- function(trackman.data){
  # Set up filler field for previous pitch
  trackman.data$`Previous.Pitch` <- 1
  
  # Replace filler data with previous pitch info
  for(i in 1:nrow(trackman.data)){
    if(trackman.data$PitchofPA[i] == 1){
      trackman.data$`Previous.Pitch`[i] <- "First Pitch"
    }
    else(trackman.data$`Previous.Pitch`[i] <- trackman.data$TaggedPitchType[(i-1)])
  }
  #----Combine Date and Time Fields----
  # Use Lubridate package to combine Date and Time
  trackman.data$Date <- parse_date_time(paste(trackman.data$Date, trackman.data$Time), "%Y-%m-%d %H:%M:%OS")
  
  # Rearrange columns to fit Previous Pitch field with old setup (before TFP)
  trackman.data <- trackman.data[,c(1:75, 168, 76:167)]
  
  # Return the finished df
  return(trackman.data)
}

# Run function to prep all
files.prepped <- lapply(list.files, FUN = TM_prep) 

# rbind
# Combine all df's into one
comb.df <- ldply(files.prepped,rbind)

# Change field name for TaggedHitType (due to trackman v3)
comb.df$`HitType` <- comb.df$TaggedHitType #create new duplicate column
comb.df <- comb.df[,c(1:23, 169, 25:168)] #rearrange column order


comb.df$Date <- format(comb.df$Date, "%m/%d/%Y %I:%M:%S")


#sum(TM.Data$GameID %in% comb.df$GameID)

# Delete existing games (mainly for ND home games)
TM.Data <- TM.Data[!(TM.Data$GameID %in% comb.df$GameID), ]
# Delete existing game with GameID not showing in new data (bc it had to be split up)

# Combine existing df with new data
TM.Data <- rbind(TM.Data, comb.df)

# Overwrite existing csv (old way: save as specific file and copy/paste)
setwd("/Users/jerodjunkins/Desktop/NDSAP Baseball/Trackman Data Sources")
write.csv(x = TM.Data, file = "Trackman - Master.csv", na = "", row.names = FALSE)
``


#delete soon

#TM.Data2 <- TM.Data

#head(TM.Data2)

#TM.Data2$Year <- format(as.Date(TM.Data2$Date, format="%m/%d/%Y %I:%M:%S"),"%Y")

#unique(TM.Data2$Year)

#TM.Data3 <- subset(TM.Data2, Year!="2017" & Year!="2018")
#TM.Data3 <- subset(TM.Data3, select = -c(Year))

#TM.Data4 <- subset(TM.Data2, Year=="2017" | Year=="2018")
#TM.Data4 <- subset(TM.Data4, select = -c(Year))
#head(TM.Data4)