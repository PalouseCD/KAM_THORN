# Gerrit Bass 4/21/23 
# Script to take data files exported from WISKI into the "data" folder and append them into one file placed in the "outputs" folder
# Need to find a way to append all the files without closing it inbetween to make it faster


library(plyr)
library(tidyverse)
library(lubridate)


#GETTING all the .csv file names from folder
data_files <- list.files("data",pattern = "*.csv")

#only use below command to create a new file to append data too
#file.create("outputs/appended_KAMTHORN.csv")

# Function import and clean data
KAMTHORN_clean<-function(file){ #Cleaning function for PCD data
  data<- read_csv(file.path("data",file), skip = 15,
                  col_names = c("Date", "Time", "Value","State_of_Value"),
                  na = "NA") #imports file
  
  Parameter <- sapply(strsplit(file,"_"), `[`, 2) %>% # Take the second half of the file name that has the parameter in it
    str_replace(".csv","") #remove the ".csv" from the parameter name
  
  data2 <- data %>% 
    mutate(datetime = mdy_hms(paste(data$Date, data$Time)),  #changes date time to posixct format
           Station = sapply(strsplit(file,"_"), `[`, 1), # create a "Station" by dividing the string in two using "_" as the sperator and taking the first half
           Parameter = Parameter) %>% # inserting the "Parameter" variable created above 
    select(5,6,7,3) #cleaning and getting rid of columns so we are left with datetime, station, parameter, and value
  
  write_csv(data2, file = "outputs/appended_KAMTHORN.csv", append = TRUE)
}

lapply(data_files, KAMTHORN_clean)


long_kamthorn <- read_csv("./outputs/appended_KAMTHORN.csv")

long_kamthorn <- long_kamthorn %>%
  rename(datetime = 1,
         Station =2,
         Param = 3,
         Value = 4) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  distinct()
  

wide_kamthorn <- pivot_wider(long_kamthorn, names_from = 3,values_from = 4)

write_csv(wide_kamthorn, file = "outputs/wide_KAMTHORN.csv")
