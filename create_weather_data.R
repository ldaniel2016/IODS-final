# IODS course - final assignment
# Datawrangling program
# Laila Daniel, email: ldaniel@cs.helsinki.fi


CleanData <- function(data) {
  # Cleans a data  from bad observations and variables and  NAs 
  # Args:
  #   data: data retrived from the database
  #  
  #
  # Returns:
  #   A cleaned data object of the same form as the input argument
  
  
  # Split the dataframe data  to two  (1) station_info, seasons, analysis_time, forecast_peroiod,  and timestamps 
  # and (2) station_data which consists of observations and ECMWF model data 
  
  
  station_info <- data[,c(1:5)]
  station_data <- data[,6:(ncol(data))]
                       
  no.of.obs <- nrow(data)
  
  # first we will delete faulty variables with are NA most of the time
  
  # calculating the number of NAs in variables
  
  no.of.na <- apply(X = station_data[,], MARGIN = 2, FUN = function(x) sum(is.na(x)))
  
  # na.tolerance is taken as one fourth of the observations
  na.tolerance <- no.of.obs/ 4
  
  # taking only the variables that have no.of.na < na.tolerance
  good.variables <- which(no.of.na < na.tolerance)
  
  # removing the variables which that have na.tolerance > no.of.points/4
  # Two variables "FG10_3" (10 meter wind gust in the last 3 hours) and 
  # "MX2T3" (Maximum temperature at 2 meter  in the last 3 hours) are removed
  station_data <- station_data[, good.variables]
  
  # searching the complete cases and keeping only them
  # removing all incomplete observations
  complete.rows <- complete.cases(station_data)
  station_data <- station_data[complete.rows,]
  
  # removing constant variables
  station_data <- station_data[,apply(X= station_data, MARGIN = 2, FUN = function(v) var(v) != 0)]
  
  # retaining only the corresponding complete.rows of station_data in station_info
  station_info <- station_info[complete.rows,]
  
  # returning the cleaned data by combining the new station_info dataframe and the new station_data dataframe
  data <- cbind.data.frame(station_info, station_data)
  
  return(data)
}


# Main program
# Load the weather data for Kumpula station data (station_id  2998) 
# from the file "data/station2998_all_data_with_timestamps.csv"
# Clean the data, removing the NAs an d variables of constant variance.
# The original data frame has 299520 observations of 34 variables and 
# the cleaned dataframe has 290982 observations of 32 variables.
# The cleaned datframe is saved in data/station2998_T2_D2_Obs_model_data_with_timestamps.csv


# The file "data/station2998_all_data_with_timestamps.csv" to the dataframe df. The variables of df are 
# "station_id","season", "analysis_time", "forecast_period", "timestamp", "T2Obs", "MSL"            
# "T2","D2", "U10", "V10", "TP", "LCC", "MCC", "HCC", "SKT", "FG10_3", "MX2T3", "RH_950"    
# "T_950", "RH_925", "T_925", "RH_850", "T_850", "RH_700", "T_700", "RH_500",  "T_500"          
# "T2_M1","T_950_M1", "T_925_M1" , "DECLINATION", "D2_M1" and "D2Obs"   
# See the final assignmant report for the detailed info on the variables
# df has 299520 observations of 34 variables

df <- read.table("/home/daniel/lstudies/IODS2017/IODS-final/data/station2998_all_data_with_timestamps.csv", sep = ",", header = TRUE)


# The cleaned data has 290982 observations of 32 variables. Two variables "FG10_3" (10 meter wind gust in the last 3 hours) 
# and "MX2T3" (Maximum temperature at 2 meter  in the last 3 hours) are removed as they have a very large number of NAs

data <- CleanData(df)


# Set the directory 
setwd("/home/daniel/lstudies/IODS2017/IODS-final")

# Save the cleaned data of station2988  
write.table(data, file = "data/station2998_T2_D2_Obs_model_data_with_timestamps.csv", sep = ",")

# saving a part of this data to show an example of a time series data

data_00_08 <- data[which(data$forecast_period == 8 & data$analysis_time == 1 ),]
write.table(data_00_08, file = "data/timeseries_example.csv", sep = ",")
