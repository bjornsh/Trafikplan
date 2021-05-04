############################################################################
############################################################################
###                                                                      ###
###                PUBLIC TRANSPORT SUPPLY AND APPRAISAL                 ###
###               HEAD DEVELOPER: BJÖRN SCHULTE-HERBRUGGEN               ###
###                    DEVELOPER 2: NIKOS PAPAKATSIKAS                   ###
###                                                                      ###
############################################################################
############################################################################

###  This programme gets data from Uppsalas Lokaltrafik API on the public         
###  transport lines and stops. It uses a shapefile to find the co-ordinates of   
###  the stops and link them to units such as specific municipalities and         
###  'tätorter'. The supply is appraised according to specific indicators,        
###  selected in an input excel file. More detailed supply data is gathered       
###  from the API for example number of buses during the peak hour and specific   
###  departure and arrival times. The appraisal result is exported.

##################################################################
##                         Dependencies                         ##
##################################################################

required_packages_list <- c("httr",
                            "rlist",
                            "jsonlite",
                            "dplyr",
                            "sp",
                            "rgdal",
                            "raster",
                            "readxl",
                            "tidyr",
                            "stringr",
                            "lubridate",
                            "rgeos")

required_scripts_list <- c("get_stop_and_line_data.R",
                           "create_api_input_dataframe.R",
                           "get_api_data.R")

for (pack in required_packages_list){
    if(pack %in% installed.packages() == FALSE){install.packages(pack)}
}

lapply(required_packages_list, require, character.only = TRUE)
sapply(paste0(getwd(),"/00_skript/src/", required_scripts_list), source)

APIkey_GTFS_regional_static_data <- "ed1a746e1a454d7a99aab00f2b64f581"
#possibly later add code to download the static data here


#Call 00_hpl_data_API.r to get all lines and stops and connect them to municipalities and tätorter.
vector_lines_excluded <-  c("990", "982", "984", "986", "988")
storreg_tat <- c("Stockholm", "Gävle", "Uppsala", "Västerås")
get_stop_and_line_data(vector_lines_excluded, 
                       storreg_tat)


#Call 01_skapa_ramverk.r to create the data-frame for the API call based on the desired indicators.
desired_indicators = c(
    "1.1",
    "1.2",
    "2.1",
    "2.2",
    "3.1",
    "3.2"
)
create_api_input_dataframe(desired_indicators)


#Call 02_ul_api.r to call the API and get supply data to assess supply on the indicators.
peak_hours <- c("06", "07", "08", "15", "16", "17", "18")
off_peak_hours <- c("05", "09", "10", "11", "12", "13", "14", "19", "20", "21", "22", "23")
alternatives_to_count = 3
run_date = "2021-05-04"

get_api_data(peak_hours,
             off_peak_hours,
             TRUE,
             alternatives_to_count,
             run_date)
