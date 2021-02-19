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
                            "stringr")

required_scripts_list <- c("get_stop_and_line_data.R",
                           "create_api_input_dataframe.R",
                           "get_api_data.R")

for (pack in required_packages_list){
    if(pack %in% installed.packages() == FALSE){install.packages(pack)}
}

lapply(required_packages_list, require, character.only = TRUE)
sapply(paste0(getwd(),"/00_skript/src/", required_scripts_list), source)

APIkey_GTFS_regional_static_data <- "ed1a746e1a454d7a99aab00f2b64f581"
#possibly add code to download the static data here


#Call 00_hpl_data_API.r to get all lines and stops and connect them to municipalities and tätorter.
get_stop_and_line_data()


#Call 01_skapa_ramverk.r to create the data-frame for the API call based on the desired indicators.
create_api_input_dataframe()


#Call 02_ul_api.r to call the API and get supply data to assess supply on the indicators.
get_api_data()


#TO DO

##code dynamic components to decide the parameters of the analysis
##(max tt / max number of stops / which lines / what is peak)
##total trips available


##kolla varför det kraschar med hpl:erna, skapa ett kontrollsteg

##granska aggreggeringen och borttagning av hållplatserna - är det något som saknas?

##förstå dplyr kodningen, se över om det kan förenklas