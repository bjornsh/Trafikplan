#From API GTFS Regional Static Data

#https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=KEY

routes = read.csv2("01_input_data/ul/routes.txt",
                   sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

stops = read.csv2("01_input_data/ul/stops.txt",
                  sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

stop_times = read.csv2("01_input_data/ul/stop_times.txt",
                       sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

trips = read.csv2("01_input_data/ul/trips.txt",
                  sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

dat = stop_times %>%  
    left_join(., trips, by = "trip_id") %>%
    left_join(., stops, by = "stop_id") %>%
    left_join(., routes, by = "route_id") %>%
    mutate(hpl_id = substr(stop_id, 8, 13))

## hållplats koordinater
hpl_koord = dat %>%
    group_by(hpl_id) %>%
    summarise(lat = round(mean(as.numeric(stop_lat)), 5), lon = round(mean(as.numeric(stop_lon)), 5)) # data är på hållplatsläge nivå (det kan finnas ett antal hållplatsläge som del av en hållplats). mean skapar en position per hållplats 

## linje nummer och linjekategori
linje_kat = dat %>%
    distinct(route_short_name, route_desc) %>%
    rename(linje_nr = route_short_name, linje_kat = route_desc)