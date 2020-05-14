library(httr)
library(rlist)
library(jsonlite)
library(dplyr)
library(sp)
library(rgdal)
library(raster)

`%notin%` <- Negate(`%in%`)

#### import API hpl data ####
baseurl = "https://api.ul.se/api/v3/line/"
linje = list()

#linje_list = c("1", "3") 

for(i in 1:999){ # alternativ: length(linje_list)
  ul = fromJSON(paste0(baseurl, i), flatten=TRUE)
  linje[[i+1]] <- cbind(ul$name, ul$pointsOnRoute)
}

alla_linjer <- rbind_pages(linje)
#alla_linjer <- rbind_pages(linje[sapply(linje, length)>0])

colnames(alla_linjer) = c("linje", "hpl_id", "hpl_namn", "area", "lat", "long")

LinjeExklud = c("990", "982", "984", "986", "988") # exkludera linjer som är sjukresebuss eller färja

HplIdKoordinat = alla_linjer %>% 
  filter(linje %notin% LinjeExklud) %>%
  dplyr::select(hpl_id, lat, long) %>% 
  distinct()

AllaLinjerPerHplID = alla_linjer %>% 
  dplyr::select(linje, hpl_id) %>% 
  distinct() %>% # tex linje 2 har 700600 två gånger
  group_by(hpl_id) %>% 
  summarise(AllaLinjer = paste(linje, collapse = "_"))

write.csv2(HplIdKoordinat, paste0("01_input_data/HplIdKoordinat_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)
write.csv2(AllaLinjerPerHplID, paste0("01_input_data/AllaLinjerPerHplID_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)



### matcha hpl med shapefil ####

# Konvertera API hållplats koordinater från WGS84 till SWEREF (för att merge med shapefiler) 
UnikKoordinat = HplIdKoordinat %>% 
  dplyr::select(lat, long)

UnikKoordinatKOPIA = UnikKoordinat

# definera projection string
SWEREF99TM = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# WGS84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

coordinates(UnikKoordinat) = ~long+lat 
proj4string(UnikKoordinat) = CRS("+init=epsg:4326")
UnikKoordinat_sweref <- as_data_frame(spTransform(UnikKoordinat, CRS(SWEREF99TM)))


# load shapefiles
# SCG tätort shapefil är mer akkurat än landmäteriets, dvs det ingår bara ytan där det finns befolkning. 
# Lantmäteriet använder en buffer. Dessutom följer 2018 befolkningsdata med i shapefilen

kommun=readOGR("01_input_data/shp/ak_riks.shp", 
               stringsAsFactors = FALSE, verbose = FALSE, encoding = "UTF-8")


# ladda ner, unzip och importera shp till R
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")

download.file("https://www.scb.se/contentassets/3ee03ca6db1e48ff808b3c8d2c87d470/to2018_swe99tm_arcview.zip",
              tf)

fname = unzip(tf, list=TRUE)$Name[2:5]
unzip(tf, files=fname, exdir=td, overwrite=TRUE)
td2 = gsub("\\\\", "/", td)
tatort <- readOGR(paste0(td2, "/To2018_Swe99TM.shp"), stringsAsFactors = FALSE, verbose = FALSE, encoding = "UTF-8")
unlink(td)

# merge hpl coordinates with shapefiles 
hpl_kommun <- data.frame(coordinates(UnikKoordinat_sweref),
                         extract(kommun, UnikKoordinat_sweref))
hpl_tatort <- data.frame(coordinates(UnikKoordinat_sweref),
                         extract(tatort, UnikKoordinat_sweref))

# append files: alla hpl med all meta data
hpl_meta = cbind(HplIdKoordinat, 
                 hpl_kommun[,c("long", "lat", "KOMMUNNAMN", "LANSNAMN")], 
                 hpl_tatort[,c("TATORTSKOD", "TATORT", "BEF")])

colnames(hpl_meta) = c("HplID", "Hpl_Wgs84North", "Hpl_Wgs84East", "Hpl_SwerefEast", "Hpl_SwerefNorth", "KommunNamn", "LanNamn", "TatortKod",
                       "TatortNamn", "Befolkning2018") # "DesoID", "RutID", 

storregional = c("Stockholm", "Gävle", "Uppsala", "Västerås")

hpl_meta = hpl_meta %>% 
  mutate(HplID = as.character(hpl_meta$HplID),
         Befolkning2018 = as.numeric(Befolkning2018)) %>%
  mutate(TatortTyp = ifelse(Befolkning2018 >= 50 &  Befolkning2018 < 200, "Småort",
                            ifelse(Befolkning2018 >= 200 &  Befolkning2018 < 1000, "Mindre tätort",
                                   ifelse(Befolkning2018 >= 1000 &  Befolkning2018 < 7000, "Medelstor tätort",
                                          ifelse(Befolkning2018 >= 7000 & TatortNamn %notin% storregional, "Större tätort",
                                                 ifelse(TatortNamn %in% storregional, "Storregional kärn", "XXX")))))) %>%
  mutate(TatortTyp = ifelse(is.na(TatortTyp), "Utanför tätort", TatortTyp)) %>%
  mutate(HplID = as.character(HplID))

# det krävs datum i filnamn eftersom API data reflektera idags tidtabell
write.csv2(hpl_meta, paste0("01_input_data/HplData_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)

# hpl_meta = read.csv2("01_input_data/HplData_2020-05-13.csv")

