get_stop_and_line_data <- function(excluded_lines, storregionala_tatorter){
  
#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

## Hämta ID och koordinater för ULs hållplatser och identifiera i vilken kommun och tätort de är
## skapa input filen för skriptet "01_skapa_ramverk"

`%notin%` <- Negate(`%in%`)

#---------------------------------------------------------------------------------------------------
# Hämta hållplats data
#---------------------------------------------------------------------------------------------------

## import API hpl data
baseurl = "https://api.ul.se/api/v3/line/"
linje = list()

for(i in 1:999){ # alternativ: length(linje_list)
  r = GET(paste0(baseurl, i))
  if(status_code(r) == 200){
    ul = fromJSON(paste0(baseurl, i), flatten=TRUE)
  
    if (!is.null(ul)){
      if (ul$name != "Ersättningsbuss"){
        linje <- append(linje, list(cbind(ul$lineNo, ul$name, ul$pointsOnRoute)))
      }
      else{
        print(paste0("Linje ", ul$lineNo, " är en Ersättningsbuss och har exkluderats.
                     Linjens sträcka är ", ul$description))
      }
    }
  }
}

#linje <- linje[which(sapply(linje, is.data.frame))]
alla_linjer <- rbind_pages(linje) ##!!will fail if some of the objects are lists instead of DFs,
                                  ##!!e.g. if "ersättningsbuss"

colnames(alla_linjer) = c("linje", "linje_str", "hpl_id", "hpl_namn", "area", "lat", "long")

## exkludera linjer som är sjukresebuss eller färja ##!!update with new data columns
#LinjeExklud = c("990", "982", "984", "986", "988") 
LinjeExklud = excluded_lines


## df med hållplats ID och respektive koordinater i WGS84
HplIdKoordinat = alla_linjer %>% 
  filter(linje %notin% LinjeExklud) %>%
  dplyr::select(hpl_id, lat, long) %>% 
  arrange(hpl_id) %>%
  group_by(hpl_id) %>%
  distinct(hpl_id, .keep_all = TRUE) # unik hpl ID
  
## df med alla linjer som trafikera en hpl 
AllaLinjerPerHplID = alla_linjer %>% 
  dplyr::select(linje, hpl_id) %>% 
  mutate(hpl_id = as.character(hpl_id)) %>%
  distinct() %>% # tex linje 2 har hpl 700600 två gånger
  group_by(hpl_id) %>% 
  dplyr::summarise(AllaLinjer = paste(linje, collapse = "_"))


write.csv2(HplIdKoordinat, paste0("00_skript/data/interim/HplIdKoordinat_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)
write.csv2(AllaLinjerPerHplID, paste0("00_skript/data/interim/AllaLinjerPerHplID_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)



#---------------------------------------------------------------------------------------------------
# Extrahera geometadata
#---------------------------------------------------------------------------------------------------

## Konvertera API hållplats koordinater från WGS84 till SWEREF (för att merge med shapefiler) 
UnikKoordinat = HplIdKoordinat %>%
  ungroup() %>%
  dplyr::select(lat, long)

#UnikKoordinatKOPIA = UnikKoordinat

## definera projection string
SWEREF99TM = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# WGS84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

coordinates(UnikKoordinat) = ~long+lat 
proj4string(UnikKoordinat) = CRS("+init=epsg:4326")
UnikKoordinat_sweref <- as.data.frame(spTransform(UnikKoordinat, CRS(SWEREF99TM)))

# load shapefiles
# SCG tätort shapefil är mer akkurat än landmäteriets, dvs det ingår bara ytan där det finns befolkning. 
# Lantmäteriet använder en buffer. Dessutom följer 2018 befolkningsdata med i shapefilen

kommun=readOGR("00_skript/data/input/shp/ak_riks.shp", 
               stringsAsFactors = FALSE, verbose = FALSE, encoding = "UTF-8")


## ladda ner, unzip och importera tätort shp till R

##td = tempdir()
##tf = tempfile(tmpdir=td, fileext=".zip")

# download.file("https://www.scb.se/contentassets/3ee03ca6db1e48ff808b3c8d2c87d470/to2018_swe99tm_arcview.zip",
#               tf)
# 
# fname = unzip(tf, list=TRUE)$Name[2:5]
# unzip(tf, files=fname, exdir=td, overwrite=TRUE) ##!!Needed to unzip manually..
# td2 = gsub("\\\\", "/", td)
# tatort <- readOGR(paste0(td2, "/To2018_Swe99TM.shp"), stringsAsFactors = FALSE, verbose = FALSE, encoding = "UTF-8")
# unlink(td)

tatort <- readOGR("00_skript/data/input/shp/To2018_Swe99TM.shp", stringsAsFactors = FALSE, verbose = FALSE, use_iconv = TRUE, encoding = "UTF-8")


## några av SCB tätorter är del av ULs stadstrafik och bör kategoriseras som Uppsala tätort
## Ultuna = T0654; Sävja = T0632; Håga = T0566
omkateg = c("T0654", "T0632", "T0566")  

tatort@data = tatort@data %>% 
  mutate(TATORT = ifelse(TATORTSKOD %in% omkateg, "Uppsala", TATORT))

## merge hpl coordinates with shapefiles 
hpl_kommun <- data.frame(coordinates(UnikKoordinat_sweref), ##!!had issue with install rgeos
                         raster::extract(kommun, UnikKoordinat_sweref))
hpl_tatort <- data.frame(coordinates(UnikKoordinat_sweref),
                         raster::extract(tatort, UnikKoordinat_sweref))

##!! next chunk doesn't work, use bind_cols
## append files: alla hpl och all meta data
#hpl_meta = cbind(HplIdKoordinat, 
#                 hpl_kommun[c("long", "lat", "KOMMUNNAMN", "LANSNAMN")], 
#                 hpl_tatort[c("TATORTSKOD", "TATORT", "BEF")])

hpl_meta <- HplIdKoordinat %>%
  bind_cols(hpl_kommun[c("long", "lat", "KOMMUNNAMN", "LANSNAMN")]) %>%
  bind_cols(hpl_tatort[c("TATORTSKOD", "TATORT", "BEF")])

colnames(hpl_meta) = c("HplID", "Hpl_Wgs84North", "Hpl_Wgs84East", "Hpl_SwerefEast", "Hpl_SwerefNorth", "KommunNamn", "LanNamn", "TatortKod",
                       "TatortNamn", "Befolkning2018") # "DesoID", "RutID", 


## Bålsta buss och tåg station ligger utanför tätorten och fick inget tätortnamn: raster::extract()
hpl_meta = hpl_meta %>%
  as.data.frame() %>%
  mutate(TatortNamn = ifelse(HplID == "705003" | HplID == "105112", "Bålsta", TatortNamn),
         TatortKod = ifelse(HplID == "705003" | HplID == "105112", "T0520", TatortKod),
         Befolkning2018 = ifelse(HplID == "705003" | HplID == "105112", "15210", Befolkning2018))

storregional = storregionala_tatorter

hpl_meta = hpl_meta %>% 
  mutate(HplID = as.character(hpl_meta$HplID),
         Befolkning2018 = as.numeric(Befolkning2018)) %>%
  mutate(TatortTyp = case_when(
    Befolkning2018 >= 50 & Befolkning2018 < 200 ~ "Småort",
    Befolkning2018 < 1000  ~ "Mindre tätort",
    Befolkning2018 < 7000  ~ "Medelstor tätort",
    Befolkning2018 >= 7000 & TatortNamn %notin% storregional  ~ "Större tätort",
    TatortNamn %in% storregional  ~ "Storregional kärn",
    TRUE ~ "Utanför tätort"
  ))


## spara resultatfilen
## det krävs datum i filnamn eftersom API data reflekterar idags tidtabell
write.csv2(hpl_meta, paste0("00_skript/data/interim/HplData_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)

}

