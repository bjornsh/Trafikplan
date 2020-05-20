# Indikator 5.1: Alla tätorter ska ha förbindelser som möjliggör resa till gymnasieskola. Åtminstone någon förbindelse bör också finnas 
# under helger och skollov. I vissa stråk och reserelationer kan anropsstyrd trafik utgöra ett alternativ eller komplement, 
# bland annat som matartrafik ut till befintlig busslinje. 

# Tolkning: Från alla hpl i alla övriga tätorter (200-999 befolkning) i länet till alla gymnasieskolor 
# i länet finns minst 1 resemöjlighet med max 1 byte under fm rusningstiden och i motsat riktning på  
# em rusningstiden på en vardagar och lördag och söndag

# antal kombinationer av alla hpl i tätorter och nära skolr = 30 000. 
# För att effektivisera API körningar används min antal hpl per "övrig tätorter" och per 500m skolbuffer som trafikeras av alla linjer
# t ex i tätort 1: hpl1 = linje 5, hpl2 = linje 6, hpl3 = linje 5 och 6 då ingår bara hpl3 i analysen
# det kan dock betyda att hpl med mindre körtider inte ingår i analysen


library(tidyr)
library(dplyr)
library(rgdal)
library(rgeos)
library(raster)
library(spatialEco)


rm(list = ls())
invisible(gc())

`%notin%` <- Negate(`%in%`)

skol=readOGR("01_input_data/shp/skolor/Buffer_of_Skolor_20200518.shp", 
             stringsAsFactors = FALSE, verbose = FALSE, encoding = "UTF-8")

HplIdKoordinat = read.csv2("01_input_data/HplData_2020-05-19.csv")

# transform skol shp till WGS84
skol = spTransform(skol, CRS("+init=epsg:4326"))


#### identifiera alla hållplatser inom 500m radius av länets gymnasieskolor ######


# Konvertera API hållplats koordinater från WGS84 till SWEREF (för att merge med shapefiler) 
UnikKoordinat = HplIdKoordinat %>% 
  dplyr::select(Hpl_Wgs84North, Hpl_Wgs84East)

UnikKoordinatKOPIA = UnikKoordinat

# definera projection string för koorinater
coordinates(UnikKoordinat) = ~Hpl_Wgs84East+Hpl_Wgs84North 
proj4string(UnikKoordinat) = CRS("+init=epsg:4326")
UnikKoordinat_df <- as_data_frame(UnikKoordinat)

# identifiera koordinater som ligger inom skolbuffer. 
# raster::extract fungerade inte. 
# spatialEco::point.in.poly fungerade på ett korrekt sätt där koordinater som ligger inom ett område där buffer från >1 skolor overlappa allokeras koordinaten till fler skolor, dock förlora point.in.polygon koordinat metadata
# rgeos::over identifiera en buffer även där det finns overlap, som är inte helt korrekt men fungerar för den typen analysen
merge = over(UnikKoordinat, skol)
merge1 = cbind(merge, HplIdKoordinat[,c(1:3,6,8,9,11)]) # lägg till koordinater etc

HplInomSkolBuffer = merge1 %>% filter(!is.na(Namn))

# det finns ingen gymnasieskola i länet utan en hpl inom buffer område
skol@data %>% 
  filter(skol@data$Namn %notin% unique(HplInomSkolBuffer$Namn))

### identifera hpl i övriga tätorter ####
# ladda ner, unzip och importera tätort shp till R
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")

download.file("https://www.scb.se/contentassets/3ee03ca6db1e48ff808b3c8d2c87d470/to2018_swe99tm_arcview.zip",
              tf)

fname = unzip(tf, list=TRUE)$Name[2:5]
unzip(tf, files=fname, exdir=td, overwrite=TRUE)
td2 = gsub("\\\\", "/", td)
tatort <- readOGR(paste0(td2, "/To2018_Swe99TM.shp"), stringsAsFactors = FALSE, verbose = FALSE, encoding = "UTF-8")
unlink(td)


# tre SCB tätorter är del av ULs stadstrafik och kategoriseras om till Uppsala tätort
# Ultuna (T0654), Sävja (T0632), Håga (T0566)
omkateg = c("T0654", "T0632", "T0566") 
tatort@data = tatort@data %>% 
  mutate(TATORT = ifelse(TATORTSKOD %in% omkateg, "Uppsala", TATORT))


# identifiera alla övriga tätorter i Uppsala län & lägg till hpl i respektive tätort
OvrigTatort = tatort@data %>% 
  mutate(BEF = as.numeric(BEF)) %>% 
  filter(LANSKOD == "03" & (BEF >= 200 & BEF < 1000)) %>%
  dplyr::select(KOMMUNNAMN, TATORTSKOD, TATORT) %>% 
  left_join(., HplIdKoordinat[,c("HplID", "Hpl_Wgs84North", "Hpl_Wgs84East", "TatortKod")],
                          by = c("TATORTSKOD" = "TatortKod")) %>%
  arrange(KOMMUNNAMN, TATORT) %>%
  rename(KommunNamn = KOMMUNNAMN, TatortKod = TATORTSKOD, Tatort = TATORT) 

# identifiera övriga tätorter utan hållplats
OvrigTatort %>% filter(is.na(HplID)) 

OvrigTatort1 = OvrigTatort %>% 
  filter(!is.na(HplID)) 

# identifiera min antal hpl per "övrig tätorter" som trafikeras av alla linjer
MinAntalHplOvrigTatort = OvrigTatort1 %>% 
  left_join(., hpl, by = c("HplID" = "hpl_id")) %>% 
  dplyr::select(Tatort, AllaLinjer, HplID) %>%
  separate_rows(., AllaLinjer, convert = TRUE) %>%
  distinct(Tatort, AllaLinjer, .keep_all = TRUE) %>%
  distinct(Tatort, HplID) 


# alla hpl i tätorter till alla hpl inom skol buffer = 30688 resor -> bör effektiviseras
# nrow(HplInomSkolBuffer) * nrow(OvrigTatort1)



#### Minska antal hpl som ingår i API skriptet ####
hpl = read.csv2("01_input_data/AllaLinjerPerHplID_2020-05-19.csv")

# ett antal hpl inom samma buffer trafikeras av exakt samma linjer
HplInomSkolBuffer %>% 
  left_join(., hpl, by = c("HplID" = "hpl_id")) %>%
  group_by(Namn, AllaLinjer) %>% tally() %>% print(., n = Inf)

# tex finns det 3 hpl inom Ansgargymnasiet buffer som trafikeras av: 1_7_770
HplInomSkolBuffer %>% 
  left_join(., hpl, by = c("HplID" = "hpl_id")) %>%
  filter(Namn == "Ansgargymnasiet")

nrow(HplInomSkolBuffer) # 224

# ta bort hpl inom samma buffer och med samma linje kombination = 154. inte tillräckligt bra
HplInomSkolBuffer %>% 
  left_join(., hpl, by = c("HplID" = "hpl_id")) %>% 
  mutate(RowNumber = row_number()) %>% # add rownumber for later join
  dplyr::select(Namn, AllaLinjer) %>%
  distinct() %>% nrow()

# identifiera minimum antal hpl per skola som trafikeras av alla linjer
MinAntalHplInomSkolBuffer = HplInomSkolBuffer %>% 
  left_join(., hpl, by = c("HplID" = "hpl_id")) %>% 
  dplyr::select(Namn, AllaLinjer, HplID) %>%
#  distinct(Namn, AllaLinjer, .keep_all = TRUE) %>%
#  filter(Namn == "Sjogrenska gymnasiet" | Namn == "Westerlundska gymnasiet") %>%
  separate_rows(., AllaLinjer, convert = TRUE) %>%
  distinct(Namn, AllaLinjer, .keep_all = TRUE) %>%
  distinct(Namn, HplID) 

### från 30 000 till 500 resor
# nrow(HplInomSkolBuffer) * nrow(OvrigTatort1)
# nrow(MinAntalHplOvrigTatort) * nrow(MinAntalHplInomSkolBuffer)

### kombinera hpl i övriga tätorter med min antal hpl i skolbuffer ### 

# merge alla tätort hpl till alla skol hållplatser
TatortSkolHpl = merge(MinAntalHplOvrigTatort[,"HplID"], MinAntalHplInomSkolBuffer[,"HplID"])
colnames(TatortSkolHpl) = c("HplIDTatort", "HplIDSkol")
























