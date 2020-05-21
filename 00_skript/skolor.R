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
library(jsonlite)
library(stringr)


rm(list = ls())
invisible(gc())

`%notin%` <- Negate(`%in%`)

hpl = read.csv2("01_input_data/AllaLinjerPerHplID_2020-05-19.csv")

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


### API ####

# definera morgon och kväll rusningstider
fm_rusning = c("06", "07", "08") # från tätort till skolan
em_rusning = c("15", "16", "17", "18") # från skolan till tätort
timekort = c("06", "07", "08", "15", "16", "17", "18")


#### välj test ellr full körning
# df = TatortSkolHpl[c(1,20,30, 40,50,100,200),] # test run, ta bort för full körning
df = TatortSkolHpl # full run

# test run
# time = c("06:00:00", "15:00:00")

# full run
time = c("06:00:00", "07:00:00", "08:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00")

dateVardag = "2020-05-25"
dateHelg = "2020-05-30"
date = c("2020-05-25", "2020-05-30")

n = length(time)
df1 = do.call("rbind", replicate(n, df, simplify = FALSE))

timereplokal = rep(time, each=length(start))
daterep = rep(date, each=length(timereplokal))

df2 = data.frame(df1, dateVardag, timereplokal)
colnames(df2) = c("HplIDTatort", "HplIDSkol", "Datum", "timereplokal")

# skapa df för fm- och em-rusningstider där start och stop hpl byter plats mellan fm och em
df3 = df2 %>% 
  filter(substr(timereplokal,1, 2) %in% em_rusning) %>%
  rename(HplIDTatort = HplIDSkol, HplIDSkol = HplIDTatort) %>%
  bind_rows(., df2[substr(df2$timereplokal,1, 2) %in% fm_rusning,]) %>%
  rename(StartHplID = HplIDSkol, StopHplID = HplIDTatort)

# lägg till samma rader med byt datum (helg)
df4 = df3 %>%
  mutate(Datum = gsub(dateVardag, dateHelg, Datum)) %>%
  bind_rows(., df3)


# API tolka tider som UTC, därför omvandlas lokatid här till UTC
df4 = df4 %>% 
  mutate(DateTimeLokal = as.POSIXct(paste(Datum, timereplokal), tz="Europe/Berlin"),
         DateTimeUTC = format(DateTimeLokal, tz="UTC",usetz=TRUE))

# skapa url för varje resa
df4$url = paste0("https://api.ul.se/api/v4/journeys?",
                 "fromPointId=",df4$StartHplID,"&fromPointType=","0",
                 "&toPointId=", df4$StopHplID,"&toPointType=","0",
                 "&dateTime=", substr(df4$DateTimeUTC, 1, 10),"T",substr(df4$DateTimeUTC, 12, 19), "Z",
                 "&directionType=", "0",
                 "&trafficTypes=","1,2,3,4,5,6,7,8,9,10,11")


# skicka frågor till API (1000 resor ~ 6min) 
tid1 = Sys.time()
Start = list()
Stop = list()
StartHplID = list()
StopHplID = list()
StartTid = list()
AnkomstTid = list()
AntalBytePerResa = list()
Linje = list()

for(i in 1:nrow(df4)){
  try({
  data = fromJSON(paste0(df4$url[i])) 
  message("Hämta resa ", i)
  Start[[i]] = data$from$name
  Stop[[i]] = data$to$name
  StartHplID[[i]] = data$from$id
  StopHplID[[i]] = data$to$id
  StartTid[[i]] = data$departureDateTime
  AnkomstTid[[i]] = data$arrivalDateTime
  AntalBytePerResa[[i]] = data$noOfChanges
  })
}

tid2 = Sys.time()

tid2-tid1

##### slå ihopp allt i en df
fin = do.call(rbind, Map(data.frame, 
                         Start=Start, Stop = Stop, StartHplID = StartHplID, StopHplID = StopHplID,
                         StartTid = StartTid, AnkomstTid = AnkomstTid, AntalBytePerResa = AntalBytePerResa))

write.csv2(fin, "02_output_data/temp/skol.csv", row.names = F)

# ta bort duplikater 
# om det inte finns tillräckligt många turer per timma (API försöker alltid att hitta 6 turer per resa) hittar APIn turer på en annan tid 
# om man sen söker efter turer kl 06 hittar man samma turer och antal turer får inte adderas 
fin1 = fin %>% distinct() 
write.csv2(fin1, "02_output_data/temp/skol1.csv", row.names = F)

# skapa nya variabler 
# APIs svar är i UTC -> byta från UTC till lokal tid
fin2 = fin1 %>%
  filter(substr(StartTid, 1, 10) %in% date) %>% # filtrera bort alla rader med datum som är INTE datum som används i sökningen
  mutate(StartTid = as.POSIXct(paste(substr(StartTid, 1, 10), substr(StartTid, 12, 19), sep = " "), tz="UTC"),
         AnkomstTid = as.POSIXct(paste(substr(AnkomstTid, 1, 10), substr(AnkomstTid, 12, 19), sep = " "), tz="UTC"),
         StartTidLokal = format(StartTid, tz="Europe/Berlin",usetz=TRUE),
         AnkomstTidLokal = format(AnkomstTid, tz="Europe/Berlin",usetz=TRUE),
         DygnsTimma = paste0("t", substr(StartTidLokal, 12, 13)),
         ResTid = AnkomstTid - StartTid,
         StartStop = paste0(Start, "_", Stop)) %>% 
  dplyr::select(-StartTid, -AnkomstTid)

write.csv2(fin2, "02_output_data/temp/skol2.csv", row.names = F)

# identifiera första och sista dygnstimma som ingår i data
FirstHr = sort(fin2$DygnsTimma)[1]
LastHr = sort(fin2$DygnsTimma)[length(fin2$DygnsTimma)]


AntalTurerPerBytePerDygnsTimma = fin2 %>% 
  count(StartStop, StartHplID, StopHplID, AntalBytePerResa, DagTyp = substr(StartTidLokal,1, 10), DygnsTimma) %>%
  spread(DygnsTimma, n, fill=0) %>%
  gather(DygnsTimma, AntalTurer, FirstHr:LastHr) %>%
  mutate(DygnsTimma = substr(DygnsTimma, 2, 3),
         TimmaTyp = ifelse(DygnsTimma %in% fm_rusning, "fm_rusning",
                           ifelse(DygnsTimma %in% em_rusning, "em_rusning", NA))) %>%
  filter(!is.na(TimmaTyp)) 

# det finns inga skolor med samma namn
# HplInomSkolBuffer %>% dplyr::select(id, Namn) %>% distinct() %>% group_by(id, Namn) %>% summarise(n = n()) %>% arrange(-n)  
  
fin3 = fin2 %>% 
    count(StartStop, StartHplID, StopHplID, AntalBytePerResa, DagTyp = substr(StartTidLokal,1, 10), DygnsTimma) %>%
    spread(DygnsTimma, n, fill=0) %>%
    gather(DygnsTimma, AntalTurer, FirstHr:LastHr) %>%
    mutate(DygnsTimma = substr(DygnsTimma, 2, 3),
           TimmaTyp = ifelse(DygnsTimma %in% fm_rusning, "fm_rusning",
                             ifelse(DygnsTimma %in% em_rusning, "em_rusning", NA))) %>%
    filter(!is.na(TimmaTyp)) %>%
    left_join(., HplInomSkolBuffer[,c("Namn", "HplID")], # add skol info
              by = c("StartHplID" = "HplID"))  %>%
    left_join(., HplInomSkolBuffer[,c("Namn", "HplID")], 
              by = c("StopHplID" = "HplID")) %>%
    mutate(SkolNamn = ifelse(is.na(Namn.x), Namn.y, Namn.x)) %>%
    dplyr::select(-Namn.x, -Namn.y) %>%
    left_join(., OvrigTatort1[,c("Tatort", "HplID")], # add tätort info
              by = c("StartHplID" = "HplID"))  %>%
    left_join(., OvrigTatort1[,c("Tatort", "HplID")], 
              by = c("StopHplID" = "HplID")) %>%
    mutate(Tatort = ifelse(is.na(Tatort.x), Tatort.y, Tatort.x)) %>%
    dplyr::select(-Tatort.x, -Tatort.y) %>%
    filter(as.numeric(AntalBytePerResa) <= 1) %>% # bara resor med <=1 byte ingår
    group_by(SkolNamn, Tatort, DagTyp, TimmaTyp) %>% # "DygnsTimma" behövs inte eftersom 1 per TimmaTyp är tillräckligt
    summarise(SummaAntalTurer = sum(as.numeric(AntalTurer))) %>% # antal turer med <= 1 byte
    filter(SummaAntalTurer > 0) %>% 
    spread(TimmaTyp, SummaAntalTurer, fill = 0)

# skapa df för med alla reserelationer som bör finnas i API resultat
AllaSkolTatortKombinationer = merge(skol@data$Namn, unique(OvrigTatort1$Tatort))
AllaSkolTatortKombinationer$DagTyp = dateVardag

AllaSkolTatortKombinationer2 = AllaSkolTatortKombinationer %>% 
  mutate(DagTyp = dateHelg) %>%
  bind_rows(., AllaSkolTatortKombinationer)

colnames(AllaSkolTatortKombinationer2) = c("SkolNamn", "Tatort", "DagTyp")  

fin4 = AllaSkolTatortKombinationer2 %>% 
  left_join(., fin3, by = c("SkolNamn", "Tatort", "DagTyp")) %>%
  mutate(Resultat = ifelse(em_rusning >=1 & fm_rusning >= 1, "Ok", "EjOk")) %>%
  dplyr::select(-em_rusning, -fm_rusning) %>%
  spread(DagTyp, Resultat, fill = "EjOk")

fin4$Resultat = ifelse(fin4[,3] == "Ok" & fin4[,4] == "Ok", "VardagOchHelgOK",
                       ifelse(fin4[,3] == "Ok" & fin4[,4] == "EjOk", "VardagOK",
                              ifelse(fin4[,3] == "EjOk" & fin4[,4] == "Ok", "HelgOK",
                                     ifelse(fin4[,3] == "EjOk" & fin4[,4] == "EjOk", "EjOK", "XXX"))))

SkolSummary = fin4 %>% group_by(Resultat) %>% tally()

write.csv2(fin4, "02_output_data/skolor.csv", row.names = F)
write.csv2(SkolSummary, "02_output_data/skolor_ResultatKort.csv", row.names = F)













