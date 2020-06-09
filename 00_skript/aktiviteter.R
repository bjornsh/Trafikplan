#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

## Utvärdera TFP indikator 4.2
## Indikator tolkning: Från alla hpl i alla medelstora tätorter (1000-6999 befolkning) som är inte kommunhuvudort till alla respektive 
## kommunhuvudorter i länet finns minst 1 resmöjlighet med max 1 byte mellan 1900 och 2159 på vardagar och helger
## plus en resemöjlighet i motsatt riktning inom samma tidsperiod (för att komma hem).




#---------------------------------------------------------------------------------------------------
# Städa
#---------------------------------------------------------------------------------------------------
rm(list = ls())
invisible(gc())


#---------------------------------------------------------------------------------------------------
# Libraries
#---------------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(rgdal)
library(rgeos)
library(raster)
library(jsonlite)


`%notin%` <- Negate(`%in%`)


#---------------------------------------------------------------------------------------------------
# Hämta data
#---------------------------------------------------------------------------------------------------

## tätort shp
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")

download.file("https://www.scb.se/contentassets/3ee03ca6db1e48ff808b3c8d2c87d470/to2018_swe99tm_arcview.zip",
              tf)

fname = unzip(tf, list=TRUE)$Name[2:5]
unzip(tf, files=fname, exdir=td, overwrite=TRUE)
td2 = gsub("\\\\", "/", td)
tatort <- readOGR(paste0(td2, "/To2018_Swe99TM.shp"), stringsAsFactors = FALSE, verbose = FALSE, encoding = "UTF-8")
unlink(td)


#---------------------------------------------------------------------------------------------------
# Datahantering
#---------------------------------------------------------------------------------------------------

## tre SCB tätorter är del av ULs stadstrafik och kategoriseras om till Uppsala tätort
## Ultuna (T0654), Sävja (T0632), Håga (T0566)
omkateg = c("T0654", "T0632", "T0566") 
tatort@data = tatort@data %>% 
  mutate(TATORT = ifelse(TATORTSKOD %in% omkateg, "Uppsala", TATORT))


## identifiera medelstora tätorter som är inte kommun huvudtätort
## Bålsta = huvudort för Håbo kommun; Skutskär = huvudort för Älvkarleby kommun. För andra kommuner är kommunnamn = huvudortnamn
huvudort = c("Enköping", "Heby", "Bålsta", "Knivsta", "Tierp", "Uppsala", "Skutskär", "Östhammar")

medelstor.shp = tatort[tatort@data$LAN_NAMN == "Uppsala län" &
                     tatort@data$TATORT %notin% huvudort &
                     as.numeric(tatort@data$BEF) >= 1000 &
                     as.numeric(tatort@data$BEF) < 7000,]

huvudort.shp = tatort[tatort@data$TATORT %in% huvudort,]


## hämta tätort ID för medelstora- och huvudtätorter 
medelstortatortkod = medelstor.shp@data$TATORTSKOD
medelstortatortnamn = medelstor.shp@data$TATORT

huvudortkod = huvudort.shp@data$TATORTSKOD

## identifiera relevanta hpl per tätorttyp
HplIdKoordinat = read.csv2("01_input_data/HplData_2020-05-19.csv")

HplMedelStoraTat = HplIdKoordinat %>% filter(TatortKod %in% medelstortatortkod)
HplHuvudTat = HplIdKoordinat %>% filter(TatortKod %in% huvudortkod)


## skapa en df med hpl ID och tätort namn för medel och huvud tätorter 
AllaHplMedelHuvud = HplMedelStoraTat[,c("HplID", "TatortNamn")] %>% 
  bind_rows(., HplHuvudTat[,c("HplID", "TatortNamn")])


## länk medelstora tätorter till respektive huvudtätort
## ladda df med en hpl per tätort
hpl = read.csv("01_input_data/TatortMedEnHpl.csv", sep = ";", 
               stringsAsFactors=FALSE, encoding = "UTF-8")
colnames(hpl) = c("TatortNamn", "HplNamn", "HplID")


## eftersom det måste finnas resmöjligheter till huvudtätorten och hem
## skapas en df med start och stop hpl i båda riktningar
TillHuvud = HplMedelStoraTat %>% 
  mutate(huvudort = ifelse(KommunNamn == "Håbo", "Bålsta",
                           ifelse(KommunNamn == "Älvkarleby", "Skutskär", KommunNamn))) %>%
  left_join(., hpl, by = c("huvudort" = "TatortNamn")) %>%
  dplyr::rename(StartHplID = HplID.x, StopHplID = HplID.y) %>%
  dplyr::select(StartHplID, StopHplID) %>%
  mutate(Typ = "MedelTillHuvudTatort")


df = TillHuvud %>%
  dplyr::select(StartHplID, StopHplID) %>% # byta order för hemresa
  mutate(Typ = "HuvudTillMedelTatort") %>%
  rename(StartHplID = StopHplID, # byta namn för att bind_rows fungera
         StopHplID = StartHplID) %>%
  dplyr::bind_rows(., TillHuvud) %>%
  dplyr::select(StartHplID, StopHplID, Typ)


## test att append fungerade
# df %>% filter(StartHplID == "700600")
# df %>% filter(StartHplID == "700600") %>% group_by(Typ) %>% tally()
# df %>% filter(StopHplID == "700600")
# df %>% filter(StopHplID == "700600") %>% group_by(Typ) %>% tally()


#---------------------------------------------------------------------------------------------------
# Skapa API input fil
#---------------------------------------------------------------------------------------------------
time = c("19:00:00", "20:00:00", "21:00:00")
timekort = c("19", "20", "21")

# resmöjligheter behövs på vardagar och helger
dateVardag = "2020-05-25"
dateHelg = "2020-05-30"

n = length(time)
df1 = do.call("rbind", replicate(n, df, simplify = FALSE))

timereplokal = rep(time, each=length(start))
daterep = rep(dateVardag, each=length(timereplokal))

df2 = data.frame(df1, daterep, timereplokal)

## byt datum till helg och append data - vardag och helg resor i samma df
df3 = df2 %>% 
  mutate(daterep = gsub(dateVardag, dateHelg, daterep)) %>%
  bind_rows(., df2)

## konvertera tid till UTC
df3 = df3 %>% 
  mutate(DateTimeLokal = as.POSIXct(paste(daterep, timereplokal), tz="Europe/Berlin"),
         DateTimeUTC = format(DateTimeLokal, tz="UTC",usetz=TRUE))

## skapa url för varje resa
df3$url = paste0("https://api.ul.se/api/v4/journeys?",
                 "fromPointId=",df3$StartHplID,"&fromPointType=","0",
                 "&toPointId=", df3$StopHplID,"&toPointType=","0",
                 "&dateTime=", substr(df3$DateTimeUTC, 1, 10),"T",substr(df3$DateTimeUTC, 12, 19), "Z",
                 "&directionType=", "0",
                 "&trafficTypes=","1,2,3,4,5,6,7,8,9,10,11")



#---------------------------------------------------------------------------------------------------
# Hämta data från API 
#---------------------------------------------------------------------------------------------------

## skicka frågor till API (1000 resor ~ 6min) 
Start = list()
Stop = list()
StartHplID = list()
StopHplID = list()
StartTid = list()
AnkomstTid = list()
AntalBytePerResa = list()
Linje = list()

for(i in 1:nrow(df3)){
  try({
  data = fromJSON(paste0(df3$url[i])) 
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

## slå ihopp allt i en df
fin = do.call(rbind, Map(data.frame, 
                         Start=Start, Stop = Stop, StartHplID = StartHplID, StopHplID = StopHplID,
                         StartTid = StartTid, AnkomstTid = AnkomstTid, AntalBytePerResa = AntalBytePerResa))

write.csv2(fin, "02_output_data/temp/aktiviteter.csv", row.names = F)



#---------------------------------------------------------------------------------------------------
# Skapa resultat
#---------------------------------------------------------------------------------------------------

## ta bort duplikater 
## om det inte finns tillräckligt många turer per timma (API försöker alltid att hitta 6 turer per resa) hittar APIn turer på en annan tid 
## om man sen söker efter turer kl 06 hittar man samma turer och antal turer får inte adderas 
fin1 = fin %>% distinct() 
write.csv2(fin1, "02_output_data/temp/aktiviteter1.csv", row.names = F)


# skapa nya variabler 
# APIs svar är i UTC -> byta från UTC till lokal tid
fin2 = fin1 %>%
  filter(substr(StartTid, 1, 10) == dateVardag | 
           substr(StartTid, 1, 10) == dateHelg) %>% # filtrera bort alla rader med datum som är INTE datum som används i sökningen
  mutate(StartTid = as.POSIXct(paste(substr(StartTid, 1, 10), substr(StartTid, 12, 19), sep = " "), tz="UTC"),
         AnkomstTid = as.POSIXct(paste(substr(AnkomstTid, 1, 10), substr(AnkomstTid, 12, 19), sep = " "), tz="UTC"),
         StartTidLokal = format(StartTid, tz="Europe/Berlin",usetz=TRUE),
         AnkomstTidLokal = format(AnkomstTid, tz="Europe/Berlin",usetz=TRUE),
         DygnsTimma = paste0("t", substr(StartTidLokal, 12, 13)),
         ResTid = AnkomstTid - StartTid,
         StartStop = paste0(Start, "_", Stop)) %>% 
  dplyr::select(-StartTid, -AnkomstTid)

write.csv2(fin2, "02_output_data/temp/aktiviteter2.csv", row.names = F)

# identifiera första och sista dygnstimma som ingår i data
FirstHr = sort(fin2$DygnsTimma)[1]
LastHr = sort(fin2$DygnsTimma)[length(fin2$DygnsTimma)]


AntalTurerPerBytePerDygnsTimma = fin2 %>% 
  group_by(StartStop, StartHplID, StopHplID, AntalBytePerResa, DygnsTimma) %>% # "count" fungerar inte
  tally() %>%
  tidyr::spread(DygnsTimma, n, fill=0) %>%
  gather(DygnsTimma, AntalTurer, FirstHr:LastHr) %>%
  mutate(DygnsTimma = substr(DygnsTimma, 2, 3)) %>%
  filter(DygnsTimma %in% timekort) # filtrera bort turer som ligger utanför tidsramen

temp = AntalTurerPerBytePerDygnsTimma %>%
  left_join(., AllaHplMedelHuvud, by =  c("StartHplID" = "HplID")) %>%
  dplyr::rename(StartTatort = TatortNamn) %>%
  left_join(., AllaHplMedelHuvud, by =  c("StopHplID" = "HplID")) %>%
  dplyr::rename(StopTatort = TatortNamn) %>%
  left_join(., df, by =  c("StartHplID" = "StartHplID", "StopHplID" = "StopHplID"))


## vilka resrelationer bör finnas
Resrelation = medelstor.shp@data %>% 
  dplyr::select(MedelTatort = TATORT, HuvudTatort = KOMMUNNAMN) %>% 
  mutate(HuvudTatort = ifelse(HuvudTatort == "Håbo", "Bålsta",
                              ifelse(HuvudTatort == "Älvkarleby", "Skutskär", HuvudTatort)),
         concat = paste(MedelTatort, HuvudTatort, sep = "_"),
         concat_inv = paste(HuvudTatort, MedelTatort, sep = "_"))
  
## vilka resrelationer finns
resultat = temp %>% 
  filter(AntalBytePerResa <= 1) %>% # max 1 byte
  group_by(Typ, StartTatort, StopTatort) %>% 
  summarise(SummaAntalTurer = sum(AntalTurer)) %>%
  arrange(desc(Typ), StartTatort, StopTatort) %>%
  print(., n = Inf)

temp2 = resultat %>%
  mutate(concat = paste(StartTatort, StopTatort, sep = "_")) %>%
  ungroup() %>%
  dplyr::select(concat)

## bör finnas men finns inte
saknas = Resrelation %>% filter(concat %notin% temp2$concat | 
                         concat_inv %notin% temp2$concat) %>% 
  mutate(Typ = "saknas",
         SummaAntalTurer = 0) %>%
  dplyr::select(Typ, StartTatort = MedelTatort, StopTatort = HuvudTatort, SummaAntalTurer)

## slå ihop finns och finns inte
resultat2 = rbind(resultat, saknas)

resultat2 = resultat2 %>% 
  filter(Typ == "MedelTillHuvudTatort" | Typ == "saknas") %>%
  ungroup() %>%
  dplyr::select(-Typ) %>% print(., n = Inf)

write.csv2(resultat2, "02_output_data/aktiviteter.csv", row.names = F)


