get_indicator_aktiviteter <- function(
  #vector_peak_hrs = c("06", "07", "08", "15", "16", "17", "18"),
  #vector_off_peak_hrs = c("05", "09", "10", "11", "12", "13", "14", "19", "20", "21", "22", "23"),
  #test_run = FALSE,
  alternatives = 6,
  hpl_file_date = today(),
  api_date_vd = today(),
  api_data_h = today()
)
{

#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

## Utvärdera TFP indikator 4.2

## Indikator 4.2: Förbindelser som möjliggör resa till och från aktiviteter på kvällen i kommunhuvudorten.
## Förbindelser ska finnas under helger och skollov. 

## Indikator tolkning: Från alla hpl i alla medelstora tätorter (1000-6999 befolkning) som är inte kommunhuvudort till alla respektive 
## kommunhuvudorter i länet finns minst 1 resmöjlighet med max 1 byte mellan 1900 och 2159 på vardagar och helger
## plus en resemöjlighet i motsatt riktning inom samma tidsperiod (för att komma hem).




#---------------------------------------------------------------------------------------------------
# Städa
#---------------------------------------------------------------------------------------------------
invisible(gc())




`%notin%` <- Negate(`%in%`)


#---------------------------------------------------------------------------------------------------
# Hämta data
#---------------------------------------------------------------------------------------------------

## SCB tätort shp
tatort <- readOGR("00_skript/data/input/shp/To2018_Swe99TM.shp",
                  stringsAsFactors = FALSE, verbose = FALSE, use_iconv = TRUE, encoding = "UTF-8")


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
HplIdKoordinat = read.csv2(paste0("00_skript/data/interim/HplData_", hpl_file_date, ".csv"))

HplMedelStoraTat = HplIdKoordinat %>% filter(TatortKod %in% medelstortatortkod)
HplHuvudTat = HplIdKoordinat %>% filter(TatortKod %in% huvudortkod)


## skapa en df med hpl ID och tätort namn för medel och huvud tätorter 
AllaHplMedelHuvud = HplMedelStoraTat[,c("HplID", "TatortNamn")] %>% 
  bind_rows(., HplHuvudTat[,c("HplID", "TatortNamn")])


## länk medelstora tätorter till respektive huvudtätort
## ladda df med en hpl per tätort
hpl = read.csv("00_skript/data/input/TatortMedEnHpl.csv", sep = ";", 
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
dateVardag = api_date_vd
dateHelg = api_date_h

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

get_JSON_data_from_UL_API <- function(url_api){
  tryCatch({
    data = fromJSON(url_api)
  },
  error = function(e) {return(NA)}
  )
}

extract_data_from_API_df <- function(api_df, data_to_extract, max_alternatives = 6){
  if(is.na(api_df)){
    rep(NA, times = max_alternatives)
  }
  else{
    switch(data_to_extract,
           "from_name" = api_df$from$name[1:max_alternatives],
           "to_name" = api_df$to$name[1:max_alternatives],
           "from_id" = api_df$from$id[1:max_alternatives],
           "to_id" = api_df$to$id[1:max_alternatives],
           "dep_time" = api_df$departureDateTime[1:max_alternatives],
           "arr_time" = api_df$arrivalDateTime[1:max_alternatives],
           "changes" = api_df$noOfChanges[1:max_alternatives]
    )
  }
}

ptm <- proc.time()
latest_row <- 1
while(latest_row <= nrow(df3)){
  
  last_row <- min(nrow(df3), latest_row + 9)
  x <- lapply(df3$url[latest_row:last_row], get_JSON_data_from_UL_API)
  message("Hämtat resor ", latest_row, " - ", last_row)
  Start[latest_row:last_row] = lapply(x, extract_data_from_API_df, "from_name", alternatives)
  Stop[latest_row:last_row] = lapply(x, extract_data_from_API_df, "to_name", alternatives)
  StartHplID[latest_row:last_row] = lapply(x, extract_data_from_API_df, "from_id", alternatives)
  StopHplID[latest_row:last_row] = lapply(x, extract_data_from_API_df, "to_id", alternatives)
  StartTid[latest_row:last_row]  = lapply(x, extract_data_from_API_df, "dep_time", alternatives)
  AnkomstTid[latest_row:last_row]  = lapply(x, extract_data_from_API_df, "arr_time", alternatives)
  AntalBytePerResa[latest_row:last_row]  = lapply(x, extract_data_from_API_df, "changes", alternatives)
  rm(x)
  latest_row <- last_row + 1
}
proc.time() - ptm

## slå ihopp allt i en df
fin = do.call(rbind, Map(data.frame, 
                         Start=Start, Stop = Stop, StartHplID = StartHplID, StopHplID = StopHplID,
                         StartTid = StartTid, AnkomstTid = AnkomstTid, AntalBytePerResa = AntalBytePerResa))

write.csv2(fin, paste0("00_skript/data/interim/aktiviteter_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)


#---------------------------------------------------------------------------------------------------
# Skapa resultat
#---------------------------------------------------------------------------------------------------

## ta bort duplikater 
## om det inte finns tillräckligt många turer per timma (API försöker alltid att hitta 6 turer per resa) hittar APIn turer på en annan tid 
## om man sen söker efter turer kl 06 hittar man samma turer och antal turer får inte adderas 
fin1 = fin %>% distinct() 

write.csv2(fin1, paste0("00_skript/data/interim/aktiviteter1_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)

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

write.csv2(fin2, paste0("00_skript/data/interim/aktiviteter2_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)


## identifiera första och sista dygnstimma som ingår i data
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

## slå ihop "finns" och "finns inte"
resultat2 = rbind(resultat, saknas)

resultat2 = resultat2 %>% 
  filter(Typ == "MedelTillHuvudTatort" | Typ == "saknas") %>%
  ungroup() %>%
  dplyr::select(-Typ) %>% print(., n = Inf)

write.csv2(resultat2, paste0("00_skript/data/output/resultat_aktiviteter_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)

}