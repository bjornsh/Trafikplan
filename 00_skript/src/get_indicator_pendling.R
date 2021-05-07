get_indicator_pendling <- function(
  #vector_peak_hrs = c("06", "07", "08", "15", "16", "17", "18"),
  #vector_off_peak_hrs = c("05", "09", "10", "11", "12", "13", "14", "19", "20", "21", "22", "23"),
  #test_run = FALSE,
  alternatives = 6,
  api_date = today()
)
  {

#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

## Utvärdera TFP indikator 4.1

## Indikator 4.1: Från alla medelstora tätorter inom länet ska finnas: 
## Förbindelser till kommunhuvudort och relevanta regional och storregionala orter som möjliggör resor till arbete och studier 

## Indikator tolkning: Från minst en hpl per medelstor tätort (1000-6999 befolkning, som är inte kommunhuvudort) inom länet ska finnas:
## minst 1 resmöjlighet med max 1 byte under FM rusning (bara vardag) till relevanta regional och storregionala orter och 
## minst 1 resmöjlighet med max 1 byte i motsatt riktning under EM rusning (bara vardag)



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

## senaste filen med hållplatskoordinater och metadata
HplIdKoordinat = sort(list.files("00_skript/data/interim/", pattern = "HplData*"), decreasing = TRUE)[1]
HplIdKoordinat = read.csv2(paste0("00_skript/data/interim/", HplIdKoordinat))

## senaste filen med alla linjer per hållplats
AllaLinjerPerHplID = sort(list.files("00_skript/data/interim/", pattern = "AllaLinjerPerHplID*"), decreasing = TRUE)[1]
AllaLinjerPerHplID = read.csv2(paste0("00_skript/data/interim/", AllaLinjerPerHplID))



#---------------------------------------------------------------------------------------------------
# Identifiera START hållplatser
#---------------------------------------------------------------------------------------------------

## tre SCB tätorter är del av ULs stadstrafik och kategoriseras om till Uppsala tätort
## Ultuna (T0654), Sävja (T0632), Håga (T0566)
omkateg = c("T0654", "T0632", "T0566") 
tatort@data = tatort@data %>% 
  mutate(TATORT = ifelse(TATORTSKOD %in% omkateg, "Uppsala", TATORT))

## definiera huvudtätorter inom länet
huvudort = c("Enköping", "Heby", "Bålsta", "Knivsta", "Tierp", "Uppsala", "Skutskär", "Östhammar")

## filtrera på medelstora tätorter som är inte huvudtätorter
medelstor.shp = tatort[tatort@data$LAN_NAMN == "Uppsala län" &
                         tatort@data$TATORT %notin% huvudort &
                         as.numeric(tatort@data$BEF) >= 1000 &
                         as.numeric(tatort@data$BEF) < 7000,]

## hämta tätort ID för medelstoratätorter 
medelstortatortkod = medelstor.shp@data$TATORTSKOD
medelstortatortnamn = medelstor.shp@data$TATORT

## antal mdelstora tätorter i Uppsala län
length(medelstortatortnamn)

## identifiera hpl för mdelstora tätorter
HplMedelStoraTat = HplIdKoordinat %>% filter(TatortKod %in% medelstortatortkod)


## identifiera minimum antal hpl per "medelstor tätort" som behövs för att alla linjer som trafikerar tätorten är med
## rimligt effektivisering eftersom det finns ingen restidsbegränsning 
## antal byte som är en begränsning påverkas inte eftersom alla linjer är med
## (det är dock en antagning eftersom en extra byte kan teoretiskt leder till en kortare restid och därmed föreslås av reseroboten
## extra byte brykar leda till en markant extra restid och därmed bedöms antagning ok)
StartHpl = HplMedelStoraTat %>% 
  dplyr::select(StartHplID = HplID, StartKommun = KommunNamn, StartTatort = TatortNamn) %>%
  left_join(., AllaLinjerPerHplID, by = c("StartHplID" = "hpl_id")) %>% 
  separate_rows(., AllaLinjer, convert = TRUE) %>%
  distinct(StartTatort, AllaLinjer, .keep_all = TRUE) %>%
  distinct(StartTatort, StartHplID, StartKommun)




#---------------------------------------------------------------------------------------------------
# Identifiera STOP hållplatser
#---------------------------------------------------------------------------------------------------

## filtrera huvudorter inom länet
huvudort.shp = tatort[tatort@data$TATORT %in% huvudort,]

## hämta tätort ID för huvudtätorter 
huvudortkod = huvudort.shp@data$TATORTSKOD

## identifiera hpl i huvudtätorter
HplHuvudTat = HplIdKoordinat %>% filter(TatortKod %in% huvudortkod)

## antal hpl i huvudtätorter
nrow(HplHuvudTat)


## identifiera minimum antal hpl per "medelstor tätort" som behövs för att alla linjer som trafikerar tätorten är med
## rimligt effektivisering eftersom det finns ingen restidsbegränsning 
## antal byte som är en begränsning påverkas inte eftersom alla linjer är med
## (det är dock en antagning eftersom en extra byte kan teoretiskt leder till en kortare restid och därmed föreslås av reseroboten
## extra byte brykar leda till en markant extra restid och därmed bedöms antagning ok)
StopHpl = HplHuvudTat %>% 
  left_join(., AllaLinjerPerHplID, by = c("HplID" = "hpl_id")) %>% 
  dplyr::select(TatortNamn, AllaLinjer, HplID) %>%
  separate_rows(., AllaLinjer, convert = TRUE) %>%
  distinct(TatortNamn, AllaLinjer, .keep_all = TRUE) %>%
  distinct(TatortNamn, HplID) %>%
  mutate(StopKommun = ifelse(TatortNamn == "Bålsta", "Enköping",
                             ifelse(TatortNamn == "Skutskär", "Älvkarleby", TatortNamn)))

## kolla om alla huvudtätorter är med
table(StopHpl$TatortNamn)

## effektiviseringsresultat (andelen av hållplatser som behövs inte längre = 90%)
(nrow(HplHuvudTat) - nrow(StopHpl)) / nrow(HplHuvudTat)



## skapa df med pendlingsrelevanta stop hållplatser
TatortNamn = c("Gävle", "Stockholm", "Västerås", "Sala", "Strängnäs", "Märsta", "Sigtuna", "Rimbo", "Norrtälje" )
HplNamn = c("Gävle central", "Stockholm city", "Västerås centralstation", "Sala station", 
            "Strängnäs station", "Märsta station", "Sigtuna busstation", "Rimbo station", "Norrtälje busstation" ) 
HplID = c("480119", "191718", "660600", "793036", "194027", "191048", "191040", "191171", "191044")
andra_stop = as.data.frame(cbind(TatortNamn, HplNamn, HplID))


## skapa df med alla hpl och tätort info
append1 = StartHpl %>% rename(HplID = StartHplID, Kommun = StartKommun, Tatort = StartTatort) %>% mutate_all(as.character)
append2 = StopHpl %>% rename(Kommun = StopKommun, Tatort = TatortNamn) %>% mutate_all(as.character)
append3 = andra_stop %>% rename(Tatort = TatortNamn) %>% dplyr::select(HplID, Tatort) %>% mutate_all(as.character)

alla_hpl = bind_rows(append1, append2, append3) 
  

#---------------------------------------------------------------------------------------------------
# Slå ihopp start och stop hållplatser
#---------------------------------------------------------------------------------------------------

## lägg till hpl i kommunens huvudorter
df = StartHpl %>% 
  left_join(., StopHpl, by = c("StartKommun" = "StopKommun")) %>%
  rename(StopTatort = TatortNamn, StopHplID = HplID) %>%
  mutate(StartHplID = as.character(StartHplID),
         StopHplID = as.character(StopHplID))


## Lägg till andra pendlings orter (alla möjliga kombinationer: start-stop)
alla = expand.grid(unique(df$StartHplID), andra_stop$HplID)
colnames(alla) = c("StartHplID", "StopHplID")
alla = alla %>% mutate(StartHplID = as.character(StartHplID),
                StopHplID = as.character(StopHplID))

## lägg till metadata
## append resor till kommun huvudort och pendlingsorter
TillHuvud = alla %>% 
  left_join(., unique(df[,c("StartHplID", "StartKommun", "StartTatort")]), by = "StartHplID") %>% 
  left_join(., andra_stop[,c("HplID", "TatortNamn")], by = c("StopHplID" = "HplID")) %>% 
  rename(StopTatort = TatortNamn) %>% 
  bind_rows(., df) %>%
  mutate(typ = "TillHuvud")


## skapa resor i motsatt riktning och append båda riktningar
TillHem = TillHuvud %>%
  rename(StartHplID = StopHplID, StopHplID = StartHplID, # byta namn för att bind_rows fungera
         StopKommun = StartKommun,
         StartTatort = StopTatort, StopTatort = StartTatort) %>%
  mutate(typ = "TillHem") 




#---------------------------------------------------------------------------------------------------
# Skapa API input fil
#---------------------------------------------------------------------------------------------------

dateVardag = api_date

fm_rusning = c("06:00:00", "07:00:00", "08:00:00")
em_rusning = c("15:00:00", "16:00:00", "17:00:00", "18:00:00")
timekort = c("06", "07", "08", "15", "16", "17", "18")
fm_timekort = c("06", "07", "08")
em_timekort = c("15", "16", "17", "18")


## skapa df för till huvud på FM
df_till_huvud = TillHuvud %>% 
  dplyr::select(StartHplID, StopHplID) 

n = length(fm_rusning)
df1_till_huvud = do.call("rbind", replicate(n, df_till_huvud, simplify = FALSE))

timereplokal = rep(fm_rusning, each=nrow(df_till_huvud))
daterep = rep(dateVardag, each=length(timereplokal))

df2_till_huvud = data.frame(df1_till_huvud, daterep, timereplokal)


## skapa df för hemresa på EM
df_till_hem = TillHem %>% 
  dplyr::select(StartHplID, StopHplID) 

n = length(em_rusning)
df1_till_hem = do.call("rbind", replicate(n, df_till_hem, simplify = FALSE))

timereplokal = rep(em_rusning, each=nrow(df_till_hem))
daterep = rep(dateVardag, each=length(timereplokal))

df2_till_hem = data.frame(df1_till_hem, daterep, timereplokal)


## append
df3 = dplyr::bind_rows(df2_till_huvud, df2_till_hem)
         

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

write.csv2(fin, paste0("00_skript/data/interim/pendling_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)


#---------------------------------------------------------------------------------------------------
# Skapa resultat
#---------------------------------------------------------------------------------------------------

## ta bort duplikater 
## om det inte finns tillräckligt många turer per timma (API försöker alltid att hitta 6 turer per resa) hittar APIn turer på en annan tid 
## om man sen söker efter turer kl 06 hittar man samma turer och antal turer får inte adderas 
fin1 = fin %>% distinct() 

write.csv2(fin1, paste0("00_skript/data/interim/pendling1_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)

## skapa nya variabler 
## APIs svar är i UTC -> byta från UTC till lokal tid
fin2 = fin1 %>%
  filter(substr(StartTid, 1, 10) == dateVardag) %>% # filtrera bort alla rader med datum som är INTE datum som används i sökningen
  mutate(StartTid = as.POSIXct(paste(substr(StartTid, 1, 10), substr(StartTid, 12, 19), sep = " "), tz="UTC"),
         AnkomstTid = as.POSIXct(paste(substr(AnkomstTid, 1, 10), substr(AnkomstTid, 12, 19), sep = " "), tz="UTC"),
         StartTidLokal = format(StartTid, tz="Europe/Berlin",usetz=TRUE),
         AnkomstTidLokal = format(AnkomstTid, tz="Europe/Berlin",usetz=TRUE),
         DygnsTimma = paste0("t", substr(StartTidLokal, 12, 13)),
         ResTid = AnkomstTid - StartTid,
         StartStop = paste0(Start, "_", Stop)) %>% 
  dplyr::select(-StartTid, -AnkomstTid)

write.csv2(fin2, paste0("00_skript/data/interim/pendling2_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)


## identifiera första och sista dygnstimma som ingår i data
FirstHr = sort(fin2$DygnsTimma)[1]
LastHr = sort(fin2$DygnsTimma)[length(fin2$DygnsTimma)]


## för varje resrelation, beräkna antal turer per antal byte och dygnstimma
AntalTurerPerBytePerDygnsTimma = fin2 %>% 
  group_by(StartStop, StartHplID, StopHplID, AntalBytePerResa, DygnsTimma) %>% # "count" fungerar inte
  tally() %>%
  tidyr::spread(DygnsTimma, n, fill=0) %>%
  gather(DygnsTimma, AntalTurer, FirstHr:LastHr) %>%
  mutate(DygnsTimma = substr(DygnsTimma, 2, 3)) %>%
  filter(DygnsTimma %in% timekort) %>% ## filtrera bort turer som ligger utanför tidsramen
  mutate(StartHplID = as.character(StartHplID), StopHplID = as.character(StopHplID), ## behövs för join
         Riktning = ifelse(DygnsTimma %in% fm_timekort, "TillHuvud", ## Indikator dimension
                           ifelse(DygnsTimma %in% em_timekort, "TillHem", "XXX"))) %>%
  left_join(., alla_hpl, by =  c("StartHplID" = "HplID")) %>% ## tätort och kommun info
  rename(StartTatort = Tatort, StartKommun = Kommun) %>%
  left_join(., alla_hpl, by =  c("StopHplID" = "HplID")) %>%
  rename(StopTatort = Tatort, StopKommun = Kommun)


## max antal resor per medelstor tätort till huvudort
rusultat_till_huvud = AntalTurerPerBytePerDygnsTimma %>%
  filter(Riktning == "TillHuvud" & as.numeric(AntalBytePerResa) <= 1) %>%
  group_by(StartTatort, StopTatort) %>%
  summarise(MaxAntalTurer = max(AntalTurer)) %>%
  mutate(TillHuvudOk = ifelse(MaxAntalTurer > 0, "Ok", "EjOk"))


## max antal resor per huvudort till medelstor tätort (till hem)
rusultat_till_hem = AntalTurerPerBytePerDygnsTimma %>%
  filter(Riktning == "TillHem" & as.numeric(AntalBytePerResa) <= 1) %>%
  group_by(StartTatort, StopTatort) %>%
  summarise(MaxAntalTurer = max(AntalTurer)) %>%
  mutate(TillHemOk = ifelse(MaxAntalTurer > 0, "Ok", "EjOk"))


## slå ihopp
resultat1 = rusultat_till_huvud %>%
  left_join(., rusultat_till_hem, by = c("StartTatort" = "StopTatort",  "StopTatort" = "StartTatort")) %>%
  rename(TillHuvud_MaxAntalTurer = MaxAntalTurer.x, TillHem_MaxAntalTurer = MaxAntalTurer.y) %>%
  mutate(TillHuvudOchHemOk = ifelse(TillHuvudOk == "Ok" & TillHemOk == "Ok", "Ok", "EjOk"))


## är alla reserelationer med i resultat filen?
## skapa en df med alla reserelationer
alla_start_stop_relationer = as.data.frame(unique(TillHuvud[,c("StartTatort", "StopTatort")]))


## slå ihopp alla reserelationer med resultat filen
resultat2 = alla_start_stop_relationer %>%
  left_join(., resultat1, by = c("StartTatort", "StopTatort")) %>%
  mutate(TillHuvudOk = replace_na(TillHuvudOk, "EjOk"),
         TillHemOk = replace_na(TillHemOk, "EjOk"),
         TillHuvudOchHemOk = replace_na(TillHuvudOchHemOk, "EjOk")) %>%
  dplyr::select(-TillHuvud_MaxAntalTurer, -TillHem_MaxAntalTurer)


## andel huvudorter och relevanta regional och storregionala orter man kan resa till från en medelstortätort
resultat_per_starttatort = resultat2 %>% 
  group_by(StartTatort, TillHuvudOchHemOk) %>% 
  tally() %>% 
  spread(TillHuvudOchHemOk, n, fill = 0) %>% 
  mutate(AndelStopTatortOk = Ok / sum(EjOk, Ok)) %>%
  dplyr::select(StartTatort, AndelStopTatortOk)
  

## andel medelstortätort man kan resa till från en huvudorter eller relevant regional och storregionala ort 
resultat_per_stoptatort = resultat2 %>% 
  group_by(StopTatort, TillHuvudOchHemOk) %>% 
  tally() %>% 
  spread(TillHuvudOchHemOk, n, fill = 0) %>% 
  filter(!is.na(StopTatort)) %>%
  mutate(AndelStartTatortOk = Ok / sum(EjOk, Ok)) %>%
  dplyr::select(StopTatort, AndelStartTatortOk)


## Check: antal medelstor tatorter i Uppsala län = antal start tätorter i resultat filen
length(unique(alla_start_stop_relationer$StartTatort)) == length(resultat_per_starttatort$StartTatort)
length(unique(alla_start_stop_relationer$StopTatort)) == length(resultat_per_stoptatort$StopTatort) 

alla_start_stop_relationer %>% filter(StopTatort %notin% resultat_per_stoptatort$StopTatort)



write.csv2(resultat2, paste0("00_skript/data/output/resultat_pendling_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)
write.csv2(resultat_per_starttatort, paste0("00_skript/data/output/resultat_pendling_starttatort_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)
write.csv2(resultat_per_stoptatort, paste0("00_skript/data/output/resultat_pendling_stoptatort_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)


}