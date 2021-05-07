get_api_data <- function(vector_peak_hrs = c("06", "07", "08", "15", "16", "17", "18"),
                         vector_off_peak_hrs = c("05", "09", "10", "11", "12", "13", "14", "19", "20", "21", "22", "23"),
                         test_run = FALSE,
                         alternatives = 6,
                         api_date = today()){

# indikatorer 1.1, 1.2, 2.1, 2.2, 3.1, 3.2
#rm(list = ls())
invisible(gc())

plan = read.csv2("00_skript/data/interim/AnalysPlanInputForAPI.csv")


# antal unika hpl per tätort
# plan %>% group_by(StartTatort) %>% summarise(n = n_distinct(StartHplID)) %>% arrange(desc(n)) %>% print(., n = Inf)
# plan %>% group_by(StopTatort) %>% summarise(n = n_distinct(StopHplID)) %>% arrange(desc(n)) %>% print(., n = Inf)


# definera rusnings- och ej-rusningstimmar. 0000 - 0459 ingår inte i analysen
#rusning = c("06", "07", "08", "15", "16", "17", "18")
rusning = vector_peak_hrs
#ejrusning = c("05", "09", "10", "11", "12", "13", "14", "19", "20", "21", "22", "23")
ejrusning = vector_off_peak_hrs

# test run, ta bort för full körning
if(test_run){
plan = plan[c(1,20,30, 40,50,100,200),]
}

# skapa df för senare användning
StartTatort = plan %>% dplyr::select(StartTatort, StartHplID) %>% distinct()
StopTatort = plan %>% dplyr::select(StopTatort, StopHplID) %>% distinct()  


####### Skapa input data
# start = c("700600", "700600", "700600", "700600","700600", "700600", "700600")
# stop = c("191718", "480119", "660600", "781210", "705003", "780050", "719200")

df = plan %>% dplyr::select(StartHplID, StopHplID)

# test run
if(test_run){
  time = c("05:00:00", "08:00:00", "11:00:00", "15:00:00")# midnatt = "00:00:00"
#time = c("06:00:00", "07:00:00", "08:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00") # midnatt = "00:00:00"
} else {
  time = c("05:00:00", "06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00",
  "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00", "20:00:00",
   "21:00:00", "22:00:00", "23:00:00") # midnatt = "00:00:00"
}

# full run
# time = c("05:00:00", "06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00",
#          "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00", "20:00:00",
#          "21:00:00", "22:00:00", "23:00:00") # midnatt = "00:00:00"

date = api_date

n = length(time)
df1 = do.call("rbind", replicate(n, df, simplify = FALSE))

timereplokal = rep(time, each=dim(df1[1]))
daterep = rep(date, each=length(timereplokal))

df2 = data.frame(df1, daterep, timereplokal)

# API tolka tider som UTC, därför omvandlas lokatid här till UTC
df2 = df2 %>% 
  mutate(DateTimeLokal = as.POSIXct(paste(daterep, timereplokal), tz="Europe/Berlin"),
         DateTimeUTC = format(DateTimeLokal, tz="UTC",usetz=TRUE))

# skapa url för varje resa
df2$url = paste0("https://api.ul.se/api/v4/journeys?",
                 "fromPointId=",df2$StartHplID,"&fromPointType=","0",
                 "&toPointId=", df2$StopHplID,"&toPointType=","0",
                 "&dateTime=", substr(df2$DateTimeUTC, 1, 10),"T",substr(df2$DateTimeUTC, 12, 19), "Z",
                 "&directionType=", "0",
                 "&trafficTypes=","1,2,3,4,5,6,7,8,9,10,11")

#clear some memory
rm(daterep, timereplokal)


# skicka frågor till API (1000 resor ~ 6min) 
Start = list()
Stop = list()
StartHplID = list()
StopHplID = list()
StartTid = list()
AnkomstTid = list()
AntalBytePerResa = list()
Linje = list()

##!! remove rows with NAs
df2 <- df2 %>% filter(!is.na(StartHplID) & !is.na(StopHplID))


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
while(latest_row <= nrow(df2)){
  
    last_row <- min(nrow(df2), latest_row + 9)
    x <- lapply(df2$url[latest_row:last_row], get_JSON_data_from_UL_API)
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

##### slå ihopp allt i en df
fin = do.call(rbind, Map(data.frame, 
                         Start=Start, Stop = Stop, StartHplID = StartHplID, StopHplID = StopHplID,
                         StartTid = StartTid, AnkomstTid = AnkomstTid, AntalBytePerResa = AntalBytePerResa))

write.csv2(fin, paste0("00_skript/data/output/API_data_", api_date, ".csv"), row.names = F)

# ta bort duplikater 
# om det inte finns tillräckligt många turer per timma (API försöker alltid att hitta 6 turer per resa) hittar APIn turer på en annan tid 
# om man sen söker efter turer kl 06 hittar man samma turer och antal turer får inte adderas 
fin1 = fin %>% distinct() 
write.csv2(fin1, paste0("00_skript/data/output/API_distinct_data_", api_date, ".csv"), row.names = F)


# skapa nya variabler 
# APIs svar är i UTC -> byta från UTC till lokal tid
fin2 = fin1 %>%
  filter(substr(StartTid, 1, 10) == date) %>% # filtrera bort alla rader med datum som är INTE datum som används i sökningen
  mutate(StartTid = as.POSIXct(paste(substr(StartTid, 1, 10), substr(StartTid, 12, 19), sep = " "), tz="UTC"),
         AnkomstTid = as.POSIXct(paste(substr(AnkomstTid, 1, 10), substr(AnkomstTid, 12, 19), sep = " "), tz="UTC"),
         StartTidLokal = format(StartTid, tz="Europe/Berlin",usetz=TRUE),
         AnkomstTidLokal = format(AnkomstTid, tz="Europe/Berlin",usetz=TRUE),
         DygnsTimma = paste0("t", substr(StartTidLokal, 12, 13)),
         ResTid = AnkomstTid - StartTid,
         StartStop = paste0(Start, "_", Stop)) %>% 
  dplyr::select(-StartTid, -AnkomstTid)

write.csv2(fin2, "00_skript/data/output/fin2.csv", row.names = F)

# identifiera första och sista dygnstimma som ingår i data
FirstHr = sort(fin2$DygnsTimma)[1]
LastHr = sort(fin2$DygnsTimma)[length(fin2$DygnsTimma)]


AntalTurerPerBytePerDygnsTimma = fin2 %>% 
  count(StartStop, StartHplID, StopHplID, AntalBytePerResa, DygnsTimma) %>%
  spread(DygnsTimma, n, fill = 0) %>%
  gather(DygnsTimma, AntalTurer, all_of(FirstHr):all_of(LastHr)) %>%
  mutate(DygnsTimma = substr(DygnsTimma, 2, 3),
         TimmaTyp = ifelse(DygnsTimma %in% rusning, "rusning",
                           ifelse(DygnsTimma %in% ejrusning, "ejrusning", NA))) %>%
  filter(!is.na(TimmaTyp))


# när "plan" är redo med lägg till  
  # left_join(., plan[,c("StartHplID", "StopHplID", "AntalTurerPerRusningTimma", "AntalTurerPerEjRusningTimma")],
  #           by = c("StartHplID" = "StartHplID", "StopHplID" = "StopHplID")) %>%
  #    filter AntalBytePerResa > AntalTurerPerRusningT, ta bort


fin3 = fin2 %>% 
  count(StartStop, StartHplID, StopHplID, AntalBytePerResa, DygnsTimma) %>%
  spread(DygnsTimma, n, fill=0) %>%
  gather(DygnsTimma, AntalTurer, all_of(FirstHr):all_of(LastHr)) %>%
  mutate(DygnsTimma = substr(DygnsTimma, 2, 3),
         TimmaTyp = ifelse(DygnsTimma %in% rusning, "rusning",
                    ifelse(DygnsTimma %in% ejrusning, "ejrusning", NA))) %>%
  filter(!is.na(TimmaTyp)) %>%
  left_join(., plan[,c("StartHplID", "StopHplID", "AntalTurerPerRusningTimma", "AntalTurerPerEjRusningTimma", "MaxAntalByte")],
            by = c("StartHplID" = "StartHplID", "StopHplID" = "StopHplID")) %>%
  filter(AntalBytePerResa <= 1) %>%
#  filter(AntalBytePerResa <= MaxAntalByte) %>% # use when input table has data for MaxAntalByte
  group_by(StartStop, StartHplID, StopHplID, TimmaTyp, DygnsTimma) %>%
  summarise(SummaAntalTurer = sum(AntalTurer)) %>%
  ungroup() %>%
  left_join(., plan[,c("StartHplID", "StopHplID", "AntalTurerPerRusningTimma", "AntalTurerPerEjRusningTimma")],
            by = c("StartHplID" = "StartHplID", "StopHplID" = "StopHplID")) %>%
  mutate(AntalTurer = as.numeric(SummaAntalTurer)) %>%
  mutate(TillrackligAntalTurer = ifelse((TimmaTyp == "rusning" & AntalTurer >= 2) |
                                          (TimmaTyp == "ejrusning" & AntalTurer >= 1), "Ja", "Nej"))
  

#### Slutresultat #####

# länk PLAN kolumns för att identifier vilka tätorter det handlar om 
### DET GÅR INTE ATT ADDERA ANTAL RESOR PER STARTTATORT OCH STOPTATORT KOMBINATION - hpl kan ligga på samma resa och därmed är det dubbelräkning
### DET MÅSTE VARA MAX ANTALTURER
fin4 = fin3 %>% 
  left_join(., StartTatort, by = "StartHplID") %>%
  left_join(., StopTatort, by = "StopHplID")


# här används max antal turer per timma reserelation, dvs om det finns 3 hpl i tätort X och 2 i tätort Y, finns det sex reserelationer för tätort X-Y
# för varje reserelation får man antal turer per timma. Här används max som filtrerar den högsta per timma ---- är det en bra strategi???????????

fin5 = fin4 %>%
  group_by(StartTatort, StopTatort, TimmaTyp, DygnsTimma) %>%
  summarise(MaxAntalTurerPerTimmaPerTatortTatort = max(AntalTurer)) %>%  
  mutate(TillrackligAntalTurer = ifelse(MaxAntalTurerPerTimmaPerTatortTatort >=2, "Ja", "Nej")) %>%
  group_by(StartTatort, StopTatort, TimmaTyp, TillrackligAntalTurer) %>%
  summarise(AntalTimma = n()) %>%
  spread(TillrackligAntalTurer, AntalTimma, fill = 0) %>%
  mutate(AndelTimmarMedTillrackligTurer = Ja / (Ja+Nej)) %>%
  dplyr::select(-Ja, -Nej) %>%
  spread(TimmaTyp, AndelTimmarMedTillrackligTurer)

# print(fin5, n = Inf)

fin6 = plan %>% 
  dplyr::select(Indikator, StartTatort, StopTatort) %>%
  distinct() %>%
  right_join(., fin5, by = c("StartTatort", "StopTatort")) %>%
  arrange(Indikator) %>%
  dplyr::select(Indikator, StartTatort, StopTatort, Rusningstimmar = rusning, Lagtrafiktimmar = ejrusning) %>%
  mutate(Rusningstimmar = round(Rusningstimmar, 2), 
         Lagtrafiktimmar = round(Lagtrafiktimmar, 2))
  
# print(fin6, n = Inf)

write.csv2(fin6, paste0("00_skript/data/output/Resultat_", substr(Sys.time(), 1, 10), ".csv"), row.names = F)
}
