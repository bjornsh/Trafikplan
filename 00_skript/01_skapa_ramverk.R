library(dplyr)
library(readxl)

rm(list = ls())
invisible(gc())

`%notin%` <- Negate(`%in%`)


# Indikatorplan
plan = read_excel("Z:/aa_utredning/Trafikplan/analysplan.xlsx", sheet = "data")

# alla hpl med tätort info
dat = read.csv2("01_input_data/HplData_2020-05-14.csv", stringsAsFactors=FALSE)

# ladda df med en hpl per tätort
hpl = read.csv("01_input_data/TatortMedEnHpl.csv", sep = ";", 
               stringsAsFactors=FALSE, encoding = "UTF-8")

colnames(hpl) = c("TatortNamn", "HplNamn", "HplID")


# filtrera indikatorer
plan1 = plan %>% filter((!is.na(StartTatort) | !is.na(StopTatort)) & 
                          (Indikator == "1.1" | 
                          Indikator == "1.2" | 
                          Indikator == "2.1" | 
                          Indikator == "2.2" | 
                          Indikator == "3.1" | 
                          Indikator == "3.2" | 
                          Indikator == "4.1"))  

# lägg till HplID för tätorter där vi använder bara en eller två hpl (för att effektivisera API körning) 
plan2 = plan1 %>%
  left_join(., hpl[,c("TatortNamn", "HplID")], by = c("StartTatort" = "TatortNamn")) %>%
  rename(StartHplID = HplID) %>%
  left_join(., hpl[,c("TatortNamn", "HplID")], by = c("StopTatort" = "TatortNamn")) %>%
  rename(StopHplID = HplID)

# lägg till HplID för tätorter där alla hpl inom tätort är med
plan3 = plan2 %>%
  left_join(., dat[,c("TatortNamn", "HplID")], by =c("StartTatort" = "TatortNamn")) %>%
  rename(StartHplID2 = HplID) %>%
  left_join(., dat[,c("TatortNamn", "HplID")], by =c("StopTatort" = "TatortNamn")) %>%
  rename(StopHplID2 = HplID)

# ta bort hållplatser som krashar skriptet
exclude = c("191174", "780562")

# om det finns HplID från join med EN hpl per tätort, ta bort rader med alla andra hpl i samma tätort
plan4 = plan3 %>% 
  mutate(StartHplID = ifelse(is.na(StartHplID), StartHplID2, StartHplID),
                 StopHplID = ifelse(is.na(StopHplID), StopHplID2, StopHplID)) %>%
  dplyr::select(-StartHplID2, -StopHplID2) %>%
  distinct() %>% # ta bort duplikater
  filter(StartHplID %notin% exclude & StopHplID %notin% exclude) %>%
  as.data.frame()

write.csv2(plan4, "01_input_data/AnalysPlanInputForAPI.csv", row.names = F)

