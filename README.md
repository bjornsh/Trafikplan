# Trafikplan

Jämför existerande utbud med TFP grundläggande utbudsnivåer https://www.regionuppsala.se/Global/UL/Dokument/Trafikfo%cc%88rso%cc%88rjningsprogram.web-11maj2017.pdf  


## Skript

**00_hpl_data_API.R**  

* Hämta hållplatskoordinater via ULs API  
* Skapa hållplats geometadata genom matchning med kommun och tätort shapefiler 

**01_skapa_ramverk.R**  

* Skapa en df med start- och stophållplatser enligt TFP ramverk som kan användas i nästa skript 

**02_ul_api.R**  

* Skicka frågor till UL reserobot och hämta resedata
* Hantera data och skapa resultatfil



## TODO
* lägg till skolor