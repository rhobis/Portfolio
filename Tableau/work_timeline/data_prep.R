### Load libraries -------------------------------------------------------------
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(googlesheets4)



### Get data from Google Docs --------------------------------------------------
sheet_id <- '1XEraqkr0ajmwfFLczyzAqL4WFKPNM_D6ETfPGYws3LY'
dati <- 
    read_sheet(sheet_id) %>% 
    mutate(client = str_detect(note, 'cliente'),
           client = ifelse(client, 'yes', 'no'),
           client = ifelse(is.na(client), 'no', 'yes'),
           sciopero = ifelse(str_starts(sciopero, 's'), 'yes', 'no')
           ) 
# View(dati)



### Prepare data to make polygons in Tableau -----------------------------------
dati1 <- 
    dati %>% 
    select(-note,-arrivo_casa) %>%
    mutate( path = 1:nrow(dati)) %>% 
    pivot_longer(partenza_casa:uscita_lavoro,
                 names_to = 'evento',
                 values_to = 'ora') %>%
    filter(!is.na(ora)) %>%
    mutate(
        connection = sapply(evento, function(x){ switch(x,
                                                        partenza_casa = 1,
                                                        arrivo_lavoro = 2,
                                                        inizio_pranzo = 3,
                                                        fine_pranzo   = 4,
                                                        uscita_lavoro = 5)
        } 
        )
    )


dati2 <-
    dati %>% 
    select(-note,-partenza_casa) %>%
    mutate( path = (2*nrow(dati)):(nrow(dati)+1)) %>%
    pivot_longer(arrivo_lavoro:arrivo_casa,
                 names_to = 'evento',
                 values_to = 'ora') %>%
    filter(!is.na(ora)) %>%
    mutate(
        connection = sapply(evento, function(x){ switch(x,
                                                        arrivo_lavoro = 1,
                                                        inizio_pranzo = 2,
                                                        fine_pranzo   = 3,
                                                        uscita_lavoro = 4,
                                                        arrivo_casa   = 5)
        } 
        )
    )




out <- 
    rbind(dati1, dati2) %>%
    pivot_longer(durata_lavoro:durata_spostamenti, 
                 names_to='evento_aggr',
                 values_to='Event_duration') %>%
    mutate(Event_duration = Event_duration*24) #change duration to no. of hours




### Export final data ----------------------------------------------------------

write_xlsx(out, 'work_timeline_polygons.xlsx')





