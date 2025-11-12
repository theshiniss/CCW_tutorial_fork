
#getwd()
#setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(here)

trt_0_30_with_cens <- read.csv(here("Data","trt_0_30_with_cens.csv")) #(GP) upload del dataset creato allo step precedente: 
                                                                      #(GP) dataset che contiene dati time-to-event con un osservazione per individuo 
                                                                      ##### con tempi di fu censored, outcome e flag  


long_trt_0_30 <- trt_0_30_with_cens %>%
  # Transform Discharge_date to a proper Date object that allows arithmetic with days
  mutate(discharge_date = as.Date(Discharge_date, format = "%Y-%m-%d")) %>%
  
  # Generate two rows per individual with start_interval = 0 and 30   #(GP) Ogni paziente viene "sdoppiato" in due periodi di osservazione: uno parte il giorno 0, l'altro il 30imo
  crossing(start_interval = c(0, 30)) %>%                             #(GP) funzione di tidyr, permette di creare tutte le combinazioni possibili, restituisce un tibble
  
  # Calculate the new date for each interval
  mutate(date = discharge_date + start_interval) %>%
  
  # generate multiple rows for each individual based on intervals     #(GP) questa riga sembra ridondante, non fa nulla di più di crossing() ????
  unnest_longer(start_interval) %>%                                   #(GP) necessario se si ha una lista, ad esempio "start_interval = list(c(0, 30))"
  
  # Make sure nobody who has less than 30 days of follow-up gets a second observation  #(GP) rimuove la seconda osservazione (>30) per chi ha meno di 30gg di fu 
  # Be careful: add "start_interval == 30" at the end (not "start_interval == 0")
  filter(!(Cens_followup <= 30 & Cens_nostart == 0 & start_interval == 30)) %>%  #(GP) sta filtrando (eliminando) chi ha meno di 30 gg di fu AND
                                                                                 #(GP) NON è stato censurato per non-inizio (cioè HA iniziato o è morto <30)  AND
                                                                                 #(GP) riga duplicata nel crossing() 
  
  mutate(date = discharge_date + start_interval,  # Calculate the start date of the interval
    intv_age = as.integer(difftime(date, as.Date(birthdate, format = "%Y-%m-%d"), units = "days") / 365.25),  #(GP) var birthdate was not in Date format 
    
    long_outcome = case_when(                #(GP) variabile che indica se l'evento è avvenuto in quell'intervallo

      start_interval == 0 & Cens_followup > 30 ~ 0,              #(GP) riga 0-30, FU > 30 gg, il paziente ha superato il giorno 30, quindi nei primi 30 gg non è avvenuto l'evento
      start_interval == 0 & Cens_followup <= 30 ~ Cens_outcome,  #(GP) riga 0-30, FU ≤ 30 gg, il paziente ha terminato il fu entro 30gg, usa Cens_outcome per assegnare l'evento(1=evento, 0=censura)
      
      start_interval == 30 & Cens_nostart == 1 ~ 0,                       #(GP) riga 30-180, censurato per non-inizio, il paziente è stato censurato al giorno 30, quindi non osserviamo cosa succede dopo
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 0, #(GP) riga 30-180, FU > 180 gg, il paziente ha superato il giorno 180, quindi tra 30-180 non è avvenuto l'evento
      start_interval == 30 & Cens_nostart == 0 ~ Cens_outcome             #(GP) riga 30-180, FU ≤ 180 gg, il fu termina tra 30-180 quindi usa Cens_outcome per assegnare evento
      ),
    
    long_cens_nostart = case_when(          #(GP) indica se il paziente è stato censurato perché non ha iniziato il trattamento entro il giorno 30
      start_interval == 0 & Cens_followup > 30 ~ 0, #(GP) La censura per non-inizio avviene al giorno 30, quindi nella prima riga (0-30) non può ancora essere avvenuta
      start_interval == 0 & Cens_followup <= 30 ~ 0, # (GP) SAME
      
      start_interval == 30 & Cens_nostart == 1 ~ 1, # (GP) Questo è il flag per identificare chi viene censurato per non aver iniziato --> serve per calcolare i pesi IPCW
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 0, #(GP)  il paziente NON è stato censurato per non-inizio (ha iniziato o è morto prima) 
      start_interval == 30 & Cens_nostart == 0 & Cens_followup <= 180 ~ 0, #(GP) SAME
      ),
    
    interval_fu = case_when(               #(GP) durata del follow-up nell'intervallo
      start_interval == 0 & Cens_followup > 30 ~ 30,
      start_interval == 0 & Cens_followup <= 30 ~ Cens_followup,
      
      start_interval == 30 & Cens_nostart == 1 ~ 0,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 150,                 #(GP) sarebbe 180-30, durata intervallo
      start_interval == 30 & Cens_nostart == 0 & Cens_followup <= 180 ~ Cens_followup - 30  #(GP) 
      ),
    
    end_interval = case_when(
      start_interval == 0 & Cens_followup > 30 ~ 30,
      start_interval == 0 & Cens_followup <= 30 ~ Cens_followup,
      start_interval == 30 & Cens_nostart == 1 ~ 30,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 180,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup <= 180 ~ Cens_followup
      ),
    
  #(GP) flag per chi ha iniziato il trattamento "recentemente" (giorni 23-30) o chi è stato censurato al giorno 30  
    recentstart = case_when(# This is for convenience and to signify that these people's hypothetical treatment
                            # regimen would involve starting at day 30, rather than earlier
                            start_interval == 30  & Cens_nostart == 1 ~ 1,
                            # We need to create a "recent start" variable to ensure we only upweight those who started near 
                            # the end of the interval (e.g. within the last 7 days)
                            start_interval == 30  & Cens_nostart == 0 & RX_start >= 23 ~ 1,
                            start_interval == 30  & Cens_nostart == 0 & RX_start < 23 ~ 0)
    ) 


  


# Split this long data set into 3 different data sets. #(GP) Divisione del dataset in tre gruppi
first_30_days <- long_trt_0_30 %>% filter(start_interval == 0) #(GP) Primi 30 giorni: tutte le osservazioni nel periodo 0-30 giorni 

other_days <- long_trt_0_30 %>% filter(start_interval == 30 & recentstart == 1) #(GP) "Recent starters": chi ha iniziato tra i giorni 23-30 o chi è stato censurato al giorno 30

other_days_not_recent <- long_trt_0_30 %>%
  filter(!(start_interval == 0 | (start_interval == 30 & recentstart == 1))) #(GP) Non-recent: chi ha iniziato prima del giorno 23 (esclusi dall'analisi pesata)

# For the first_30_weights dataset, assign 1 to IPCW
first_30_weights <- first_30_days %>% mutate(Cumulative_IPCW = 1) #(GP) peso = 1 (non serve pesare, tutti sono "a rischio") --osservati nella finestra corretta e non clonati

# For the second data set, we need to fit a logistic regression model predicting the probability 
# of being uncensored at the start of the interval among those with recentstart=1 
# (i.e., the people who were censored at day 30 and the people who started treatment from day 23 to day 30.
    
model <- glm(
  long_cens_nostart ~ intv_age + as.numeric(sex == 1)  + renal,     #(GP) Calcolo degli IPCW: modello che predice la probabilità di essere censurato (non aver iniziato) al giorno 30 
  data = other_days,                                             
  family = binomial(link = "logit")
)

summary(model)

other_days_for_IPCW <- other_days %>%
  mutate(FU_uncens = 1 - predict(model, type = "response")) # prob of *uncensored* #(GP) applicato solo ai "recent starters" --secondo gruppo
                                                                                   #(GP)  FU_uncens = 1 - P(censored) è la probabilità di rimanere non censurato
summary(other_days_for_IPCW$FU_uncens) 
# sex:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  #GP valore max divesro 0.06656
# 0.03140 0.03855 0.04547 0.04466 0.04916 0.06652 

# as.numeric(sex==1): same

# Generate the IPCWs
other_days_weights <- bind_rows(other_days_for_IPCW, other_days_not_recent) %>%   #(GP) unisce "recent" e "non-recent"
  arrange(ID, start_interval)  %>% # sort by ID and start_interval
  mutate(
    Cumulative_IPCW = case_when(
      recentstart == 1 & long_cens_nostart == 0 ~ 1 / FU_uncens,  # Recently started and uncensored               #(GP) calcola i pesi IPCW
      recentstart == 0 & long_cens_nostart == 0 ~ 1,              # Didn't start recently, uncensored
      TRUE ~ 0                                                    
    )
  )



combined_data <- bind_rows(other_days_weights, first_30_weights) %>% #(GP) unisce ai pesi (1) assegnati sopra --riga 95
  arrange(ID, start_interval)  

# Non-zero weights:
wted_trt_0_30 <- combined_data %>%
  filter(Cumulative_IPCW != 0) 

# Zero weights
zeros_0_30 <- combined_data %>%
  filter(Cumulative_IPCW == 0)  

save(wted_trt_0_30, file = "Data/wted_trt_0_30.Rdata")
save(zeros_0_30, file = "Data/zeros_0_30.Rdata")

summary(wted_trt_0_30$Cumulative_IPCW)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.667   1.000  31.308  #(GP) valore max viene diverso (31.320)????

