library(tidyverse)
library(haven)
library(vetr)
library(here)

baseccwcohort <- read_sas(here("Data","baseccwcohort.sas7bdat"))


###### CLONING #####
# Next, let's clone individuals into four different data sets representing four potential treatment regimens:
# Treatment 0 to 30 days after MI (trt0_30)
# Treatment 0 to 90 days after MI (trt0_90)
# Treatment 30 to 90 days after MI (trt30_90)
# No treatment after MI (notrt), separate from the main regimens in the manuscript

# (MM): RX_start is the day of treatment initiation after MI. Null for those who never start treatment. Why is calculated from Discharge_date?


trt0_30 <- baseccwcohort %>%
  mutate(t0censnotrt = case_when(RX_start == 0 ~ 1, 
                                 TRUE ~ 0),
         t0cens30_90 = case_when(RX_start == 0 ~ 1, 
                                 TRUE ~ 0))

trt0_90 <- trt0_30
trt30_90 <- trt0_30
notrt <- trt0_30


# Let's do the trt0_30 cohort first. We need to censor everyone who doesn't start by day 30 (entro il giorno 30) at day 30*
# This is directly translated from the SAS code
trt_0_30_with_cens <- trt0_30 %>%
  # First, let's sort out the people with less than 30 days of followup. They should use their original values
  mutate(Cens_followup = case_when(followup < 31 ~ followup,                #(MM): followup is the total days of followup after MI 
                                   is.na(RX_start) | RX_start > 30 ~ 30,    #(MM): if they never start treatment (#2A) or start after day 30 (#4A), censor at day 30
                                   RX_start < 31 ~ followup                 #(MM): if they start treatment before day 31, use their original followup (#3A)
  ),
  Cens_outcome = case_when(followup < 31 ~ outcome,                  #(MM): if they have less than 31 days of followup, use their original outcome
                           is.na(RX_start) | RX_start > 30 ~ 0,      #(MM): if they never start treatment (#2A) or start after day 30 (#4A), set outcome to 0 (censored)
                           RX_start < 31 ~ outcome                   #(MM): if they start treatment before day 31, use their original outcome (#3A)
  ),
  Cens_nostart = case_when( # We also want to make it clear this is a separate censoring mechanism  (MM): need for weighting
    # if we decided to weight for it
    followup < 31 ~ 0,
    # This flag makes it clear they were censored for not starting
    is.na(RX_start) | RX_start > 30 ~ 1,                             #(MM): (#2A and #4A)
    RX_start < 31 ~ 0)                                               #(MM): (#3A)
  ) 


# (MM:) Now, let's do the trt0_90 cohort. We need to censor everyone who doesn't start by day 90 at day 90
trt_0_90_with_cens <- trt0_90 %>%
  mutate(Cens_followup = case_when(followup < 91 ~ followup,              #(MM): 
                                   is.na(RX_start) | RX_start > 90 ~ 90,  #(MM): (#2B)
                                   RX_start < 91 ~ followup               #(MM): (#3B and #4B)
  ),
  Cens_outcome = case_when(followup < 91 ~ outcome,                       #(MM):
                           is.na(RX_start) | RX_start > 90 ~ 0,           #(MM): (#2B) 
                           RX_start < 91 ~ outcome                        #(MM): (#3B and #4B)
  ),
  Cens_nostart = case_when( 
    followup < 91 ~ 0,                                                    #(MM): 
    is.na(RX_start) | RX_start > 90 ~ 1,                                  #(MM): (#2B)  
    RX_start < 91 ~ 0)                                                    #(MM): (#3B and #4B)
  ) 


# (MM:) per essere sicuri di non aver lasciato qualche opzione non valutata nei precedenti case_when forse metterei un TRUE ~ '99999999' alla fine di ogni case_when



trt_30_90_with_cens <- trt30_90 %>%
  mutate(Cens_followup = case_when( # First, let's deal with censoring the people who start within the first 30 days.
                                   RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ RX_start,
                                   RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ followup,
                                   
                                   # Next, we need to deal with the people who have less than 91 days of followup. 
                                   # Their followup should be unchanged and they should not get a flag for either censoring reason
                                   followup < 91 ~ followup,
                                   
                                   # Next, let's deal with people who never start or start after day 90. 
                                   # Their censoring date should be day 90, and their reason should be not starting
                                   is.na(RX_start) | RX_start > 90 ~ 90,
                                   
                                   # Now, we need to create the follow-up for people who started an RX within the window.
                                   TRUE ~ followup
                                   ),
    Cens_outcome = case_when(RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ 0,
                             RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ outcome,
                             followup < 91 ~ outcome,
                             is.na(RX_start) | RX_start > 90 ~ 0,
                             TRUE ~ outcome),
    Cens_startearly = case_when(RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ 1,
                                RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ 0,
                                followup < 91 ~ 0,
                                is.na(RX_start) | RX_start > 90 ~ 0,
                                TRUE ~ 0),
    Cens_nostart = case_when(RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ 0,
                             RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ 0,
                             followup < 91 ~ 0,
                             is.na(RX_start) | RX_start > 90 ~ 1,
                             TRUE ~ 0)
    )


notrt_with_cens <- notrt %>%
  mutate(Cens_followup = case_when(is.na(RX_start) ~ followup,
                                   !is.na(RX_start) & followup >= RX_start ~ RX_start,
                                   TRUE ~ followup
                                   ),
    Cens_outcome = case_when(is.na(RX_start) ~ outcome,
                             !is.na(RX_start) & followup >= RX_start ~ 0,
                             TRUE ~ outcome
                             ),
    Cens_start = case_when(is.na(RX_start) ~ 0,
                           !is.na(RX_start) & followup >= RX_start ~ 1,
                           TRUE ~ 0
                           )
    )


trt_0_30_with_cens |> write_csv(here("R","trt_0_30_with_cens.csv"))
trt_0_90_with_cens |> write_csv(here("R","trt_0_90_with_cens.csv"))
trt_30_90_with_cens |> write_csv(here("R","trt_30_90_with_cens.csv"))
notrt_with_cens |> write_csv(here("R","notrt_with_cens.csv"))






