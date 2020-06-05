## PROJECT:  whats trending
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  align FY20 HFR data
## NOTE:     migrated over from pump_up_the_jam
## DATE:     2020-05-05
## UPDATED:  2020-06-05


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)
library(Wavelength)



# GLOBAL VARIABLES --------------------------------------------------------

datain <- "Data"
dataout <- "Dataout"


# IMPORT ------------------------------------------------------------------

  #files downloaded from Google drive in 00_import_data

  #Periods 2020.01-2020.07
    path_sql <- here(datain, "HFR_2020.07_TX_ 20200528.zip")

    df_tx <- hfr_read(path_sql)
    
  #update for 2020.08
    path_08full <- here(datain, "HFR_2020.08_Tableau_20200604.zip")
    
    df_08full <- hfr_read(path_08full)
    

# CLEAN -------------------------------------------------------------------

  #filter full 2020.08 data to just vars of interest
    df_08full <- df_08full %>% 
      filter(indicator %in% c("TX_CURR", "TX_MMD"),
             hfr_pd == 8)
    
  #bind data to Tx dataset
    df_tx <- bind_rows(df_tx, df_08full)
    rm(df_08full)
    
  #remove SQL export row break
    df_tx <- df_tx %>% 
      mutate(primepartner = str_remove(primepartner, "\r$"))
  
  #adjust MMD disagg to be an full indicator
    df_tx <- df_tx %>% 
      mutate(indicator = case_when(indicator == "TX_MMD" & str_detect(otherdisaggregate, "3( |m)") ~ "TX_MMD.u3",
                                   indicator == "TX_MMD" & str_detect(otherdisaggregate, "3-5") ~ "TX_MMD.35",
                                   indicator == "TX_MMD" & str_detect(otherdisaggregate, "6") ~ "TX_MMD.o6",
                                   TRUE ~ indicator)) 
    
  # aggregate age/sex total by date (sum)
    df_tx <- df_tx %>%
      filter(indicator %in% c("TX_CURR", "TX_MMD.u3", "TX_MMD.35", "TX_MMD.o6")) %>% 
      group_by(operatingunit, countryname, snu1, psnu, orgunit, orgunituid,
               fy, hfr_pd, date,
               mech_code, mech_name, primepartner,
               indicator) %>% 
      summarise_at(vars(mer_targets, mer_results, hfr_results = val), sum, na.rm = TRUE) %>%
      ungroup()
    
  #aggregate up to hfr period (max)
    df_tx <- df_tx %>% 
      group_by(operatingunit, countryname, snu1, psnu, orgunit, orgunituid,
               fy, hfr_pd,
               mech_code, mech_name, primepartner,
               indicator) %>% 
      summarise_at(vars(mer_targets, mer_results, hfr_results), max, na.rm = TRUE) %>% 
      ungroup()
  
  #replace //N with NA
    df_tx <- df_tx %>% 
      mutate_all(~ na_if(., "\\N")) %>% 
      mutate_all(~ na_if(., "NULL"))
    
  #pull mechlist from DATIM to assign orphan UIDs to OUs
    df_mechlist <-  pull_mech()
    
  #rename mechlist names for merging
    df_mechlist <- df_mechlist %>% 
      select(mech_code, 
             operatingunit_d = operatingunit,
             mech_name_d = mech_name,
             primepartner_d = primepartner)
    
  #merge melist on to fix orphaned UIDs
    df_tx <- df_tx %>% 
      left_join(df_mechlist, by = "mech_code") 
    
  #replace orphaned UIDs so they have an OU home
    df_tx <- df_tx %>% 
      mutate(operatingunit = ifelse(is.na(operatingunit), operatingunit_d, operatingunit),
             mech_name = ifelse(is.na(mech_name), mech_name_d, mech_name),
             primepartner_d = ifelse(is.na(primepartner), primepartner_d, primepartner_d)) %>% 
      select(-ends_with("_d"))
    
    

# EXPORT DATA -------------------------------------------------------------

  write_csv(df_tx, here(dataout, "HFR_FY20_TXCURR.csv"), na = "")    
