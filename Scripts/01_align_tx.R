## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  align FY20 HFR data
## DATE:     2020-05-05
## UPDATED:  2020-05-29


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)
library(Wavelength)



# GLOBAL VARIABLES --------------------------------------------------------

datain <- "Data"
dataout <- "Dataout"


# IMPORT ------------------------------------------------------------------

  #Periods 2020.01-2020.07
    #https://drive.google.com/open?id=1Ykz8nVBwRLRbiAqOoDM57XxSDcT4EFdZ
    path_sql <- here(datain, "HFR_2020.07_TX_ 20200528.zip")

    df_tx <- hfr_read(path_sql)
    

# CLEAN -------------------------------------------------------------------

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

# EXPLORE -----------------------------------------------------------------

    #site count
      df_site_cnt_mer <- df_tx %>% 
        filter(mer_targets > 0) %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "sites_mer")
      
      df_tx %>% 
        filter(hfr_results > 0) %>% 
        distinct(operatingunit, orgunituid, hfr_pd) %>% 
        count(operatingunit, hfr_pd, name = "sites_hfr") %>% 
        complete(hfr_pd, nesting(operatingunit), fill = list(n = 0)) %>% 
        full_join(df_site_cnt_mer) %>% 
        arrange(operatingunit, hfr_pd) %>% 
        mutate(sites_hfr = ifelse(is.na(sites_hfr), 0, sites_hfr),
               completeness = sites_hfr / sites_mer)
      
      df_tx_ou <- df_tx %>% 
        group_by(operatingunit, fy, hfr_pd) %>% 
        summarise_at(vars(hfr_results, mer_results, mer_targets), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate_all(~ na_if(., -Inf))
        
      df_tx_ou  %>% 
        ggplot(aes(hfr_pd, hfr_results)) +
        geom_hline(aes(yintercept = mer_targets)) +
        geom_col() +
        facet_wrap(~operatingunit, scales = "free_y")
      
      
      df_tx_mech <- df_tx %>% 
        group_by(operatingunit, fy, hfr_pd, mech_code) %>% 
        summarise_at(vars(hfr_results, mer_results, mer_targets), sum, na.rm = TRUE) %>% 
        ungroup() %>% 
        mutate_all(~ na_if(., -Inf)) %>% 
        filter_at(vars(hfr_results, mer_results, mer_targets), any_vars(!is.na(.) & . != 0))
      
      df_tx_mech  %>% 
        ggplot(aes(hfr_pd, hfr_results)) +
        geom_hline(aes(yintercept = mer_targets)) +
        geom_col() +
        facet_wrap(~operatingunit + mech_code, scales = "free_y",  
                   labeller = label_wrap_gen(multi_line=FALSE))
      