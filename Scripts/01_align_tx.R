## PROJECT:  whats trending
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  align FY20 HFR data
## NOTE:     migrated over from pump_up_the_jam
## DATE:     2020-05-05
## UPDATED:  2020-06-06


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
  
  #filter cols with no mer or hfr values 
    df_tx <- df_tx %>% 
      filter_at(vars(starts_with("mer"), "hfr_results"), any_vars(!is.na(.) & .!= 0))
    

# FLAG IMPORTANT SITES ----------------------------------------------------

  #logic and code from pump_up_the_jam 02_datim_flags
    
  # thresholds for the targets and results cutoffs
    targets_thresh <- 0.80
    results_thresh <- 0.80
    
  # Below thresholds are to allow for inclusion of large sites in an ou w/ few overall sites
    site_thresh_targets <- 0.20
    site_thresh_results <- 0.20
    
  # aggregate to level of detail desired
  # flag sites with results but no targets, flag number of mechs per site
    df_flags <- df_tx %>%
      filter(hfr_pd == min(hfr_pd)) %>% 
      filter_at(vars(starts_with("mer")), any_vars(!is.na(.) & .!= 0)) %>% 
      group_by(orgunituid, mech_code, fy, indicator, operatingunit) %>%
      summarise_at(vars(starts_with("mer")), sum, na.rm = TRUE) %>%
      ungroup() %>%
      group_by(orgunituid, indicator) %>%
      add_tally(name = "mechs_per_site") %>%
      ungroup() %>%
      mutate(results_no_targets = if_else(mer_results >= 0 & mer_targets == 0, 1, 0)) %>%
      group_by(operatingunit, indicator) %>%
      mutate(
        site_targets_sh = mer_targets / sum(mer_targets, na.rm = TRUE),
        site_results_sh = mer_results / sum(mer_results, na.rm = TRUE),
        
        # Flagging sites that are more than 20% of total in case there are 2 or 3 sites (Angola)
        flag_targets_sh = if_else(site_targets_sh >= site_thresh_targets, 1, 0),
        flag_results_sh = if_else(site_results_sh >= site_thresh_results, 1, 0)
      ) %>%
      add_tally(name = "ou_total_sites") %>%
      ungroup() %>%
      arrange(operatingunit, indicator, orgunituid, mech_code)      
    
  # calculate site weights - need to know 1) site targets/results in aggregate
  # ou targets/results in aggregate to get shares --> importance weights
    df_flags_wgts <- df_flags %>%
      arrange(operatingunit, indicator, desc(site_targets_sh)) %>%
      group_by(operatingunit, indicator) %>%
      mutate(
        run_sum_targets = cumsum(site_targets_sh),
        lag_run_sum_targets = lag(run_sum_targets, n = 1, order_by = desc(site_targets_sh)),
        impflag_targets = case_when(
          run_sum_targets <= targets_thresh | lag_run_sum_targets <=  targets_thresh ~ 1,
          flag_targets_sh == 1 ~ 1, # grab Angola type site distributions here
          TRUE ~ 0,
        ),
        impflag_targets_count = sum(impflag_targets),
        impflag_targets_sh = (impflag_targets_count / ou_total_sites)
      ) %>%
      ungroup() %>%
      # Now with results
      arrange(operatingunit, indicator, desc(site_results_sh)) %>%
      group_by(operatingunit, indicator) %>%
      mutate(
        run_sum_results = cumsum(site_results_sh),
        lag_run_sum_results = lag(run_sum_results, n = 1, order_by = desc(site_results_sh)),
        impflag_results = case_when(
          run_sum_results <= results_thresh | lag_run_sum_results <= targets_thresh ~ 1,
          flag_results_sh == 1 ~ 1, # grab Angola type site distributions here
          TRUE ~ 0,
        ),
        impflag_results_count = sum(impflag_results),
        impflag_results_sh = (impflag_results_count / ou_total_sites)
      ) %>%
      ungroup() %>%
      # flag VIP sites
      mutate(impflag_both = if_else(impflag_results == 1 & impflag_targets == 1, 1, 0))
    
  #limit to just site flags for binding
    df_flags_wgts <- df_flags_wgts  %>% 
      select(orgunituid, mech_code, impflag_targets, impflag_results, impflag_both)
    
  #binding flag onto tx_curr data
    df_tx <- left_join(df_tx, df_flags_wgts, by = c("orgunituid", "mech_code"))
    
  #fill impflag_ NAs with zeros
    df_tx <- df_tx %>% 
      mutate_at(vars(starts_with("impflag")), ~ ifelse(is.na(.), 0, .))


# EXPORT DATA -------------------------------------------------------------

  write_csv(df_tx, here(dataout, "HFR_FY20_TXCURR.csv"), na = "")    
