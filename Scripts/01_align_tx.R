## PROJECT:  whats trending
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  align FY20 HFR data
## NOTE:     migrated over from pump_up_the_jam
## DATE:     2020-05-05
## UPDATED:  2021-05-04


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)
library(Wavelength)


# GLOBAL VARIABLES --------------------------------------------------------

datain <- "Data"
dataout <- "Dataout"

# IMPORT ------------------------------------------------------------------

  #function
    import_sqlview <- function(file){
      df <- file %>% 
        hfr_read() %>% 
        filter(indicator %in% c("TX_CURR", "TX_MMD"))
      
      # if(str_detect(file, "456"))
      #   df <- filter(df, hfr_pd != 6)
      
      return(df)
      
    }
  
  #data created in 01_align_tx
    df_tx <- import_sqlview("../Wavelength/out/joint/HFR_Tableau_SQLview.csv")

    

# CLEAN -------------------------------------------------------------------
    
  #drop where all missing
    df_tx <- df_tx %>% 
      filter_at(vars(mer_targets, mer_results, val), dplyr::any_vars(!is.na(.)))
    
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
               indicator, expect_reporting) %>% 
      summarise_at(vars(mer_targets, mer_results, hfr_results = val), sum, na.rm = TRUE) %>%
      ungroup()
    
  #aggregate MER data
    df_tx_mer <- df_tx %>%
      select(-hfr_results) %>% 
      group_by(operatingunit, countryname, snu1, psnu, orgunit, orgunituid,
               fy, hfr_pd, date,
               mech_code, mech_name, primepartner,
               indicator, expect_reporting) %>% 
      summarise_at(vars(mer_targets, mer_results), max, na.rm = TRUE) %>% 
      ungroup() 
    
  #spread hfr variables (prep to then take last obs) and drop any blank rows
    df_tx_hfr <- df_tx %>%
      select(-c(mer_targets, mer_results)) %>% 
      spread(indicator, hfr_results) #%>%
      # filter_at(vars(starts_with("TX")), any_vars(!is.na(.) & .!= 0)) 
  
  #replace NA with zero for any mmd reported row 
  # (needed for the fill later so an NA does not take on the last reported share that may exist)
    df_tx_hfr <- df_tx_hfr %>% 
      rowwise() %>% 
      mutate(mmd_reported = sum(TX_MMD.u3, TX_MMD.35, TX_MMD.o6, na.rm = TRUE) > 0,
             txcurr_reported = case_when(is.na(TX_CURR) | TX_CURR == 0 ~ FALSE,
                                         TX_CURR > 0 ~ TRUE)) %>% 
      ungroup() %>% 
      mutate(across(starts_with("TX_MMD"), ~ ifelse(mmd_reported == TRUE & is.na(.), 0, .))) 
    
  #create TX_CURR if there are MMD values and no TX_CURR
    df_tx_hfr <- df_tx_hfr %>% 
      rowwise() %>% 
      mutate(TX_CURR = ifelse(txcurr_reported == FALSE & mmd_reported == TRUE, 
                              sum(TX_MMD.u3, TX_MMD.35, TX_MMD.o6, na.rm = TRUE), TX_CURR)) %>% 
      ungroup() 
    
  #create MMD shares (to fill down where TX_CURR reported but no MMD from prior weeks in period )
    df_tx_hfr <- df_tx_hfr %>% 
      rowwise() %>% 
      mutate(across(starts_with("TX_MMD"), list(share = ~.x / TX_CURR))) %>% 
             # across(starts_with("TX_MMD"), ~ ifelse(is.na(.) | is.infinite(.) | is.nan(.), NA, .)))
      ungroup()
    
  #fill shares (fill shares for MMD to calc where missing)
    df_tx_hfr <- df_tx_hfr %>% 
      arrange(operatingunit, orgunituid, mech_code, date) %>% 
      group_by(operatingunit, orgunituid, mech_code, hfr_pd) %>% 
      fill(ends_with("share")) %>% 
      ungroup() #%>% 
      # mutate(across(ends_with("share"), ~ ifelse(TX_CURR %in% c(0, NA), NA, .))) 
    
  #create mmd values where TX_CURR reported and MMD is not
    df_tx_hfr <- df_tx_hfr %>% 
      mutate(TX_MMD.u3 = ifelse(TX_CURR > 0 & mmd_reported == FALSE, round(TX_MMD.u3_share * TX_CURR), TX_MMD.u3),
             TX_MMD.35 = ifelse(TX_CURR > 0 & mmd_reported == FALSE, round(TX_MMD.35_share * TX_CURR), TX_MMD.35),
             TX_MMD.o6 = ifelse(TX_CURR > 0 & mmd_reported == FALSE, round(TX_MMD.o6_share * TX_CURR), TX_MMD.o6),
      ) %>% 
      select(-ends_with("reported"), -ends_with("share"))

  #filter for only the last available date for each pd x orgunit x mech
    df_tx_hfr <- df_tx_hfr %>% 
      group_by(operatingunit, countryname, snu1, psnu, orgunit, orgunituid,
               fy, hfr_pd, date,
               mech_code, mech_name, primepartner) %>% 
      filter(date == max(date)) %>% 
      ungroup() 
    
  #reshape and remove date (only one obs per pd now)
    df_tx_hfr <- df_tx_hfr %>% 
      gather(indicator, hfr_results, starts_with("TX")) 
      # gather(indicator, hfr_results, starts_with("TX"), na.rm = TRUE) 
    
  #merge back together & drop temp data fames
    df_tx <- full_join(df_tx_hfr, df_tx_mer) 
    rm(df_tx_hfr, df_tx_mer)
  
  #replace //N with NA
    df_tx <- df_tx %>% 
      mutate(across(where(is.character), na_if, "\\N")) %>% 
      mutate(across(where(is.character), na_if, "NULL"))
    
  #filter cols with no mer or hfr values 
    # df_tx <- df_tx %>% 
    #   filter_at(vars(starts_with("mer"), "hfr_results"), any_vars(!is.na(.) & .!= 0))
    

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

  write_csv(df_tx, here(dataout, "HFR_FY20-21_TXCURR.csv"), na = "")    
