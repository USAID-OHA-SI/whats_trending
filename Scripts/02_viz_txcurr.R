## PROJECT:  Pump up the jam
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  review and visualize TX_CURR HFR data
## DATE:     2020-05-13
## UPDATED:  2020-07-25


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(here)
library(Wavelength)
library(scales)
library(extrafont)
library(glitr)
library(patchwork)
library(RColorBrewer)
library(COVIDutilities)
library(rnaturalearth)
library(sf)


# GLOBAL VARIABLES --------------------------------------------------------

  datain <- "Data"
  dataout <- "Dataout"
  
  #hfr date
  hfr_date <- "[2020-07-13]"
  
  # paste((1:12), '=', viridis_pal(direction = -1)(12))
  heatmap_pal <- c("1"  = "#FDE725FF", "2"  = "#C2DF23FF", "3"  = "#85D54AFF",
                   "4"  = "#51C56AFF", "5"  = "#2BB07FFF", "6"  = "#1E9B8AFF", 
                   "7"  = "#25858EFF", "8"  = "#2D708EFF", "9"  = "#38598CFF", 
                   "10" = "#433E85FF", "11" = "#482173FF", "12" = "#440154FF")
  
  posneg_pal <- brewer.pal(3, "BrBG")

  # range <- c("low", "med",  "high")
  # bivar_pal <- c("#e8e8e8", "#5ac8c8", "#ace4e4", 
  #                "#be64ac", "#3b4994", "#8c62aa",
  #                "#dfb0d6", "#5698b9", "#a5add3")
  # 
  # bivar_options <- 
  #   tibble(pre = paste0("pre-", range), 
  #          post = paste0("post-", range)) %>% 
  #   complete(post, nesting(pre)) %>% 
  #   unite(position, c(pre, post), sep = ", ") %>%
  #   pull()
  
  # paste(bivar_options, "=", bivar_pal)
  bivar_map <- c("pre-high, post-high" = "#e8e8e8",
                 "pre-low, post-high" = "#5ac8c8",
                 "pre-med, post-high" = "#ace4e4",
                 "pre-high, post-low" = "#be64ac",
                 "pre-low, post-low" = "#3b4994",
                 "pre-med, post-low" = "#8c62aa",
                 "pre-high, post-med" = "#dfb0d6",
                 "pre-low, post-med" = "#5698b9",
                 "pre-med, post-med" = "#a5add3")
  
  pandemic_date <- who_pandemic() %>% pull(date)

# IMPORT ------------------------------------------------------------------

  #data created in 01_align_tx
    df_tx <- vroom(here(dataout, "HFR_FY20_TXCURR.csv"))

  #org hierarchy
    df_orgheirarchy <- list.files(datain, "org", full.names = TRUE) %>% 
      vroom()

  #covid
    df_covid <- pull_jhu_covid()
    
  #countyr names for mapping
    world <- ne_countries()
    
    
# CLEAN UP ----------------------------------------------------------------

  #clean up
    df_tx <- df_tx %>% 
      filter(!is.na(operatingunit)) %>% 
      mutate(operatingunit = recode(operatingunit,
                                    "Democratic Republic of the Congo" = "DRC",
                                    "Dominican Republic" = "DR",
                                    "Western Hemisphere Region" = "WHR")) 
    
  #covert mech_code to character
    df_tx <- df_tx %>% 
      mutate(mech_code = as.character(mech_code))
  
  #align dates with hfr_pds
    df_pds <- hfr_identify_pds(2020) %>% 
      group_by(hfr_pd) %>% 
      summarise(hfr_pd_date_min = min(date),
                hfr_pd_date_max = max(date)) %>% 
      ungroup() 
      # mutate(hfr_pd = (2020 + hfr_pd/100) %>% as.character)
    
  #remove mmd
    df_txcurr <- filter(df_tx, indicator == "TX_CURR")
    
  #create flags for whether site reported HFR and if site exists in DATIM
    df_txcurr <- df_txcurr %>% 
      mutate_at(vars(mer_results, mer_targets), ~ ifelse(is.na(.), 0, .)) %>% 
      mutate(has_hfr_reporting = hfr_results > 0 ,
             is_datim_site = mer_results > 0 | mer_targets > 0)
    
  #keep only DATIM sites
    df_txcurr <- df_txcurr %>% 
      filter(is_datim_site == TRUE)
    
  #extract mmd - max MER values to fill MMD pre spreading
    df_mmd <- df_tx %>% 
      group_by(orgunituid, mech_code, fy) %>% 
      mutate(mer_targets = max(mer_targets, na.rm = TRUE),
             mer_results = max(mer_results, na.rm = TRUE)) %>% 
      ungroup() %>% 
      spread(indicator, hfr_results) %>% 
      rename_all(tolower)
    
  #create a MMD Unknown indicator
    df_mmd <- df_mmd %>% 
      rowwise() %>% 
      mutate(tx_mmd.unkwn = tx_curr - sum(tx_mmd.u3, tx_mmd.35, tx_mmd.o6, na.rm = TRUE)) %>% 
      ungroup()
    
  #replace values where MMD > TX_CURR
    df_mmd <- df_mmd %>% 
      mutate(across(c(tx_mmd.u3, tx_mmd.35, tx_mmd.o6), ~ ifelse(tx_mmd.unkwn < 0, NA, .))) %>% 
      mutate(tx_mmd.unkwn = ifelse(tx_mmd.unkwn < 0, tx_curr, tx_mmd.unkwn))    
    
  #covid markers
    df_covid_case10 <- df_covid %>% 
      filter(tenth_case == 1) %>% 
      group_by(countryname) %>% 
      filter(date == min(date)) %>% 
      ungroup() %>% 
      select(countryname, date)
    
  #crosswalk for mapping
    df_crosswalk <- tibble(countryname_ne = world$admin, iso = world$iso_a3)
    
    rm(world)

# INTERPOLATE -------------------------------------------------------------

  #setup for interpolation: 
  #  replace 0's w/ NA; count reporting pds
    df_txcurr <- df_txcurr %>%
      mutate(hfr_results = na_if(hfr_results, 0)) %>% 
      group_by(mech_code, orgunituid) %>% 
      mutate(pds_reported = sum(!is.na(hfr_results))) %>% 
      ungroup()
    
  #id sites that need to be dropped (need min of 2pds for interpolation)
    df_nonipol_sites <- df_txcurr %>%
      filter(pds_reported < 2) %>% 
      mutate(hfr_results_ipol = hfr_results, 
             is_ipol = FALSE)
    
  #interpolate
    df_txcurr <- df_txcurr %>%
      filter(pds_reported >= 2) %>% 
      group_by(mech_code, orgunituid) %>% 
      mutate(hfr_results_ipol = approx(hfr_pd, hfr_results, hfr_pd)$y %>% round) %>% 
      ungroup() %>% 
      mutate(is_ipol = !is.na(hfr_results_ipol) & is.na(hfr_results))
    
  #apend non interpolated sites
    df_txcurr <- bind_rows(df_txcurr, df_nonipol_sites)
    
    
# CALCULATE COMPLETENESS --------------------------------------------------
    
    #aggregate to country
      df_comp <- df_txcurr %>% 
        group_by(hfr_pd, indicator, operatingunit) %>% 
        summarise_at(vars(has_hfr_reporting, is_datim_site, mer_targets), sum, na.rm = TRUE) %>% 
        ungroup()
    
    #calculate completeness
      df_comp <- df_comp %>% 
        mutate(completeness = has_hfr_reporting / is_datim_site,
               completeness = ifelse(is.nan(completeness) | is.infinite(completeness), NA, completeness),
               completeness_band = case_when(completeness < .05 ~ 1,
                                             completeness <= 1 ~ round(completeness/.1, 0),
                                             !is.na(completeness) ~ 12))

    #site count
      df_comp <- df_comp %>% 
        mutate(ou_sitecount = paste0(operatingunit, " (", comma(is_datim_site, 1), ")"))
      
    #filter
      df_comp <- df_comp %>% 
        filter_at(vars(has_hfr_reporting, is_datim_site, mer_targets), any_vars(. != 0 | is.na(.)))
    
    #clean up period
      df_comp <- df_comp %>% 
        left_join(df_pds) %>% 
        mutate(date_lab = paste0(format.Date(hfr_pd_date_max, "%b %d"), "\n(",
                                 str_pad(hfr_pd, 2, pad = "0"), ")"),
               date_lab = fct_reorder(date_lab, hfr_pd_date_max))
      
    #viz completeness
      viz_comp <- df_comp %>% 
        ggplot(aes(date_lab, fct_reorder(ou_sitecount, mer_targets, sum), fill = completeness_band)) +
        geom_tile(color = "white", size = 0.25) +
        geom_text(aes(label = percent(completeness, 1),
                      color = ifelse(completeness_band <= 1, "dark", "light")),
                  size = 2.5, na.rm = TRUE) +
        scale_fill_viridis_c(limits = c(1, 12), label = NULL, direction = -1, na.value = "gray80") +
        scale_color_manual(values = c("dark" = "gray30", "light" = "white"), guide = FALSE) +
        labs(subtitle = "HFR Site Completeness",
             y = NULL, x = NULL, color = "Site Type",
             caption ="Note: Completeness derived by comparing HFR reporting against sites with DATIM results/targets") +
        # theme_minimal() + 
        si_style_nolines() +
        theme(legend.position = "none")
      
    #viz mer targets
      viz_targ <- df_comp %>% 
        filter(hfr_pd == max(hfr_pd)) %>% 
        mutate(trgt_lab = case_when(mer_targets > 1000000 ~ paste0(round(mer_targets/1000000, 1), "m"),
                                    mer_targets > 10000 ~ paste0(round(mer_targets/1000, 0), "k"),
                                    mer_targets == 0 ~ "0",
                                    TRUE ~ paste0(round(mer_targets/1000, 1), "k"))) %>% 
        ggplot(aes(mer_targets, fct_reorder(operatingunit, mer_targets, sum))) +
        geom_blank(aes(mer_targets * 1.2)) +
        geom_col(fill = heatmap_pal[10]) +
        geom_text(aes(label = trgt_lab), family = "Source Sans Pro",
                  color = "gray50", hjust = -.2) +
        labs(subtitle = "MER Targets (USAID)",
             x = NULL, y = NULL,
             caption = paste("Source: MER [FY20Q2i] + HFR", hfr_date)) +
        scale_x_continuous(expand = c(0.005, 0.005)) +
        scale_y_discrete(expand = c(0.005, 0.005)) +
        si_style_nolines() +
        theme(axis.text.x=element_text(color = "white"))
      
    #combine viz
      viz_comp + viz_targ + 
        plot_annotation(title = "FY20 HFR TX_CURR COMPLETENESS AND MER TARGETS") &
        theme(plot.title = element_text(family = "Source Sans Pro", face = "bold"))
      
      ggsave("HFR_TX_Comp.png", path = "Images", width = 10, height = 5.625, dpi = 300)
  
      
      
# CONSISTENT REPORTING ----------------------------------------------------
      
    #identify the number of periods
      pds <- unique(df_txcurr$hfr_pd) %>% length()
      
    #identify which site x mechs had reporting every period 
      df_complete_orgunits <- df_txcurr %>% 
        filter(hfr_results > 0) %>% 
        group_by(orgunituid, mech_code) %>% 
        filter(n() == pds) %>% 
        ungroup() %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "complete_sites")
    
    #identify which site x mechs had reporting/interpoled data every period 
      df_complete_ipol_orgunits <- df_txcurr %>% 
        filter(hfr_results_ipol > 0) %>% 
        group_by(orgunituid, mech_code) %>% 
        filter(n() == pds) %>% 
        ungroup() %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "complete_sites_ipol")
      
    #get a share of sites reporting every pd
      df_complete_share <- df_txcurr %>% 
        distinct(operatingunit, orgunituid) %>% 
        count(operatingunit, name = "all_sites") %>% 
        full_join(df_complete_orgunits, by = "operatingunit") %>% 
        full_join(df_complete_ipol_orgunits, by = "operatingunit") %>% 
        mutate(complete_sites = ifelse(is.na(complete_sites), 0, complete_sites),
               complete_sites_ipol = ifelse(is.na(complete_sites_ipol), 0, complete_sites_ipol),
               share = complete_sites / all_sites,
               share_ipol = complete_sites_ipol / all_sites,
               ou_count = paste0(operatingunit, " (", comma(complete_sites, 1), "/", comma(all_sites, 1), ")"),
               ou_count_ipol = paste0(operatingunit, " (", comma(complete_sites_ipol, 1), "/", comma(all_sites, 1), ")"))
      
    #viz, completeness w/ original data
      df_complete_share %>% 
        ggplot(aes(share, fct_reorder(ou_count, share, sum))) +
        geom_col(fill = heatmap_pal[10], width = .8) +
        geom_vline(xintercept = seq(from = 0, to = 1, by = .1), color = "white") +
        geom_col(aes(x = 1), fill = NA, width = .8, color = heatmap_pal[10]) +
        geom_text(aes(label = percent(share, 1)),
                  hjust = -.1, family = "Source Sans Pro", color = "gray30") +
        labs(x = NULL, y = NULL,
             title = paste("SHARE OF SITES BY OU REPORTING IN ALL", pds, "PERIODS"),
             caption = paste("Source: HFR", hfr_date)) +
        scale_x_continuous(labels = percent, expand = c(0.005, 0.005)) +
        scale_y_discrete(expand = c(0.005, 0.005)) +
        si_style_nolines() +
        theme(axis.text.x = element_blank())
      
      ggsave("HFR_TX_SitesAllPds.png", path = "Images", width = 10, height = 5.625, dpi = 300)
      
    #viz, completeness w/ interpolation
      df_complete_share %>% 
        ggplot(aes(share, fct_reorder(ou_count_ipol, share, sum))) +
        geom_col(aes(share_ipol), fill = heatmap_pal[7], width = .8) +
        geom_col(fill = heatmap_pal[10], width = .8) +
        geom_vline(xintercept = seq(from = 0, to = 1, by = .1), color = "white") +
        geom_col(aes(x = 1), fill = NA, width = .8, color = heatmap_pal[10]) +
        geom_text(aes(share_ipol, label = percent(share_ipol, 1)),
                  hjust = -.1, family = "Source Sans Pro", color = "gray30") +
        labs(x = NULL, y = NULL,
             title = paste("SHARE OF SITES BY OU REPORTING IN ALL", pds, "PERIODS WHEN INTERPOLATING"),
             caption = paste("Source: HFR", hfr_date)) +
        scale_x_continuous(labels = percent, expand = c(0.005, 0.005)) +
        scale_y_discrete(expand = c(0.005, 0.005)) +
        si_style_nolines() +
        theme(axis.text.x = element_blank())

      ggsave("HFR_TX_SitesAllPds_Ipol.png", path = "Images", width = 10, height = 5.625, dpi = 300)

# MMD ---------------------------------------------------------------------

    #aggrgate mmd to OU level
      df_mmd_ctry <- df_mmd %>%
        group_by(operatingunit, hfr_pd) %>% 
        summarise_at(vars(starts_with("mer"), starts_with("tx")), sum, na.rm = TRUE) %>% 
        ungroup() 
      
    #gather mmd to create shares for +/< 3mo  
      df_mmd_ctry <- df_mmd_ctry %>% 
        gather(indicator, value, starts_with("tx_mmd")) %>% 
        mutate(indicator = ifelse(indicator %in% c("tx_mmd.35", "tx_mmd.o6"), "tx_mmd.o3", indicator)) %>% 
        group_by(operatingunit, indicator, hfr_pd, tx_curr, mer_targets) %>% 
        summarise(value = sum(value, na.rm = TRUE)) %>% 
        group_by(operatingunit, hfr_pd) %>% 
        mutate(share = value / sum(value)) %>% 
        ungroup() %>% 
        mutate(operatingunit = fct_reorder(operatingunit, mer_targets, .desc = TRUE),
               indicator = factor(indicator, c("tx_mmd.unkwn", "tx_mmd.o3", "tx_mmd.u3")))
      
    #add in dates
      df_mmd_ctry <-  left_join(df_mmd_ctry, df_pds)
    
    #get OUs ordering by TX_CURR targets
      ctry_tx_targets <- df_tx %>% 
        filter(hfr_pd == max(hfr_pd)) %>% 
        count(operatingunit, wt = mer_targets, sort = TRUE) %>% 
        pull(operatingunit)
      
      
    #align names
      df_covid_case10 <- df_covid_case10 %>% 
        mutate(countryname = recode(countryname,
                                    "Congo (Kinshasa)" = "Democratic Republic of the Congo")) 
        
    #align and aggregate to OU level
      df_covid_case10_ctry <-  df_tx %>% 
        distinct(operatingunit, countryname) %>% 
        left_join(df_covid_case10) %>%
        group_by(operatingunit) %>% 
        summarise(date = min(date, na.rm = TRUE))
      
    #filter covid countries to PEPFAR ones and align factor
      df_covid_case10_ctry <- df_covid_case10_ctry %>% 
        filter(operatingunit %in% unique(df_mmd_ctry$operatingunit)) %>% 
        mutate(operatingunit = factor(operatingunit, ctry_tx_targets))
      

      df_mmd_ctry %>% 
        filter(indicator != "tx_mmd.unkwn",
               !is.na(operatingunit),
               !is.nan(share)) %>% 
        ggplot(aes(hfr_pd_date_max, share)) +
        geom_vline(data = df_covid_case10_ctry, aes(xintercept = date), 
                   color = "gray70", size = 1.5) +
        geom_path(aes(group = indicator, color = indicator), size = .9) +
        geom_point(aes(group = indicator, color = indicator)) +
        facet_wrap(~ operatingunit) +
        scale_y_continuous(label = percent, breaks = c(0,.5,1)) +
        scale_x_date(date_breaks = "2 month", date_labels = "%b") +
        scale_color_manual(values = c(heatmap_pal[10], "gray80"),
                           labels = c("+3 months","<3 months")) +
        si_style_ygrid() +
        labs(x = NULL, y = NULL,
             title = "SOME SHIFTS TOWARDS INCREASED MMD IN THE FACE OF COVID-19",
             subtitle = "countries ordered by TX_CURR targets",
             caption = paste("Source: HFR", hfr_date)) + 
        theme(#legend.position = "none",
              legend.title = element_blank(),
              axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 9),
              panel.spacing = unit(1, "lines"))
      
      ggsave("HFR_MMD.png", path = "Images", width = 12, height = 7, dpi = 400)  
      
      
# GROWTH TRENDS FOR CONSISTENT SITES --------------------------------------
      
    #filter to where reporting is greater than 0 and for all pds
      df_txcurr_comp <- df_txcurr %>% 
        filter(hfr_results_ipol > 0) %>% 
        group_by(orgunituid, mech_code) %>% 
        filter(n() == pds) %>% 
        ungroup()
      
    #aggregate to OU level and create growth metric
      df_txcurr_comp <- df_txcurr_comp %>% 
        group_by(operatingunit, hfr_pd) %>% 
        summarise(hfr_results_ipol  = sum(hfr_results_ipol, na.rm = TRUE), 
                  mer_targets = sum(mer_targets, na.rm = TRUE), 
                  n = n()
        ) %>% 
        ungroup() %>% 
        group_by(operatingunit) %>% 
        mutate(growth = (hfr_results_ipol - lag(hfr_results_ipol, order_by = hfr_pd)) / lag(hfr_results_ipol, order_by = hfr_pd)) %>% 
        ungroup() %>% 
        filter(hfr_pd != 1) %>% 
        mutate(ou_count = paste0(operatingunit, " (", comma(n, 1), ")"))
      
    #clean up period
      df_txcurr_comp <- df_txcurr_comp %>% 
        left_join(df_pds, by = "hfr_pd") %>% 
        mutate(date_lab = paste0(format.Date(hfr_pd_date_max, "%b %d"), "\n(",
                                 str_pad(hfr_pd, 2, pad = "0"), ")"),
               date_lab = fct_reorder(date_lab, hfr_pd_date_max))
    
    #viz
      df_txcurr_comp %>% 
        ggplot(aes(hfr_pd_date_max, growth, group = ou_count)) +
        geom_col(aes(fill = growth > 0)) +
        geom_hline(yintercept = 0, color = "gray40") +
        facet_wrap(~ fct_reorder(ou_count, mer_targets, sum, .desc = TRUE), scales = "free_y") +
        scale_y_continuous(labels = percent_format(.1)) +
        scale_x_date(date_breaks = "8 weeks", date_labels = "%b %d") +
        scale_fill_manual(values = c(heatmap_pal[6], "#433E85FF")) +
        # scale_fill_manual(values = c(posneg_pal[1], posneg_pal[3])) +
        labs(x = NULL, y = NULL,
             title = "TX_CURR GROWTH",
             subtitle =  "only sites that report every period (using interpolated data)",
             caption = paste("Source: HFR", hfr_date)) +
        si_style_ygrid() +
        theme(strip.text = element_text(face = "bold"),
              axis.text.x = element_text(size = 8),
              panel.spacing = unit(2, "lines"),
              legend.position = "none")
      
      
      # ggsave("HFR_TX_Growth_SitesAllPds.png", path = "Images", width = 10, height = 5.625, dpi = 300)
  

# OU TRENDS ---------------------------------------------------------------

    #aggregate to ou level
      df_trends_ctry <-  df_txcurr %>% 
        group_by(countryname, indicator, hfr_pd) %>% 
        summarise_at(vars(mer_targets, mer_results, hfr_results, hfr_results_ipol, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
        ungroup() %>%
        mutate_at(vars(hfr_results, hfr_results_ipol, mer_targets, mer_results), ~ na_if(., 0)) %>% 
        arrange(countryname,hfr_pd)
      
    #calculate completeness
      df_trends_ctry <- df_trends_ctry %>% 
        mutate(completeness = has_hfr_reporting / is_datim_site,
               completeness = ifelse(is.nan(completeness) | is.infinite(completeness), NA, completeness),
               completeness_band = case_when(completeness < .05 ~ 1,
                                             completeness <= 1 ~ round(completeness/.1, 0),
                                             !is.na(completeness) ~ 12) #%>% as.character
        )
    #clean up pd
      df_trends_ctry <- df_trends_ctry %>% 
        mutate(hfr_pd = as.integer(hfr_pd)) %>% 
        left_join(df_pds, by = "hfr_pd")
      
    #setup range
      df_trends_ctry <- df_trends_ctry %>% 
        group_by(countryname) %>% 
        mutate(max_range = max(hfr_results, mer_results, 50/1.2, na.rm = TRUE) * 1.2) %>% 
        ungroup()
      
      
    viz_trends <- function(ctry_sel) {
      
      ctry_case10 <- df_covid_case10 %>% 
        filter(countryname == ctry_sel) %>% 
        pull()
      
      v <- df_trends_ctry %>% 
        filter(countryname == ctry_sel) %>% 
        ggplot(aes(hfr_pd_date_max, hfr_results)) +
        geom_blank(aes(y = max_range), na.rm = TRUE) +
        geom_vline(xintercept = pandemic_date, color = "gray80", size = 1.5, na.rm = TRUE) +
        geom_vline(xintercept = ctry_case10, color = "gray70", size = 1.5, na.rm = TRUE) +
        geom_hline(aes(yintercept = 0), color = "gray30", na.rm = TRUE) +
        geom_area(color = heatmap_pal[9], fill = heatmap_pal[9], size = 1, alpha = .2, na.rm = TRUE) +
        geom_label(aes(y = 0, label = percent(completeness, 1)), vjust = -.6,
                   label.size = 0, size = 3,
                   family = "Source Sans Pro", color = heatmap_pal[9], na.rm = TRUE) +
        geom_text(aes(x = pandemic_date -1, y = max(max_range) *.9), 
                  label = "WHO declared emergency", hjust = 1.1, na.rm = TRUE,
                  family = "Source Sans Pro Light", size = 2, color = "gray70") +
        geom_hline(aes(yintercept = mer_results), linetype = "dashed", color = "gray20", na.rm = TRUE) +
        scale_y_continuous(label = comma, expand = c(-0, 0)) +
        labs(x = NULL, y = NULL, color = "Completeness (0% - 100%)",
             title = "Current on Treatment Trends Pre/Post COVID",
             subtitle = "site completeness indicated at base",
             caption = "dotted line identifies FY20Q1 reported value") +
        si_style_ygrid() +
        theme(plot.title = element_text(size = 11, color = "gray30"),
              plot.subtitle = element_text(size = 7),
              plot.caption = element_text(size = 5),
              axis.text = element_text(size = 7),
              axis.title = element_text(size = 7),
              strip.text = element_text(size = 7, hjust = .50))
      
      if(length(ctry_case10) > 0){
       v <-  v + 
          geom_text(aes(x = ctry_case10, y = max(max_range) *.9),
                    label = "10th case", hjust = -.2, na.rm = TRUE,
                    family = "Source Sans Pro Light", size = 2, color = "gray70")
      }
      
      return(v)
      
    }
     
    
# SITE COUNT PER PERIOD ---------------------------------------------------
    
    
    df_rpt_sites <- df_txcurr %>% 
      mutate(site_type = ifelse(impflag_targets == 1, "Large", "Small")) %>% 
      group_by(countryname, hfr_pd, site_type) %>% 
      summarise_at(vars(mer_targets, has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(no_reporting = has_hfr_reporting- is_datim_site,
             share_reporting = has_hfr_reporting/is_datim_site,
             share_noreporting = share_reporting-1,
             type_sitecount = paste0(site_type, " (", comma(is_datim_site, 1), ")"),
             ou_sitecount = paste0(countryname, " (", comma(is_datim_site, 1), ")")) %>% 
      left_join(df_pds) %>% 
      mutate(date_lab = paste0(format.Date(hfr_pd_date_max, "%b %d"), "\n(",
                               str_pad(hfr_pd, 2, pad = "0"), ")"),
             date_lab = fct_reorder(date_lab, hfr_pd_date_max))
    
    
    viz_rpt_rates <- function(ctry_sel) {
      df_rpt_sites %>% 
        filter(countryname == ctry_sel) %>% 
        ggplot(aes(hfr_pd_date_max, share_reporting)) +
        # geom_vline(xintercept = pandemic_date, color = "gray80", size = 2, na.rm = TRUE) +
        # geom_vline(xintercept = df_covid_case10 %>% filter(countryname == ctry_sel) %>% pull(), 
        #            color = "gray70", size = 2, na.rm = TRUE) +
        geom_col(aes(y = 1), fill = grey10k, alpha = 0.75) +
        geom_col(aes(fill = ifelse(hfr_pd_date_max > pandemic_date, "#b1c7b3", "#d8e3d8"))) +
        geom_errorbar(aes(ymin = share_reporting, ymax = share_reporting), size=0.5, 
                      # width = 0.8,
                      colour = grey50k) +
        geom_hline(yintercept = 0) +
        geom_text(aes(y = 1.15, label = comma(has_hfr_reporting, 1)), color = "gray30",
                  size = 3, family = "Source Sans Pro") +
        expand_limits(y = 1.3) +
        facet_grid(type_sitecount ~.) +
        # facet_grid(type_sitecount ~., switch = "y") +
        scale_fill_identity() +
        scale_y_continuous(label = percent) +
        scale_x_date(date_breaks = "4 weeks", date_labels = "%b %d") +
        labs(x = NULL, y = "share of sites reporting",
             title = "Sites Reporting Each Period",
             subtitle = "large sites = contributing 80% of all targets",
             caption = "Completeness derived by comparing HFR reporting against sites with DATIM results/targets"
        ) +
        si_style_nolines() +
        theme(#strip.placement = "outside",
          plot.title = element_text(size = 11, color = "gray30"),
          plot.subtitle = element_text(size = 7),
          plot.caption = element_text(size = 5),
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          strip.text = element_text(size = 7, hjust = .50) #, face = "bold")
        )  
    }
    
    

# GROWTH CHANGE -----------------------------------------------------------

    pds <- df_txcurr %>% 
        distinct(hfr_pd) %>% 
        filter(hfr_pd > 3) %>% 
        pull()
      
    df_growth <- df_txcurr %>% 
        filter(hfr_pd %in% pds,
               hfr_results_ipol > 0) %>% 
        group_by(orgunituid, mech_code) %>% 
        filter(n() == length(pds)) %>% 
      ungroup()
        
    df_growth_ou <- df_growth %>% 
      group_by(countryname, hfr_pd) %>% 
      summarize_at(vars(hfr_results_ipol, mer_targets, is_datim_site),sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      group_by(countryname) %>% 
      mutate(delta = (hfr_results_ipol/lag(hfr_results_ipol, order_by = hfr_pd)) -1) %>% 
      ungroup() %>% 
      mutate(ou_sitecount = paste0(countryname, " (", comma(is_datim_site, 1), ")")) %>% 
      filter(hfr_pd != 4)
    
    df_growth_ou <- df_growth_ou %>% 
      left_join(df_pds)
    
    viz_growth <- function(ctry_sel){
      
      site_cnt <- df_growth_ou %>% 
        filter(countryname == ctry_sel) 
      
      if(nrow(site_cnt) > 0) {
        
        site_cnt <- site_cnt%>%
          pull(is_datim_site) %>% 
          max()
        
        pds <- (min(df_growth_ou$hfr_pd) - 1) %>% 
          str_pad(2, pad = "0") %>% 
        paste0("2020.",., "-", max(df_growth_ou$hfr_pd))
        
        v <- df_growth_ou %>% 
          filter(countryname == ctry_sel) %>% 
          mutate(hfr_pd = str_pad(hfr_pd, 2, pad = "0"),
                 lab = paste0(format.Date(hfr_pd_date_max, "%b %d"), " (",
                              str_pad(hfr_pd, 2, pad = "0"), ")", 
                              ":   ", percent(delta, .1))) %>% 
          ggplot(aes(1, fct_rev(hfr_pd))) +
          geom_point(color = NA, na.rm = TRUE) +
          geom_text(aes(label =lab), size = 4, color = "gray30",
                    family = "Source Sans Pro") +
          si_style_nolines() +
          labs(x = NULL, y = NULL, 
               title = "Treatment Growth Each Period",
               subtitle = paste0("only sites reporting in all ", pds, " periods (",
                                 comma(site_cnt, 1),")"),
               caption = "interpolated data") +
          theme(axis.text.y = element_blank(),
                axis.text.x = element_blank(),
                plot.title = element_text(size = 11, color = "gray30"),
                plot.subtitle = element_text(size = 7),
                plot.caption = element_text(size = 5),
                strip.text = element_text(size = 7, hjust = .50))
      } else {
       v <- NULL
      }
      
      return(v)
    }
    
    

# MAP ---------------------------------------------------------------------

    df_repgap <- df_txcurr %>% 
      filter(hfr_pd >=4) 
      
    df_repgap <- df_repgap %>%
      mutate(pd = ifelse(hfr_pd >= 7, "post", "pre")) %>%
      group_by(operatingunit, countryname, orgunituid,
               mech_code, pd) %>%
      summarise(has_hfr_reporting = sum(has_hfr_reporting, na.rm = TRUE),
                is_datim_site = sum(is_datim_site, na.rm = TRUE)) %>%
      ungroup()
      
    df_repgap <- df_repgap %>%
      left_join(iso_map, by = c("countryname" = "operatingunit")) %>%
      select(-regional)
    
    df_repgap <- df_orgheirarchy %>%
      select(orgunituid, latitude, longitude) %>%
      left_join(df_repgap, .)
    
    df_repgap <- left_join(df_repgap, df_crosswalk) %>% 
      select(-iso)
      

    viz_rpt_map <- function(ctry_sel){
      
      #filter to select countyr
        df_repgap_ctry <- df_repgap %>% 
          filter(countryname == ctry_sel)
      
      #id country name from natural earth
        ctry_sel_ne <- unique(df_repgap_ctry$countryname_ne)
      
      #country and admin1 borders
        if(ctry_sel == "South Sudan"){
          ctry_adm0 <-  ne_countries(country = ctry_sel, scale = 'medium', returnclass = 'sf')
          ctry_adm1 <- ctry_adm0
        } else {
          ctry_adm1 <- ne_states(country = ctry_sel_ne, returnclass = 'sf') 
          ctry_adm0 <- summarise(ctry_adm1, placeholder = max(min_zoom))
        }
        
      #deal with SA island
        if(ctry_sel == "South Africa") {
          
          ctry_adm0 <- ctry_adm0 %>% 
            st_transform(st_crs(4326)) %>% 
            st_crop(c(ymin = -35.603719,
                      ymax = -21.718680, 
                      xmax = 34.804688, 
                      xmin = 13.886719))
          
          
          ctry_adm1 <- ctry_adm1 %>% 
            st_transform(st_crs(4326)) %>% 
            st_crop(c(ymin = -35.603719,
                      ymax = -21.718680, 
                      xmax = 34.804688, 
                      xmin = 13.886719))
        }
      #transfrom
        ctry_adm0 <- ctry_adm0 %>% 
          st_transform(crs = st_crs(3857)) %>% 
          st_as_sf()
        
        ctry_adm1 <- ctry_adm1 %>% 
          st_transform(crs = st_crs(3857)) %>% 
          st_as_sf()
        
       
      
      #create hex grid for country
        ctry_hex <- ctry_adm0 %>% 
          st_make_grid(what = 'polygons', cellsize = 30000, square = F) %>% 
          st_as_sf() 
        
      #create id for merging
        ctry_hex <- mutate(ctry_hex, id = row_number())
      
      #limit dataset to country, ensure coords exist 
        df_mapdata <- df_repgap %>% 
          filter(countryname == ctry_sel) %>% 
          filter_at(vars(latitude, longitude), any_vars(!is.na(.)))
        
      #transform from decimal degrees to projection in meters
        df_mapdata <- df_mapdata %>% 
          st_as_sf(coords = c("longitude", "latitude"),
                   crs = st_crs(4326)) %>% 
          st_transform(crs = st_crs(3857))
     
        
      if(nrow(df_mapdata) > 0){
        
      #bind hex ids onto data for join post aggregation
        df_mapdata <- st_join(df_mapdata, ctry_hex, join = st_intersects)
        
      #clip hexes to country border
        suppressWarnings(
          ctry_hex <- st_intersection(ctry_hex, ctry_adm0) 
        )
      
      #how many sites didn't map to a bin?
      # df_mapdata %>% 
      #   select(-geometry) %>% 
      #   as_tibble() %>% 
      #   distinct(orgunituid, id) %>% 
      #   count(is.na(id))
      
      #remove geometry and aggregate to calc hex completeness
        df_mapdata <- df_mapdata %>% 
          select(-geometry) %>% 
          as_tibble() %>% 
          group_by(countryname, id, pd) %>% 
          summarise_at(vars(has_hfr_reporting, is_datim_site), sum, na.rm = TRUE) %>% 
          ungroup()
      
      #create completeness pre/post and reshape to get one variable
        df_mapdata <- df_mapdata %>% 
          mutate(reporting_rate = has_hfr_reporting/is_datim_site,
                 status = case_when(reporting_rate <= .25 ~ paste0(pd, "-low"),
                                    reporting_rate < .75 ~ paste0(pd, "-med"),
                                    TRUE ~ paste0(pd, "-high"))) %>% 
          select(-has_hfr_reporting, -is_datim_site, -reporting_rate)  %>% 
          spread(pd, status) %>% 
          unite(position, c(pre, post), sep = ", ")
      
      #join aggregated data to hex
        df_mapdata <- left_join(ctry_hex, df_mapdata, by = "id")
        
      #hex map
        m <- df_mapdata %>% 
          filter(!is.na(id)) %>% 
          ggplot() +
          geom_sf(aes(fill = position), color = 'gray80') +
          geom_sf(data = ctry_adm1, fill = NA, size = 1, color = "gray60") +
          scale_fill_manual(values = bivar_map) +
          si_style_nolines() +
          labs(x = NULL, y = NULL,
               title = "Identifying Reporting Gaps in 2020",
               subtitle = "problem areas = dark pink and purple") +
          theme_void() +
          theme(text = element_text(family = "Source Sans Pro"),
                legend.position = "none",
                plot.title = element_text(size = 11, color = "gray30"),
                plot.subtitle = element_text(size = 7),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                panel.grid = element_blank())
        
      } else {
         m <- ctry_hex %>% 
           ggplot() +
           geom_sf(color = 'gray80', fill = NA) +
           geom_sf(data = ctry_adm1, fill = NA, size = 1, color = "gray60") +
           scale_fill_manual(values = bivar_map) +
           si_style_nolines() +
           labs(x = NULL, y = NULL,
                title = "Identifying Reporting Gaps",
                subtitle = "no sites with coordinates") +
           theme_void() +
           theme(text = element_text(family = "Source Sans Pro"),
                 legend.position = "none",
                 plot.title = element_text(size = 11, color = "gray30"),
                 plot.subtitle = element_text(size = 7),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 panel.grid = element_blank())
      }
        
        return(m)
    }
    
    
    
    

# PLOT GRAPHIC ------------------------------------------------------------

    # lgnd <- image_read("Images/bivar_legend.png")
   
    
    viz_combo <- function(ctry_sel){
      
      print(ctry_sel)
      
      p1 <- viz_trends(ctry_sel)
      p2 <- viz_rpt_rates(ctry_sel)    
      p3 <- viz_rpt_map(ctry_sel)
      p4 <- viz_growth(ctry_sel)
      
      if(is.null(p4))
        p4 <- plot_spacer()
      
      combo <- p3 + (p1 / (p2 + p4  & theme(strip.placement = NULL))) +
        plot_annotation(
          title = paste(toupper(ctry_sel), "| ASSESSING COVID’S IMPACT THROUGH HFR"),
          caption = 'HFR DATA | NOT FOR DISTRIBUTION OUTSIDE USAID'
        ) & 
        theme(text = element_text(family = "Source Sans Pro"),
              plot.title = element_text(face = "bold"),
              plot.caption = element_text(color = "gray30"))
      
      ctry_sel <- str_remove_all(ctry_sel, " ")
      
      filename <- paste0("HFR_ReportingTrends_", ctry_sel, ".png")
      
      ggsave(filename, path = "Images", dpi = 330, height = 5.625, width = 10)
      
      return(combo)
      
    
    }
    
    viz_combo("Nigeria")
    

  ctrys <- df_txcurr %>% 
    filter(hfr_pd == 1) %>% 
    group_by(countryname) %>% 
    summarise_at(vars(mer_targets, is_datim_site), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    arrange(desc(mer_targets)) %>%
    # print(n = Inf)
    pull(countryname)
  
  walk(ctrys[27:39], viz_combo)
  walk(ctrys, viz_combo)
  
  viz_combo(ctrys[2])
  