## PROJECT:  whats trending
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  import data from GDrive
## DATE:     2020-06-05
## UPDATED:  



# DEPENDENCIES ------------------------------------------------------------

library(googledrive)
library(tidyverse)
library(glamr)
library(Wavelength)


# GLOBAL VARIABLES --------------------------------------------------------
  
  drive_fldr <- "1SgZkdG5uu-Syy6DYsNbTrzDqUmK4fSgF" 
  
  tx_file <- "HFR_2020.07_TX_20200528.csv"
  
  pd8_file <- "HFR_2020.08_Tableau_20200604.csv"

  drive_fldr_hierarchy <- "1enffr2NUZAz5eabUAGmfdXKoKWnLlWZ5"
  
# SETUP FOLDERS -----------------------------------------------------------
  
  folder_setup()  
  
# OAUTH -------------------------------------------------------------------
  
  drive_auth()



# ID CURRENT ORG HIERARCHY FILE -------------------------------------------

  org_file <- drive_ls(as_id(drive_fldr_hierarchy), 
                        pattern = "HFR_FY20_GLOBAL_orghierarchy") %>% 
    pull(name)
  
# IMPORT ------------------------------------------------------------------

  import_drivefile(drive_fldr, tx_file)
  import_drivefile(drive_fldr, pd8_file)
  import_drivefile(drive_fldr_hierarchy, org_file)


  