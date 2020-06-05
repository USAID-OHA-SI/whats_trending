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


# GLOBAL VARIABLES --------------------------------------------------------
  
  drive_fldr <- "1SgZkdG5uu-Syy6DYsNbTrzDqUmK4fSgF" 
  
  tx_file <- "HFR_2020.07_TX_20200528.csv"
  
  pd8_file <- "HFR_2020.08_Tableau_20200604.csv"

  
# SETUP FOLDERS -----------------------------------------------------------
  
  folder_setup()  
  
# OAUTH -------------------------------------------------------------------
  
  drive_auth()


# IMPORT ------------------------------------------------------------------

  import_drivefile(drive_fldr, tx_file)
  import_drivefile(drive_fldr, pd8_file)

