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


# GDRIVE IMPORT FUNCTION --------------------------------------------------


  import_drivefile <- function(drive_folder, filename, folderpath = "Data", zip = TRUE){
    
    #get files + ids from google drive folder
      files <- drive_ls(as_id(fldr))
    
    #pull id for specific file
      id <- files %>% 
        filter(name == filename) %>% 
        pull(id)
      
    #download file and store in Data folder
      drive_download(as_id(id), path = file.path(folderpath, filename), overwrite = TRUE)
    
    if(zip == TRUE){
      #zip file
      orig_wd <- getwd()
      setwd(folderpath)
      filename %>% 
        str_replace("csv$", "zip") %>% 
        zip(filename)
      #remove 
      unlink(filename)
      setwd(orig_wd)
    }
    
  }


# IMPORT ------------------------------------------------------------------

  
  import_file(drive_fldr, tx_file)
  import_file(drive_fldr, pd8_file)

