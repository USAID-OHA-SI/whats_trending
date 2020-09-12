## PROJECT:  whats trending
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  import data from GDrive
## DATE:     2020-06-05
## UPDATED:  2020-09-11



# DEPENDENCIES ------------------------------------------------------------

library(googledrive)
library(tidyverse)
library(glamr)
library(Wavelength)

  
# SETUP FOLDERS -----------------------------------------------------------
  
  folder_setup()  
  

# DOWNLOAD DATA -----------------------------------------------------------
  
  #OAuth
    drive_auth()
  
  #drive location
  fldr_id <- "1pYb12gKBtwcRPaOTXpYXL548H_PrvUob"
  
  #files
  files <- drive_ls(as_id(fldr_id), pattern = "View") %>% pull(name)
  
  #download
  walk(files, ~ import_drivefile(fldr_id, .x))


  #drive location
    fldr_id <- "1CLxz5MUmEsmLYb9-ZjIQ7IiAlRMwU1LK"
  
  #files
    files <- drive_ls(as_id(fldr_id), pattern = "View.*(123|456)") %>% pull(name)
  
  #download
    walk(files, ~ import_drivefile(fldr_id, .x))


# PULL HIERARCHY ----------------------------------------------------------


  #DATIM username
    user <- ""
    
  #pull country uids
    ouuids <- identify_ouuids(user, mypwd(user)) %>%
      filter(is.na(regional)) %>%
      pull(id)
    
  #pull hierarchy
    df_orgs <- map_dfr(ouuids, 
                      ~ pull_hierarchy(.x, user, mypwd(user)))
    
  #save
    hfr_export(df_orgs, "Data", type = "orghierarchy")
    
  
  