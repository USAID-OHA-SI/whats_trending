## PROJECT:  whats trending
## AUTHOR:   achafetz | USAID
## LICENSE:  MIT
## PURPOSE:  import data from GDrive
## DATE:     2020-06-05
## UPDATED:  2020-07-14



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
    fldr_id <- "1CLxz5MUmEsmLYb9-ZjIQ7IiAlRMwU1LK"
  
  #files
    files <- drive_ls(as_id(fldr_id), pattern = "View") %>% pull(name)
  
  #download
    walk(files, ~ import_drivefile(fldr_id, .x))


# PULL HIERARCHY ----------------------------------------------------------


  #DATIM username
    user <- ""
    
  #pull country uids
    ctry_uids <- get_outable(user, mypwd(user)) %>% pull(countryname_uid)
    
  #pull hierarchy
    df_orgs <- map_dfr(ctry_uids, 
                      ~ pull_hierarchy(.x, user, mypwd(user)))
    
  #save
    hfr_export(df_orgs, "Data", type = "orghierarchy")
    
  
  