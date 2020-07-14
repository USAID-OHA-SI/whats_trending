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

  
# SETUP FOLDERS -----------------------------------------------------------
  
  folder_setup()  
  

# DOWNLOAD DATA -----------------------------------------------------------
  
  #OAuth
    drive_auth()
  
  #drive location
    fldr_id <- "1oEFvmfFnTc9eAlSzxhbBG2C7mYCcs1YJ"
  
  #files
    files <- drive_ls(as_id(fldr_id), pattern = "View") %>% pull(name)
  
  #download
    walk(files, ~ import_drivefile(fldr_id, .x))


  