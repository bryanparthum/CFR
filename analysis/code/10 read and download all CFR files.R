####################################################
##########################################  PREAMBLE
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c('xml2','purrr','httr','stringr','here') ## you can add more packages here
lapply(packages, pkgTest)

####################################################
################################################  GO
####################################################

## SET WORKING DIRECTORY
setwd(here())

## reads one volume within a given year, removes the namespace, and saves to a file
get_one_volume <- function(volume_link, read_this_year, file_path){
  
  CFR_volume <- read_xml(volume_link) %>%
    xml_ns_strip()
  
  filename <- paste0("title40_",str_extract(volume_link, "vol\\d+"),"_", read_this_year, ".xml")
  
  write_xml(CFR_volume, file = here("..","store","read_xml_from_web", read_this_year, filename))
  
  Sys.sleep(10)
  
  print(paste(read_this_year, volume_link))
}

## reads all volumes for a given year, includes creating necessary save directories
read_one_year <- function(read_this_year){
  
  get_year_url <- paste0("http://www.govinfo.gov/bulkdata/CFR/", read_this_year, "/title-40")
  
  file_path <- here("..","store","read_xml_from_web", read_this_year)
  
  if (!dir.exists(file_path))
    dir.create(file_path, recursive = TRUE)
  
  links <-  GET(get_year_url) %>%
    httr::content("parsed") %>%
    xml_find_all(".//link") %>%
    xml_text(.) 
  
  links <- subset(links, endsWith(links, ".xml"))
  
  Sys.sleep(10)
  
  walk(links, get_one_volume, read_this_year = read_this_year, file_path = file_path )
}

## EXECUTE
ptm <- proc.time()

years <- as.character(seq(1997, 2019))

walk(years, read_one_year)

proc.time() - ptm

# # Start the clock!
# ptm <- proc.time()
# 
# read_one_year("2019")
# 
# # Stop the clock
# proc.time() - ptm

