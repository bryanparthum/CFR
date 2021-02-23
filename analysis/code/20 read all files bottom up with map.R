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
packages <- c('xml2','purrr','furrr') ## you can add more packages here
lapply(packages, pkgTest)

####################################################
################################################  GO
####################################################

source("bottom up functions with map.R")

years <- as.character(seq(1997, 2019))

output_dir <- here::here("..","store", "processed_xml_bottom_up")

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}


read_and_process_one_year <- function(year){
  # Start the clock!
  ptm <- proc.time()
  
  infile_path <- file.path("..","store", "read_xml_from_web", year)
  
  files <- list.files(infile_path, full.names = TRUE)
  
  year_xml <- future_map_dfr(files, process_file) %>%
    mutate(CFR_year = year)
  
  outfile <- file.path(output_dir, paste0("processed_xml_", year, ".rda"))
  
  # Stop the clock
  ptm2 <- proc.time() - ptm
  
  print(year)
  print(ptm2)
  
  save(year_xml, file = outfile, compress = TRUE)
}


plan(multiprocess)

# Start the clock!
ptm <- proc.time()

walk(years, read_and_process_one_year)

# Stop the clock
proc.time() - ptm
