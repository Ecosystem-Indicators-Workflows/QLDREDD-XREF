here::i_am("scripts/00-data-download.R")

# Lookup table / database
# The REDD can be downloaded as a file in csv format from this url:

source_url <- "https://www.qld.gov.au/__data/assets/file/0026/160667/REDD_v13.1_2024.csv"

out_dir <- here::here("data","source-data")
if (!dir.exists(out_dir))
  dir.create(out_dir, recursive = TRUE)

out_file <- here::here(out_dir, basename(source_url))

download.file(source_url, out_file)

# spatial data
# We downloaded Version 13.1 from 2021 from the [Queensland Spatial Catalogue](https://qldspatial.information.qld.gov.au/catalogue/custom/detail.page?fid={8FDF54D2-654C-4822-8295-1D8E8E772373}), and copied the spatial data into a local folder for further processing.

# Data download from Queensland Spatial Catalogue requires filling a form to request a link for downloading. This link is sent via email and is only accessible for a limited time. This data source cannot be integrated into this reproducible workflow. 

# dir(here::here(out_dir, "QSC_Extracted_Data_20250827_210544309483-12528"))