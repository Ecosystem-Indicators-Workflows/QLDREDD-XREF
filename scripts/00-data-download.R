here::i_am("scripts/00-data-download.R")

# The REDD can be downloaded as a file in csv format from this url:

source_url <- "https://www.qld.gov.au/__data/assets/file/0026/160667/REDD_v13.1_2024.csv"


out_dir <- here::here("data","source-data")
if (!dir.exists(out_dir))
  dir.create(out_dir, recursive = TRUE)

out_file <- here::here(out_dir, basename(source_url))

download.file(source_url, out_file)