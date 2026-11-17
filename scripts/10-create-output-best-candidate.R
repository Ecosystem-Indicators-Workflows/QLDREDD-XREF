## The purpose of this script is to create a spatial output from source
## spatial data using cross-referencing information consolidated in
## membership tables.

## Source classification and spatial data (units):
## > REDD (regional ecosystems)
## Target classification (units):
## > IUCN Global Ecosystem Typology (ecosystem functional groups)
## Output format:
## > Raster GeoTiff files with best candidate target unit, membership and
## > number of supporting interpreters

## This script summarises the minimum set of steps to complete four stages
## of the cross-referencing workflow:
## 1) Data preparation
## 2) Data validation
## 3) Synthesis
## 4) Read additional data
## 5) Outputs in preferred format

## Recommended usage:
## R --vanilla < scripts/10-create-output-best-candidate.R

## Step O: Setup

### Benchmarking
start_time <- Sys.time()

### Libraries

#### tidyverse libraries
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

#### Read and write other formats
# library(readxl)
# library(foreign)
# library(jsonlite)

#### For loops
library(foreach)

#### Plots
# library(ggplot2)

#### Spatial data
library(terra)
library(sf)
library(fasterize)

#### Cross-referencing
library(ecoSynthesis)
library(geco)

### Paths
here::i_am("scripts/10-create-output-best-candidate.R")

gisdata_dir <- here::here("data/source-data")
xref_dir <- here::here("data/xrefdata")
out_dir <- here::here("results", "output1-best-candidate-raster")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

### Custom functions

source(here::here("scripts", "functions.R"))

### Predefined parameters

the_round <- 3
epsg_code <- "EPSG:9473" # Australian Albers

lut_file <- here::here(gisdata_dir, "REDD_v13.1_2024.csv")
gis_file <- here::here(
  gisdata_dir,
  "QSC_Extracted_Data_20250827_210544309483-12528",
  "data.gpkg"
)


## Step 1: data preparation

### Lookup table source classification

LUT <- read_csv(lut_file) |>
  mutate(eligible = DEFUNCT == "Active")

# Read GIS data

REDD_xy <- read_sf(gis_file) |>
  st_transform(crs(epsg_code))

re_labels <- str_split_fixed(REDD_xy$re_label, "/", 5) |> data.frame()

d0 <- pivot_longer(
  re_labels,
  everything(),
  names_to = "re_component",
  values_to = "NAME"
)

d1 <- REDD_xy |>
  st_drop_geometry() |>
  pivot_longer(
    cols = all_of(sprintf("re%s", 1:5)),
    names_to = "sub_component",
    values_to = "re_name"
  ) |>
  select(re_full = re, sub_component, re_name)

d2 <- REDD_xy |>
  st_drop_geometry() |>
  pivot_longer(
    cols = all_of(sprintf("pc%s", 1:5)),
    names_to = "pc_component",
    values_to = "proportion"
  ) |>
  select(pc_component, proportion)

mapped_units <- bind_cols(d0, d1, d2) |> filter(NAME != "")

MAP_LUT <- mapped_units |>
  transmute(NAME, re_name, mapped = TRUE) |>
  unique() |>
  left_join(LUT, by = "NAME") |>
  select(NAME, re_name, SHORT_DESCRIPTION, mapped)


REDD_LUT <- lookup_tbl(
  MAP_LUT,
  id = "re_name",
  name = "SHORT_DESCRIPTION",
  eligible = "mapped"
)

### Lookup table target classification

IUCN_GET <- lookup_tbl(IUCN_GET_LUT, "efg_id", "efg_name")

### Membership tables

memb_data <- foreach(
  j = dir(xref_dir, pattern = ".csv"),
  .combine = bind_rows
) %do%
  {
    metadata <- read_csv(
      here::here(xref_dir, j),
      col_names = c("name", "value"),
      n_max = 12,
      show_col_types = F
    )[, 1:2]
    data <- read_csv(here::here(xref_dir, j), skip = 12, show_col_types = F)
    stringr::str_extract(j, "V[0-9]")
    cat(sprintf(
      "Read %s, interpreter %s\n%s\n",
      j,
      metadata[1, 2],
      metadata[11, 2]
    ))

    if ("EFG_1" %in% colnames(data)) {
      guess_round <- str_extract(j, "(?<=V)[0-9]")
      cat(sprintf("Assigning this to round %s\n", guess_round))

      pivot_wide_membership_table(
        xref = data,
        original_names = "MAPUNIT",
        target_names = sprintf("EFG_%s", 1:4),
        membership_values = sprintf("memb_%s", 1:4),
        expert_name = metadata[[1, 2]],
        round = as.numeric(guess_round)
      )
    }
  }

memb_data |>
  filter(!is.na(memb), memb > 0) |>
  group_by(expert, round) |>
  summarise(
    `source ids` = n_distinct(`MAPUNIT`),
    `target ids` = n_distinct(EFG),
    .groups = "drop"
  )

## Step 2: data validation

REDD_memb <- memb_data |>
  filter(!is.na(memb), memb > 0) |>
  membership_tbl(
    source_id = "MAPUNIT",
    target_id = "EFG",
    membership = "memb",
    interpreter = "expert",
    round = "round"
  )

check_membership(REDD_memb, source = REDD_LUT, target = IUCN_GET)

REDD_memb |>
  filter(!EFG %in% IUCN_GET$efg_id) |>
  summarise(
    n_distinct(`MAPUNIT`),
    .by = c(expert, round, EFG)
  )

REDD_clean <- clean_membership(
  REDD_memb,
  source = REDD_LUT,
  target = IUCN_GET,
  action = "check"
)

# REDD_clean |>
#   mutate(dups = grepl("duplicated ass", flag)) |>
#   summarise(
#     source_units = n_distinct(`MAPUNIT`),
#     .by = c(expert, round, dups)
#   )

# REDD_clean <- REDD_clean |> filter(!grepl("duplicated record", flag))

# ## Step 3: synthesis

REDD_best_summary <- summarise_membership(
  REDD_clean,
  keep_all = TRUE,
  which_round = the_round
)

# ## Step 4: read additional data for outputs

f1 <- REDD_xy$re %in% REDD_best_summary$MAPUNIT
f2 <- REDD_xy$re1 %in% REDD_best_summary$MAPUNIT
f3 <- REDD_xy$re2 %in% REDD_best_summary$MAPUNIT
f4 <- REDD_xy$re3 %in% REDD_best_summary$MAPUNIT
f5 <- REDD_xy$re4 %in% REDD_best_summary$MAPUNIT
f6 <- REDD_xy$re5 %in% REDD_best_summary$MAPUNIT

table(f1 | f2 | f3 | f4 | f5 | f6)

REDD_xy |> filter(!(f1 | f2 | f3 | f4 | f5 | f6))

# ### 4b. Standard pixel values for Global Ecosystem Atlas
# ### + the T5.6

t5_6_info <- tibble(
  EFG = "T5.6",
  efg_name = "Semi-arid woodlands",
  efg_pixel_value = 35,
  efg_hex_code = "#EEB4B4",
  efg_rgb_code = "238,180,180"
) #rosybronw2

efg_info <- geco_efg_data() |>
  select(efg_code, efg_name, efg_pixel_value, efg_hex_code, efg_rgb_code) |>
  rename("EFG" = "efg_code") |>
  bind_rows(t5_6_info) |>
  arrange(efg_pixel_value)

efg_pixel_values <- efg_info |>
  select(EFG, efg_pixel_value)

# ## Step 5: export outputs

xref_best_file <- "REDD-xref-best-candidate.csv"
efg_lookup_file <- "IUCN-GET-pixel-lookup.csv"
xref_clr_file <- "REDD-xref-target.clr"
xref_aux_file <- "REDD-xref-target.tiff.aux.xml"
xref_qml_file <- "REDD-xref-target.qml"
xref_tiff_file <- "REDD-xref-target.tiff"
memb_tiff_file <- "REDD-xref-membership.tiff"
supp_tiff_file <- "REDD-xref-supporting.tiff"

# ### 5a. Export tabular synthesis tables

xref_best_tbl <- REDD_LUT |>
  as_tibble() |>
  left_join(REDD_best_summary, by = c("re_name" = "MAPUNIT")) |>
  left_join(efg_info, by = "EFG") |>
  select(
    NAME,
    re_name,
    SHORT_DESCRIPTION,
    mapped,
    EFG,
    efg_name,
    efg_pixel_value,
    efg_hex_code,
    efg_rgb_code,
    round,
    prop_memb,
    total_memb,
    supporting,
    total_interp
  )

write_csv(
  xref_best_tbl,
  here::here(out_dir, xref_best_file)
)

# ### 5b. Export tabular lookup  tables

efg_lookup_tbl <- xref_best_tbl |>
  filter(!is.na(EFG)) |>
  summarise(
    n_REDD_units = n_distinct(re_name),
    .by = c(efg_pixel_value, EFG, efg_name, efg_hex_code, efg_rgb_code)
  ) |>
  left_join(
    as_tibble(IUCN_GET) |>
      select(efg_id, realm_id, realm_name, biome_id, biome_name),
    by = c("EFG" = "efg_id")
  ) |>
  select(
    efg_pixel_value,
    EFG,
    efg_name,
    realm_id,
    realm_name,
    biome_id,
    biome_name,
    efg_hex_code,
    efg_rgb_code,
    n_REDD_units
  ) |>
  arrange(efg_pixel_value)

write_csv(
  efg_lookup_tbl,
  here::here(out_dir, efg_lookup_file)
)

# ### 5c. Export colour file

clr_lines <- efg_lookup_tbl |>
  tidyr::separate(efg_rgb_code, into = c("R", "G", "B"), sep = ",") |>
  transmute(
    line = sprintf(
      "%d %s %s %s 255 %s %s",
      efg_pixel_value,
      R,
      G,
      B,
      EFG,
      efg_name
    )
  ) |>
  pull(line)

writeLines(
  clr_lines,
  here::here(out_dir, xref_clr_file)
)

# ### 5d. Export style file

qml_entries <- efg_lookup_tbl |>
  transmute(
    entry = sprintf(
      '        <paletteEntry value="%d" color="%s" alpha="255" label="%s %s"/>',
      efg_pixel_value,
      efg_hex_code,
      EFG,
      efg_name
    )
  ) |>
  pull(entry)

qml_lines <- c(
  "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>",
  '<qgis version="3.34.0" minScale="1e+08" maxScale="0" hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories">',
  "  <pipe>",
  "    <provider>",
  '      <resampling enabled="false" maxOversampling="2" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour"/>',
  "    </provider>",
  '    <rasterrenderer band="1" type="paletted" opacity="1" alphaBand="-1" nodataColor="">',
  "      <rasterTransparency/>",
  "      <minMaxOrigin>",
  "        <limits>None</limits>",
  "        <extent>WholeRaster</extent>",
  "        <statAccuracy>Estimated</statAccuracy>",
  "        <cumulativeCutLower>0.02</cumulativeCutLower>",
  "        <cumulativeCutUpper>0.98</cumulativeCutUpper>",
  "        <stdDevFactor>2</stdDevFactor>",
  "      </minMaxOrigin>",
  "      <colorPalette>",
  qml_entries,
  "      </colorPalette>",
  "    </rasterrenderer>",
  '    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>',
  '    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>',
  '    <rasterresampler maxOversampling="2"/>',
  "  </pipe>",
  "  <blendMode>0</blendMode>",
  "</qgis>"
)
writeLines(qml_lines, here::here(out_dir, xref_qml_file))

# ### 5f. Export raster files
# ### Convert vector to raster using `fasterize`

# ### Combined attribute table (vector data)

REDD_xy_best <- unique(mapped_units) |>
  left_join(
    REDD_best_summary,
    by = c("re_name" = "MAPUNIT"),
    relationship = "many-to-many"
  ) |>
  left_join(efg_pixel_values, by = "EFG")

REDD_xy_best |>
  group_by(re_full, EFG) |>
  summarise(agg_memb = sum(prop_memb * proportion), .groups = "drop") # slice max


REDD_xy_best <- REDD_xy |>
  left_join(
    REDD_best_summary,
    by = c("re1" = "MAPUNIT"),
    relationship = "many-to-one"
  ) |>
  left_join(efg_pixel_values, by = "EFG")

# REDD_xy_best |> transmute(re1,EFG,prop_memb,pc1,prop_memb_w=pc1*prop_memb) |> pull(prop_memb_w) |> hist()

REDD_xy_2nd <- REDD_xy |>
  left_join(
    REDD_best_summary,
    by = c("re2" = "MAPUNIT"),
    relationship = "many-to-one"
  ) |>
  left_join(efg_pixel_values, by = "EFG")

# REDD_xy_2nd |> transmute(re2,EFG,prop_memb,pc2,prop_memb_w=pc2*prop_memb) |> pull(prop_memb_w) |> hist()

REDD_xy_compare <- bind_cols(
  {
    REDD_xy_best |>
      st_drop_geometry() |>
      transmute(
        re1,
        re2,
        pc1,
        pc2,
        EFG1 = EFG,
        memb1 = prop_memb,
        prop_memb_w1 = pc1 * prop_memb
      )
  },
  {
    REDD_xy_2nd |>
      st_drop_geometry() |>
      transmute(EFG2 = EFG, memb2 = prop_memb, prop_memb_w2 = pc2 * prop_memb)
  }
)

REDD_xy_compare |>
  filter(!is.na(EFG2)) |>
  mutate(same = EFG1 == EFG2) |>
  pull(same) |>
  table()

REDD_xy_compare |>
  filter(!is.na(EFG2), EFG1 != EFG2) |>
  mutate(second = prop_memb_w2 > prop_memb_w1) |>
  pull(second) |>
  table()

# ### template raster file
# rez <- 90
# REDD_raster <- raster(REDD_xy, res = rez)

# #### Best candidate

# target_raster <- fasterize(
#   REDD_xy_best,
#   REDD_raster,
#   field = "efg_pixel_value",
#   fun = "first"
# ) |>
#   rast()

# ctab <- efg_info |>
#   filter(EFG %in% REDD_xy_best$EFG) |>
#   transmute(value = efg_pixel_value, color = efg_hex_code)
# coltab(target_raster) <- ctab

# rast_metadata <- c(
#   PROJECT = "Ecosystem Indicators Workflows",
#   TITLE = "Best candidate ecosystem functional group",
#   DESCRIPTION = "Synthesis of crossreference from REDD 006 to IUCN GET"
# )
# metags(target_raster) <- rast_metadata
# raster_attributes <- efg_lookup_tbl |>
#   transmute(
#     id = coalesce(efg_pixel_value, 255L),
#     label = paste(EFG, efg_name, sep = " ")
#   )
# levels(target_raster) <- raster_attributes
# writeRaster(
#   target_raster,
#   here::here(out_dir, xref_tiff_file),
#   overwrite = TRUE
# )

# # out_file <- here::here(out_dir, "REDD-xref-target.tiff.vat.dbf")
# # write.dbf(data.frame(raster_attributes), file = out_file)

# #### Best candidate membership

# memb_raster <- fasterize(
#   REDD_xy_best,
#   REDD_raster,
#   field = "prop_memb",
#   fun = "max"
# ) |>
#   rast()

# rast_metadata[
#   "TITLE"
# ] <- "Membership for the best candidate ecosystem functional group"

# metags(memb_raster) <- rast_metadata

# writeRaster(memb_raster, here::here(out_dir, memb_tiff_file), overwrite = TRUE)

# #### Supporting interpreters

# supp_raster <- fasterize(
#   REDD_xy_best,
#   REDD_raster,
#   field = "supporting",
#   fun = "max"
# ) |>
#   rast()

# rast_metadata["TITLE"] <- "Nr. of interpreters supporting assignment"

# metags(supp_raster) <- rast_metadata

# writeRaster(supp_raster, here::here(out_dir, supp_tiff_file), overwrite = TRUE)

## Step X: Benchmarking and session info

end_time <- Sys.time()
end_time - start_time

sessionInfo()
