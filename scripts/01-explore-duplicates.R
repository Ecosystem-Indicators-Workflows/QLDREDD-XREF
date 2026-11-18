#' ---
#' title: Exploration of duplicates
#' author: JR Ferrer-Paris
#' date: 17 Nov 2026
#' format: html
#' toc: true
#' ---

#' The purpose of this script is to import and validate the cross-referencing information consolidated in membership tables.

#' This scripts identifies duplicates and potential problems before further steps of analysis and synthesis.

#' This script is intended for interactive use and data exploration.

#' **Source classification and spatial data (units)**: DPIRD 006 (vegetation associations)

#' **Target classification (units)**: IUCN Global Ecosystem Typology (ecosystem functional groups)
#'
#' ## Step O: Setup

#| label: Benchmarking
start_time <- Sys.time()

#| label: Libraries
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
# library(readxl)
# library(foreign)
# library(jsonlite)
library(foreach)
# library(ggplot2)
# library(terra)
library(sf)
# library(fasterize)
#### Cross-referencing
library(ecoSynthesis)
# library(geco)

#| label: paths
here::i_am("scripts/10-create-output-best-candidate.R")
gisdata_dir <- here::here("data/source-data")
xref_dir <- here::here("data/xrefdata")
rdata_dir <- here::here("data/processed_data")


#| label: Custom functions
source(here::here("scripts", "functions.R"))

#| label: Predefined parameters
the_round <- 4
lut_file <- here::here(gisdata_dir, "REDD_v13.1_2024.csv")
gis_file <- here::here(
  gisdata_dir,
  "QSC_Extracted_Data_20250827_210544309483-12528",
  "data.gpkg"
)


#' ## Step 1: Prepare Lookup tables
#'
#' The original lookup table in csv format (REDD)

#| label: Lookup table source classification
LUT <- read_csv(lut_file) |>
  mutate(eligible = DEFUNCT == "Active")

#' This file does not include the `re1` codes used in the spatial layer. the `NAME`` column matches the values in the `re_label`, however, these are concatenated.
#'
#' A work-around is to combine the information of the GIS data and LUT data

#| label: Read GIS data
REDD_xy <- read_sf(gis_file)

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

#' We create a data frame with the information of all mapped units, and attach the short descriptions for the higher level units. This will become our lookup table for the classification, here we assigned all mapped units as eligible except 'water'
#'

#| label: LUT from mapped units
mapped_units <- bind_cols(d0, d1, d2) |> filter(NAME != "")

MAP_LUT <- mapped_units |>
  transmute(NAME, re_name, mapped = TRUE) |>
  unique() |>
  left_join(LUT, by = "NAME") |>
  select(NAME, re_name, SHORT_DESCRIPTION, mapped) |>
  mutate(eligible = re_name != "water")


REDD_LUT <- lookup_tbl(
  MAP_LUT,
  id = "re_name",
  name = "SHORT_DESCRIPTION",
  eligible = "eligible"
)

#' For the target classification we include all units from the current version of the GET and we add the T5.6 Semi-arid woodlands.

#| label: Lookup table target classification

IUCN_GET <- bind_rows(
  IUCN_GET_LUT,
  {
    tibble(
      efg_id = "T5.6",
      efg_name = "Semi-arid woodlands",
      eligible = TRUE
    )
  }
) |>
  lookup_tbl("efg_id", "efg_name")


#' ## Step 2: Prepare membership tables
#' Read all membership tables. Note that the column name changed from `MAPUNIT` to `re1` to reflect the column in the spatial data
#'

#| label: Membership tables

memb_data <- foreach(
  j = dir(xref_dir, pattern = ".csv"),
  .combine = bind_rows
) %do%
  {
    mtbl <- read_csv_meta_data(here::here(xref_dir, j)) |>
      rename(any_of(c("re1" = "MAPUNIT")))
    check_input_data(mtbl, check_duplicates = "re1")
    pivot_wide_membership_table(
      xref = mtbl,
      original_names = "re1",
      target_names = sprintf("EFG_%s", 1:4),
      membership_values = sprintf("memb_%s", 1:4),
      expert_name = attributes(mtbl)$interpreter,
      round = as.numeric(attributes(mtbl)$round)
    )
  }

#' there were messages about duplicates, but those were in preliminary rounds. If we focus on the last round, we can ignore those duplicates for now.
#'

#' Let's also check some totals:

#| label: Totals in the table
memb_data |>
  filter(!is.na(memb), memb > 0) |>
  group_by(expert, round) |>
  summarise(
    `source ids` = n_distinct(`re1`),
    `target ids` = n_distinct(EFG),
    .groups = "drop"
  )

#' ## Step 3 data validation

#' The number of 'rounds' is different per interpreter, we want to upgrade the round column so that we keep the last round from each interpreter

#| label: Filter table to last round
REDD_memb <- memb_data |>
  filter(!is.na(memb), memb > 0) |>
  mutate(
    upgraded_round = case_when(
      (expert %in% "David_Keith") & (round == 3) ~ 4L,
      (expert %in% "Eda Addicott") & (round == 2) ~ 4L,
      TRUE ~ round
    )
  ) |>
  filter(upgraded_round == 4) |> ## keep only the last round
  membership_tbl(
    source_id = "re1",
    target_id = "EFG",
    membership = "memb",
    interpreter = "expert",
    round = "upgraded_round"
  )

#' The number of 'rounds' is different per interpreter, we want to upgrade the round column so that we keep the last round from each interpreter
#'
#| label: Check membership
check_membership(REDD_memb, source = REDD_LUT, target = IUCN_GET)

REDD_clean <- clean_membership(
  REDD_memb,
  source = REDD_LUT,
  target = IUCN_GET,
  action = "check"
)

#' This shows only a small numbers of records with problems.

REDD_memb |>
  filter(!EFG %in% IUCN_GET$efg_id)

REDD_memb |>
  filter(!re1 %in% REDD_LUT$re_name)

#' We are going to drop these for now.

REDD_clean <- clean_membership(
  REDD_memb,
  source = REDD_LUT,
  target = IUCN_GET
)

#' ## Save the data

save(
  REDD_clean,
  REDD_LUT,
  IUCN_GET,
  file = here::here(rdata_dir, "REDD-xref-round-4-data.rda")
)


#' ## Benchmarking and session info

#| label: Step X Benchmarking and session info
end_time <- Sys.time()
end_time - start_time
sessionInfo()
