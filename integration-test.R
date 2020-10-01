## ----setup, include=FALSE--------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  cache = FALSE
)


## --------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(config))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(renv))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(fs))
library(here)


## --------------------------------------------------------------
detect_packages <- function() {
  packages <- renv::dependencies()$Package
  sort(unique(packages))
}

detect_packages()


## --------------------------------------------------------------
devtools::session_info()


## --------------------------------------------------------------
file_name <- "TestPortfolio_Input.csv"
example_dataset <- here("sample_files", "20_input_files", file_name)

expect_true(file_exists(example_dataset))


## --------------------------------------------------------------
expected_dataset <- here("working_dir", "20_Raw_Inputs", file_name)

if (file_exists(expected_dataset)) {
  warn(glue("Removing existing file: {expected_dataset}"))
  file_delete(expected_dataset)
}

file_copy(example_dataset, expected_dataset)

expect_true(file_exists(expected_dataset))


## --------------------------------------------------------------
ensure_empty_directory <- function(directory) {
  if (dir_exists(directory)) {
    dir_delete(directory)
  }

  dir_create(directory)

  invisible(directory)
}

children <- c("30_Processed_Inputs", "40_Results", "50_Outputs")
(paths <- here("working_dir", children))

walk(paths, ensure_empty_directory)


## --------------------------------------------------------------
is_sibling <- function(x) {
  parent <- path_dir(here())
  dir_exists(path(parent, x))
}

repos <- c("pacta-data", "create_interactive_report", "PACTA_analysis")
all_siblings <- all(map_lgl(repos, is_sibling))

expect_true(all_siblings)


## --------------------------------------------------------------
expect_equal(path_file(here()), "PACTA_analysis")


## --------------------------------------------------------------
# What value is currently assigned to the variable `portfolio_name_ref_all`?
show_pattern_in_file <- function(file, pattern) {
  grep(pattern, readLines(file), value = TRUE)
}

(files <- dir_ls(regexp = "web_tool_script"))

this_pattern <- "portfolio_name_ref_all.*<-"
matched <- map(files, show_pattern_in_file, pattern = this_pattern)
walk(matched, writeLines)

script_has_this_pattern <- grepl(this_pattern, matched)
expect_true(all(script_has_this_pattern))


## --------------------------------------------------------------
config_1 <- here(
  "working_dir", 
  "10_Parameter_File", 
  "TestPortfolio_Input_PortfolioParameters.yml"
)

expect_true(file_exists(config_1))


## --------------------------------------------------------------
look_into <- function(path, n = -1L) {
  lines <- readLines(path, n, encoding = "UTF-8")
  writeLines(lines)
}

look_into(config_1)


## --------------------------------------------------------------
config_2 <- here("parameter_files", "WebParameters_2dii.yml")

expect_true(file_exists(config_2))


## --------------------------------------------------------------
make_config_portable <- function(config) {
  lines <- readLines(config, encoding = "UTF-8")
  lines <- make_paths_portable(lines)
  writeLines(lines, config)
  
  invisible(config)
}

make_paths_portable <- function(x) {
  x %>% 
    root_field_path("project_location_ext", pattern = "PACTA_analysis") %>% 
    root_field_path("data_location_ext", pattern = "pacta-data") %>% 
    root_field_path("template_location", pattern = "create_interactive_report")
}

root_field_path <- function(x, field, pattern) {
  parent <- path_dir(here())
  value <- path(parent, extract_from(x, pattern))
  sub(glue("({field}:[ ]?).*"), glue("\\1{value}/"), x)
}

extract_from <- function(x, pattern) {
  line <- grep(pattern, x, value = TRUE)
  sub(glue(".*({pattern}.*)"), "\\1", line)
}

make_config_portable(config_2)


## --------------------------------------------------------------
config_paths <- config::get(file = config_2)$paths
all_paths_exist <- all(map_lgl(config_paths, dir_exists))

expect_true(all_paths_exist)


## --------------------------------------------------------------
look_into(config_2)


## ----message=FALSE---------------------------------------------
dir_has_files <- function(path) {
  stopifnot(is_dir(path))
  
  contents <- dir_ls(path, recurse = TRUE)
  has_files <- any(map_lgl(contents, is_file))
  has_files
}

# source another script with arguments
source_with_args <- function(file, ...){
  passArgs <<- function(trailingOnly){
    list(...)
  }
  source(file)
}

out_1 <- path("working_dir", "30_Processed_Inputs")

expect_false(dir_has_files(out_1))
source("web_tool_script_1.R")
# system(paste("Rscript web_tool_script_1.R", "TestPortfolio_Input"))
expect_true(dir_has_files(out_1))


## ----message=FALSE---------------------------------------------
out_2 <- path("working_dir", "40_Results")

expect_false(dir_has_files(out_2))
source("web_tool_script_2.R")
expect_true(dir_has_files(out_2))


## ----message=FALSE---------------------------------------------
out_3 <- path("working_dir", "50_Outputs")

expect_false(dir_has_files(out_3))
source("web_tool_script_3.R") 
expect_true(dir_has_files(out_3))


## --------------------------------------------------------------
outputs <- path("working_dir", "50_Outputs")

css <- dir_ls(outputs, recurse = TRUE, regexp = "[.]css")
expect_true(length(css) > 0L)

js <- dir_ls(outputs, recurse = TRUE, regexp = "[.]js")
expect_true(length(js) > 0L)

index <- dir_ls(outputs, recurse = TRUE, regexp = "index[.]html")
expect_true(length(index) > 0L)

zip <- dir_ls(outputs, recurse = TRUE, regexp = "[.]zip")
expect_true(length(zip) > 0L)


## --------------------------------------------------------------
look_into(index, n = 20L)

dir_tree(path(outputs, "TestPortfolio_Input"), recurse = FALSE)


## --------------------------------------------------------------
dir_ls(path("..", "pacta-data", "2019Q4", "cleaned_files"))

