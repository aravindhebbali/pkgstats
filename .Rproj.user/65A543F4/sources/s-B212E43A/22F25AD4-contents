library(readr)
library(dplyr)

yday      <- lubridate::today() - 1
base_url  <- "http://cran-logs.rstudio.com/2018/"
today_url <- paste0(base_url, yday, ".csv.gz")
pkg_data  <- read_csv(today_url)
pkg_data

pkg_data %>%
  select(package) %>%
  filter(package == "olsrr" | package == "descriptr" |
           package == "inferr" | package == "rfm" | 
           package == "blorr") %>%
  group_by(package) %>%
  tally() %>%
  arrange(desc(n))

pkg_data %>%
  select(package) %>%
  filter(package == "olsrr" |  package == "descriptr" |
         package == "inferr" | package == "rfm" | 
           package == "blorr") %>%
  group_by(package) %>%
  tally() %>%
  pull(n) %>%
  sum()


pkg_data %>%
  select(package, country) %>%
  filter(package == "olsrr") %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n))

pkg_data %>%
  select(package, country) %>%
  filter(package == "blorr") %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n))

pkg_data %>%
  select(package, country) %>%
  filter(package == "rfm") %>%
  group_by(country) %>%
  tally() %>%
  pull(n) %>%
  sum()

