library(readr)
library(dplyr)
library(lubridate)
library(glue)

yday      <- today() - 1
base_url  <- "http://cran-logs.rstudio.com/2019/"
today_url <- glue(base_url, as.character(yday), ".csv.gz")
pkg_data  <- read_csv(today_url)

base_data <- 
  pkg_data %>%
  select(package, country) %>%
  filter(package == "olsrr" | package == "descriptr" |
           package == "inferr" | package == "rfm" |
           package == "blorr" | package == "vistributions" |
           package == "rbin" | package == "xplorerr")

# package downloads
base_data %>%
  group_by(package) %>%
  tally() %>%
  arrange(desc(n))

# daily total downloads
daily_count <-
  base_data %>%
  group_by(package) %>%
  tally() %>%
  pull(n) %>%
  sum()

daily_count

# countries
base_data %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n)) 

pkg_summary <- function(pkg_data , pkg_name) {
  
  pkg_data %>%
    select(package, country) %>%
    filter(package == pkg_name) %>%
    group_by(country) %>%
    tally() %>%
    arrange(desc(n))
  
}

pkg_summary(pkg_data, "olsrr")

pkg_summary(pkg_data, "rfm")

pkg_summary(pkg_data, "descriptr")

pkg_summary(pkg_data, "blorr")

pkg_summary(pkg_data, "inferr")

pkg_summary(pkg_data, "vistributions")

pkg_summary(pkg_data, "rbin")

pkg_summary(pkg_data, "xplorerr")

# stats
comp_total <- function(cran_data, n) {
  cran_data %>%
    purrr::map_dbl(n) %>%
    sum()
}

pkgs <- c("olsrr", "descriptr", "inferr", "rfm", "blorr", "vistributions", 
          "rbin", "xplorerr")
out   <- purrr::map(pkgs, pkginfo::get_cran_downloads) 
total <- comp_total(out, 4)
month_total <- comp_total(out, 3)
week_total <- comp_total(out, 2)
pkg_name <- tibble(pkg = pkgs)
overall <- cbind(pkg_name, purrr::map_df(out, dplyr::bind_rows))
overall %>%
  dplyr::arrange(desc(last_month))

# total count
annual  <- pull(tally(overall, total), n)
monthly <- pull(tally(overall, last_month), n)
weekly  <- pull(tally(overall, last_week), n)
tibble(annual, monthly, weekly)

# latest count
tally(overall, total) + daily_count
