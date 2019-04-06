#' @importFrom RCurl url.exists
#' @importFrom magrittr %>%
pst_check_date <- function(date) {
  date %>%
    pst_prep_url() %>%
    url.exists()
}

#' @importFrom lubridate year
pst_prep_url <- function(date) {
  base_url  <- "http://cran-logs.rstudio.com/"
  year      <- year(date)
  paste0(base_url, "/", year, "/", date, ".csv.gz")
}

#' @importFrom readr read_csv
pst_get_data <- function(date) {
  if (pst_check_date(date)) {
    date %>%
      pst_prep_url() %>%
      read_csv()
  }
}

pst_get_downloads <- function(data, pkgname) {
  data %>%
    select(package, country) %>%
    filter(package == pkgname)
}
