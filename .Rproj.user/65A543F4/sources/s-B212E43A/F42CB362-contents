library(rvest)
library(glue)
library(pingr)
library(rlang)
library(dplyr)
library(purrr)
library(tibble)
library(magrittr)
library(httr)
library(cranlogs)
library(gh)
library(magrittr)
library(xml2)

# downloads

cran_downloads("inferr", when = "last-week")
cran_downloads("descriptr", when = "last-week")
cran_downloads("rfm", when = "last-week")
cran_downloads("olsrr", when = "last-week")
cran_downloads("blorr", when = "last-week")

# last day
cran_downloads("olsrr", when = "last-day")

# last week
cran_downloads("olsrr", when = "last-week") %>%
  select(count) %>%
  sum()

# last month
cran_downloads("olsrr", when = "last-month") %>%
  select(count) %>%
  sum()

# total
release_date <- lubridate::as_date("2017-05-01")
recent_date  <- lubridate::today() - 1
cran_downloads("olsrr", from = release_date, to = recent_date) %>%
  select(count) %>%
  sum()


# cran check results
check_results <- function(package_name) {
  
  # check internet connection
  if (is_na(ping("www.google.com", count = 3L))) {
    stop("Please ensure that you are connected to the internet.", call. = FALSE)
  }
  
  # generate CRAN url
  url <- glue(
    "https://cran.r-project.org/web/checks/check_results_", package_name, ".html"
  )
  
  # results table
  read_html(url) %>%
    html_nodes("table") %>%
    html_table() %>%
    extract2(1)
}

check_results("olsrr")
package_name <- "available"
packages <- c("available", "dplyr", "inferr", "descriptr")
map_df(packages, check_results) %>%
  select(Flavor, Status) %>%
  filter(Status != "OK")

# github

pkg_github <- function(repo) {
  
  pkg_name <- paste0("/repos/", repo)
  url      <- httr::modify_url("https://api.github.com", path = pkg_name)
  resp     <- httr::GET(url)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  
}

pkg_github_stars <- function(repo) {
  
  result <- pkg_github(repo)
  result$stargazers_count
  
}

pkg_github_stars <- function(repo) {
  
  result <- pkg_github(repo)
  result$open_issues_count
  
}


# build passing 
# - travis ci (api)
pkg_travis_status <- function(package_name) {
  
  pkg_name <- paste0("repos/rsquaredacademy/", package_name)
  url      <- httr::modify_url("https://api.travis-ci.org", path = pkg_name)
  resp     <- httr::GET(url)
  
  httr::content(resp, "parsed") %>%
    xml2::as_list() %>%
    use_series('Projects') %>%
    use_series('Project') %>%
    attributes() %>%
    use_series('lastBuildStatus') %>%
    extract(1)
  
}



# appveyor (api)
pkg_appveyor_status <- function(package_name) {
  
  pkg_name <- paste0("/api/projects/rsquaredacademy/", package_name)
  url      <- httr::modify_url("https://ci.appveyor.com", path = pkg_name)
  resp     <- httr::GET(url)
  result   <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  result$build$status
  
}


# code coverage (api)
pkg_code_coverage <- function(package_name) {
  
  pkg_name <- paste0("/api/gh/rsquaredacademy/", package_name)
  url      <- httr::modify_url("https://codecov.io", path = pkg_name)
  resp     <- httr::GET(url)
  result   <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  result$commit$totals$c
  
}


# twitter mentions (api)

# stack overflow questions (api)

# reddit questions (api)

