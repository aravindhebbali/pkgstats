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


cran_downloads("inferr", when = "last-week")
cran_downloads("descriptr", when = "last-week")
cran_downloads("rfm", when = "last-week")
cran_downloads("olsrr", when = "last-week")
cran_downloads("blorr", when = "last-week")

# last day
cran_downloads("olsrr", when = "last-day")

# last week
cran_downloads(c("olsrr", "blorr", "rfm", "descriptr", "inferr"),
               when = "last-week") %>%
  select(count) %>%
  sum()

cran_downloads(c("olsrr", "blorr", "rfm", "descriptr", "inferr"),
               when = "last-month") %>%
  select(count) %>%
  sum()


# total olsrr
release_date <- as_date("2017-05-01")
recent_date  <- today() - 1
cran_downloads("olsrr", from = release_date, to = recent_date) %>%
  select(count) %>%
  sum()



check_results <- function(package_name) {

  if (!is_online()) {
    stop("Please ensure that you are connected to the internet.", call. = FALSE)
  }

  url <- glue(
    "https://cran.r-project.org/web/checks/check_results_", package_name, ".html"
  )

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

  pkg_name <- glue("/repos/", repo)
  url      <- modify_url("https://api.github.com", path = pkg_name)
  resp     <- GET(url)
  fromJSON(content(resp, "text"), simplifyVector = FALSE)

}

pkg_github_stars <- function(repo) {

  result <- pkg_github(repo)
  result$stargazers_count

}

pkg_open_issues <- function(repo) {

  result <- pkg_github(repo)
  result$open_issues_count

}


# build passing
# - travis ci (api)
pkg_travis_status <- function(package_name) {

  pkg_name <- glue("repos/rsquaredacademy/", package_name)
  url      <- modify_url("https://api.travis-ci.org", path = pkg_name)
  resp     <- GET(url)

  content(resp, "parsed") %>%
    as_list() %>%
    use_series('Projects') %>%
    use_series('Project') %>%
    attributes() %>%
    use_series('lastBuildStatus') %>%
    extract(1)

}



# appveyor (api)
pkg_appveyor_status <- function(package_name) {

  pkg_name <- glue("/api/projects/rsquaredacademy/", package_name)
  url      <- modify_url("https://ci.appveyor.com", path = pkg_name)
  resp     <- GET(url)
  result   <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  result$build$status

}


# code coverage (api)
pkg_code_coverage <- function(package_name) {

  pkg_name <- glue("/api/gh/rsquaredacademy/", package_name)
  url      <- modify_url("https://codecov.io", path = pkg_name)
  resp     <- GET(url)
  result   <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  result$commit$totals$c

}


# twitter mentions (api)

# stack overflow questions (api)

# reddit questions (api)



library(readr)
library(dplyr)
library(lubridate)
library(glue)

yday      <- today() - 1
base_url  <- "http://cran-logs.rstudio.com/2018/"
today_url <- glue(base_url, as.character(yday), ".csv.gz")
pkg_data  <- read_csv(today_url)


pkg_data %>%
  select(package) %>%
  filter(package == "olsrr" | package == "descriptr" |
           package == "inferr" | package == "rfm" |
           package == "blorr") %>%
  group_by(package) %>%
  tally() %>%
  arrange(desc(n))

pkg_data %>%
  select(package, country) %>%
  filter(package == "olsrr" | package == "descriptr" |
           package == "inferr" | package == "rfm" |
           package == "blorr") %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n))

pkg_data %>%
  select(package, country) %>%
  filter(package == "olsrr") %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n))

pkg_data %>%
  select(package, country) %>%
  filter(package == "rfm") %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n))

pkg_data %>%
  select(package, country) %>%
  filter(package == "descriptr") %>%
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
  filter(package == "inferr") %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n))
