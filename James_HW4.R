require(tidyverse)
require(lubridate)

## FUNCTIONS TO GET THE XML PARSED HTML FROM SITE
get_site_content <- function( url ){
  require( httr )
  # get the site response
  response <- httr::GET( url )
  # extract the content
  content <- httr::content( x = response, as = 'text', encoding = 'utf-8' )
  # return 
  return( content )
}

content_to_parsed_html <- function( content ){
  require( xml2 )
  # parse the html with xml2
  parsed_html <- xml2::read_html( content )
  # return
  return( parsed_html )
}

## Get stuff
url <- 'https://www.universalhub.com/crime/'
neighborhoods <- c('allston', 'back-bay', 'beacon-hill','brighton','charlestown',
                   'chinatown','dorchester','downtown','east-boston','fenway','hyde-park',
                   'jamaica-plain','mattapan','mission-hill','north-end',
                   'roslindale','roxbury','south-boston','south-end','west-roxbury')

## Function to get rid of leading '\n' and trailing white spaces
clean_text <- function(text) {
  gsub("^[\n]|[ \t]+$","",text)
}

## Extract Type/Hour/Neighborhood Functions
extract_table_rows <- function( parsed_html ){
  require( rvest )
  # extract the tbody element(s)
  trs <- rvest::html_nodes( x = parsed_html, xpath = '//tbody/tr' )
  # return 
  trs
}

extract_type_from_tr <- function( tr ){
  require( rvest )
  # extract the cell element from the table entry
  type_name_el <- rvest::html_nodes( x = tr, xpath = './/td[contains(@class,"field-name")]' )
  # extract the text from the cell
  type_name <- rvest::html_text( type_name_el )
  # return
  type_name
}

extract_date_from_tr <- function( tr ){
  require( rvest )
  # extract the cell element from the table entry
  date_text_el <- rvest::html_nodes( x = tr, xpath = './/td[contains(@class,"field-crime-date")]' )
  # extract the text from the cell
  date_text <- rvest::html_text( date_text_el )
  # convert to mdy_hm
  date_mdy_hm <- lubridate::mdy_hm(date_text)
  # return
  date_mdy_hm
}

extract_neighborhood_from_tr <- function( tr ){
  require( rvest )
  # extract the cell element from the table entry
  neighborhood_text_el <- rvest::html_nodes( x = tr, xpath = './/td[contains(@class,"field-street")]' )
  # extract the text from the cell
  neighborhood_text <- rvest::html_text( neighborhood_text_el )
  # return
  neighborhood_text
}

gsub("^[\n]|[ \t]+$","",all_crimes[1,3])

# Tibble
all_crimes <- NULL
for(neighborhood in neighborhoods){
  testurl <- paste0(url,neighborhood,".html")
  neighborhood_crimes <- get_site_content(testurl) %>% 
    content_to_parsed_html() %>%
    extract_table_rows() %>%
    tibble(Date = extract_date_from_tr(.), 
           Crime_Type = extract_type_from_tr(.), 
           Neighborhood = extract_neighborhood_from_tr(.)) %>%
    mutate(Hour = lubridate::hour(Date))
  all_crimes <- bind_rows(all_crimes,neighborhood_crimes)
}

#What are the five most common crime types (aggregated across neighborhoods and hours), 
#and how many of each such crime occurred? Be alert for misspellings!
