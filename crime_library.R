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
  # clean
  type_name <- clean_text(type_name)
  # return
  type_name
}

extract_date_from_tr <- function( tr ){
  require( rvest )
  # extract the cell element from the table entry
  date_text_el <- rvest::html_nodes( x = tr, xpath = './/td[contains(@class,"field-crime-date")]' )
  # extract the text from the cell
  date_text <- rvest::html_text( date_text_el )
  # clean
  date_text <- clean_text(date_text)
  # convert to mdy_hm
  date_mdy_hm <- lubridate::mdy_hm(date_text)
  # return
  date_mdy_hm
}

#extract_street_from_tr <- function( tr ){
#  require( rvest )
#  # extract the cell element from the table entry
#  street_text_el <- rvest::html_nodes( x = tr, xpath = './/td[contains(@class,"field-street")]' )
#  # extract the text from the cell
#  street_text <- rvest::html_text( street_text_el )
#  # clean
#  street_text <- clean_text(street_text)
#  # return
#  street_text
#}

## Get stuff
url <- 'https://www.universalhub.com/crime/'
neighborhoods <- c('allston', 'back-bay', 'beacon-hill','brighton','charlestown',
                   'chinatown','dorchester','downtown','east-boston','fenway','hyde-park',
                   'jamaica-plain','mattapan','mission-hill','north-end',
                   'roslindale','roxbury','south-boston','south-end','west-roxbury')


## Creating the Tibble
all_crimes <- NULL
for(nbor in neighborhoods){
  # Append '.html' to all the places except mission hill
  testurl <- case_when(
    nbor != "mission-hill" ~ paste0(url,nbor,".html"),
    TRUE ~ paste0(url,nbor)
  )
  # Create tibble with extracted data
  nbor_text <- get_site_content(testurl) %>% 
    content_to_parsed_html() %>%
    extract_table_rows()
  
  neighborhood_crimes <-  tibble(Date = extract_date_from_tr(nbor_text), 
                                 Crime_Type = extract_type_from_tr(nbor_text), 
                                 Neighborhood = nbor) %>%
    mutate(Hour = lubridate::hour(Date))
  
  # Bind this to the overall tibble
  all_crimes <- bind_rows(all_crimes,neighborhood_crimes)
}

# Add the additional page for Dorchester
testurl <- paste0(url,"dorchester.html","?page=1")

nbor_text <- get_site_content(testurl) %>% 
  content_to_parsed_html() %>%
  extract_table_rows()

neighborhood_crimes <-  tibble(Date = extract_date_from_tr(nbor_text), 
                               Crime_Type = extract_type_from_tr(nbor_text), 
                               Neighborhood = "dorchester") %>%
  mutate(Hour = lubridate::hour(Date))

all_crimes <- bind_rows(all_crimes,neighborhood_crimes)

## Cleaning Crime Types
# See all the categories
unique(all_crimes$Crime_Type)

# There are 6 different spellings of "shooting". combine them:
shoot.idx <- which(all_crimes$Crime_Type %in% c("shooting","Shootin","Shoting","Shotting", " Shooting"))
all_crimes$Crime_Type[shoot.idx] <- "Shooting"

# One instance of misspelled "Assault"
assaul.idx <- which(all_crimes$Crime_Type == "Assaul")
all_crimes$Crime_Type[assaul.idx] <- "Assault"

# 6 instances without crime type
all_crimes$Crime_Type[all_crimes$Crime_Type == ""] <- "NA"

# 1 with an extra space in the name
all_crimes$Crime_Type[all_crimes$Crime_Type == " Illegal gun possession"] <- "Illegal gun possession"
