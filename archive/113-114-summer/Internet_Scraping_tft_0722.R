rm(list=ls())
library(rvest)

page_link = "https://tactics.tools/info/set-update"

# Exploration

## Parses it into a structured format (XML/ HTML document)

tft_page = read_html(page_link) ### This does not run javascript

## This returns "externalptr"

str(tft_page)

## Excracts all visible text from the parsed HTML doc

html_text(tft_page)

# It turns out that this method only extracts static and plain words. Not dynamic.





