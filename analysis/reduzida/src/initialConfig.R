rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)
paleta_its <- c("#A3248B","#54318C","#255C8E",
                "#00A6B7",'#124559', "#8a8a8a", "#6c6c6c", 
                "#323232", "#adadad", "#E1F1FD", '#ff8e09')
`%nin%`<- Negate(`%in%`)

# Pacotes -----------------------------------------------------------------

library(corpus)
library(DBI)
library(RPostgreSQL)
library(emojifont)
library(gghighlight)
library(readr)
library(tidyverse)
library(readxl)
library(scales)
library(DT)
library(waffle)
library(dplyr)
library(tidytext)
library(stringr)
library(reshape2)
library(ggwordcloud)
library(ggpmisc)
library(xlsx)
library(data.table)
library(rtweet)
library(formattable)
library(kableExtra)
library(igraph)
library(tm)
library(ggraph)
library("htmltools")
library(webshot2)
library(extrafont)
loadfonts()
library(showtext)
library(ggtext)
library(wordcloud2)
font_add(family = "FontAwesome5Free-Solid", regular = "~/Downloads/fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", regular = "~/Downloads/fa-regular-400.ttf")
font_add(family = "FontAwesome5Brands-Regular", regular = "~/Downloads/fa-brands-400.ttf")
showtext_auto()


.export_formattable <- function(f, file, width = "40%", height = NULL, 
                                background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

font_add_google("Open Sans", "Open Sans")

textprocessing <- function(x)
{ 
  x <- gsub("(f|ht)tp(s?)://\\S+", ' ', x, perl = T) ## Remove URLs
  x <- gsub('[^[:graph:]]', ' ', x, text, perl = F)
  x <- gsub('\\b+rt', ' ', x) ## Remove RT
  x <- gsub('#\\S+', ' ', x) ## Remove Hashtags
  x <- gsub('@\\S+', ' ', x) ## Remove Mentions
  x <- gsub('[[:cntrl:]]', ' ', x) ## Remove Controls and special characters
  x <- gsub("\\d", ' ', x) ## Remove Controls and special characters
  x <- gsub('[[:punct:]]', ' ', x) ## Remove Punctuations
  x <- gsub("^[[:space:]]*","", x) ## Remove leading whitespaces
  x <- gsub("[[:space:]]*$","", x) ## Remove trailing whitespaces
  x <- gsub(' +',' ', x) ## Remove extra whitespaces
}
