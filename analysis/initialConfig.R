rm(list = ls()) # Limpando o "Enviroment"
options(scipen = 999)
paleta_its <- c("#A3248B","#54318C","#255C8E","#00A6B7","#E1F1FD", "#8a8a8a", "#6c6c6c")
`%nin%`<- Negate(`%in%`)

# Pacotes -----------------------------------------------------------------

library(readr)
library(base)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(readxl)
library(scales)
library(DT)
library(waffle)
library(dplyr)
library(tidytext)
library(stringr)
library(reshape2)
library(ggwordcloud)
library(xlsx)
library(data.table)
library(rtweet)
