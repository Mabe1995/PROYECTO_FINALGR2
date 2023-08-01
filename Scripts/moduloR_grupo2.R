#Cargar librerias
library(openxlsx)
library(magrittr)
library(tidyverse)
library(tibble)
library(tidyr)

#Cargar bases
balance_2014<-read.xlsx("Data/balances_2014.xlsx")
balance_2014<-tibble(balance_2014)
str(balance_2014)

#Crear la base
#balance_2014 %>% select(nombre_cia,)