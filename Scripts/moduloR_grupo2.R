#Cargar librerias
library(openxlsx)
library(magrittr)
library(tidyverse)
library(tibble)
library(tidyr)

#Cargar bases
balance_2014<-read.xlsx("Data/balances_2014.xlsx")
ciiu<-read.xlsx("Data/ciiu.xlsx")
cias<-read.xlsx("Data/cias_codebook.xlsx")

balance_2014<-tibble(balance_2014)
str(balance_2014)

#Crear la base
balance_2014 %>%
  rename(actividad_eco= ciiu4_nivel1,subactividad=ciiu4_nivel6) %>% 
  left_join(ciiu,by=c("actividad_eco"="CODIGO")) %>% view()
  
  mutate(liquidez_corriente=v345/v539     )
  select(nombre_cia, situacion, tipo, pais, provincia, canton, ciudad,
         actividad_eco,subactividad)
