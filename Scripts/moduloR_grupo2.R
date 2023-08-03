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
balance<-balance_2014 %>%
  rename(CODIGO= ciiu4_nivel1) %>% 
  mutate(liquidez_corriente=v345/v539,endeu_activo=v599/v499,
         ende_patrimo=v599/v698,ende_activo_fijo=v698/v498, 
         apalancamiento=v499/v698) %>% 
  select(nombre_cia, situacion, tipo, pais, provincia, canton, ciudad,
         CODIGO,ciiu4_nivel6,trab_direc,trab_admin,
         liquidez_corriente,endeu_activo,ende_patrimo,
         ende_activo_fijo,apalancamiento) %>% 
  merge(ciiu, by=("CODIGO")) %>% rename(actividad=DESCRIPCION)%>%
  select(!c(CODIGO,NIVEL)) %>%  rename(CODIGO=ciiu4_nivel6) %>% 
  merge(ciiu, by=("CODIGO")) %>% rename(subactividad=DESCRIPCION)%>%
  select(!c(CODIGO,NIVEL)) %>% 
  mutate(actividad=as.factor(actividad),
         subactividad=as.factor(subactividad),
         situacion=as.factor(situacion),
         tipo=as.factor(tipo)) %>% 
  view()

##depurar la base
cantidad_de_NA <- sum(is.na(balance))

## total de empresas por actividad Y  canton
balance %>% group_by(actividad,canton) %>% summarise(total_empresas=n()) %>% 
  view("base2")

## total empresas por canton 
balance %>% group_by(canton) %>% summarise(total_empresas=n()) %>% 
  view("base2")


levels(as.factor(balance$situacion))
levels(as.factor(balance$provincia))
## Gráficamente muestra el comparativo de los indicadores financieros de liquidez y
## solvencia por Status y provincia.
attach(balance)

azuay<-balance %>% filter(provincia=="AZUAY")

ggplot(balance, aes(y=liquidez_corriente, fill = situacion)) +
  geom_histogram() +   geom_density()+
  labs(title = "Comparativo de Liquidez por Provincia y Status",
       x = "Provincia", y = "Valor") +
  facet_wrap(~provincia)+
  theme(legend.position = "none")
