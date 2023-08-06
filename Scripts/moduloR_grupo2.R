#Cargar librerias
library(openxlsx)
library(magrittr)
library(tidyverse)
library(tibble)
library(tidyr)
library(ggplot2)

#Cargar bases
balance_2014<-read.xlsx("Data/balances_2014.xlsx")
ciiu<-read.xlsx("Data/ciiu.xlsx")
cias<-read.xlsx("Data/cias_codebook.xlsx")

balance_2014<-tibble(balance_2014)
str(balance_2014)

#Crear la base

balance<-
  balance_2014 %>%
  inner_join(ciiu, by=c("ciiu4_nivel1"="CODIGO")) %>% 
  inner_join(ciiu, by=c("ciiu4_nivel6"="CODIGO"))%>% 
  mutate(liquidez_corriente=v345/v539,endeu_activo=v599/v499,
         ende_patrimo=v599/v698,ende_activo_fijo=v698/v498, 
         apalancamiento=v499/v698) %>% 
  rename(actividad=DESCRIPCION.x,
         subactividad=DESCRIPCION.y) %>% 
  select(nombre_cia, situacion, tipo, pais, provincia, canton, ciudad,fecha_const,
         tamanio, actividad ,subactividad,trab_direc,trab_admin,
         liquidez_corriente,endeu_activo,ende_patrimo,
         ende_activo_fijo,apalancamiento) %>% 
  mutate(actividad=as.factor(actividad),
         subactividad=as.factor(subactividad),
         situacion=as.factor(situacion),
         tipo=as.factor(tipo)) %>%
  view()


## total de empresas por actividad Y  canton
empresas_por_canton <- balance %>% group_by(actividad,canton) %>% summarise(total_empresas=n()) %>% 
  view("base2")

## total empresas por canton 
balance %>% group_by(canton) %>% summarise(total_empresas=n()) %>% 
  view("base2")


##depurar la base
cantidad_de_NA <- sum(is.na(balance))
balance<-balance[is.finite(balance$liquidez_corriente), ] %>% view()
balance<-balance[is.finite(balance$apalancamiento), ] %>% view()
variables<-c("liquidez_corriente","apalancamiento","endeu_activo","ende_patrimo",
             "ende_activo_fijo")
finitos <- function(data, columna) {
  data_filtrado <- data[is.finite(data[[columna]]), ]
  return(data_filtrado)
}

balance<-finitos(balance,"endeu_activo") 
balance<-finitos(balance,"ende_patrimo") 
balance<-finitos(balance,"ende_activo_fijo") 
colSums(is.na(balance))
base1 <- na.omit(balance)
colSums(is.na(base1))

levels(as.factor(balance$situacion))
levels(as.factor(balance$provincia))

## Gráficamente muestra el comparativo de los indicadores financieros de liquidez y
## solvencia por situacion y provincia.
attach(balance)

activas<-balance %>% filter(situacion=="ACTIVA")
impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}
activas$liquidez_corriente<-impute_outliers(activas$liquidez_corriente)

plot(activas$liquidez_corriente)
ggplot(activas, aes(x=fecha_const, y=liquidez_corriente, color = provincia, group=provincia))   +
  geom_line() +
  geom_hline(data = activas %>% group_by(provincia) %>% summarise(ymin = quantile(liquidez_corriente, 0.25), ymed = median(liquidez_corriente), ymax = quantile(liquidez_corriente, 0.75)),
             aes(yintercept = ymin), linetype = "dashed", color = "red") +
  geom_hline(data = activas %>% group_by(provincia) %>% summarise(ymin = quantile(liquidez_corriente, 0.25), ymed = median(liquidez_corriente), ymax = quantile(liquidez_corriente, 0.75)),
             aes(yintercept = ymed), linetype = "dashed", color = "purple") +
  geom_hline(data = activas %>% group_by(provincia) %>% summarise(ymin = quantile(liquidez_corriente, 0.25), ymed = median(liquidez_corriente), ymax = quantile(liquidez_corriente, 0.75)),
             aes(yintercept = ymax), linetype = "dashed", color = "black") +
  labs(title = "Comparativo de Indicadores de Liquidez por situacion ACTIVA y provincia",
       x = "Fecha constitución",
       y = "Liquidez") +
  facet_wrap(~provincia)+
  theme_minimal()+
  theme(legend.position = "none")


activas$apalancamiento<-impute_outliers(activas$apalancamiento)
plot(activas$apalancamiento)
ggplot(activas, aes(x=fecha_const, y=apalancamiento, color = provincia, group=provincia))   +
  geom_line() +
  geom_hline(data = activas %>% group_by(provincia) %>% summarise(ymin = quantile(apalancamiento, 0.25), ymed = median(apalancamiento), ymax = quantile(apalancamiento, 0.75)),
             aes(yintercept = ymin), linetype = "dashed", color = "red") +
  geom_hline(data = activas %>% group_by(provincia) %>% summarise(ymin = quantile(apalancamiento, 0.25), ymed = median(apalancamiento), ymax = quantile(apalancamiento, 0.75)),
             aes(yintercept = ymed), linetype = "dashed", color = "purple") +
  geom_hline(data = activas %>% group_by(provincia) %>% summarise(ymin = quantile(apalancamiento, 0.25), ymed = median(apalancamiento), ymax = quantile(apalancamiento, 0.75)),
             aes(yintercept = ymax), linetype = "dashed", color = "black") +
  labs(title = "Comparativo de Indicadores de Apalancamiento por situacion ACTIVA y provincia",
       x = "Fecha constitución",
       y = "apalancamiento") +
  facet_wrap(~provincia)+
  theme_minimal()+
  theme(legend.position = "none")

#Gráficamente muestra el comparativo de los indicadores financieros de liquidez y
#solvencia por tipo de empresa.

ggplot(activas, aes(x=fecha_const, y=liquidez_corriente, color = tipo, group=tipo))   +
  geom_line() +
  geom_hline(data = activas %>% group_by(tipo) %>% summarise(ymin = quantile(liquidez_corriente, 0.25), ymed = median(liquidez_corriente), ymax = quantile(liquidez_corriente, 0.75)),
             aes(yintercept = ymin), linetype = "dashed", color = "red") +
  geom_hline(data = activas %>% group_by(tipo) %>% summarise(ymin = quantile(liquidez_corriente, 0.25), ymed = median(liquidez_corriente), ymax = quantile(liquidez_corriente, 0.75)),
             aes(yintercept = ymed), linetype = "dashed", color = "purple") +
  geom_hline(data = activas %>% group_by(tipo) %>% summarise(ymin = quantile(liquidez_corriente, 0.25), ymed = median(liquidez_corriente), ymax = quantile(liquidez_corriente, 0.75)),
             aes(yintercept = ymax), linetype = "dashed", color = "black") +
  labs(title = "Comparativo de Indicadores de Liquidez por situacion ACTIVA y tipo",
       x = "Fecha constitución",
       y = "Liquidez") +
  facet_wrap(~tipo)+
  theme_minimal()+
  theme(legend.position = "none")



ggplot(activas, aes(x=fecha_const, y=apalancamiento, color = tipo, group=tipo))   +
  geom_line() +
  geom_hline(data = activas %>% group_by(tipo) %>% summarise(ymin = quantile(apalancamiento, 0.25), ymed = median(apalancamiento), ymax = quantile(apalancamiento, 0.75)),
             aes(yintercept = ymin), linetype = "dashed", color = "red") +
  geom_hline(data = activas %>% group_by(tipo) %>% summarise(ymin = quantile(apalancamiento, 0.25), ymed = median(apalancamiento), ymax = quantile(apalancamiento, 0.75)),
             aes(yintercept = ymed), linetype = "dashed", color = "purple") +
  geom_hline(data = activas %>% group_by(tipo) %>% summarise(ymin = quantile(apalancamiento, 0.25), ymed = median(apalancamiento), ymax = quantile(apalancamiento, 0.75)),
             aes(yintercept = ymax), linetype = "dashed", color = "black") +
  labs(title = "Comparativo de Indicadores de Apalancamiento por situacion ACTIVA y tipo",
       x = "Fecha constitución",
       y = "Liquidez") +
  facet_wrap(~tipo)+
  theme_minimal()+
  theme(legend.position = "none")

#¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes?

micro_pequeña <- balance %>%
  filter(tamanio%in% c("MICRO", "PEQUEÑA")) %>%
  summarise(Promedio1 = mean(endeu_activo))
micro_pequeña

grande <- balance %>%
  filter(tamanio%in% c("GRANDE")) %>%
  summarise(Promedio2 = mean(endeu_activo))
grande

comparativo<- tibble(Tamaño=c("Micro+Pequeña","Grande"),
                     Media=c(micro_pequeña$Promedio1,grande$Promedio2)) %>% view()


ggplot(comparativo, aes(x = Tamaño, y = Media, fill = Tamaño)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Endeudamiento del Activo entre Micro + Pequeña y Grande",
       x = "Tamaño de Empresa", y = "Valor de Endeudamiento del Activo") +
  theme_minimal()

#¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
#60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?

levels(as.factor(balance$tipo))
liquidez_compania<- balance %>% select(tipo,trab_direc,trab_admin,liquidez_corriente) %>% 
  filter(trab_direc>60,trab_admin>=100,trab_admin<=800) 

liquidez_compania %>% group_by(tipo)%>% 
  summarise(Promedio3 = mean(liquidez_corriente))

ggplot(liquidez_compania, aes(x = tipo, y = liquidez_corriente, fill = tipo)) +
  geom_jitter(alpha=1,color="gray")+
  geom_boxplot(alpha=0.1) +
  labs(title = "Comparación de Liquidez por Tipo de Compañía",
       x = "Tipo de Compañía", y = "Liquidez") +
  theme_minimal()+
  theme(legend.position = "none")

#Describe el top 10 de empresas con mayor apalancamiento.
balance %>% select(nombre_cia,apalancamiento) %>% 
  arrange(desc(apalancamiento)) %>% 
  head(10)