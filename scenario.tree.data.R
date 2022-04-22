# Codigo juntar vigilancia db 
# Estimacion de la sensibilidad del sistema de vigilancia epidemiologica para PPC
#Alfredo Acosta
# alfredojavier55@gmail.com
# Phd Candidate University of Sao Paulo
# Preventive veterinary medicine department

library(dplyr); library(ggplot2); library(scales)
setwd("~/Dropbox/0.USP/1 Projeto/Conferir-dados")

#1 importando - modificando colnames de csv VEO por Rep_csv ----
# Estos arquivos tem os colnames certos
vge0 <- read.csv("Rep_General_EventosSan0.csv", colClasses = "character",encoding = "UTF-8")
vr0 <- read.csv("Rep_ResultadosGEV0.csv", colClasses = "character",encoding = "UTF-8")
vc0 <- read.csv("Rep_CierreGEV0.csv", colClasses = "character",encoding = "UTF-8")

#Estos arquivos tem os colnames errados, mas a informacao de cedula certa (baixados desde o sizse 11.12.2019)
vge <- read.csv("VEO1_G.csv", colClasses = "character", encoding = "UTF-8")
vr <- read.csv("VEO1_R.csv", colClasses = "character", encoding = "UTF-8")
vc <- read.csv("VEO1_C.csv", colClasses = "character",encoding = "UTF-8")

colnames(vge) <- colnames(vge0)
colnames(vr) <- colnames(vr0)
colnames(vc) <- colnames(vc0)

rm(vge0, vr0, vc0)

#2 acoplando bancos ----
#Banco Reporte geral de eventos
vge$canton <- vge$cantón
vge$cantón <- NULL

#Banco Resultados de eventos
vr$cant_muestras <- as.numeric(vr$cant_muestras)
vr$positivos <- as.numeric(vr$positivos)
vr$negativos <- as.numeric(vr$negativos)
vr$reactivos <- as.numeric(vr$reactivos)
vr$indeterminados <- as.numeric(vr$indeterminados)
vr$canton <- vr$cantón
vr$cantón <- NULL

# Banco Cierre de eventos
vc$existentes <- as.numeric(vc$existentes)
vc$enfermos <- as.numeric(vc$enfermos)
vc$muertos <- as.numeric(vc$muertos)
vc$sacrificad <- as.numeric(vc$sacrificad)
vc$canton <- vc$cantón
vc$cantón <- NULL

#3 Linkage dos reportes ----
v0 <- full_join(vc, vr)
v1 <- full_join(v0,vge)

# Filtro para notificaciones y casos
# para casos, activar filtros de diagnostico

table(v2$detalle_diagnóstico)
table(v2$prueba_solicitada)

v2 <- v1 %>%
  filter(especie == "PORCINOS")%>%
  filter(detalle_diagnóstico == "peste porcina clasica")%>% #filtering to see the general surveillance
  filter(prueba_solicitada != "peste porcina clásica (ac.)") %>% #filtering to see the general surveillance
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta, prueba_solicitada, detalle_diagnóstico)%>%
  summarise(existente=sum(existentes), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), 
            indeterm=sum(indeterminados),reactivo=sum(reactivos))

length(unique(v2$orden))
# 1652 eventos hasta 11/2020
# 1882 incluido 10/2021

#Usei código para importar diferente devo voltar a pasta com arquivos
setwd("~/Dropbox/0.USP/2 Projeto graduação/Projeto/Caraterização")

#4 base de vigilancia antes do size (base excel, arquivo pessoal do projeto ppc) ----
as <- read.csv("bd-2014-presizse.csv", colClasses = "character",
               encoding = "UTF-8")

as$existente <- as.numeric(as$existente)
as$mortos <- as.numeric(as$mortos)
as$sacrifi <- as.numeric(as$sacrifi)
as$afetados <- as.numeric(as$afetados)
as$enfermo <- as.numeric(as$enfermo)
as$pos <- as.numeric(as$pos)
as$total_muestras <- as.numeric(as$total_muestras)
as$indeterm <- as.numeric(as$indeterm)
as$reactivo <- as.numeric(as$reactivo)
as$prueba_solicitada <- as.character(NA)
as$detalle_diagnóstico <-as.character(NA)

as <- as[,c(1:31,41,42,32:40)]
all_equal(v2,as)

colnames(as)
colnames(v2)
v2 <- data.frame(v2)

v2 <- rbind(v2,as)

library(lubridate)
v2$ano <- year(dmy(v2$f_1er_enfermo)) #22/03/2017
v2$ano <- year(ymd(v2$f_1er_enfermo)) #22/03/2017 to use with new 2021
v2$month <- month(ymd(v2$f_1er_enfermo))

# 1757 eventos 
# 1074

# I am deleting a case that is duplicated
v2 <- v2[v2$orden != "1",] #0700860679 loja paltas casamba
# Deleting 4 bovine registries
v2 <- v2[v2$orden != "2382",] 
v2 <- v2[v2$orden != "1096",] 
v2 <- v2[v2$orden != "4114",] 
v2 <- v2[v2$orden != "6270",] 
v2 <- v2[v2$orden != "-9",] #industrial PED don diego
# v2 <- v2[v2$orden != "-90",] #EVENTO CAMAL deleted because of no origin
# v2 <- v2[v2$orden != "980",] #EVENTO CAMAL ECOLOGICO DEL COLTA

length(unique(v2$orden))
#1759 com atualizacao 10/10/2020 todas as patologias
#1990 com atualizacao 10/10/2021 todas as patologias

# 5 -- Vigilancia geral suinos ----
# numero de notificaciones
library(lubridate)
# Day of notification
v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$month <- floor_date(v2$f_1er_enfermo, "month")

v2 %>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)))

v2 %>%
  group_by(ano)%>%
  filter(ano >"2013")%>%
  summarise(notifi=length(unique(orden)))

library(RColorBrewer)
display.brewer.pal(n=8,name="Set1")
brewer.pal(n=8,name="Set1")
show_col(viridis_pal()(4))


v2 %>%
  group_by(month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            diagnosticados=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(month,notifi_geral), fill="#377EB8")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Vigilancia geral PSC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

#6 Using the case-control db ----
setwd("~/Dropbox/0.USP/7.Publicações/Risk factors and space-time analysis associated with presentation of Classical Swine Fever in Ecuador/")
v2 <- read.csv(file = "base-16.9.21.csv", colClasses = "character")

v2$f_1er_enfermo <- ymd(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$month <- floor_date(v2$f_1er_enfermo, "month")
v2$Month <- month(v2$month)


#7 -- Vigilancia especifica ----
# Numero de casos e notificacoes ----
v2 %>%
  group_by(ano)%>%
  # filter(ano == "2020")%>%
  summarise(notifi_PPC=length(unique(orden)),
            surtos_PPC=sum(pos >= 1, na.rm = TRUE),
            Per=surtos_PPC/notifi_PPC)


#< Fig.2 Distribution of events ----
# Eventos discriminated by case and control, cat
v2 %>%
  group_by(month)%>%
  filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_PPC=n())%>%
  ggplot()+
  geom_col(aes(month,notifi_PPC), fill="#377EB8")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificações PSC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

v2 %>%
  group_by(month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_PPC=n(),
            surtos_PPC=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(month,surtos_PPC), fill="#377EB8")+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos confirmados PSC",
       x="Meses")+
  theme_minimal()+
  theme(text = element_text(size = 14))

v2 %>%  
  group_by(month, Type=fcaso) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(month, Number_of_events), fill="#377EB8")+
  facet_grid(rows = vars(Type))+
  # facet_grid(rows = vars(Type), scales = "free_y")+
  labs(y="Number of events", 
       x="Date of start of the event (month)",
       fill="Herd type:")+
  theme_light()+
  theme(text = element_text(size = 14),
        axis.text.y =element_text(size=12),
        axis.text.x =element_text(size=12),
        legend.position = "top",
        legend.text = element_text(size=10),
        legend.key.size = unit(4, "mm"))+
  scale_fill_viridis_d(direction=-1)


# Number of notifications by type of notificator
v2 %>%
  group_by(fnotificador)%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  summarise(notifi=n(), 
            surto = sum(as.numeric(caso), na.rm = TRUE)) 

1 1Owner          343    +75
2 2Agrocalidad     29     +5
3 3Sensor         179    +54



# Vigilancia clinica casos ----
# (usar v2 del caso control)
v2 %>%
  group_by(ano)%>%
  filter(ano >2014)%>%
  filter(caso== "1")%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  summarise(T_notifi=length(unique(orden)), 
            poblacion=sum(as.numeric(populacao)),
            # surtos=length(unique(subset(orden, pos>=1))),
            suinos_amostrados=sum(as.numeric(total_muestras), na.rm = TRUE),
            positivos=sum(as.numeric(pos)),
            por_positi=positivos/suinos_amostrados)

# Vigilancia clinica especifica totales
(usar v2 caso control)
v2 %>%
  group_by(ano)%>%
  filter(ano >2014)%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  summarise(T_notifi=length(unique(orden)), 
            poblacion=sum(as.numeric(populacao)),
            # surtos=length(unique(subset(orden, pos>=1))),
            suinos_amostrados=sum(as.numeric(total_muestras), na.rm = TRUE),
            por_amostragem=)



