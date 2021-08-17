library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(sf) # Simple Features for R, CRAN v1.0-0
library(skimr) # Compact and Flexible Summaries of Data 
options(scipen = 999) # Eliminar notación cientifica


# ¿Cómo varió anualmente el precio de venta en USD de terrenos en CABA en el período 2018-2020?
# ¿Se puede evidenciar una caída causada por la pandemia?

barrios <- st_read("data/barrios/barrios_badata.shp") %>% 
  select(BARRIO, COMUNA)


#-----------------------------------------------------------
## TERRENOS 2020 

terrenos2020 <- st_read("data/terrenos-2020/210517_Terrenos_Vta_Anual2020.shp")
skim (terrenos2020)
# media m2 en USD: 2358
# mediana m2 en USD: 1918
# se evidencia sesgo positivo (distribución con cola a la derecha)
# desvío standard: 1600 USD

sum(is.na(terrenos2020))  #hay 6 instancias nulas que se eliminarán

terrenos2020 <- terrenos2020 %>% 
  drop_na() %>% #elimino instancias nulas
  select(DOLARM2) %>% #sólo me quedo con la columna de precio del m2 en USD
  st_join(barrios) #uno espacialmente con los barrios
  
head(terrenos2020)

### AGRUPADOS POR BARRIO

terrenos2020_barrio <- terrenos2020 %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>%
  drop_na() %>% 
  summarise(PROMEDIOUSD_2020=mean(DOLARM2)) #promedio por barrio

ggplot()+
  geom_bar(data=terrenos2020_barrio, aes(y=reorder(BARRIO, -PROMEDIOUSD_2020), weight=PROMEDIOUSD_2020, fill=BARRIO), show.legend = FALSE)+
  scale_fill_viridis_d()+
  labs(x="USD", 
       y="Barrio",
       title="Promedio m2 en 2020", 
       caption="Fuente: GCBA")+
  theme_minimal()


#-----------------------------------------------------------
## TERRENOS 2019  

terrenos2019 <- st_read("data/terrenos-2019/Terrenos_venta_2019.shp")
skim (terrenos2019)
# media m2 en USD: 2686
# mediana m2 en USD: 2177
# se evidencia sesgo positivo (distribución con cola a la derecha)
# la media y la mediana son mayores en el período 2019 que el  2020. 
# el desvío standard también es mayor (1908 USD), indicando mayor dispensión de valores

sum(is.na(terrenos2019))  #en este caso hay 21 instancias nulas

terrenos2019 <- terrenos2019 %>% 
  select(OPERACION, PRECIOUSDM) %>% 
  drop_na () %>% 
  filter(OPERACION=="VTA") %>% 
  st_join(barrios)


### AGRUPADOS POR BARRIO

terrenos2019_barrio<- terrenos2019 %>% 
  as.data.frame() %>% 
  group_by(BARRIO) %>% 
  drop_na() %>% 
  summarise(PROMEDIOUSD_2019=mean(PRECIOUSDM))

ggplot()+
  geom_bar(data=terrenos2019_barrio, aes(y=reorder(BARRIO, -PROMEDIOUSD_2019), weight=PROMEDIOUSD_2019, fill=BARRIO), show.legend = FALSE)+
  scale_fill_viridis_d()+
  labs(x="USD", 
       y="Barrio",
       title="Promedio m2 en 2019", 
       caption="Fuente: GCBA")+
  theme_minimal()


#-----------------------------------------------------------
## TERRENOS 2018

terrenos2018 <- st_read("data/terrenos-2018/terrenos-valor-de-oferta.shp")
skim (terrenos2018)
# media m2 en USD: 2461
# mediana m2 en USD: 1958
# se evidencia sesgo positivo (distribución con cola a la derecha)
# la media y la mediana son menores que el 2019, pero mayores que el 2020
# se observa un crecimiento hacia 2019, con caída en el 2020
# el desvío standard es de 1878 USD
# no se registran terrenos en Puerto Madero

# aprovechando que los sets de datos de 2018 y 2019 comparten la misma estructura, voy a crear funciones para sistematizar los pasos previos

filtrado_terrenos <- function(df) {
  df <- df %>% 
    select(OPERACION, PRECIOUSDM) %>% 
    filter(OPERACION=="VTA") %>% 
    st_join(barrios)
}

agrupamiento_terrenos<- function(df) {
  df <- df %>% 
    as.data.frame() %>% 
    drop_na() %>% 
    group_by(BARRIO) %>% 
    summarise(PROMEDIOUSD_2018=mean(PRECIOUSDM))
}  

terrenos2018 <- terrenos2018 %>% 
  filtrado_terrenos()

terrenos2018_barrio <- terrenos2018 %>% 
agrupamiento_terrenos()

ggplot()+
  geom_bar(data=terrenos2018_barrio, aes(y=reorder(BARRIO, -PROMEDIOUSD_2018), weight=PROMEDIOUSD_2018, fill=BARRIO), show.legend = FALSE)+
  scale_fill_viridis_d()+
  labs(x="USD", 
       y="Barrio",
       title="Promedio m2 en 2018", 
       caption="Fuente: GCBA")+
  theme_minimal()


#-----------------------------------------------------------
## VARIACIÓN % ANUAL

terrenos <- left_join (terrenos2018_barrio, terrenos2019_barrio, by="BARRIO")
terrenos <- terrenos %>% 
  left_join (terrenos2020_barrio, by="BARRIO") %>% 
  mutate (VARIACION_18_19=((PROMEDIOUSD_2019/PROMEDIOUSD_2018)-1)*100,
         VARIACION_19_20=((PROMEDIOUSD_2020/PROMEDIOUSD_2019)-1)*100,
         VARIACION_18_20=((PROMEDIOUSD_2020/PROMEDIOUSD_2018)-1)*100)

# imprimo ordenado por mayor variación en los precios durante el período 2019-2020
print(terrenos[order(terrenos$VARIACION_19_20, decreasing= T),])


ggplot(terrenos)+
  geom_bar(aes(y=reorder(BARRIO, -VARIACION_19_20), weight=VARIACION_19_20, fill="Variación 2019-2020"), color="black", alpha=.8)+
  geom_bar(aes(y=BARRIO, weight=VARIACION_18_19, fill="Variación 2018-2019"), color="black", linetype="dashed", alpha=.7)+
  labs(x="USD", 
       y="Barrio",
       title="Variación porcentual de precios de los terrenos en venta",
       subtitle="Períodos 2018-2019, 2019-2020",
       caption="Fuente: GCBA",
       fill="Referencia")+
  geom_vline (xintercept = 0, linetype="dashed", size=1)+
  scale_fill_manual(values = c("gray", "brown4"))+
  theme_minimal()

# Se observa que la mayoría de los barrios sufrió una importante caída en el período 2019-2020 
# Es llamativo es que si observamos el período anterior (2018-2019), la tendencia pareciera revertirse
# Es decir que en general los barrios que habían experimentado una reciente variación positiva en el precio del suelo, sufieron una caída mayor y visceversa, a lo que a fines prácticos llamaremos "efecto rebote"


## BARRIOS QUE REGISTRAN EFECTO REBOTE (cambio de signo de variación interanual)

sum(terrenos$VARIACION_18_19 == 0 | terrenos$VARIACION_19_20==0) 
#hay 0 instancias con variación 0.00, por lo que la confición igual no será incluída

terrenos <- terrenos %>% 
  mutate(rebote = case_when(
    (VARIACION_18_19 < 0 & VARIACION_19_20 < 0) ~ "Nulo",
    (VARIACION_18_19 > 0 & VARIACION_19_20 > 0) ~ "Nulo",
    (VARIACION_18_19 < 0 & VARIACION_19_20 > 0) ~ "Positivo",
    (VARIACION_18_19 > 0 & VARIACION_19_20 < 0) ~ "Negativo"))

head(terrenos)

#veamoslo nuevamente
ggplot()+
  geom_bar(data=terrenos, aes(y=reorder(BARRIO, -VARIACION_19_20), weight=VARIACION_19_20, fill="Variación 2019-2020"), color="black", alpha=.8)+
  geom_bar(data=terrenos %>% filter (rebote=="Positivo"), 
           aes(y=BARRIO, weight=VARIACION_18_19, fill="Variación 2018-2019 \nRebote positivo"), color="black", linetype="dashed", alpha=.7)+
  geom_bar(data=terrenos %>% filter (rebote=="Negativo"), 
           aes(y=BARRIO, weight=VARIACION_18_19, fill="Variación 2018-2019 \nRebote negativo"), color="black", linetype="dashed", alpha=.7)+
  labs(x="USD", 
       y="Barrio",
       title="Variación porcentual de precios de los terrenos en venta",
       subtitle="Período 2019-2020 + Efecto rebote en variación interanual",
       caption="Se omiten los casos que continuaron con la misma tendencia \nFuente: GCBA",
       fill="Referencia")+
  geom_vline (xintercept = 0, linetype="dashed", size=1)+
  scale_fill_manual(values = c("indianred", "darkseagreen", "brown4"))+
  theme_minimal()
 
write.csv(terrenos, "data/terrenos.csv") #guardo la información generada para levantarla luego
