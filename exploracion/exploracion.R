library(tidyverse)
library(Amelia)
library(rstatix)

data_features <- read_csv("../data/training_set_features.csv")
data_labels <- read_csv("../data/training_set_labels.csv")
data <- data_features %>% bind_cols(data_labels)

colnames(data)

Amelia::missmap(data)

sum(is.na(data$h1n1_concern))

#La clase h1n1_vaccine está desbalanceada. 75% de las filas son no vacunadas
data %>% count(h1n1_vaccine)
data %>% ggplot(aes(x=h1n1_vaccine,fill=h1n1_vaccine)) + geom_bar()
#La clase seasonal no está desbalanceada
data %>% ggplot(aes(x=seasonal_vaccine)) + geom_bar()
# Muy relevante relaciones seasonal_h1n1. Si tiene la h1n1 es muy probable que tenga la seasonal (no al revés)
data %>% group_by(seasonal_vaccine,h1n1_vaccine) %>% count()

data %>% fill("Nulo")

#h1n1_concern variable ordinal Level of concern about the H1N1 flu.
#0 = Not at all concerned; 
#1 = Not very concerned; 
#2 = Somewhat concerned; 
#3 = Very concerned.
#En general la gente no vacunada suele sudar más pero no tampoco hay enormes diferencias
sum(is.na(data$h1n1_concern))
data %>% ggplot(aes(x=h1n1_concern, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=h1n1_concern, fill=as.factor(seasonal_vaccine))) + geom_bar()



#h1n1_knowledge level of knowledge about h1n1 flu. Variable ordinal
#0 No knowledge
# 1 A little knowledge
# 2 a lot of knowledge
# EL no tener ningún conocimiento afecta, entre tenerun poco y muchisimo no hay gran diferencia
# Pocos nulos
sum(is.na(data$h1n1_knowledge))
data %>% ggplot(aes(x=h1n1_knowledge, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=h1n1_knowledge, fill=as.factor(seasonal_vaccine))) + geom_bar()

#behavioral_antiviral_meds. Si ha tomado medicacioń antiviral Variable binaria
# Se puede despreciar completamente para la seasonal
# Tambien despreciable para h1n1 aunque menos
sum(is.na(data$behavioral_antiviral_meds))
data %>% ggplot(aes(x=behavioral_antiviral_meds, fill=as.factor(h1n1_vaccine))) + geom_bar()

data %>% ggplot(aes(x=behavioral_antiviral_meds, fill=as.factor(seasonal_vaccine))) + geom_bar()

# behavioral face mask. Si se ha comprado una mascarilla
#Algo de información en h1n1_vaccine
# Poca información en seasonal
sum(is.na(data$behavioral_face_mask))
data %>% ggplot(aes(x=behavioral_face_mask, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=behavioral_face_mask, fill=as.factor(seasonal_vaccine))) + geom_bar()



# behavioral wash hands. Si se lava frecuentemente las manos
# Relevante en h1n1 
# Relativamente irrelevante para la seasonal
sum(is.na(data$behavioral_wash_hands))
data %>% ggplot(aes(x=behavioral_wash_hands, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=behavioral_wash_hands, fill=as.factor(seasonal_vaccine))) + geom_bar()

# Behavioral large gatherings, si ha reducido su presencia en eventos mayores
#Irrelevante en h1n1
#Levemente relevante en seasonal pero tmpoco divide claramente
sum(is.na(data$behavioral_large_gatherings))
data %>% ggplot(aes(x=behavioral_large_gatherings, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=behavioral_large_gatherings, fill=as.factor(seasonal_vaccine))) + geom_bar()



# Behavioral outside home, Si ha reducido el contacto con gente fuera de la casa
#No divide claramente en h1n1
#NO divide claramente en seasonal
sum(is.na(data$behavioral_outside_home))
data %>% ggplot(aes(x=behavioral_outside_home, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=behavioral_outside_home, fill=as.factor(seasonal_vaccine))) + geom_bar()


# Behavioral touch face,Si evita tocarse los ojos la nariz o la boca. Binaria
#No divide claramente en h1n1
#NO divide claramente en seasonal
sum(is.na(data$behavioral_touch_face))
data %>% ggplot(aes(x=behavioral_touch_face, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=behavioral_touch_face, fill=as.factor(seasonal_vaccine))) + geom_bar()

# En general las variables de comportamiento no dan información clara

#Doctor recc h1n1. Biriable si el doctor recomendó la vacuna
#Divide bien la h1n1
#Divide bien la seasonal_vaccine
#2160 nulos
sum(is.na(data$doctor_recc_h1n1))
data %>% ggplot(aes(x=doctor_recc_h1n1, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=doctor_recc_h1n1, fill=as.factor(seasonal_vaccine))) + geom_bar()


#Igual que la anterior pero con la seasonal
#Divide bien la seasonal_vaccine pero no la h1n1
sum(is.na(data$doctor_recc_seasonal))
data %>% ggplot(aes(x=doctor_recc_seasonal, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=doctor_recc_seasonal, fill=as.factor(seasonal_vaccine))) + geom_bar()

#La mayoría de médicos que no recomiendan la seasonal tampoco recomeindan la h1n1
# ¿Nueva variable recomienda vacunas?
data %>% ggplot(aes(x=doctor_recc_seasonal, fill=as.factor(doctor_recc_h1n1))) + geom_bar()


#chronic_med_condition. Factor binario que indica si se tienen o no una enfermedad crónica
#No existe una diferencia clara aunque puede ser levemetne util en h1n1
#Relevante en seasonal_vaccine divide bien el
#971 nulos
sum(is.na(data$chronic_med_condition))



data %>% mutate(chronic_med_condition = replace(chronic_med_condition,
                                                is.na(chronic_med_condition),
                                                "Na")) %>%
                  ggplot(aes(x=chronic_med_condition, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% mutate(chronic_med_condition = replace(chronic_med_condition,
                                                is.na(chronic_med_condition),
                                                "Na")) %>%
  ggplot(aes(x=chronic_med_condition, fill=as.factor(seasonal_vaccine))) + geom_bar()


#child_under_6_months Factor binario que indica si tiene contacto diario con niños 
#Ligeramente relevante en h1n1
#Relevante en seasonal_vaccine divide bien el
# 820 nulos
sum(is.na(data$child_under_6_months))
data %>% ggplot(aes(x=chronic_med_condition, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=chronic_med_condition, fill=as.factor(seasonal_vaccine))) + geom_bar()


#health_worker si es trabajador de la salud
#Claramente diferenciador en seasonal_vaccine
#Ligerisamente relevante en h1n1_vaccine
sum(is.na(data$health_worker))
data %>% ggplot(aes(x=health_worker, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=health_worker, fill=as.factor(seasonal_vaccine))) + geom_bar()

#health_insurance si tiene seguro médico
#Elemento claramente diferenciador en h1n1. Los factores económicos parecen ser muy relevantes
#Parece que los Na también dividen bastante el conjunto de datos deberían de 
#tener un tratamiento especial
#Elemento también diferenciador en seasonal_vaccine pero sin tanta diferencia en NaN

sum(is.na(data$health_insurance))
data <- data %>% mutate(health_insurance = replace(health_insurance,
                                                  is.na(health_insurance),
                                                  "Na"))
data %>% ggplot(aes(x=health_insurance, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=health_insurance, fill=as.factor(seasonal_vaccine))) + geom_bar()

#opinion_h1n1_vacc_effective Si cree que la vacuna es efectiva
#Ordinal del 1 al 5 donde 1 es muy poco efectiva y 5 mucho
#Elemento claramente diferenciador en h1n1
#Diferencia también en la seasonal pero un poco menos.

sum(is.na(data$opinion_h1n1_vacc_effective))
data %>% ggplot(aes(x=opinion_h1n1_vacc_effective, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=opinion_h1n1_vacc_effective, fill=as.factor(seasonal_vaccine))) + geom_bar()

#opinion h1n1 risk. del 1 al 5 el riesgo de contagiarse no teniendo la vacuna
#Diferenciador en h1n1 pero puede inducir a error
#lgieramente diferencidaor en seasonal
sum(is.na(data$opinion_h1n1_risk))
data %>% ggplot(aes(x=opinion_h1n1_risk, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=opinion_h1n1_risk, fill=as.factor(seasonal_vaccine))) + geom_bar()


#opinion h1n1 sick from vacc. Preocupacion por pnoerse malo con la vacuna del 1 a l5 ordinal.
#Diferenciador en h1n1 pero puede inducir a error
#lgieramente diferencidaor en seasonal
#No discriminador
sum(is.na(data$opinion_h1n1_sick_from_vacc))
data %>% ggplot(aes(x=opinion_h1n1_sick_from_vacc, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=opinion_h1n1_sick_from_vacc, fill=as.factor(seasonal_vaccine))) + geom_bar()

#opinion seas_risk. del 1 al 5 el riesgo de contagiarse no teniendo la vacuna estacional
#Ligeramente diferenciador en h1n1
#claramente diferenciador en seasonal
sum(is.na(data$opinion_seas_risk_risk))
data %>% ggplot(aes(x=opinion_seas_risk, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=opinion_seas_risk, fill=as.factor(seasonal_vaccine))) + geom_bar()


#opinion seas_risk. del 1 al 5 efectividad de  la vacuna estacional
#Ligeramente diferenciador en h1n1
#Diferenciador leve en seas
sum(is.na(data$opinion_seas_vacc_effective))
data %>% ggplot(aes(x=opinion_seas_vacc_effective, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=opinion_seas_vacc_effective, fill=as.factor(seasonal_vaccine))) + geom_bar()


#opinion seas_sick_vacc. ordinal del 1 al 5
#Nada claro en h1n1
#Nada claro tampoco 
sum(is.na(data$opinion_seas_sick_from_vacc))
data %>% ggplot(aes(x=opinion_seas_sick_from_vacc, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=opinion_seas_sick_from_vacc, fill=as.factor(seasonal_vaccine))) + geom_bar()


#age group fatores ordinales
#Levemente diferenciador
#Enormemente diferenciador
sum(is.na(data$age_group))
data %>% ggplot(aes(x=age_group, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=age_group, fill=as.factor(seasonal_vaccine))) + geom_bar()


#education fatores ordinales
#Nada diferenciador
#Quizá leve por su asoicaición a la renta pero no al metería dentro
sum(is.na(data$education))
data %>% ggplot(aes(x=education, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=education, fill=as.factor(seasonal_vaccine))) + geom_bar()

#raza
#Puede ser interesante asocaido a renta
sum(is.na(data$race))
data %>% ggplot(aes(x=race, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=race, fill=as.factor(seasonal_vaccine))) + geom_bar()

#income_povertu. Income anual con respecot al cneso de 2008
#Levemente diferenciadora 
# em ambas
sum(is.na(data$income_poverty))
data %>% ggplot(aes(x=income_poverty, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=income_poverty, fill=as.factor(seasonal_vaccine))) + geom_bar()

#estado marital. nada claro
sum(is.na(data$income_poverty))
data %>% ggplot(aes(x=marital_status, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=marital_status, fill=as.factor(seasonal_vaccine))) + geom_bar()


#rent_or_own si alquila o no
# Leve importancia
sum(is.na(data$rent_or_own))
data %>% ggplot(aes(x=rent_or_own, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=rent_or_own, fill=as.factor(seasonal_vaccine))) + geom_bar()



sum(is.na(data$employment_status))
#Claramente diferenciador
#Claramente diferenciadr e nals dos
data %>% ggplot(aes(x=employment_status, fill=as.factor(h1n1_vaccine))) + geom_bar()
data %>% ggplot(aes(x=employment_status, fill=as.factor(seasonal_vaccine))) + geom_bar()

#hh5_geo_region espondent's residence using a 10-region geographic classification
#defined by the U.S. Dept. of Health and Human Services. 
#Values are represented as short random character strings.
#Bastante relevante. Además a partir de esta varibale se puede incorporar
#nueva información de internet
sum(is.na(data$hhs_geo_region))
data %>% ggplot(aes(x=hhs_geo_region, fill=as.factor(h1n1_vaccine))) + geom_bar() + 
  coord_flip()
data %>% ggplot(aes(x=hhs_geo_region, fill=as.factor(seasonal_vaccine))) + geom_bar() +
  coord_flip()

#census_msa residencia respecto a las metroplitan statiscial areas.
#no relevante
sum(is.na(data$census_msa))
data %>% ggplot(aes(x=census_msa, fill=as.factor(h1n1_vaccine))) + geom_bar() + 
  coord_flip()
data %>% ggplot(aes(x=census_msa, fill=as.factor(seasonal_vaccine))) + geom_bar() +
  coord_flip()



#númnero de adultos en el mismo hogar
#Ligeramente relevante em ambas
sum(is.na(data$household_adults))
data %>% ggplot(aes(x=household_adults, fill=as.factor(h1n1_vaccine))) + geom_bar() + 
  coord_flip()
data %>% ggplot(aes(x=household_adults, fill=as.factor(seasonal_vaccine))) + geom_bar() +
  coord_flip()



#númnero de niños en el mismo hogar
#Ligeramente relevante em ambas
#similar a la anterior
sum(is.na(data$household_children))
data %>% ggplot(aes(x=household_children, fill=as.factor(h1n1_vaccine))) + geom_bar() + 
  coord_flip()
data %>% ggplot(aes(x=household_children, fill=as.factor(seasonal_vaccine))) + geom_bar() +
  coord_flip()


#Vacía en su enorme mayoría
#Contiene algunos valroes bastante diferenciadores pero puede ser por falta
#de variedad muestral
sum(is.na(data$employment_industry))
data %>% ggplot(aes(x=employment_industry, fill=as.factor(h1n1_vaccine))) + geom_bar() + 
  coord_flip()
data %>% ggplot(aes(x=employment_industry, fill=as.factor(seasonal_vaccine))) + geom_bar() +
  coord_flip()

#employument occupation
#Petado de nulos también
data %>% ggplot(aes(x=employment_occupation, fill=as.factor(h1n1_vaccine))) + geom_bar() + 
  coord_flip()
data %>% ggplot(aes(x=employment_occupation, fill=as.factor(seasonal_vaccine))) + geom_bar() +
  coord_flip()

library(randomForest)
#Vamos a ver si encontramos algunas variables mejores

rf_mod <- rand_forest(mode="classification", trees=200)
data_fit <- read_csv("../data/training_set_features_preprocessed.csv") %>% bind_cols(data_labels) %>%
  mutate_all(as.factor) %>% select(- respondent_id)

rf_imp <- randomForest(seasonal_vaccine ~ . , data = data_fit, importance = TRUE)
is.data.frame(rf_imp$importance)
as.data.frame(rf_imp$importance) %>% rownames_to_column("variable") %>% 
  select(variable, MeanDecreaseGini) %>% 
  arrange(-MeanDecreaseGini) %>% 
  ggplot(aes(y=reorder(variable,MeanDecreaseGini),x=MeanDecreaseGini)) + geom_col() +
  labs(x="Decrecimiento medio de la pureza del nodo", y="Variable",
       title="Importancia de variables para seasonal_vaccine")

rf_imp_h1 <- randomForest(h1n1_vaccine ~ . , data = data_fit, importance = TRUE)

as.data.frame(rf_imp_h1$importance) %>% rownames_to_column("variable") %>% 
  select(variable, MeanDecreaseGini) %>% 
  arrange(-MeanDecreaseGini) %>% 
  ggplot(aes(y=reorder(variable,MeanDecreaseGini),x=MeanDecreaseGini)) + geom_col() +
  labs(x="Decrecimiento medio de la pureza del nodo", y="Variable",
       title="Importancia de variables para vaccine_h1n1")

