library(tidyverse)

res <- read_csv2("driven_data_results.csv") %>% rename(Orden = Indice)

res %>% ggplot(aes(x = Puntuacion, y = reorder(Algoritmo,-Orden), label=Puntuacion)) + 
  geom_col(fill="slateblue") + labs(y="Algoritmo", x= "ROC-AUC") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
