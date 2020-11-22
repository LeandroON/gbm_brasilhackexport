library(tidyverse)

# OBSERVAÇÃO IMPORTANTE: esse arquivo contém apenas o script para gerar
# gráficos para exploração visual - rodar primeiro tratamento_dados.R

# visualizações estadias de granel sólido - Brasil
ggplot(granel_final %>%
         filter(`Porto de estadia atual`!="BRSSZ - SANTOS"),
       aes(tempo_estadia)) + geom_histogram() +
  geom_vline(xintercept = median(granel_final %>% 
                                   filter(`Porto de estadia atual`!="BRSSZ - SANTOS") %>% 
                                   pull(tempo_estadia)), 
             color = "blue", linetype = 2) +
  geom_vline(xintercept = mean(granel_final %>% 
                                 filter(`Porto de estadia atual`!="BRSSZ - SANTOS") %>% 
                                 pull(tempo_estadia)), 
             color = "red", linetype = 2) +
  scale_x_log10() +
  xlab("Tempo de estadia (horas)") +
  ylab("Quantidade de estadias") +
  labs(title = "Exceto Santos - Granel vegetal")

# tempo ocioso - Brasil - histograma
ggplot(granel_final %>%
         filter(`Porto de estadia atual`!="BRSSZ - SANTOS"),
       aes(tempo_ocioso)) + geom_histogram() +
  geom_vline(xintercept = median(granel_final %>% 
                                   filter(`Porto de estadia atual`!="BRSSZ - SANTOS") %>% 
                                   pull(tempo_ocioso), na.rm = T), 
             color = "blue", linetype = 2) +
  geom_vline(xintercept = mean(granel_final %>% 
                                 filter(`Porto de estadia atual`!="BRSSZ - SANTOS") %>% 
                                 pull(tempo_ocioso), na.rm = T), 
             color = "red", linetype = 2) +
  scale_x_log10() +
  xlab("Tempo ocioso - berço (horas)") +
  ylab("Quantidade de estadias") +
  labs(title = "Exceto Santos - Granel vegetal")

# visualizações estadias de granel sólido/ com ou sem carga geral - Santos
# 2735 estadias
ggplot(granel_final %>%
         filter(`Porto de estadia atual`=="BRSSZ - SANTOS"),
       aes(tempo_estadia)) + geom_histogram() +
  geom_vline(xintercept = median(granel_final %>% 
                                   filter(`Porto de estadia atual`=="BRSSZ - SANTOS") %>% 
                                   pull(tempo_estadia)), 
             color = "blue", linetype = 2) +
  geom_vline(xintercept = mean(granel_final %>% 
                                 filter(`Porto de estadia atual`=="BRSSZ - SANTOS") %>% 
                                 pull(tempo_estadia)), 
             color = "red", linetype = 2) +
  scale_x_log10() +
  xlab("Tempo de estadia (horas)") +
  ylab("Quantidade de estadias") +
  labs(title = "Porto de Santos - Granel vegetal")

# tempo ocioso - Brasil - histograma
ggplot(granel_final %>%
         filter(`Porto de estadia atual`=="BRSSZ - SANTOS"),
       aes(tempo_ocioso)) + geom_histogram() +
  geom_vline(xintercept = median(granel_final %>% 
                                   filter(`Porto de estadia atual`=="BRSSZ - SANTOS") %>% 
                                   pull(tempo_ocioso), na.rm = T), 
             color = "blue", linetype = 2) +
  geom_vline(xintercept = mean(granel_final %>% 
                                 filter(`Porto de estadia atual`=="BRSSZ - SANTOS") %>% 
                                 pull(tempo_ocioso), na.rm = T), 
             color = "red", linetype = 2) +
  scale_x_log10() +
  xlab("Tempo ocioso - berço (horas)") +
  ylab("Quantidade de estadias") +
  labs(title = "Porto de Santos - Granel vegetal")

# timeline tempo médio estadia por mês - Santos
ggplot(granel_final %>% 
         filter(`Porto de estadia atual` == "BRSSZ - SANTOS" &
                  tempo_ocioso > 0) %>% 
         mutate(atracacao = floor_date(`Atracação Efetiva`, unit = "month")) %>% 
         group_by(atracacao) %>% 
         summarise(estadia = mean(tempo_estadia)),
       aes(atracacao, estadia)) + geom_line(color = "blue", size = 0.7) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b/%Y") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Mês/ano da atracacação efetiva") +
  ylab("Tempo médio de estadia (horas)") +
  labs(title = "Santos - Granel vegetal")

# timeline tempo ocioso médio por mês - Santos

ggplot(granel_final %>% 
         filter(`Porto de estadia atual` == "BRSSZ - SANTOS" &
                  tempo_ocioso > 0) %>% 
         mutate(atracacao = floor_date(`Atracação Efetiva`, unit = "month")) %>% 
         group_by(atracacao) %>% 
         summarise(ocioso = mean(tempo_ocioso)),
       aes(atracacao, ocioso)) + geom_line(color = "blue", size = 0.7) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b/%Y") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Mês/ano da atracacação efetiva") +
  ylab("Tempo médio ocioso (horas)") +
  labs(title = "Santos - Granel vegetal")

# distribuição peso total das cargas - granel vegetal - Brasil
ggplot(granel_final %>% 
         filter(`Porto de estadia atual` != "BRSSZ - SANTOS"),
       aes(peso_bruto)) + geom_histogram() +
  geom_vline(xintercept = median(granel_final %>%
                                   filter(`Porto de estadia atual` != "BRSSZ - SANTOS") %>% 
                                   pull(peso_bruto)), 
             color = "blue", linetype = 2) +
  geom_vline(xintercept = mean(granel_final %>% 
                                 filter(`Porto de estadia atual` != "BRSSZ - SANTOS") %>% 
                                 pull(peso_bruto)), 
             color = "red", linetype = 2) +
  xlab("Peso bruto da carga (kg)") +
  ylab("Quantidade de estadias") +
  labs(title = "Exceto Santos - Granel vegetal")

# distribuição peso total das cargas - granel vegetal - Santos
ggplot(granel_final %>% 
         filter(`Porto de estadia atual` == "BRSSZ - SANTOS"),
       aes(peso_bruto)) + geom_histogram() +
  geom_vline(xintercept = median(granel_final %>%
                                   filter(`Porto de estadia atual` == "BRSSZ - SANTOS") %>% 
                                   pull(peso_bruto)), 
             color = "blue", linetype = 2) +
  geom_vline(xintercept = mean(granel_final %>% 
                                 filter(`Porto de estadia atual` == "BRSSZ - SANTOS") %>% 
                                 pull(peso_bruto)), 
             color = "red", linetype = 2) +
  xlab("Peso bruto da carga (kg)") +
  ylab("Quantidade de estadias") +
  labs(title = "Porto de Santos - Granel vegetal")

# relação peso da carga x tempo da estadia
ggplot(granel_final,
       aes(peso_bruto, tempo_estadia,
           color = ifelse(`Porto de estadia atual` == "BRSSZ - SANTOS",
                          "Porto de Santos",
                          "Outros"))) + geom_point() +
  scale_color_brewer(type = "qual", name = "Porto da estadia") +
  xlab("Peso Bruto da carga (kg)") +
  ylab("Tempo de estadia (horas)") +
  labs(title = "Brasil - Granel Vegetal")

