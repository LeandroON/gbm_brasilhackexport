library(tidyverse)
library(data.table)
library(lubridate)
library(caret)
library(runner)

setwd("seu_diretorio_de_trabalho")

# 68 registros duplicados, logo não será possível realizar previsão bem 
# calibrada para esse subset, por isso a exclusão
estadias = fread("Estadia/Estadia.csv", encoding = "UTF-8", na.strings = "") %>% 
  filter(!is.na(`Atracação Prevista`) & !is.na(`Desatracação Prevista`)) %>% 
  filter(trimws(gsub("#", "", `Atracação Prevista`))!="" & trimws(gsub("#", "", `Desatracação Prevista`))!="") %>% 
  filter(!duplicated(`Número do DUV`))

# tratamentos e selecao de variáveis - base de estadias
# Há muitas variáveis que replicam informações com nomes diferentes,
# por isso a aseleção é importante para as fases posteriores do trabalho
# (análise exploratória e modelagem)
estadias = estadias %>% 
  mutate(area_porto = sapply(strsplit(`Local(is) Atracação (área do porto > berço > cabeço)`, " > "),
                             function(x)x[1]),
         berco = sapply(strsplit(`Local(is) Atracação (área do porto > berço > cabeço)`, " > "),
                        function(x)x[2]),
         `Atracação Prevista` = as.POSIXct(`Atracação Prevista`, format = "%d/%m/%Y %H:%M"),
         `Atracação Efetiva` = as.POSIXct(`Atracação Efetiva`, format = "%d/%m/%Y %H:%M"),
         `Desatracação Prevista` = as.POSIXct(`Desatracação Prevista`, format = "%d/%m/%Y %H:%M"),
         `Desatracação Efetiva` = as.POSIXct(`Desatracação Efetiva`, format = "%d/%m/%Y %H:%M"),
         is_reatracado = as.numeric(!is.na(`Local(is) e Data(s) Reatracação (área do porto > berço > ca`)),
         tempo_estadia = time_length(interval(`Atracação Efetiva`, `Desatracação Efetiva`), unit = "hours"),
         diff_atracacao = time_length(interval(`Atracação Prevista`, `Atracação Efetiva`), unit = "hours"),
         diff_desatracacao = time_length(interval(`Desatracação Prevista`, `Desatracação Efetiva`), unit = "hours"),
         mes_atracacao = month(`Atracação Efetiva`) %>% as.character() %>% 
           factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))) %>% 
  group_by(`Porto de estadia atual`, area_porto, berco) %>% 
  arrange(`Porto de estadia atual`, area_porto, berco, `Atracação Efetiva`) %>% 
  mutate(tempo_ocioso = time_length(interval(lag(`Desatracação Efetiva`), `Atracação Efetiva`), unit = "hours")) %>% 
  select(-`Estadia Off-Shore`, -`Local(is) Atracação (área do porto > berço > cabeço)`, 
         -`Local(is) e Data(s) Reatracação (área do porto > berço > ca`, -`Bandeira da Embarcação`, 
         -`Finalidade da Embarcação`, `Tipo de Embarcação`, -`Motivo de Atracação`,
         -`Tipo de Viagem Chegada`, -`Tipo de Viagem Saída`) %>%  
  filter(tempo_estadia >= 1)

# tratamento e consolidação - base de cargas a granel

granel = fread("Cargas/GRANEL.csv", na.strings = "") 

granel = granel %>% 
  filter(!is.na(`Número do DUV`) & !is.na(`Peso bruto da mercadoria`) &
           !is.na(`Tipo de tráfego`) & !duplicated(`Número do Conhecimento`)) %>% 
  mutate(NCM_4d = as.numeric(substr(`NCM Mercadoria`, 1, 4)),
         secao_NCM = as.numeric(substr(`NCM Mercadoria`, 1, 2)),
         pais_porto_destino = substr(`Porto de destino da mercadoria`, 1, 2),
         `País de origem da mercadoria` = ifelse(is.na(`País de origem da mercadoria`),
                                                  "Brasil",
                                                  `País de origem da mercadoria`)
         ) %>%
rename(tipo_trafego = `Tipo de tráfego`,
       pais_origem_merc = `País de origem da mercadoria`)

# transformação dos dados qualitativos em 0 e 1, para a fase de modelagem,
# considerando que há cargas com mais de um valor nessas variáveis, é preciso
# cuidado na consolidação para manter essa informação!
granel_dummy = granel %>% 
  select(tipo_trafego, pais_origem_merc,
         pais_porto_destino) %>% 
  dummyVars(formula = "~ .")

granel2 = granel %>% 
  select(`Número do DUV`, tipo_trafego, pais_origem_merc,
         pais_porto_destino) %>% 
  {cbind(., as.data.frame(predict(object = granel_dummy, newdata = .)))} %>% 
  select(-tipo_trafego, -pais_origem_merc,
         -pais_porto_destino) %>% 
  group_by(`Número do DUV`) %>%
  summarise_all(function(x){if(any(x==1)) 1 else 0})

# dados de carga agrupados pelos primeiros 4 dígitos do NCM
# pesos brutos arredondados para evitar inconsistências no join dos dados
granel3 = granel %>% 
  select(`Número do DUV`, `Peso bruto da mercadoria`, NCM_4d) %>% 
  group_by(`Número do DUV`, NCM_4d) %>% 
  summarise(peso_bruto_NCM_4d = sum(round(`Peso bruto da mercadoria`, 0))) %>% 
  summarise(peso_bruto = sum(peso_bruto_NCM_4d),
            NCM_4d_princ = first(as.character(NCM_4d[peso_bruto_NCM_4d == max(peso_bruto_NCM_4d)])),
            perc_NCM_4d_princ = max(peso_bruto_NCM_4d)/sum(peso_bruto_NCM_4d))

# dados de carga agrupados pelos primeiros 2 dígitos do NCM
# pesos brutos arredondados para evitar inconsistências no join dos dados
granel4 = granel %>% 
  select(`Número do DUV`, `Peso bruto da mercadoria`, secao_NCM) %>% 
  group_by(`Número do DUV`, secao_NCM) %>%
  summarise(peso_bruto_secao_NCM = sum(round(`Peso bruto da mercadoria`, 0))) %>% 
  summarise(peso_bruto = sum(peso_bruto_secao_NCM),
            secao_NCM_princ = first(as.character(secao_NCM[peso_bruto_secao_NCM == max(peso_bruto_secao_NCM)])),
            perc_secao_NCM_princ = max(peso_bruto_secao_NCM)/sum(peso_bruto_secao_NCM))

granel_consolidado = inner_join(granel4, granel3) %>% 
  inner_join(granel2)

# Há registros da base de cargas a granel não têm estadia correspondente
# (alguns devido aos filtros de dados faltantes/ duplicados aplicados)

granel_final = inner_join(estadias, granel_consolidado) %>% 
  filter(secao_NCM_princ %in% c(9, 10, 11, 12, 17, 18, 23))

# manter apenas a base granel_final na memória
rm(list = ls()[!ls()%in%c("granel_final")])

# dados de prancha por terminal/ NCM

tabela_prancha = granel_final %>% 
  ungroup() %>% 
  group_by(`Porto de estadia atual`, area_porto, berco, NCM_4d_princ) %>% 
  summarise(peso_bruto_total = sum(peso_bruto),
            tempo_estadia_total = sum(tempo_estadia),
            tempo_estadia_medio = mean(tempo_estadia),
            peso_bruto_media = mean(peso_bruto),
            perc_NCM_4d_princ = mean(perc_NCM_4d_princ),
            qtd_navios = n(),
            prancha_media = sum(peso_bruto)/sum(tempo_estadia)) %>% 
  arrange(NCM_4d_princ, desc(prancha_media)) %>% 
  filter(NCM_4d_princ %in% c("1005", "1201", "1701", "2302", "2304") &
           qtd_navios > 5)

# ranking de eficiência dos terminais de granel sólido vegetal,
# considerando prancha média
write.csv2(tabela_prancha, "prancha_media_granel_vegetal_ranking.csv", 
           row.names = F)

# tabela para suprir informações cruciais para a fase de modelagem
# uma vez que não há dados de chuvas e outros impactos localizados na
# dimensão temporal, realizamos uma agregação das pranchas médias por
# terminal/ NCM nos 30 dias imediatamente anteriores, e por porto/ NCM
# nos 5 dias anteriores, além das médias dos últimos 5 embarques por
# terminal/NCM (independente da dimensão temporal).

tabela_prancha_last5 = granel_final %>% 
  ungroup() %>% 
  group_by(`Porto de estadia atual`, area_porto, berco, NCM_4d_princ) %>%
  arrange(`Porto de estadia atual`, area_porto, berco, NCM_4d_princ, `Atracação Efetiva`) %>% 
  mutate(prancha_media = sum(peso_bruto)/sum(tempo_estadia),
         prancha_media_last5 = rollapplyr(peso_bruto/tempo_estadia,list(-(5:1)),mean,fill=NA),
         prancha_media_last_30d = runner(peso_bruto/tempo_estadia, mean,
                                         k = "30 days", lag = 1,
                                         idx = `Atracação Efetiva`)) %>% 
  ungroup() %>% 
  group_by(`Porto de estadia atual`, NCM_4d_princ) %>%
  arrange(`Porto de estadia atual`, NCM_4d_princ, `Atracação Efetiva`) %>% 
  mutate(prancha_media_porto_last_5d = runner(peso_bruto/tempo_estadia, mean,
                                              k = "5 days", lag = 1,
                                              idx = `Atracação Efetiva`)) %>% 
  mutate(prancha_media_last_30d = ifelse(is.na(prancha_media_last_30d), 0, prancha_media_last_30d)) %>% 
  filter(!is.na(prancha_media_last5)) %>%
  select(-prancha_media)

# tratar dados - última escala

ultimas_escalas = fread("Estadia/Últimas Escalas.csv", encoding = "UTF-8", na.strings = "") %>%
  mutate(`Data Saída do Porto` = as.POSIXct(`Data Saída do Porto`, format = "%d/%m/%Y %H:%M")) %>%
  rename(porto_origem = Porto) %>%
  group_by(`Número do DUV`) %>%
  mutate(max_data_saida = max(`Data Saída do Porto`)) %>%
  filter(`Data Saída do Porto` == max_data_saida) %>%
  filter(!`Número do DUV` %in% `Número do DUV`[duplicated(`Número do DUV`)]) %>%
  select(-max_data_saida)

# 10 registros sem informação de porto anterior
granel_final = inner_join(granel_final, ultimas_escalas)
