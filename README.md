# gbm_brasilhackexport
Tratamento de dados, análise exploratória e modelagem para o desafio - equipe GBM

## Execução do código
#### Requisitos
R >= 3.6
RStudio >= 1.0
Bases de dados do desafio gravadas no diretório de trabalho do R com a seguinte estrutura:

- wd
  --Cargas
    --- GRANEL.csv
  -- Estadias
    --- Estadia.csv
    --- Últimas escalas.csv

### Pacotes requeridos - R
tidyverse
data.table
MASS
mvoutlier
glmnet
lubridate
caret
runner
gbm
xgboost
ranger

Caso não os tenha instalados, no R, rodar o comando install.packages(<package_name>)

**************************************************************************************
**************************************************************************************
sessionInfo():
R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=Portuguese_Brazil.1252  LC_CTYPE=Portuguese_Brazil.1252   
[3] LC_MONETARY=Portuguese_Brazil.1252 LC_NUMERIC=C                      
[5] LC_TIME=Portuguese_Brazil.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] runner_0.3.7      xgboost_1.2.0.1   ranger_0.12.1     lubridate_1.7.4  
 [5] glmnet_4.0-2      Matrix_1.2-18     mvoutlier_2.0.9   sgeostat_1.0-27  
 [9] data.table_1.12.8 forcats_0.5.0     stringr_1.4.0     dplyr_0.8.5      
[13] purrr_0.3.3       readr_1.3.1       tidyr_1.0.2       tibble_2.1.3     
[17] tidyverse_1.3.0   MASS_7.3-51.5     caret_6.0-86      ggplot2_3.3.0    
[21] lattice_0.20-38 
