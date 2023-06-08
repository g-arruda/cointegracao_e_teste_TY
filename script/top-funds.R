library(tidyverse)
library(tidyquant)
library(collapse)
library(data.table)
library(quantmod)


path <- "/home/gabriel/Documents/artigo_macro/fundos-multimercado.csv"

raw_data <- data.table::fread(path, encoding = "Latin-1") %>% 
  select(-CLASSE_ANBIMA)

# Converte DT_COMPTC para o formato de data
raw_data$DT_COMPTC <- raw_data$DT_COMPTC %>%
  as.integer() %>% # Converte para inteiro
  as.Date() # Converte para formato de data


# função que filtra os fundos com "n" dias de operação --------------------
old_funds <- function(data, dias){
  
  ##### filtrar os fundos com +1000 cotistas
  
  cotistas_funds <- data %>% 
    fgroup_by(CNPJ_FUNDO) %>% 
    fsubset(DT_COMPTC > fmax(DT_COMPTC)-5 & NR_COTST > 10)
  
  data_filtered_shareholds <- data %>% 
    fsubset(CNPJ_FUNDO %in% cotistas_funds$CNPJ_FUNDO)
  
  grouped <- data_filtered_shareholds %>% 
    fgroup_by(CNPJ_FUNDO) %>% 
    fsummarise(min_date = fmin(DT_COMPTC),
               max_date = fmax(DT_COMPTC))
  
  #filtrando fundos ativos com mais de 3 anos
  dias_op <- fmax(grouped$max_date) - dias
  
 
  active_funds <- grouped %>% 
    filter(max_date >= fmax(grouped$max_date)-10, min_date <= dias_op)

  
  filtered_data <- data %>% 
    inner_join(active_funds, by="CNPJ_FUNDO")
  
  final_df <- filtered_data %>% 
    fselect(data = DT_COMPTC,
            cnpj = CNPJ_FUNDO,
            nav = VL_PATRIM_LIQ)
  
   return(final_df)
}

fundos_antigos <- raw_data %>% old_funds(dias = 252*3)




# retorna os fundos com maior NAV acumulado -------------------------------

top_funds <- function(dados, n) {
  
  #pegando os fundos cancelados
  canceled <- dados %>%
    fsubset(nav == 0) %>% 
    fgroup_by(cnpj) %>% 
    fcount() %>% 
    arrange(desc(N)) %>% 
    fsubset(N > 200) %>% 
    pull(cnpj)
  
  df <- dados %>% 
    fsubset(cnpj %!in% canceled) %>%  #remove os fundos cancelados
    fsubset(nav != 0) %>%  # remove as linhas em que o nav é igual a 0
    fgroup_by(cnpj) %>% 
    fmutate(ret.daily = (nav - flag(nav))/ flag(nav) ) %>% 
    ungroup()
  
  
  top_funds <- df %>% 
    fgroup_by(cnpj) %>% 
    fsummarise(total.ret = prod(1 + ret.daily, na.rm = TRUE)-1) %>% 
    replace_Inf() %>% 
    arrange(desc(total.ret)) %>% 
    slice_head(n=n) %>% 
    pull(cnpj)
  
  return(top_funds)
}

top_50 <- fundos_antigos %>%
  top_funds(n=50) %>% 
  ss(-c(2,11))

# filtrando os top funds --------------------------------------------------

filtered_data <- raw_data %>% 
  fsubset(CNPJ_FUNDO %in% top_50)


data.table::fwrite(filtered_data,
                   file = "/home/gabriel/Dropbox/artigo-macro3/dados/top_40_funds.csv")



## selecionando fundos especificos

# spx_raptor_master <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "12.808.980/0001-32") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/spx_raptor_master.csv")
# 
# ibiuna_hedge_master <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "15.487.918/0001-84") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/ibiuna_hedge_master.csv")
# 
# gavea_macro_master <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "08.875.020/0001-18") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/gavea_macro_master.csv")
# 
# verde_master_fim <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "07.455.507/0001-89") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/verde_master_fim.csv")
# 
# bresser_hedge_plus <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "08.739.850/0001-18") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/bresser_hedge_plus.csv")

# 
# gap_absoluto <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "01.823.373/0001-25") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/fundos_selecionados/gap_absoluto.csv")
# 
# sharp_long_short <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "07.279.819/0001-89") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/fundos_selecionados/sharp_long_short.csv")
# 
# kapitalo_kappa <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "12.105.940/0001-24") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/fundos_selecionados/kapitalo_kappa.csv")
# 
# 
# arx_extra <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "06.041.290/0001-06") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/fundos_selecionados/arx_extra.csv")
# 
# 
# seival_fgs <- raw_data %>%
#   fsubset(CNPJ_FUNDO == "11.301.137/0001-00") %>%
#   fselect(date = DT_COMPTC, nav = VL_PATRIM_LIQ) %>%
#   write_csv(file = "/home/gabriel/Documents/artigo_macro/fundos_selecionados/seival_fgs.csv")


