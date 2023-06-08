library(data.table)
library(tidyverse)
library(quantmod)




#################################################
##########    FONTE DOS DADOS   #################

# IMA = https://data.anbima.com.br/indices
# fundos = https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/
# IBOVESPA = api yahoo finance
# IFIX = https://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-de-segmentos-e-setoriais/indice-fundos-de-investimentos-imobiliarios-ifix-estatisticas-historicas.htm
# ipca = pacote BETS (código = 433)
# PIB = BCB série 4380

#################################################
#################################################




#################################
####### IBOVESPA ###############

get_ibov <- function(){
  
  data <- getSymbols(Symbols = "^BVSP",
                     src = "yahoo",
                     auto.assign = FALSE,
                     from = "01/01/2010",
                     to = "03/03/2023")
  
  
  close <- window(data$BVSP.Close, start = "2010-01-01", end = "2023-03-11")
  
  return(close)
}

close <- get_ibov()

#write.zoo(close, file = "/home/gabriel/Dropbox/artigo-macro3/dados/ibov.csv")


#########################################
########## SEPARANDO OS FUNDOS POR CLASSE

funds <- function(){
  
  # Define o caminho onde estão armazenados os dados brutos de fundos de investimento
  path <- "/home/gabriel/Documents/dados-artigo/"
  
  # Lê todos os arquivos presentes no diretório "data-2017-2010" e cria um data frame com todas as informações
  raw_fund <- list.files(path = paste0(path,"data-2017-2010"), full.names = TRUE) %>% 
    lapply(fread) %>% 
    bind_rows()
  
  # Lê o arquivo CSV que contém as informações de classe dos fundos, seleciona apenas as colunas necessárias e mantém apenas uma única linha para cada CNPJ_FUNDO
  classe <- fread(paste0(path, "extrato/extrato.csv"),
                      encoding = "Latin-1", quote = "") %>%
    select(CNPJ_FUNDO, DENOM_SOCIAL, CLASSE_ANBIMA) %>% 
    distinct(CNPJ_FUNDO, .keep_all = TRUE)
  
  funds_compl <- fread(paste0(path, "fundos-compl.csv")) %>% 
    select(-TP_FUNDO)
  
  # Une os dados brutos de fundos e as informações de classe dos fundos com base na coluna CNPJ_FUNDO, mantendo apenas as linhas presentes em "raw_fund"
  big_data <- merge.data.table(raw_fund, classe, by = "CNPJ_FUNDO", all.x = TRUE)
  
  
  # Retorna a tabela com todas as informações unidas
  return(bind_rows(big_data, funds_compl))
}

#fwrite(funds(),"/home/gabriel/Documents/dados-artigo/funds-final.csv")




raw_data <- data.table::fread("/home/gabriel/Documents/artigo_macro/funds-final.csv",
                              encoding = "Latin-1")



funds_name <- raw_data %>%
  collapse::fselect(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC) %>%
  collapse::fgroup_by(CNPJ_FUNDO) %>%
  collapse::fsummarise(DT_COMPTC = collapse::fmax(DT_COMPTC))

filtered_data <- merge(funds_name, raw_data, by = c("CNPJ_FUNDO", "DT_COMPTC")) %>% 
  select(date = DT_COMPTC, cnpj = CNPJ_FUNDO, nome = DENOM_SOCIAL, nav = VL_PATRIM_LIQ)

#write_csv(filtered_data, "dados/funds-name.csv")

### filtrando os fundos multimercado com mais de 5 anos

library(collapse)

mult <- str_subset(raw_data$CLASSE_ANBIMA, "MULTIMERCADO -") %>% funique() #retorna um vetor


data <- raw_data %>%
  fselect(CNPJ_FUNDO, DT_COMPTC, VL_PATRIM_LIQ, CLASSE_ANBIMA, NR_COTST) %>% 
  fsubset(CLASSE_ANBIMA %in% mult)


#fwrite(data, file = "/home/gabriel/Documents/artigo_macro/fundos-multimercado.csv")

