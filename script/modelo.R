library(quantmod)
library(tidyquant)
library(lubridate)
library(urca)
library(collapse)
library(vars)
library(aod) #para performar o test de wald
library(tidyverse)


# Analisando as variáveis -------------------------------------------------

#site bacen
# Lendo o arquivo "dados/cambio-cv.csv" com delimitador "\t"
cambio <- read_delim("dados/cambio-cv.csv", delim = "\t",
                     trim_ws = TRUE,
                     locale = locale(encoding = "latin1",
                                     decimal_mark = ",")) %>%
  janitor::clean_names() %>%  # Padronizando os nomes das colunas
  select(date = data,  # Selecionando as colunas relevantes
         venda = x3696_taxa_de_cambio_livre_dolar_americano_venda_fim_de_periodo_mensal_u_m_c_us,
         compra = x3697_taxa_de_cambio_livre_dolar_americano_compra_media_de_periodo_mensal_u_m_c_us) %>%
  mutate(date = dmy(paste0("01/",.$date)),  # Convertendo a data para o formato dia/mês/ano
         cambio = (venda + compra)/2) %>%  # Calculando a média entre venda e compra
  select(date, cambio)  # Selecionando apenas as colunas de data e câmbio


# Lendo o arquivo "ibc-br.csv" do site do Banco Central do Brasil
ibc.br <- read_csv2("dados/ibc-br.csv")[-243,] %>%
  janitor::clean_names() %>%  # Padronizando os nomes das colunas
  select(date, ibc.br = x24364_central_bank_economic_activity_index_ibc_br_seasonally_adjusted_index) %>%  # Selecionando as colunas relevantes
  mutate(date = dmy(paste0("01/",.$date)),  # Convertendo a data para o formato dia/mês/ano
         ibc.br = as.numeric(ibc.br))  # Convertendo a coluna ibc.br para numérica


# Lendo o arquivo "cds-5y-dolar.csv" do site Investing.com
cds_5y <- read_csv("dados/cds-5y-dolar.csv", locale = locale(decimal_mark = ",")) %>%
  janitor::clean_names() %>%  # Padronizando os nomes das colunas
  select(date = data, cds.5y = ultimo) %>%  # Selecionando as colunas relevantes
  mutate(date = dmy(.$date))  # Convertendo a data para o formato dia/mês/ano


# Lendo o arquivo "ibov.csv" do site B3 (Bolsa de Valores do Brasil)
ibov <- read_csv2("dados/ibov.csv", skip = 1, locale=locale(encoding="latin1")) %>% 
  janitor::clean_names() %>%  # Padronizando os nomes das colunas
  mutate(data = ym(paste(.$ano, .$mes, sep = "-"))) %>%  # Convertendo a coluna de data para o formato ano-mês
  select(date = data, ibov = valor)  # Selecionando as colunas relevantes


# Lendo o arquivo "IMA-GERAL.csv" do site da ANBIMA
ima.geral <- read_csv("dados/IMA-GERAL.csv") %>% 
  janitor::clean_names() %>%  # Padronizando os nomes das colunas
  select(data = data_de_referencia, ima.geral = numero_indice) %>%  # Selecionando as colunas relevantes
  mutate(data = dmy(.$data)) %>%  # Convertendo a data para o formato dia/mês/ano
  to.period(period = "months", indexAt = "firstof", OHLC = FALSE) %>%  # Convertendo os dados para o último dia do mês
  timetk::tk_tbl(rename_index = "date")  # Transformando em uma tabela de séries temporais




# JUNTANDO TUDO EM UM DATA FRAME

merged_variable <- list(ihfa, ibov, ima.geral, cds_5y, ibc.br, cambio) %>% 
  reduce(full_join, by = "date") %>%  # Combinação das variáveis pela coluna "date"
  arrange(date) %>%  # Classificação do data frame combinado em ordem crescente pela coluna "date"
  na.omit() %>%  # Remoção de linhas com valores ausentes (NA)
  mutate_at(vars(-date), as.double)  # Transformação das colunas (exceto "date") em valores numéricos  

###dados de 01/01/2012 até 23/02/01


## criando a tabela de estatistica descritiva para o latex 
merged_variable %>%
  select(-date) %>% 
  psych::describe() %>% 
  xtable::xtable()


## Teste de raiz unitária ------------------------------


### ADF ---------------------------------------------------------------------


adf_test <- function(data){
  
  # Executa o teste ADF para todas as variáveis numéricas no conjunto de dados
  adf_test <- data %>%
    select(where(is.numeric)) %>% 
    map(~tseries::adf.test(.x, k = 1) %>% suppressWarnings())
  
  # Cria um data frame vazio para armazenar os resultados do teste ADF
  df <- data.frame(variavel = as.character(),
                   adf = as.numeric(),
                   p_valor = as.numeric())
  
  # Itera sobre os resultados do teste ADF e preenche o data frame df
  for (i in seq_along(adf_test)) {
    
    df <- rbind(df, data.frame(variavel = names(adf_test)[i],
                               adf = round(as.numeric(adf_test[[i]]$statistic), digits = 3),
                               p_valor = adf_test[[i]]$p.value))
    
  }
  
  # Adiciona uma coluna "resultado" ao data frame df com base no p-valor
  df$resultado <- case_when(df$p_valor >= 0.05 ~ "nao estacionario",
                            df$p_valor < 0.5 ~ "estacionario")
  
  # Verifica se há variáveis não estacionárias e executa o teste ADF novamente com uma diferença
  if (any(df$p_valor >= 0.05)) {
    
    # Filtra as variáveis não estacionárias
    variaveis_ne <- df %>%
      filter(resultado == "nao estacionario") %>% 
      pull(var = "variavel")
    
    # Executa o teste ADF com uma diferença para as variáveis não estacionárias
    adf_test2 <- data %>%
      select(any_of(variaveis_ne)) %>% 
      map(~diff(.x) %>% tseries::adf.test(k = 1) %>% suppressWarnings())
    
    # Cria um novo data frame para armazenar os resultados do teste ADF com uma diferença
    df2 <- data.frame(variavel = as.character(),
                      adf = as.numeric(),
                      p_valor = as.numeric())
    
    # Itera sobre os resultados do teste ADF com uma diferença e preenche o data frame df2
    for (i in seq_along(adf_test2)) {
      
      df2 <- rbind(df2, data.frame(variavel = names(adf_test2)[i],
                                   adf = round(as.numeric(adf_test[[i]]$statistic), digits = 3),
                                   p_valor = adf_test2[[i]]$p.value))
    }
    
    # Adiciona uma coluna "resultado" ao data frame df2 com base no p-valor
    df2$resultado <- case_when(df2$p_valor >= 0.05 ~ "nao estacionario",
                               df2$p_valor < 0.5 ~ "estacionario")
    
    # Cria uma lista com os resultados do teste ADF em nível e com uma diferença
    result_list <- list("Teste em nivel" = df,
                        "Teste com uma diferenca" = df2)
    
    return(result_list)
    
  }
  
  # Retorna o data frame df se não houver variáveis não estacionárias
  return(df)
  
}


adf_test(merged_variable)



# Pegando os fundos -------------------------------------------------------


top_funds <- function(){
  
  # Lê o arquivo "top_50_funds.csv" e faz a limpeza dos nomes das colunas
  data <- read_csv("dados/top_50_funds.csv") %>% 
    janitor::clean_names() %>% 
    select(cnpj = cnpj_fundo,
           date = dt_comptc,
           nav = vl_patrim_liq) 
  
  # Obtém uma lista dos CNPJs únicos dos fundos
  cnpj_funds <- funique(data$cnpj)
  
  # Agrupa os dados por CNPJ e cria uma lista de data frames separados para cada fundo
  funds <- data %>% 
    group_by(cnpj) %>%
    group_split() %>%
    map(~select(.x, date, nav)) %>% 
    set_names(cnpj_funds)
  
  # Retorna a lista de data frames dos fundos
  return(funds)
  
}




## Juntando as variaveis com os fundos ----

merge_dataframes <- function(funds, ...) {
  # Obtém a lista de data frames adicionais a serem mesclados
  dfs_to_merge <- list(...)
  
  # Mescla os data frames da lista "dfs_to_merge" com os data frames dos fundos pelo atributo "date"
  funds_var_merged <- Reduce(function(x, y) merge(x, y, by = "date"), dfs_to_merge)
  
  # Mescla os data frames dos fundos com o data frame resultante da mescla anterior, excluindo a coluna "date"
  funds_var_merged <- map(funds, ~merge(.x, funds_var_merged, by = "date")) %>% 
    map(~select(.x, -date))
  
  # Aplica o logaritmo aos valores dos data frames mesclados
  log_funds_var_merged <- funds_var_merged %>% 
    map(~apply(.x, 2, log) %>% as_tibble())
  
  # Retorna a lista de data frames com os valores logaritmizados
  return(log_funds_var_merged)
}



# Teste de cointegração ---------------------------------------------------

## Estimando o ardl --------------------------------------------------------

estimate_ardl <- function(data_list, max_lag = 3) {
  # Obtém os nomes das variáveis independentes
  independent_vars <- colnames(data_list[[1]])[-1]
  
  # Cria a fórmula para a estimação do ARDL
  formula <- as.formula(paste0("nav ~ ", paste0(independent_vars, collapse = "+")))
  
  # Estima o modelo ARDL para cada data frame da lista usando o critério BIC
  best_ardl_bic <- data_list %>%
    purrr::map(~ ARDL::auto_ardl(formula,
                                 data = .x,
                                 max_order = rep(max_lag, ncol(.x)))$best_order,
               selection = "BIC")
  
  # Cria uma lista vazia para armazenar os modelos ARDL estimados
  ardl_list <- list()
  
  # Estima o modelo ARDL para cada data frame usando a ordem determinada pelo critério BIC
  for (i in 1:length(data_list)) {
    order <- as.numeric(best_ardl_bic[[i]])
    
    ardl_list[[i]] <- ARDL::ardl(formula,
                                 data = data_list[[i]],
                                 order = order)
  }
  
  # Define os nomes dos modelos ARDL estimados com base nos nomes dos data frames originais
  names(ardl_list) <- names(data_list)
  
  # Retorna a lista de modelos ARDL estimados
  return(ardl_list)
}




# Fazendo o teste de limite em todo o df

run_bound_test <- function(ardl) {
  # Cria uma lista vazia para armazenar os resultados dos testes de limite
  bound_test <- list()
  
  # Executa o teste de limite para cada modelo ARDL da lista
  for (i in 1:length(ardl)) {
    bound_test[[i]] <- ARDL::bounds_f_test(ardl[[i]], case = 2)
  }
  
  # Define os nomes dos resultados dos testes de limite com base nos nomes dos modelos ARDL
  names(bound_test) <- names(ardl)
  
  # Realiza o teste Breusch-Godfrey para verificar a correlação serial dos resíduos
  bg_test <- set_names(ardl %>% map(~lmtest::bgtest(.x)),
                       names(ardl))
  
  # Cria um data frame vazio para armazenar os resultados do teste Breusch-Godfrey
  df_bg_test <- data.frame(cnpj = as.character(), pvalue = as.numeric())
  
  # Realiza o teste Breusch-Godfrey para cada modelo ARDL e filtra os modelos que não apresentam correlação serial
  for (i in seq_along(ardl)) {
    pvalue <- bg_test[[i]]$p.value
    
    if (pvalue >= 0.05) {
      df_bg_test <- rbind(df_bg_test,
                          data.frame(cnpj = names(ardl)[i],
                                     pvalue = pvalue))
    }
  }
  
  # Obtém a lista de CNPJs (nomes) dos modelos que passaram no teste Breusch-Godfrey
  cnpj <- df_bg_test$cnpj
  
  # Filtra os resultados dos testes de limite para os modelos selecionados
  filtered_funds <- bound_test[cnpj]
  
  # Retorna os resultados dos testes de limite filtrados
  return(filtered_funds)
}



filter_bound_test_result <- function(data){
  # Atribui o valor do argumento "bound_test_result" à variável "data"
  data <- bound_test_result
  
  # Cria um data frame vazio para armazenar os resultados filtrados
  df <- data.frame(cnpj = as.character(), estatistica = as.numeric(), pvalue = as.numeric())
  
  # Itera sobre os resultados na variável "data"
  for (i in seq_along(data)) {
    pvalue <- data[[i]]$p.value
    estatistica <- data[[i]]$statistic
    
    # Filtra os resultados com p-valor <= 0.05
    if(pvalue <= 0.05){
      df <- rbind(df,
                  data.frame(cnpj = names(data)[i],
                             estatistica = round(estatistica, 3),
                             pvalue = round(pvalue, 3)))
    }
  }
  
  # Manipula o data frame resultante
  filtered_df <- df %>%
    rownames_to_column(var = "index") %>% 
    select(-index)
  
  # Retorna o data frame filtrado
  return(filtered_df)
}



getting_the_good_funds <- function(all_funds, cointegrated_funds){
  # Lê o arquivo "funds-name.csv" e armazena os dados na variável funds_name
  funds_name <- read_csv("dados/funds-name.csv") %>% suppressWarnings()
  
  # Seleciona os fundos que serão utilizados com base nos fundos cointegrados
  funds_will_be_used <- cointegrated_funds %>% 
    # Realiza um left join entre os fundos cointegrados e os nomes dos fundos
    left_join(funds_name, by = "cnpj") %>% 
    # Filtra os fundos cujos nomes não contêm a palavra "PRIVADO"
    filter(!str_detect(nome, "PRIVADO")) %>% 
    # Ordena os fundos em ordem decrescente de acordo com o valor do nav
    arrange(desc(nav)) %>% 
    # Seleciona apenas os primeiros 10 fundos
    slice(1:10) %>% 
    # Seleciona as colunas cnpj, nome e pvalue
    select(cnpj, nome, pvalue)
  
  # Retorna os fundos que serão utilizados
  return(funds_will_be_used)
}




### analisando a relação de curto e longo prazo ----

# Obtendo a lista de fundos
funds <- top_funds()

# Mesclando os data frames dos fundos com outros data frames relevantes
funds_var_merged <- merge_dataframes(funds, ibov, ima.geral, cds_5y, ibc.br, cambio)

# Estimando os modelos ARDL
ardl <- estimate_ardl(funds_var_merged, max_lag = 3)

# Realizando os testes de limite
bound_test_result <- ardl %>% run_bound_test()

# Filtrando os fundos cointegrados
cointegrated_funds <- filter_bound_test_result(bound_test_result)

# Obtendo os fundos selecionados daqueles que passaram no teste de cointegração
cnpj_good_funds <- getting_the_good_funds(funds_var_merged, cointegrated_funds)

# Lista de nomes de fundos a serem alterados
names_to_change <- c("vokin_everest", "jgp_strategy", "opportunity_total", "dahlia_total", "spx_nimitz")

# Filtrando e renomeando os fundos selecionados
cnpj_good_funds <- cnpj_good_funds[c(3, 6, 7, 9, 10), ] %>% 
  cbind(names_to_change) %>% 
  select(-nome) %>% 
  rename(nome = names_to_change)

# Criando a lista de modelos ARDL dos fundos selecionados
goods_ardl <- set_names(
  ardl[cnpj_good_funds$cnpj],
  cnpj_good_funds$nome
)


# cnpj_good_funds[,c("cnpj", "nome")] %>% 
#   left_join(cointegrated_funds, by = "cnpj") %>% 
#   stargazer::stargazer(summary = FALSE, decimal.mark = ",")



# Calculando os valores de long-run ARDL para os fundos selecionados
long_run_ardl <- set_names(
  goods_ardl %>% map(~ARDL::multipliers(.x, type = "lr")),
  cnpj_good_funds$nome
)

# Arredondando e combinando os valores de long-run ARDL
long_run_ardl %>% 
  map(~select(.x, where(is.numeric)) %>% 
        round(3) %>% 
        cbind(long_run_ardl[[1]][1])
  )

# Calculando os valores de short-run ARDL para os fundos selecionados
short_run_ardl <- set_names(
  goods_ardl %>% map(~ARDL::multipliers(.x, type = "sr")),
  cnpj_good_funds$nome
)

# Arredondando e combinando os valores de short-run ARDL
short_run_ardl %>% 
  map(~select(.x, -Term) %>% 
        round(3) %>% 
        cbind(long_run_ardl[[1]][1])
  )


### estimando um RECM ----

recm_list <- set_names(
  goods_ardl %>% map(~ARDL::recm(.x, case=2)) %>% map(~summary(.x)),
  names(goods_ardl)
)


## Testes do modelo ARDL ---------------------------------------------------

### Normalidade ----

(jb_test <- set_names(
  goods_ardl %>% map(~tseries::jarque.bera.test(.x$residuals)),
  names(goods_ardl)
))

### Correlação serial ----

(bg_test <- set_names(goods_ardl %>% map(~lmtest::bgtest(.x)),
                      names(goods_ardl)))


### Heterocedasticidade ----
(hetero_test <- set_names(
  goods_ardl %>% map(~lmtest::bptest(.x)),
  names(goods_ardl)
))



# Toda e Yamamoto -----------------------------------------------------------

## Modelo var ---------------------------------------------------------------

## filtrando os fundos que será usado no artigo

# Criando uma lista com os dados dos fundos selecionados
goods_funds_list <- funds_var_merged[names(funds) %in% cnpj_good_funds$cnpj]



# Função para executar o teste de Yamamoto para os fundos fornecidos

toda_yamamoto <- function(funds_list, ic) {
  
  require(tidyverse)  
  require(purrr)      
  require(vars)       
  
  result <- list()    # Cria uma lista vazia para armazenar os resultados
  
  # Loop através dos elementos em funds_list
  for (i in seq_along(funds_list)) {
    
    # Cria um data frame vazio para armazenar os resultados para um fundo específico
    df_result <- data.frame(
      causa = as.character(),
      efeito = as.character(),
      qui_quadrado = as.numeric(),
      p_valor = as.numeric()
    )
    
    # Loop através das colunas do fundo atual
    for (j in 2:(ncol(funds_list[[i]]) - 1)) {
      
      # Seleciona as colunas relevantes do fundo atual para o modelo VAR
      var_selec <- funds_list[[i]][, c(1, j)] %>%
        VARselect(lag.max = 15, type = "none")
      
      var_ic <- var_selec$selection[str_detect(names(var_selec$selection),
                                               toupper(ic))] |> 
        as.numeric()
      
      # Ajusta o modelo VAR para as colunas selecionadas
      var_model <- funds_list[[i]][, c(1, j)] %>% 
        VAR(p = var_ic, type = "none")
      
      #### Teste de Wald ####
      
      if (var_model$p == 1) {
        # Se p=1, realiza o teste de Wald para o modelo VAR univariado
        
        # Teste de Wald para a ida
        test_result_ida <- wald.test(
          Sigma = vcov(var_model$varresult[[1]]),
          b = coef(var_model$varresult[[1]]),
          Terms = c(2)
        )
        
        # Teste de Wald para a volta
        test_result_volta <- wald.test(
          Sigma = vcov(var_model$varresult[[2]]),
          b = coef(var_model$varresult[[2]]),
          Terms = c(1)
        )
        
        # Adiciona os resultados aos data frames de resultados
        df_result <- rbind(
          df_result,
          data.frame(
            causa = names(var_model$varresult)[[1]],
            efeito = names(var_model$varresult)[[2]],
            qui_quadrado = test_result_ida$result$chi2['chi2'] |> as.numeric() |> round(4),
            p_valor = test_result_ida$result$chi2['P'] %>% as.numeric() |> round(4)
          ),
          data.frame(
            causa = names(var_model$varresult)[[2]],
            efeito = names(var_model$varresult)[[1]],
            qui_quadrado = test_result_volta$result$chi2['chi2'] %>% as.numeric() |> round(4),
            p_valor = test_result_volta$result$chi2['P'] %>% as.numeric() |> round(4)
          )
        )
        
      } else {
        # Se p>1, realiza o teste de Wald para o modelo VAR multivariado
        
        # Teste de Wald para a ida
        test_result_ida <- wald.test(
          Sigma = vcov(var_model$varresult[[1]]),
          b = coef(var_model$varresult[[1]]),
          Terms = seq(from = 2, to = (var_model$p * 2 - 1), by = 2)
        )
        
        # Teste de Wald para a volta
        test_result_volta <- wald.test(
          Sigma = vcov(var_model$varresult[[2]]),
          b = coef(var_model$varresult[[2]]),
          Terms = seq(1, (var_model$p * 2 - 2), 2)
        )
        
        # Adiciona os resultados aos data frames de resultados
        df_result <- rbind(
          df_result,
          data.frame(
            causa = names(var_model$varresult)[[1]],
            efeito = names(var_model$varresult)[[2]],
            qui_quadrado = test_result_ida$result$chi2['chi2'] |> as.numeric() |> round(4),
            p_valor = test_result_ida$result$chi2['P'] %>% as.numeric() |> round(4)
          ),
          data.frame(
            causa = names(var_model$varresult)[[2]],
            efeito = names(var_model$varresult)[[1]],
            qui_quadrado = test_result_volta$result$chi2['chi2'] %>% as.numeric() |> round(4),
            p_valor = test_result_volta$result$chi2['P'] %>% as.numeric() |> round(4)
          )
        )
      }
    }
    
    result[[i]] <- df_result   # Armazena os resultados para o fundo atual na lista de resultados
    
  }
  
  result <- set_names(
    result,
    names(funds_list)
  )    # Ordena os resultados por p-valor e define os nomes dos resultados com base nos nomes dos fundos
  
  
  return(result)   # Retorna a lista de resultados
  
}


# Executando a função toda_yamamoto nos fundos selecionados usando o critério de informação "aic"
ty_result <- set_names(
  toda_yamamoto(goods_funds_list, ic = "aic") %>%
    map(~filter(.x, efeito == "nav")),
  c("spx_nimitz", "vokin_everest", "dahlia_total", "opportunity_total", "jgp_strategy")
  ) |> 
  print(ty_result)


