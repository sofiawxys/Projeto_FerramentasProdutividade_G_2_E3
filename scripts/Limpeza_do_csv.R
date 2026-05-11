# ---- 1. Ler Dados Originais ----
# Lemos da pasta 'raw'
dados_originais <- read_csv(here("data", "raw", "mindmove.csv"))

# ---- 2. Limpeza e Organização ----
dados_limpos <- dados_originais |> 
  clean_names() |> 
  filter(!is.na(grupo), !is.na(idade)) |> 
  mutate(
    grupo = as.factor(grupo),
    sexo = as.factor(sexo)
  )

write_csv(dados_limpos, here("data", "processed", "mindmove_clean.csv"))