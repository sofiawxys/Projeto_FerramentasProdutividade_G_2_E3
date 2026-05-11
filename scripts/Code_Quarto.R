##Código para o Quarto


# ---- Pacotes ----
#| echo: false
#| message: false
library(tidyverse)
library(gtsummary)
library(here)

# ---- 1. Ler Dados  ----
# leitura do ficheiro já sem missings
dados <- read_csv(here("data", "processed", "mindmove_clean.csv"))

# ---- 2. Tabela 1 ----
#tabela que compara os grupos no início do estudo
tabela1 <- dados |> 
  select(grupo, idade, sexo, phq9_baseline) |> 
  tbl_summary(
    by = grupo,
    type = list (phq9_baseline ~"continuous"),
    label = list(
      idade ~ "Idade (anos)",
      sexo ~ "Género",
      phq9_baseline ~ "PHQ-9 Inicial"
    ),
    digits = all_continuous() ~ 1
  )|> 
  add_overall() |> 
  add_p()


# ---- 3. Gráfico do Outcome Primário----
dados |> 
  ggplot(aes(
    x = grupo, 
    y = phq9_semana8, 
    colour = grupo)) +
  geom_boxplot(
    outlier.shape = NA, 
    width = 0.4,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12, 
    alpha = 0.7, 
    size = 2.5 
  ) +
  scale_colour_manual(
    values = c(
      "intervencao" = "#2C3E50",
      "controlo"    = "#7A9B9E"
    )
  )+
  scale_x_discrete(
    labels = c("intervencao" = "Intervenção", "controlo" = "Controlo")
  )
+
  labs(
    title = "Resultado MindMove: Score PHQ-9 às 8 semanas",
    x = "Grupo",
    y = "Score PHQ-9 (Depressão)"
  )+
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
