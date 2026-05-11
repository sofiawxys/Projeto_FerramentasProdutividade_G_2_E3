# ---- Pacotes ----
library(tidyverse)
library(here)
library(gtsummary)
library(janitor)


# ---- 1. Ler Dados ----
dados <- read_csv(here("data", "raw", "mindmove.csv")) |> 
  clean_names()

# ---- 2. Tabela 1 ----
# tabela que compara os grupos no início do estudo
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

tabela1 # ver a tabela

# ---- 3. Gráfico do Outcome Primário ----
# Boxplot do PHQ-9 às 8 semanas
grafico1 <- dados |> 
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
  )+
  labs(
    title = "Resultado MindMove: Score PHQ-9 às 8 semanas",
    x = "Grupo",
    y = "Score PHQ-9 (Depressão)"
  )+
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

grafico1

##código para guardar as imagens

tabela1 |>
  as_gt() |>
  gt::gtsave("outputs/tabela.html")

ggsave(
  filename = "outputs/grafico_outcome_primario.png", 
  plot = grafico1,
  width = 7, 
  height = 5, 
  dpi = 150
)