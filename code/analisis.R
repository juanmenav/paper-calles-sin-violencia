library(haven)
library(survey)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(broom)
library(patchwork)
library(fixest)

setwd("ruta a carpeta 'paper calles sin violencia' aqui")

communes_plan <- read_excel("data/raw/communes_plan.xlsx", col_names= TRUE) %>% 
  select(all, `First Row`, `Second Row`, Province_Santiago) %>% 
  rename("CUT" = "all", "Tratado" = "First Row", "delete" = "Second Row", "Santiago" = "Province_Santiago") %>% 
  mutate(CUT = as.numeric(CUT),
         Tratado = as.numeric(Tratado))

enusc <- read_sav("data/raw/enusc_internual_2018_2024_enc_rpc.sav") %>% rename("CUT" = "enc_rpc")
enusc_treated <- left_join(enusc, communes_plan, by = "CUT") %>% subset(delete ==0) %>%  
  mutate(Post = ifelse(año >= 2023, 1, 0),
         Mujeres = ifelse(sexo == 1, 0, 1),
         años_pre = as_factor(año - 2023),
         Edad = ifelse(edad > 5, 1, 0)) %>%
  rename("Hogar_victimizado" = "vh_dmcs")

saveRDS(enusc_treated, "data/processed/enusc_plan.RDS")
write.csv2(enusc_treated, "data/processed/enusc_plan.csv")

enusc_treated$años_pre <- relevel(enusc_treated$años_pre, ref = "-1")


#---- Tabla 1 estadísticas descriptivas  ----

summary <- enusc_treated %>% group_by(año) %>% summarise(total = n(),
                                                         tratados = mean(Tratado)*100,
                                                         sexo = mean(Mujeres)*100,
                                                         Edad = mean(Edad)*100,
                                                         pais = mean(pad)*100,
                                                         barrio = mean(padb)*100,
                                                         dmcs = mean(Hogar_victimizado)*100
)

write_xlsx(summary, "output/estadistias_descriptivas.xlsx")

#---- Figuras 3 y 4, y Tabla 2: tendencias paralelas ----

pais <- enusc_treated  %>% group_by(año, Tratado) %>% summarise(pais = mean(pad)*100) 
barrio <- enusc_treated  %>% group_by(año, Tratado) %>% summarise(barrio = mean(padb)*100)
victimizacion <- enusc_treated  %>% group_by(año, Tratado) %>% summarise(dmcs = mean(Hogar_victimizado)*100)
paralelas <- left_join(pais, barrio, by = c("año", "Tratado"))
paralelas <- left_join(paralelas, victimizacion, by = c("año", "Tratado"))
paralelas <- subset(paralelas, año <2023)

p <- ggplot(paralelas, aes(x = año, y = pais, colour = as.factor(Tratado), group = Tratado))+
  geom_point(size = 3)+
  geom_path(linewidth = 1)+
  scale_color_discrete(name = "Grupo",
                       labels = c("0" = "Control", 
                                "1" = "Plan"))+
  labs(title = "Percibe aumento de inseguridad en el pais",
        tag = "Figura 3",
        caption = "Fuente: elaboración propia con datos ENUSC")+
  ylab("% percepción")+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"))

ggsave("output/figura3.png", plot = p, width = 9, height = 6, dpi = 300)

p1 <- ggplot(paralelas, aes(x = año, y = barrio, colour = as.factor(Tratado), group = Tratado))+
  geom_point(size = 3)+
  geom_path(linewidth = 1)+
  scale_color_discrete(name = NULL,
                       labels = NULL,
                       breaks = NULL)+
  labs(title = "Percibe aumento de inseguridad en el barrio",
       tag = "Panel A")+
  ylab("% percepión")+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"))

p2 <- ggplot(paralelas, aes(x = año, y = dmcs, colour = as.factor(Tratado), group = Tratado))+
  geom_point(size = 3)+
  geom_path(linewidth = 1)+
  scale_color_discrete(name = "Grupo",
                       labels = c("0" = "Control", 
                                  "1" = "Plan"))+
  labs(title = "Victimización delitos mayor connotacion social",
       tag = "Panel B")+
  ylab("% victimizacion")+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"))

p3 <- p1 + p2 +
  plot_annotation(
    title = "Figura 4. Inspección visual de tendencias paralelas",
    caption = "Fuente: elaboración propia con datos ENUSC",
    theme = theme(plot.title = element_text(family = "serif",
                                            face = "bold"),
                  plot.caption = element_text(family = "serif",
                                                face = "bold"),
                  )
  )


ggsave("output/figura4.png", plot = p3, width = 10, height = 4, dpi = 300)

enusc_wald <- subset(enusc_treated, año < 2024)

wald_pais <- feols(pad ~ i(años_pre, Tratado, ref = 0) |CUT+año, enusc_wald)
wald_barrio <- feols(padb ~ i(años_pre, Tratado, ref = 0) |CUT+año, enusc_wald)
wald_dmcs <- feols(`Hogar_victimizado` ~ i(años_pre, Tratado, ref = 0)|CUT+año, enusc_wald)

test_pais <- as.data.frame(wald(wald_pais)) %>% mutate(variable = "pais",
                                                       test99 = p < 0.01,
                                                       test95 = p < 0.05,
                                                       test90 = p < 0.1)
test_barrio <- as.data.frame(wald(wald_barrio)) %>% mutate(variable = "barrio",
                                                       test99 = p < 0.01,
                                                       test95 = p < 0.05,
                                                       test90 = p < 0.1)
test_dmcs <- as.data.frame(wald(wald_dmcs)) %>% mutate(variable = "dmcs",
                                                       test99 = p < 0.01,
                                                       test95 = p < 0.05,
                                                       test90 = p < 0.1)

wald_tests <- rbind(test_pais, test_barrio, test_dmcs)

write_xlsx(wald_tests, "output/wald_tests.xlsx")

enusc_wald_2018 <- subset(enusc_treated, año < 2024 & año > 2018)
wald_pais_2018 <- feols(pad ~ i(años_pre, Tratado, ref = 0) |CUT+año, enusc_wald_2018)
test_pais_2018 <- as.data.frame(wald(wald_pais_2018)) %>% mutate(variable = "pais2018",
                                                       test99 = p < 0.01,
                                                       test95 = p < 0.05,
                                                       test90 = p < 0.1)

write_xlsx(test_pais_2018, "output/wald_test_pais2018.xlsx")

#---- Limpieza de coeficientes para figuras ----

tidy_robust <- function(mod, type = "HC1", cluster = ~ CUT, conf.level = 0.95) {
  cluster <- vcovCL(mod, cluster = cluster, type = type)
  ct <- coeftest(mod, vcov. = cluster)      
  out <- tidy(ct)                   
  df  <- fixest::degrees_freedom(mod, "resid")                   
  alpha <- 1 - conf.level
  crit  <- qt(1 - alpha/2, df)             
  out %>%
    mutate(conf.low  = estimate - crit * std.error,
           conf.high = estimate + crit * std.error)
}

#---- Coeficientes Tabla 5 -----

did1 <- feols(pad ~ Post*Tratado | CUT +  año, enusc_treated)

simple_pais <- tidy_robust(did1, type = "HC1", cluster = ~ CUT, conf.level = 0.95) %>% 
  mutate(n = nobs(did1),
         r2 = r2(did1, "r2"),
         war2 = r2(did1, "war2"),
         modelo = "simple_pais") 
simple_pais$estimate <- round(simple_pais$estimate, 4)
simple_pais$std.error <- round(simple_pais$std.error, 4)
simple_pais$r2 <- round(simple_pais$r2, 4)
simple_pais$war2 <- round(simple_pais$war2, 4)

did2 <- feols(padb ~ Post*Tratado | CUT +  año, enusc_treated)

simple_barrio <- tidy_robust(did2, type = "HC1", cluster = ~ CUT, conf.level = 0.95) %>% 
  mutate(n = nobs(did2),
         r2 = r2(did2, "r2"),
         war2 = r2(did2, "war2"),
         modelo ="simple_barrio")  
simple_barrio$estimate <- round(simple_barrio$estimate, 4)
simple_barrio$std.error <- round(simple_barrio$std.error, 4)
simple_barrio$r2 <- round(simple_barrio$r2, 4)
simple_barrio$war2 <- round(simple_barrio$war2, 4)

did3 <- feols(`Hogar_victimizado` ~ Post*Tratado | CUT +  año, enusc_treated)

simple_dmcs <- tidy_robust(did3, type = "HC1", cluster = ~ CUT, conf.level = 0.95) %>% 
  mutate(n = nobs(did3),
         r2 = r2(did3, "r2"),
         war2 = r2(did3, "war2"),
         modelo ="simple_dmcs") 
simple_dmcs$estimate <- round(simple_dmcs$estimate, 4)
simple_dmcs$std.error <- round(simple_dmcs$std.error, 4)
simple_dmcs$r2 <- round(simple_dmcs$r2, 4)
simple_dmcs$war2 <- round(simple_dmcs$war2, 4)

tabla5 <- rbind(simple_pais, simple_barrio, simple_dmcs)
write_xlsx(table5, "output/tabla5.xlsx")



#---- Tablas 5-6. EH: dmcs ----
did4 <- feols(pad ~ Post*Tratado*`Hogar_victimizado` | CUT +  año, enusc_treated)

pais <- tidy_robust(did4, type = "HC1", cluster = ~ CUT, conf.level = 0.95) 

dmcs_pais <- ggplot(pais, aes(x = estimate, y = term, colour = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(tag ="Figura 5",
       title = "Efectos heterogeneos del Plan en miembros de hogares victimizados",
       subtitle = "Variable dependiente: Percepción de inseguridad en el pais",
       y = "Variables independientes",
       x = "Coeficientes",
       caption = "Modelo estimado es identico al de la Tabla 5, más las interacciones reportadas. Barras horizontales representan intervalos de confianza al 95%. Elaboración propia con datos ENUSC.",
       color = NULL) +
  scale_color_discrete(name = NULL,
                       labels = NULL,
                       breaks = NULL)+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"),
        plot.caption = element_text(face = "plain"))

ggsave("output/figura5.png", width = 9, height = 3, dpi = 300)

did5 <- feols(padb ~ Post*Tratado*`Hogar_victimizado` | CUT +  año, enusc_treated)

barrio <- tidy_robust(did5, type = "HC1", cluster = ~ CUT, conf.level = 0.95)

dmcs_barrio <- ggplot(barrio, aes(x = estimate, y = term, colour = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(tag ="Figura 6",
       title = "Efectos heterogeneos del Plan en miembros de hogares victimizados",
       subtitle = "Variable dependiente: Percepción de inseguridad en el barrio",
       y = "Variables independientes",
       x = "Coeficientes",
       caption = "Modelo estimado es identico al de la Tabla 5, más las interacciones reportadas. Barras horizontales representan intervalos de confianza al 95%. Elaboración propia con datos ENUSC.",
       color = NULL) +
  scale_color_discrete(name = NULL,
                       labels = NULL,
                       breaks = NULL)+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"),
        plot.caption = element_text(face = "plain"))

ggsave("output/figura6.png", width = 9, height = 3, dpi = 300)

#---- Tablas 7-8. EH: genero-----

did2 <- feols(pad ~ Post*Tratado*Mujeres | CUT +  año, enusc_treated)

inseguridad_pais_santiago <- tidy_robust(did2, type = "HC1", cluster = ~ CUT, conf.level = 0.95) %>% 
  mutate(n = row_number(),
         term = case_when(term %in% "`Hogar_victimizado`" ~ "DMCS",.default = term))
pais_genero <- ggplot(inseguridad_pais_santiago, aes(x = estimate, y = term, colour = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(tag ="Figura 7",
       title = "Efectos heterogeneos del Plan en mujeres",
       subtitle = "Variable dependiente: Percepción de inseguridad en el pais",
       y = "Variables independientes",
       x = "Coeficientes",
       caption = "Modelo estimado es identico al de la Tabla 5, más las interacciones reportadas. Barras horizontales representan intervalos de confianza al 95%. Elaboración propia con datos ENUSC.",
       color = NULL) +
  scale_color_discrete(name = NULL,
                       labels = NULL,
                       breaks = NULL)+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"),
        plot.caption = element_text(face = "plain"))

ggsave("output/figura7.png", width = 9, height = 3, dpi = 300)

did3_stats <- enusc_treated %>% group_by(Tratado, Post) %>% summarise(mean = mean(padb),                                                                    sd = sd(padb),
                                                                          n = n())
write_xlsx(did3_stats, "did_stats_barrio.xlsx")

did3 <- feols(padb ~ Post*Tratado*Mujeres | CUT +  año, enusc_treated)

inseguridad_barrio <- tidy_robust(did3, type = "HC1", cluster = ~ CUT, conf.level = 0.95) %>% 
  mutate(n = row_number(),
         term = case_when(term %in% "`Hogar_victimizado`" ~ "DMCS",
                                                                          .default = term))

barrio_genero <- ggplot(inseguridad_barrio, aes(x = estimate, y = term, colour = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(tag ="Figure 8",
       title = "Efectos heterogeneos del Plan en mujeres",
       subtitle = "Varaible dependiente: Percepción de inseguridad en el barrio",
       y = NULL,
       x = "Coeficientes",
       caption = "Modelo estimado es identico al de la Tabla 5, más las interacciones reportadas. Barras horizontales representan intervalos de confianza al 95%. Elaboración propia con datos ENUSC.",
       color = NULL) +
  scale_color_discrete(name = NULL,
                       labels = NULL,
                       breaks = NULL)+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"),
        plot.caption = element_text(face = "plain"))

ggsave("output/figura8.png", width = 9, height = 3, dpi = 300)


#----Tablas 9-10. EH: Edad-----
did_stats <- enusc_treated %>% group_by(Tratado, Post) %>% summarise(mean = mean(pad),                                                                 sd = sd(pad),
                                                                   n = n())
write_xlsx(did_stats, "did_stats_pais.xlsx")

did1 <- feols(pad ~ Tratado*Post*Edad | CUT + año, enusc_treated)

inseguridad_pais <- tidy_robust(did1, type = "HC1", cluster = ~ CUT, conf.level = 0.95) %>% 
  mutate(n = row_number(),
         term = case_when(term %in% "`Hogar_victimizado`" ~ "DMCS",

                                                                          .default = term))
pais_edad <- ggplot(inseguridad_pais, aes(x = estimate, y = term, colour = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(tag ="Figura 9",
       title = "Efectos heterogeneos del Plan en personas mayores de 60 años",
       subtitle = "Percepción de inseguridad en el pais",
       y = "Variables independientes",
       x = "Coeficientes",
       caption = "Modelo estimado es identico al de la Tabla 5, más las interacciones reportadas. Barras horizontales representan intervalos de confianza al 95%. Elaboración propia con datos ENUSC.",
       color = NULL) +
  scale_color_discrete(name = NULL,
                       labels = NULL,
                       breaks = NULL)+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"),
        plot.caption = element_text(face = "plain"))

ggsave("output/figura9.png", width = 9, height = 3, dpi = 300)

did4_stats <- enusc_treated %>% group_by(Tratado, Post) %>% summarise(mean = mean(padb),
                                                                              sd = sd(padb))
did4 <- feols(padb ~ Post*Tratado*Edad | CUT+ año, enusc_treated)

inseguridad_barrio <- tidy_robust(did4, type = "HC1", cluster = ~ CUT, conf.level = 0.95) %>% 
  mutate(n = row_number(),
         term = case_when(term %in% "`Hogar_victimizado`" ~ "DMCS",
                                                                            .default = term))
barrio_edad <- ggplot(inseguridad_barrio, aes(x = estimate, y = term, colour = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(tag ="Figura 10",
       title = "Efectos heterogeneos del Plan en personas mayores de 60 años",
       subtitle = "Percepción de inseguridad en el barrio",
       y = NULL,
       x = "Coeficientes",
       caption = "Modelo estimado es identico al de la Tabla 5, más las interacciones reportadas. Barras horizontales representan intervalos de confianza al 95%. Elaboración propia con datos ENUSC.",
       color = NULL) +
  scale_color_discrete(name = NULL,
                       labels = NULL,
                       breaks = NULL)+
  theme_minimal()+
  theme(text = element_text(family = "serif",
                            face = "bold"),
        plot.caption = element_text(face = "plain"))

ggsave("output/figura10.png", width = 9, height = 3, dpi = 300)
