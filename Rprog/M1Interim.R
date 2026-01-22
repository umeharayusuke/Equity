library(tidyverse)
library(dplyr)
library(ggplot2)
library(gdxrrw)
library(stringr)
library(gridExtra)
library(patchwork)
library(cowplot)
library(lemon)
library(purrr)
library(rnaturalearthdata)
library(rnaturalearth)


theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())

setwd("data")


# ghgc --------------------------------------------------------------------


files <- list(
  list(file = "global_17_SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario = "AP"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario = "GDR"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario = "GF"),
  #list(file = "global_17_SSP2_2020NDC_EffortShare_IEPC2_NoCC_No.gdx", scenario = "IEPC"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario = "PCC")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "ghgc")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    mutate(criteria = file_info$scenario)
  
  df <- rbind(df, df_temp)
}


region <-c("USA", "JPN", "XE25", "XOC", "TUR", "XER", "CAN","CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
#region <- c("USA", "JPN", "XE25","XOC","XER", "CAN")
#region <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")
#region <-c( "TUR", "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")

df1 <-    df %>% 
  filter(j %in% region) 

df1$j <- factor(df1$j, levels = c( "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF","USA", "JPN", "XE25","XOC","XER", "CAN","TUR" ))


df2 <- df1 %>%
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  mutate(
    y_pos = ifelse(y_value >= 0, y_value/100, NA),
    y_neg = ifelse(y_value < 0, y_value/100, NA)
  )

g2 <- ggplot(df2, aes(x = Year, group = criteria)) +
  
  # 背景ハイライト
  annotate("rect",
           xmin = "2030", xmax = "2040",
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.6) +
  
  # マイナス部分を淡い赤系で塗る
  geom_ribbon(aes(ymin = 0, ymax = y_neg, fill = criteria),
              alpha = 0.25) +
  
  # 0 の基準線
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  
  # ★ 各点を線で結ぶ（最小限の追加）
  geom_line(aes(
    y = ifelse(!is.na(y_pos), y_pos, y_neg),
    color = criteria
  ), linewidth = 1.1) +
  
  # 既存のポイント（そのまま）
  geom_point(aes(y = y_pos, color = criteria), size = 2.5) +
  geom_point(aes(y = y_neg, color = criteria), size = 3, shape = 17) +
  
  facet_wrap(~ j, scales = "free_y") +
  scale_x_discrete(
    breaks = c("2020", "2040", "2060", "2080", "2100")
  ) +
  labs(
    y = "emission constraint (Mt)"
  ) +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

plot(g2)

output_dir <- file.path("..", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
name <- "ghgc.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# CO2FFIND or GHGT_IMP ----------------------------------------------------------------


files <- list(
  list(file = "global_17_SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario = "AP"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario = "GDR"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario = "GF"),
  #list(file = "global_17_SSP2_2020NDC_EffortShare_IEPC2_NoCC_No.gdx", scenario = "IEPC"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario = "PCC")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "CO2FFIND_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    mutate(criteria = file_info$scenario)
  
  df <- rbind(df, df_temp)
}


region <-c("USA", "JPN", "XE25", "XOC", "TUR", "XER", "CAN","CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
#region <- c("USA", "JPN", "XE25","XOC","XER", "CAN")
#region <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")
#region <-c( "TUR", "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")

df1 <-    df %>% 
  filter(j %in% region) 

df1$j <- factor(df1$j, levels = c( "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF","USA", "JPN", "XE25","XOC","XER", "CAN","TUR" ))


g2 <- df1 %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = y_value/100, group = criteria, color = criteria)) + 
  
  annotate("rect",
           xmin = "2030", xmax = "2040",
           ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.6) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  facet_wrap(~ j, scales = "free_y") + 
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "CO2 emission (Mt)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "CO2FFIND.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# GHGT_IMP ----------------------------------------------------------------


files <- list(
  list(file = "global_17_SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario = "AP"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario = "GDR"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario = "GF"),
  #list(file = "global_17_SSP2_2020NDC_EffortShare_IEPC2_NoCC_No.gdx", scenario = "IEPC"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario = "PCC")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "GHGT_IMP_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    mutate(criteria = file_info$scenario)
  
  df <- rbind(df, df_temp)
}


region <-c("USA", "JPN", "XE25", "XOC", "TUR", "XER", "CAN","CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
#region <- c("USA", "JPN", "XE25","XOC","XER", "CAN")
#region <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")
#region <-c( "TUR", "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")

df1 <-    df %>% 
  filter(j %in% region) 

df1$j <- factor(df1$j, levels = c( "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF","USA", "JPN", "XE25","XOC","XER", "CAN","TUR" ))


g2 <- df1 %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = y_value/100, group = criteria, color = criteria)) + 
  
  annotate("rect",
           xmin = "2035", xmax = "2040",
           ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.6) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  facet_wrap(~ j, scales = "free_y") + 
  scale_x_discrete(
    breaks = c( "2020","2040", "2060",  "2080",  "2100")
  ) +
  labs(y = "Emission credit imports (Mt)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g2)


name <- "GHGT_IMP.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Financial terms ---------------------------------------------------------



files <- list(
  list(file = "global_17_SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario = "AP"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario = "GDR"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario = "GF"),
  #list(file = "global_17_SSP2_2020NDC_EffortShare_IEPC2_NoCC_No.gdx", scenario = "IEPC"),
  list(file = "global_17_SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario = "PCC")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "GHGT_IMP_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(y_value = "value") %>%
    mutate(criteria = file_info$scenario)
  
  df <- rbind(df, df_temp)
}


region <-c("USA", "JPN", "XE25", "XOC", "TUR", "XER", "CAN","CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
#region <- c("USA", "JPN", "XE25","XOC","XER", "CAN")
#region <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")
#region <-c( "TUR", "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")

df1 <-    df %>% 
  filter(j %in% region) 

#df1$j <- factor(df1$j, levels = c( "CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF","USA", "JPN", "XE25","XOC","XER", "CAN","TUR" ))


df_pghg <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "PGHG_load")
  df_temp <- gdx_data %>%
    rename(Year = "i") %>%
    rename(pghg_value = "value") %>%
    mutate(criteria = file_info$scenario)
  
  df_pghg <- rbind(df_pghg, df_temp)
}


df_ghgt <- df %>%
  filter(j %in% region)

df_pghg1 <- df_pghg %>%
  filter(j %in% region)

df_new <- df_ghgt %>%
  left_join(
    df_pghg1,
    by = c("Year", "j", "criteria")
  ) %>%
  mutate(y_value_new = y_value * pghg_value)

#region <-c("USA", "JPN", "XE25", "XNF", "XAF","CHN","IND")

df_new1 <- df_new %>% 
  filter(j %in% region) %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) 

df_new1$j <- factor(df_new1$j, levels = c( "USA", "JPN", "XE25","XOC","XER", "CAN","TUR","CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF" ))


df_new1 <- df_new1 %>%
  mutate(sign = ifelse(y_value_new >= 0, "positive", "negative"))

g_time <- ggplot(df_new1) +
  geom_bar(
    aes(
      x = factor(Year),
      y = y_value_new * 100 * 1e6 / 1e12,
      fill = criteria,
      alpha = sign,
      group = 1
    ),
    stat = "identity",
    position = "stack",
    width = 0.9
  ) +
  scale_alpha_manual(
    values = c("positive" = 1, "negative" = 0.4),
    guide = "none"   
  ) +
  scale_y_continuous(
    name = "Financial transfer through emissions trading (trillion US$)"
  ) +
  scale_x_discrete(breaks = c("2040", "2060", "2080", "2100")) +
  facet_grid(j ~ criteria, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "solid") +
  theme_1 +
  labs(fill = NULL)

print(g_time)

name <- "FT_by_ET.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g_time,
  width = 12,
  height = 16,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Cum CO2 -----------------------------------------------------------------

year <- 2100

load_and_format_data <- function(gdx_file, scenario_name, region_code, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Emi_CO2_Cum"))
  
  
  
  df$scenario <- scenario_name
  
  return(df)
}

scenarios <- list(
  list(gdx_file = "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario_name = "AP"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario_name = "GDR"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario_name = "PCC"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario_name = "GF"),
  #list(gdx_file = "SSP2_2020NDC_EffortShare_IEPC_NoCC_No.gdx", scenario_name = "IEPC"),
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "BaU")
  
)



region_code <- c("R2NonOECD", "R5OECD90+EU")
region_code <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")


all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, year)
})

all_data$scenario <- factor(all_data$scenario, levels = c("AP", "GF", "IEPC", "PCC", "GDR", "BaU"))


g4 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template / 1000, fill = VEMF), stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste("Cumlative CO2 from 2020 to 2100 (Gt)"))) + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  theme_1  +
  labs(fill = NULL) 

print(g4)


name <- "CumCO2.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g4,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Temperature -----------------------------------------------------------------

year <- 2100

load_and_format_data <- function(gdx_file, scenario_name, region_code, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Tem_Glo_Mea"))
  
  
  
  df$scenario <- scenario_name
  
  return(df)
}

scenarios <- list(
  list(gdx_file = "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario_name = "AP"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario_name = "GDR"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario_name = "PCC"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario_name = "GF"),
  #list(gdx_file = "SSP2_2020NDC_EffortShare_IEPC_NoCC_No.gdx", scenario_name = "IEPC"),
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "BaU")
  
)



region_code <- c("R2NonOECD", "R5OECD90+EU")
region_code <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")


all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, year)
})

all_data$scenario <- factor(all_data$scenario, levels = c("AP", "GF", "IEPC", "PCC", "GDR", "BaU"))


g4 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template, fill = VEMF), stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste("Temperature rise by 2100"))) + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  theme_1  +
  labs(fill = NULL) +
  theme(legend.position = "none")  


print(g4)


name <- "Tem.png"



ggsave(
  filename = file.path(output_dir, name),
  plot = g4,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# check IAMC----------------------------------------------------------------


thema <- "Emi_CO2"
thema <- "Car_Rem"
thema <- "Emi_CO2_Ene_and_Ind_Pro"
thema <- "Prc_Car"
thema <- "Pol_Cos_GDP_Los_rat"
thema <- "Pol_Cos_GDP_Los_rat_NPV_5pc"
thema <- "Pol_Cos_Cns_Los_rat"
thema <- "Pol_Cos_Cns_Los_rat_NPV_5pc"



files <- list(
  list(file = "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario = "AP"),
  list(file = "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario = "GDR"),
  list(file = "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario = "GF"),
  #list(file = "SSP2_2020NDC_EffortShare_IEPC_NoCC_No.gdx", scenario = "IEPC"),
  list(file = "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario = "PCC"),
  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "BaU")
)

df <- data.frame()

for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    filter(VEMF == thema) %>%
    rename(Year = "Y") %>%
    rename(CP = "IAMC_template") %>%
    mutate(criteria = file_info$scenario)
  
  df <- rbind(df, df_temp)
}


region <-c("USA", "JPN", "XE25", "XOC", "TUR", "XER", "CAN","CHN","IND", "XSE", "XSA", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
region <- c("USA", "JPN", "XAF","XNF","CHN","IND")
region <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")

df1 <-    df %>% 
  filter(REMF %in% region) 


g1 <- df1 %>% 
  filter(as.numeric(as.character(Year)) %% 5 == 0) %>%
  ggplot(aes(x = Year, y = CP, group = criteria, color = criteria)) + 
  annotate("rect",
          xmin = "2030", xmax = "2040",
         ymin = -Inf, ymax = Inf,
        fill = "grey85", alpha = 0.6) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ REMF, scales = "free_y") + 
  scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  labs(y = "Cumulative Welfare loss rate (%)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g1)


name  <- paste0(thema, ".png")


ggsave(
  filename = file.path(output_dir, name),
  plot = g1,
  width = 11,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# CDR_time ----------------------------------------------------------------


years <- seq(2030, 2100, 5)

load_and_format_data <- function(gdx_file, scenario_name, region_code) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y %in% years) %>%        
    filter(VEMF %in% c("Car_Rem_Bio",
                       "Car_Rem_Bio_wit_CCS", 
                       "Car_Rem_Dir_Air_Cap_wit_CCS", 
                       "Car_Rem_Enh_Wea", 
                       "Car_Rem_Frs",
                       "Car_Rem_Soi_Car_Seq"))
  
  # Rename
  df$VEMF <- recode(df$VEMF,
                    "Car_Rem_Bio_wit_CCS" = "BECCS",
                    "Car_Rem_Bio" = "Biochar",
                    "Car_Rem_Dir_Air_Cap_wit_CCS" = "DACCS",
                    "Car_Rem_Enh_Wea" = "Enhanced Weather",
                    "Car_Rem_Frs" = "Afforestation",
                    "Car_Rem_Soi_Car_Seq" = "Soil Carbon")
  
  df$scenario <- scenario_name
  return(df)
}


scenarios <- list(
  list(gdx_file = "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario_name = "AP"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario_name = "GDR"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario_name = "GF"),
  #list(gdx_file = "SSP2_2020NDC_EffortShare_IEPC_NoCC_No.gdx", scenario_name = "IEPC"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario_name = "PCC")
)

region_code <- c("R2NonOECD", "R2OECD")
region_code <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")


all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code)
})

all_data <- all_data %>%mutate(REMF = recode(REMF, "R5OECD90+EU" = "R5OECD"))

all_data$scenario <- factor(all_data$scenario, levels = c("GF", "PCC", "IEPC", "AP", "GDR"))
#all_data$REMF <- factor(all_data$REMF, levels = c("OECD", "NonOECD"))
all_data$VEMF <- factor(all_data$VEMF, 
                        levels = c("BECCS", "Enhanced Weather", "Biochar", "Soil Carbon", "DACCS", "Afforestation"))

color <- c(
  "BECCS" = "#4DAF4A",
  "Biochar" = "#E69F00",
  "Soil Carbon" = "#A65628",
  "Afforestation" = "#1B7837",
  "Enhanced Weather" = "#377EB8",
  "DACCS" = "#984EA3"
)


g_time <- ggplot(all_data) +
  geom_bar(aes(x = factor(Y), y = IAMC_template/1000, fill = VEMF), 
           stat = "identity", position = "stack", width = 0.9) +
  scale_y_continuous(name = "Carbon removal (Gt)") +
  scale_x_discrete(
    breaks = c( "2040", "2060",  "2080",  "2100")
  ) +
  facet_grid(REMF ~ scenario, scales = "free_y") +  # ← Facet by Region × Scenario
  scale_fill_manual(values = color, guide = guide_legend(reverse = FALSE)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_1 +
  labs(fill = NULL)

print(g_time)

name <- "CDR.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g_time,
  width = 11,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Car Rem -----------------------------------------------------------------

year <- 2100

load_and_format_data <- function(gdx_file, scenario_name, region_code, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Car_Rem_Bio",
                       "Car_Rem_Bio_wit_CCS", 
                       "Car_Rem_Dir_Air_Cap_wit_CCS", 
                       "Car_Rem_Enh_Wea", 
                       "Car_Rem_Frs",
                       "Car_Rem_Soi_Car_Seq"))
  
  df$VEMF <- gsub("Car_Rem_Bio_wit_CCS", "BECCS", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Bio", "Biochar", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Enh_Wea", "Enhanced Weather", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Frs", "Afforestation", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Soi_Car_Seq", "Soil Carbon", df$VEMF)
  
  
  df$scenario <- scenario_name
  
  return(df)
}

scenarios <- list(
  list(gdx_file = "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario_name = "AP"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario_name = "GDR"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario_name = "PCC"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario_name = "GF"),
  #list(gdx_file = "SSP2_2020NDC_EffortShare_IEPC_NoCC_No.gdx", scenario_name = "IEPC"),
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "BaU")
  
)



region_code <- c("R2NonOECD", "R5OECD90+EU")
region_code <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU")
region_code <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")



all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, year)
})

all_data$scenario <- factor(all_data$scenario, levels = c("AP", "GF", "IEPC", "PCC", "GDR", "BaU"))

color <- c(
  "BECCS" = "#4DAF4A",          
  "Biochar" = "#E69F00",       
  "Soil Carbon" = "#A65628",    
  "Afforestation" = "#1B7837",  
  "Enhanced Weather" = "#377EB8",
  "DACCS" = "#984EA3"           
)

all_data$VEMF <- factor(all_data$VEMF, levels = c("BECCS", "Enhanced Weather", "Biochar", "Soil Carbon", "DACCS", "Afforestation"))


g4 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template / 1000, fill = VEMF), stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste("CDR in 2100 (Gt)"))) + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  scale_fill_manual(
    values = color, 
    breaks = c("BECCS", 
               "Enhanced Weather",
               "Biochar", 
               "Soil Carbon",
               "DACCS", 
               "Afforestation"), 
    guide = guide_legend(reverse = FALSE)
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  theme_1  +
  labs(fill = NULL) 

print(g4)


name <- "CDR2100.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g4,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Primary energy  ---------------------------------------------------------


gdx_files <- list(
  "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx" = "AP",
  "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx" = "PCC",
  #"SSP2_2020NDC_EffortShare_IEPC_NoCC_No.gdx" = "IEPC",
  "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx" = "GF",
  "SSP2_BaU_NoCC_No.gdx" = "BaU",
  "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx" = "GDR"
)

year <- 2100  
textsize <- 15  

region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World" )
OECDorNON <- "off"  
non_OECD <- if (OECDorNON == "on") c("R5ASIA","R5REF","R5MAF","R5LAM") else NULL



process_data <- function(data, region_code, year, OECDorNON, non_OECD) {
  df2 <- data %>% filter(REMF %in% region_code)
  df3 <- df2 %>% filter(Y == year)
  df4 <- df3 %>% filter(VEMF %in% c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", 
                                    "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", 
                                    "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                                    "Prm_Ene_Hyd", 
                                    "Prm_Ene_Solar", "Prm_Ene_Win", 
                                    "Prm_Ene_Nuc", 
                                    "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS"))
  
  if (OECDorNON == "on") {
    df4_OECD <- df4 %>% filter(REMF == "R5OECD90+EU")
    df4_OECD$REMF <- gsub("R5OECD90+EU", "OECD", df4_OECD$REMF, fixed = TRUE)
    df4_nonOECD <- df4 %>% filter(REMF == "R2NonOECD")
    df4_nonOECD$REMF <- gsub("R2NonOECD", "NonOECD", df4_nonOECD$REMF, fixed = TRUE)
    df4 <- rbind(df4_OECD, df4_nonOECD)
  }
  
  df4$VEMF <- gsub("Prm_Ene_Hyd", "Hydro", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Solar", "Solar", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Win", "Wind", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Nuc", "Nuclear", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Coa_w_CCS|Prm_Ene_Coa_wo_CCS|Prm_Ene_Oil_w_CCS|Prm_Ene_Oil_wo_CCS|Prm_Ene_Gas_w_CCS|Prm_Ene_Gas_wo_CCS", 
                   "Fossil Fuels", df4$VEMF)
  df4$VEMF <- gsub("Prm_Ene_Bio_w_CCS|Prm_Ene_Bio_wo_CCS", 
                   "Biomass", df4$VEMF)
  
  return(df4)
}

df_list <- lapply(names(gdx_files), function(file_name) {
  data <- rgdx.param(file_name, "IAMC_template")
  df <- process_data(data, region_code, year, OECDorNON, non_OECD)
  df$scenario <- gdx_files[[file_name]]
  return(df)
})

df5 <- do.call(rbind, df_list)

df5$scenario <- factor(df5$scenario, levels = c("AP", "GF", "IEPC", "PCC", "GDR", "BaU"))
#df5$REMF <- factor(df5$REMF, levels = c("OECD",  "NonOECD"))

g2 <- ggplot(data = df5) +
  geom_bar(mapping = aes(x = scenario, y = IAMC_template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  ylab("Primary energy \n in 2100 (EJ/yr)") +
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Biomass" = "darkolivegreen2", 
      "Fossil Fuels" = "gray60",  
      "Hydro" = "lightsteelblue", 
      "Nuclear" = "moccasin",  "Solar" = "lightsalmon", "Wind" = "lightskyblue3"
    ),
    breaks = c("Biomass", "Fossil Fuels",
               "Hydro", "Nuclear",  "Solar", "Wind")
  ) +
  #scale_x_discrete(limits = c("NCLP", "1.5C-0%", "1.5C-100%")) +  
  theme_1+
  labs(fill = "") 


plot(g2)

name <- "Ene_1st.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g2,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Power generation --------------------------------------------------------

gdx_files <- c("SSP2_2020NDC_EffortShare_AP_NoCC_No",
               "SSP2_2020NDC_EffortShare_GDR_NoCC_No",
               "SSP2_2020NDC_EffortShare_PCC_NoCC_No",
               #"SSP2_2020NDC_EffortShare_IEPC_NoCC_No",
               "SSP2_2020NDC_EffortShare_GF_NoCC_No",
               "SSP2_BaU_NoCC_No")
scenarios <- c("AP", "GDR", "PCC", "GF", "BaU")  

prm_vector <- c("Prm_Ene_Coa_w_CCS", "Prm_Ene_Coa_wo_CCS", "Prm_Ene_Gas_w_CCS", "Prm_Ene_Gas_wo_CCS", "Prm_Ene_Oil_w_CCS", "Prm_Ene_Oil_wo_CCS", 
                "Prm_Ene_Hyd", "Prm_Ene_Solar", "Prm_Ene_Win",  "Prm_Ene_Nuc", "Prm_Ene_Bio_w_CCS", "Prm_Ene_Bio_wo_CCS")
sec_vector <- gsub("Prm_Ene", "Sec_Ene_Ele", prm_vector)

region_code <- c("World", "USA", "XE25", "XER", "TUR", "XOC", "CHN", "IND", "JPN", "XSE", "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World")
#region_code <- c("R2NonOECD", "R5OECD90+EU")



process_data <- function(gdx_file, scenario, region_code, year, sec_vector) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% sec_vector)
  df$scenario <- scenario
  
  df$VEMF <- gsub("Sec_Ene_Ele_Hyd", "Hydro", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Solar", "Solar", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Win", "Wind", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Nuc", "Nuclear", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Gas_w_CCS|Sec_Ene_Ele_Gas_wo_CCS|Sec_Ene_Ele_Oil_w_CCS|Sec_Ene_Ele_Oil_wo_CCS|Sec_Ene_Ele_Coa_w_CCS|Sec_Ene_Ele_Coa_wo_CCS", 
                  "Fossil Fuels", df$VEMF)
  df$VEMF <- gsub("Sec_Ene_Ele_Bio_w_CCS|Sec_Ene_Ele_Bio_wo_CCS", 
                  "Biomass", df$VEMF)
  
  return(df)
}

df_all <- data.frame()  

for (i in 1:length(gdx_files)) {
  df_temp <- process_data(gdx_files[i], scenarios[i], region_code, 2100, sec_vector)
  df_all <- rbind(df_all, df_temp)
}

df_all$scenario <- factor(df_all$scenario, levels =  c("AP", "GF", "IEPC", "PCC", "GDR", "BaU"))

if (FALSE) {
  
df_all <- df_all %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD" = "NonOECD",
                       "R5ASIA" = "Asia",
                       "R5REF" = "Former Soviet Union",
                       "R5MAF" = "Middle East and Africa",
                       "R5LAM" = "Latin America"))

#df_all$REMF <- factor(df_all$REMF, levels = c("OECD", "NonOECD"))
}

g3 <- ggplot(data = df_all) +
  geom_bar(mapping = aes(x = scenario, y = IAMC_template, fill = VEMF), 
           stat = "identity", width = 0.7) +
  ylab("Power generation \n in 2100 (EJ/yr)") +
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  scale_fill_manual(values = c(
    "Biomass" = "darkolivegreen2", 
    "Fossil Fuels" = "grey50", 
    "Geothermal" = "peru", "Hydro" = "lightsteelblue", 
    "Nuclear" = "moccasin", "Solar" = "lightsalmon", "Wind" = "lightskyblue3"
  ),
  breaks = c("Biomass",  "Fossil Fuels", 
             "Geothermal", "Hydro", "Nuclear",  "Solar", "Wind")) +
  labs(fill = "") +   
  theme_1

plot(g3)

name <- "Ene_2nd.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g3,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Final energy source -----------------------------------------------------


year <- 2100

load_and_format_data <- function(gdx_file, scenario_name, region_code, year) {
  df <- rgdx.param(gdx_file, "IAMC_template") %>%
    filter(REMF %in% region_code) %>%
    filter(Y == year) %>%
    filter(VEMF %in% c("Fin_Ene_Ele",
                       "Fin_Ene_Gas",
                       "Fin_Ene_Heat",
                       "Fin_Ene_Hyd",
                       "Fin_Ene_Liq_Oil",
                       "Fin_Ene_Liq_Bio",
                       "Fin_Ene_SolidsCoa",
                       "Fin_Ene_SolidsBio"))
  
  df$VEMF <- gsub("Fin_Ene_Ele", "Electricity", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Gas", "Gas", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Heat", "Heat", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Hyd", "Hydrogen", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Oil", "Oil", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_Liq_Bio", "Biofuel", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsCoa", "Coal", df$VEMF)
  df$VEMF <- gsub("Fin_Ene_SolidsBio", "Biomass", df$VEMF)
  
  df$scenario <- scenario_name
  
  return(df)
}


scenarios <- list(
  list(gdx_file = "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx", scenario_name = "AP"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario_name = "GDR"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario_name = "PCC"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx", scenario_name = "GF"),
  #list(gdx_file = "SSP2_2020NDC_EffortShare_IEPC_NoCC_No.gdx", scenario_name = "IEPC"),
  list(gdx_file = "SSP2_BaU_NoCC_No.gdx", scenario_name = "BaU")
  
)

region_code <- c("World", "USA", "XE25", "XER", "TUR", "XOC", "CHN", "IND", "JPN", "XSE", "XSA", "CAN", "BRA", "XLM", "CIS", "XME", "XNF", "XAF")
region_code <- c("R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM", "World")



all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, year)
})

all_data$scenario <- factor(all_data$scenario, levels =  c("AP", "GF", "IEPC", "PCC", "GDR", "BaU"))

if (FALSE) {
all_data <- all_data %>%
  mutate(REMF = recode(REMF,
                       "R5OECD90+EU" = "OECD",
                       "R2NonOECD" = "NonOECD",
                       "R5ASIA" = "Asia",
                       "R5REF" = "Former Soviet Union",
                       "R5MAF" = "Middle East and Africa",
                       "R5LAM" = "Latin America"))
#all_data$REMF <- factor(all_data$REMF, levels = c("OECD", "NonOECD"))
}

df_all_sum <- all_data %>%
  group_by(scenario, REMF) %>%
  summarise(total_emission = sum(IAMC_template, na.rm = TRUE))

color <- c( 
  "Coal"="grey70",
  "Oil"="sandybrown",
  "Gas"="moccasin",
  "Biomass"="#A9D65D",
  "Biofuel"="#DBFF70",
  "Electricity"="lightsteelblue",
  "Heat"="salmon",
  "Hydrogen"="thistle2"
)



g4 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template, fill = VEMF), stat = "identity", position = "stack", width = 0.7)+
  ylab("Final Energy \n in 2100 (EJ/yr) ") + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") + 
  scale_fill_manual(
    values = color, 
    breaks = c("Biofuel",
               "Biomass",
               "Coal",
               "Electricity",
               "Gas",
               "Heat",
               "Hydrogen",
               "Oil"
    ), 
    guide = guide_legend(reverse = FALSE)
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
  labs(fill = "") +  
  theme_1  


print(g4)


name <- "Ene_3dr.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g4,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Energy3 -----------------------------------------------------------------


g2_with_legend <- g2 + theme(legend.position = "right")
g2_legend <- get_legend(g2_with_legend)

g3_with_legend <- g3 + theme(legend.position = "right")
g3_legend <- get_legend(g3_with_legend)

g4_with_legend <- g4 + theme(legend.position = "right")
g4_legend <- get_legend(g4_with_legend)

g_main <- ggdraw() +
  draw_plot(g2 + theme(legend.position = "none"), x = 0,   y = 0.7, width = 0.8, height = 0.3) +
  draw_plot(g3 + theme(legend.position = "none"), x = 0, y = 0.4, width = 0.8, height = 0.3) +
  draw_plot(g4 + theme(legend.position = "none"), x = 0,   y = 0.1, width = 0.8, height = 0.3) +
  draw_plot_label(
    label = c("a", "b", "c"),
    x = c(0, 0, 0),
    y = c(1, 0.7, 0.4),
    size = 14
  )

g <- ggdraw() +
  draw_plot(g_main, x = 0, y = 0, width = 0.8, height = 1) +
  draw_plot(g2_legend, x = 0.7, y = 0.8, width = 0.15, height = 0.2) +
  draw_plot(g3_legend, x = 0.7, y = 0.5, width = 0.15, height = 0.2) +
  draw_plot(g4_legend, x = 0.7, y = 0.2, width = 0.15, height = 0.2)

plot(g)

name <- "Ene_combine.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

