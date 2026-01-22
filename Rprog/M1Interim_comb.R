
thema <- "Emi_CO2"
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


g4 <- df1 %>% 
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
  labs(y = "Cumulative Consumption loss rate (%)") +
  theme_1 +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

plot(g4)

# combine -----------------------------------------------------------------


g1_with_legend <- g1 + theme(legend.position = "right")
g1_legend <- get_legend(g1_with_legend)


g_main <- ggdraw() +
  draw_plot(g1 + theme(legend.position = "none"), x = 0,   y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(g2 + theme(legend.position = "none"), x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(g3 + theme(legend.position = "none"), x = 0,   y = 0, width = 0.5, height = 0.5) +
  draw_plot(g4 + theme(legend.position = "none"), x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(
    label = c("a", "b", "c", "d"),
    x = c(0, 0.5, 0, 0.5),
    y = c(1, 1, 0.5, 0.5),
    size = 14
  )

g <- ggdraw() +
  draw_plot(g_main, x = 0, y = 0, width = 0.9, height = 1) +
  draw_plot(g1_legend, x = 0.9, y = 0.4, width = 0.1, height = 0.3) 

plot(g)



name <- "Economy.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 16,
  height = 12,
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
  scale_y_continuous(name = expression(paste("Cumlative CO2 by 2100 (Gt)"))) + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  theme_1  +
  labs(fill = NULL) 

print(g4)



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


g5 <- ggplot(data = all_data) +
  geom_bar(aes(x = scenario, y = IAMC_template, fill = VEMF), stat = "identity", position = "stack", width = 0.7) +
  scale_y_continuous(name = expression(paste("Temperature rise by 2100"))) + 
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +  
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  theme_1  +
  labs(fill = NULL) +
  theme(legend.position = "none")  


print(g5)


# combine ------------------------------------------------------------------



g_main <- ggdraw() +
  draw_plot(g4 + theme(legend.position = "none"), x = 0,   y = 0.5, width = 1, height = 0.5) +
  draw_plot(g5 + theme(legend.position = "none"), x = 0,   y = 0.0, width = 1, height = 0.5) +
  draw_plot_label(
    label = c("a", "b"),
    x = c(0, 0),
    y = c(1, 0.5),
    size = 14
  )


plot(g_main)

name <- "co2_magicc.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g_main,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

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

g1 <- ggplot(df2, aes(x = Year, group = criteria)) +
  
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

plot(g1)


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


g3 <- df1 %>% 
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

plot(g3)


# combine -----------------------------------------------------------------


g2_with_legend <- g2 + theme(legend.position = "right")
g2_legend <- get_legend(g2_with_legend)


g_main <- ggdraw() +
  draw_plot(g1 + theme(legend.position = "none"), x = 0,   y = 0.66, width = 1, height = 0.33) +
  draw_plot(g2 + theme(legend.position = "none"), x = 0, y = 0.33, width = 1, height = 0.33) +
  draw_plot(g3 + theme(legend.position = "none"), x = 0,   y = 0, width = 1, height = 0.33) +
  draw_plot_label(
    label = c("a", "b", "c"),
    x = c(0, 0, 0),
    y = c(1, 0.66, 0.33),
    size = 14
  )

g <- ggdraw() +
  draw_plot(g_main, x = 0, y = 0, width = 0.9, height = 1) +
  draw_plot(g2_legend, x = 0.9, y = 0.45, width = 0.1, height = 0.2)

plot(g)



name <- "emission3.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 16,
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

#all_data$scenario <- factor(all_data$scenario, levels = c("GF", "PCC", "IEPC", "AP", "GDR"))
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


g1 <- ggplot(all_data) +
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

print(g1)



# cumulative CDR ----------------------------------------------------------



start_year <- 2030
end_year   <- 2100
load_and_format_data <- function(gdx_file, scenario_name, region_code, start_year, end_year) {
  
  df <- rgdx.param(gdx_file, "IAMC_template")
  
  # Y を数値に変換
  df$Y <- as.numeric(as.character(df$Y))
  
  df <- df %>%
    filter(REMF %in% region_code) %>%
    filter(Y >= start_year & Y <= end_year) %>%  # ← ここが正常に動くようになる
    filter(VEMF %in% c(
      "Car_Rem_Bio",
      "Car_Rem_Bio_wit_CCS", 
      "Car_Rem_Dir_Air_Cap_wit_CCS", 
      "Car_Rem_Enh_Wea", 
      "Car_Rem_Frs",
      "Car_Rem_Soi_Car_Seq"
    ))
  
  if (nrow(df) == 0) {
    message("⚠ データなし：", gdx_file, " / region=", paste(region_code, collapse=","))
  }
  
  # 名前変換
  df$VEMF <- gsub("Car_Rem_Bio_wit_CCS",         "BECCS",            df$VEMF)
  df$VEMF <- gsub("Car_Rem_Bio",                  "Biochar",          df$VEMF)
  df$VEMF <- gsub("Car_Rem_Dir_Air_Cap_wit_CCS",  "DACCS",            df$VEMF)
  df$VEMF <- gsub("Car_Rem_Enh_Wea",              "Enhanced Weather", df$VEMF)
  df$VEMF <- gsub("Car_Rem_Frs",                  "Afforestation",    df$VEMF)
  df$VEMF <- gsub("Car_Rem_Soi_Car_Seq",          "Soil Carbon",      df$VEMF)
  
  df$scenario <- scenario_name
  
  return(df)
}
# シナリオ一覧
scenarios <- list(
  list(gdx_file = "SSP2_2020NDC_EffortShare_AP_NoCC_No.gdx",  scenario_name = "AP"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GDR_NoCC_No.gdx", scenario_name = "GDR"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_PCC_NoCC_No.gdx", scenario_name = "PCC"),
  list(gdx_file = "SSP2_2020NDC_EffortShare_GF_NoCC_No.gdx",  scenario_name = "GF")
)


# 地域
region_code <- c("R5REF", "R5LAM", "R5ASIA", "R5MAF", "R5OECD90+EU","World")

# 全データ読み込み（2030〜2100 全部）
all_data <- purrr::map_dfr(scenarios, function(scenario) {
  load_and_format_data(scenario$gdx_file, scenario$scenario_name, region_code, start_year, end_year)
})

# 描画順
#all_data$scenario <- factor(all_data$scenario, levels = c("AP", "GF", "IEPC", "PCC", "GDR"))

# 色
color <- c(
  "BECCS"           = "#4DAF4A",
  "Biochar"         = "#E69F00",
  "Soil Carbon"     = "#A65628",
  "Afforestation"   = "#1B7837",
  "Enhanced Weather"= "#377EB8",
  "DACCS"           = "#984EA3"
)

# VEMF の順序
all_data$VEMF <- factor(
  all_data$VEMF,
  levels = c("BECCS", "Enhanced Weather", "Biochar", "Soil Carbon", "DACCS", "Afforestation")
)

# --------------------------
# ⭐ 2030〜2100 の累積値を計算
# --------------------------

all_data_cum <- all_data %>%
  group_by(scenario, REMF, VEMF) %>%
  summarise(
    value_cum = sum(IAMC_template) / 1000,   # Gt に直す
    .groups = "drop"
  )


# --------------------------
# ⭐ 累積値の棒グラフ（stacked bar）
# --------------------------

g2 <- ggplot(data = all_data_cum) +
  geom_bar(
    aes(x = scenario, y = value_cum, fill = VEMF),
    stat = "identity",
    position = "stack",
    width = 0.7
  ) +
  scale_y_continuous(name = expression("CDR cumulative (2030–2100, Gt)")) +
  facet_wrap(~REMF, ncol = 3, scales = "free_y") +
  scale_fill_manual(
    values = color,
    breaks = c("BECCS", "Enhanced Weather", "Biochar", "Soil Carbon", "DACCS", "Afforestation"),
    guide = guide_legend(reverse = FALSE)
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(fill = NULL) +
  theme_1

print(g2)


# combine -----------------------------------------------------------------


g2_with_legend <- g2 + theme(legend.position = "right")
g2_legend <- get_legend(g2_with_legend)

g_main <- ggdraw() +
  draw_plot(g1 + theme(legend.position = "none"), x = 0,   y = 0.5, width = 1, height = 0.5) +
  draw_plot(g2 + theme(legend.position = "none"), x = 0,   y = 0.0, width = 1, height = 0.5) +
  draw_plot_label(
    label = c("a", "b"),
    x = c(0, 0),
    y = c(1, 0.5),
    size = 14
  )

g <- ggdraw() +
  draw_plot(g_main, x = 0, y = 0, width = 0.85, height = 1) +
  draw_plot(g2_legend, x = 0.87, y = 0.45, width = 0.1, height = 0.2)

plot(g)

name <- "CDR_final.png"



ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)