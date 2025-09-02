
###Copyright (c) 8.31.2025 by Jinshuo Zhang
###Licensed under CC BY-NC-ND 4.0: https://creativecommons.org/licenses/by-nc-nd/4.0/

library(zip)
library(haven)
library(tidyverse)
library(ggridges)
library(ggsci)
library(Cairo)

rm(list = ls())
use_pal <- pal_lancet()(9)

temp_dir <- tempfile() 
dir.create(temp_dir, recursive = TRUE) 

unzip(paste0("RAWDATA/",list.files("RAWDATA")[1]), exdir = temp_dir)
unzip(paste0("RAWDATA/",list.files("RAWDATA")[2]), exdir = temp_dir)
unzip(paste0("RAWDATA/",list.files("RAWDATA")[3]), exdir = temp_dir)

rawdata <- list(
  read_dta(paste0(temp_dir,"/",list.files(temp_dir)[1])),
  read_dta(paste0(temp_dir,"/",list.files(temp_dir)[2])),
  read_dta(paste0(temp_dir,"/",list.files(temp_dir)[3]))
)

CESD8_name <- paste0("qn4",c("06","07",11,12,14,16,18,20))
var_name <- c(CESD8_name,"age","gender")

data <- list(
  rawdata[[1]][,var_name],
  rawdata[[2]][,var_name],
  rawdata[[3]][,var_name]
) |> bind_rows() |> as.data.frame()

data[data < 0] <- NA

data <- na.omit(data) 

data[,c(4,6)] <- 5 - data[,c(4,6)]

data$SCORE <- rowSums(data[, 1:8])

data$gender <- factor(ifelse(data$gender == 0, "Female","Male"))

quantiles <- quantile(data$SCORE, probs = c(0.25, 0.5, 0.75))

quantile_df <- data.frame(
  q = names(quantiles),
  value = as.numeric(quantiles)
)

Density_plot <- ggplot(data, aes(x = SCORE)) +
  geom_density(fill = use_pal[1],alpha = 0.6,adjust = 3,color = NA) +
  geom_vline(data = quantile_df, aes(xintercept = value, color = q), 
             linetype = "dashed", linewidth = 1.5) +
  scale_color_manual(values = use_pal[c(2,3,5)])+
  geom_text(data = quantile_df, 
            aes(x = value, y = 0, label = paste0(q, " = ", round(value, 1))),
            vjust = -0.5, hjust = -2.3, angle = 90,size = rel(5)) +
  labs(title = "Score Density Distribution and Quartile Points (N=80147)",
       x = "Score",
       y = "Density",
       color = "Quartile Points") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,vjust = 2,size = rel(1.5), face = "bold"),
        axis.title.x = element_text(size = rel(1.3), face = "bold"),
        axis.title.y = element_text(size = rel(1.3), face = "bold",vjust = 2),
        axis.text.y = element_text(size=rel(1.2)),
        axis.text.x = element_text(size=rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.3), face = "bold"),
        legend.key.height = unit(1, "cm"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey80",linewidth = 0.6),)

cairo_ps(filename = "Figure/Fig_Density.eps", onefile = FALSE, fallback_resolution = 700,  width = 11, height = 6)
print(Density_plot)
dev.off()

CairoJPEG(filename = "Figure_small/Fig_Density.jpeg", width = 11, height = 6,units="in",dpi=400)
print(Density_plot)
dev.off()


data$age_group <- factor(
  ifelse(data$age <= 15, "Children and Adolescents",
         ifelse(data$age <= 59, "Working-age Population", "Elderly Population")),
  levels = c("Elderly Population", "Working-age Population", "Children and Adolescents")
)

data$score_group <- ifelse(data$SCORE <= quantiles[1],"Low",
                           ifelse(data$SCORE <= quantiles[2],"Medium-Low",
                                  ifelse(data$SCORE <= quantiles[3],"Medium-High",
                                         "High" )))

sampled_data <- data %>%
  dplyr::group_by(age_group, gender, SCORE) %>%  
  dplyr::sample_frac(0.1) %>% 
  dplyr::ungroup()


Ridge_plot <- ggplot(data, aes(
  x = SCORE,
  y = age_group,
  fill = gender,
  color = gender
)) +
  geom_density_ridges(
    jittered_points = FALSE,
    scale = 0.9,
    rel_min_height = 0.01,
  ) +
  geom_point(
    data = sampled_data, 
    aes(x = SCORE, y = age_group), 
    size = 0.6,
    position = position_jitter(width = 0.5, height = 0.1), 
    alpha = 0.6 
  ) +
  scale_fill_manual(values = pal_npg(alpha = 0.5)(2),name = "Sex" ) +
  scale_color_manual(values = pal_npg(alpha = 1)(2),guide = "none" ) +
  coord_cartesian(clip = "off") +
  guides(
    fill = guide_legend(
      override.aes = list(
        fill = pal_npg(alpha = 0.8)(2),
        color = NA
      )
    )
  ) +
  theme_ridges(center = TRUE)+  
  labs(title = "Score Distribution by Age Group and Gender (N=80147)",
                                     x = "Score",
                                     y = "Age Group",
                                     color = "Sex") +
  theme_minimal() +
  theme(plot.title = element_text(vjust = 2,size = rel(1.5), face = "bold"),
        axis.title.x = element_text(size = rel(1.3), face = "bold",vjust = 5),
        axis.title.y = element_text(size = rel(1.3), face = "bold",vjust = 2),
        axis.text.y = element_text(size=rel(1.4)),
        axis.text.x = element_text(size=rel(1.4),vjust = 10),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.3), face = "bold"),
        legend.key.height = unit(1, "cm"),
        panel.background = element_blank(),)

cairo_ps(filename = "Figure/Fig_Ridge.eps", onefile = FALSE, fallback_resolution = 700,  width = 11, height = 5)
print(Ridge_plot)
dev.off()

CairoJPEG(filename = "Figure_small/Fig_Ridge.jpeg", width = 11, height = 5,units="in",dpi=400)
print(Ridge_plot)
dev.off()

data$gender <- as.numeric(data$gender)

set.seed(100)

data <- data[sample(nrow(data)), ]

split_idx <- floor(0.1 * nrow(data))

Independent_test_set <- data |> slice(1:split_idx)
data <- data |> slice((split_idx + 1):nrow(data))

saveRDS(data,"data/data.RDS")

saveRDS(Independent_test_set,"data/Independent_test_set.RDS")

rm(list = ls())

