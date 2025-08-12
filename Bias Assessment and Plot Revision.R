library(ggplot2)
library(ggpubr)
library (dplyr)

#Virus-log kobs
ggplot(dataset_new, aes(x = Virus, y = logk, fill = Virus)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(shape = Matrices),  
              width = 0.2, size = 2, alpha = 0.8) +
  stat_compare_means(method = "kruskal.test", label = "p.format", label.y = 4.8, size = 7) +
  stat_compare_means(comparisons = list(c("ssRNA", "ssDNA"), 
                                        c("ssRNA", "dsDNA"),
                                        c("ssRNA", "dsRNA"),
                                        c("ssDNA", "dsRNA")),
                     label = "p.signif", method = "wilcox.test", paired = FALSE, exact = TRUE, 
                     label.y = c(2.3, 3, 3.7, 4.4), size = 7, bracket.size = 1) +
  scale_fill_viridis_d() +  guides(fill = "none")+
  scale_shape_manual(values = 0:7) +
  labs(x = "Virus", y = "log kobs", shape = "Matrices") +
  theme_minimal(base_size = 16) +
  theme(
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18, color = "black"),
    legend.position = "right"
  )

#Matrix-log kobs
ggplot(dataset_new, aes(x = Matrices, y = logk, fill = Matrices)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(shape = Virus),  
              width = 0.2, size = 2, alpha = 0.8) +
  stat_compare_means(method = "kruskal.test", label = "p.format", label.y = 4.8, size = 7) +
  scale_fill_viridis_d() +  guides(fill = "none")+
  scale_shape_manual(values = 0:7) +
  labs(x = "Matrix", y = "log kobs", shape = "Virus") +
  theme_minimal(base_size = 16) +
  theme(
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18, color = "black"),
    legend.position = "right"
  )

#Risk of Bias
studyrisk <- studyrisk %>%
  mutate(across(exp_m:overall, ~tools::toTitleCase(.)))

studyrisk_long <- studyrisk %>%
  pivot_longer(
    cols = c(exp_m, out_m, dat_rep, repl, overall),
    names_to = "Domain",
    values_to = "Risk"
  )

domain_labels <- c(
  exp_m = "Ammonia Measurement",
  out_m = "Inactivation Data Outcome",
  dat_rep = "Data Reporting",
  repl = "Replication",
  overall = "Overall Risk"
)

studyrisk_long$Domain <- factor(studyrisk_long$Domain, levels = names(domain_labels), labels = domain_labels)

risk_colors <- c(
  "Low" = "#2ecc71",       
  "Moderate" = "#f1c40f",  
  "High" = "#e74c3c"       
)

ggplot(studyrisk_long, aes(x = Domain, y = study, fill = Risk)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_manual(values = risk_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(fill = "Risk Level") +
  coord_fixed(ratio = 1)

#Funnel Plot
library(dplyr)
library(metafor)
fplot_data <- dataset_neww %>%
  group_by(Paper) %>%
  summarise(
    mean_logk = mean(log_k, na.rm = TRUE),
    se_logk = sd(log_k, na.rm = TRUE) / sqrt(n()),
    n = n()
  )
fplot_data <- fplot_data %>%
  mutate(
    lower = mean_logk - 1.96 * se_logk,
    upper = mean_logk + 1.96 * se_logk
  )
colnames(fplot_data) <- c("StudyID", "yi", "sei")
funnel(x = fplot_data$yi, sei = fplot_data$sei,
       xlab = "log (kobs)",
       ylab = "Standard Error")
