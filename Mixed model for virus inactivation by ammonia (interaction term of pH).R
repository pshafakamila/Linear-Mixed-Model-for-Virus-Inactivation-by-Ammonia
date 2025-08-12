#plot for summary of review articles
library(ggplot2)
#matrix
matrix$Matrices <- factor(matrix$Matrices, levels = c("Buffer", "Urine", 
                                                      "Fecal sludge", "Digested sludge",
                                                      "Manure","Hatchery waste","Multiple matrices"))
ggplot(data = matrix, aes(x = Matrices, y = Paper, fill=Matrices)) +
  geom_bar(stat="identity") +
  labs(y = "Number of Article", x = "Matrix Type")+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )+ scale_fill_viridis_d()

#virus
virustype$Virus <- factor(virustype$Virus, levels = c("ssRNA", "dsDNA", 
                                                      "ssDNA", "dsRNA"))
ggplot(data = virustype, aes(x = Virus, y = Frequency, fill=Virus)) +
  geom_bar(stat="identity") +
  labs(y = "Frequency being Studied", x = "Virus Type")+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )+ scale_fill_viridis_d()

#plot for difference of kobs between virus group and matrix group
#virus group
library (ggpubr)
library (ggsci)
vtype <- ggboxplot(dataset_new, x = "Virus", y = "logk", fill = "Virus", 
                   add = "jitter", legend = "none", ylab = "log k")+
  stat_compare_means(method = "kruskal.test",label="p.format",label.y=4.8,size=7)+
  stat_compare_means(comparisons = list(c("ssRNA", "ssDNA"), 
                                        c("ssRNA", "dsDNA"),
                                        c("ssRNA", "dsRNA"),
                                        c("ssDNA", "dsRNA")),
                     label = "p.signif", method = "wilcox.test", paired = FALSE, exact = TRUE, 
                     label.y = c(2.3,3,3.7,4.4), size = 7, bracket.size = 1) + theme (element_line(linewidth=1),
                                                                                      axis.title = element_text(size = 20, face = "bold"),
                                                                                      axis.text = element_text(size = 18)) + labs(y = "log kobs", x = "Virus")
themevtype <- vtype + scale_fill_viridis_d()

#matrix group
mtype <- ggboxplot(dataset_new, x = "Matrices", y = "logk", fill = "Matrices", 
                   add = "jitter", legend = "none", ylab = "log k") +
  stat_compare_means(method = "kruskal.test", label = "p.format", label.y = 12,size =6) +
  stat_compare_means(comparisons = list(
    c("Buffer", "Feces"),
    c("Buffer", "Fecal sludge"),
    c("Buffer", "Hatchery waste"),
    c("Urine", "Feces"),
    c("Feces", "Fecal sludge"),
    c("Feces", "Manure"),
    c("Feces", "Digested sludge"),
    c("Feces", "Hatchery waste"),
    c("Fecal sludge", "Manure"),
    c("Fecal sludge", "Digested sludge"),
    c("Fecal sludge", "Hatchery waste") ),
    label = "p.signif", method = "wilcox.test", paired = FALSE, exact = TRUE,
    label.y = c(5,2,3,4,5,6,7,8,9,10,11),size=7, bracket.size = 1) + theme (
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 18)) + labs(y = "log kobs", x = "Matrix")
thememtype <- mtype + scale_fill_viridis_d()

mtype2 <- ggboxplot(dataset_new, x = "Matrices", y = "logk", fill = "Matrices", 
                    add = "jitter", legend = "none", ylab = "log k") +
  stat_compare_means(method = "kruskal.test", label = "p.format", label.y = 2,size =7) + theme (
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18))+ labs(y = "log kobs", x = "Matrix")
thememtype2 <- mtype2 + scale_fill_viridis_d()

#correlation matrix plot
library (ggcorrplot)
cor_matrix <- cor(numerical[,-(1:2)])
rownames(cor_matrix) <- colnames(cor_matrix) <- c("Temperature", "pH", "Ammonia", "kobs")
ggcorrplot(cor_matrix, lab = TRUE, lab_size = 5, 
           tl.cex = 13, tl.col = "black", colors = c("#440154", "#21918c", "#fde725"))

#check distribution of data
library (psych)
dim(numerical)
pairs.panels(numerical[,-(1:2)])

#clustering by pH to arrange data distribution
#optimum cluster number
library(factoextra)
fviz_nbclust(numerical, kmeans, method = "wss")

#optimum k is 3, because if 4, there is significant unbalance data size
#adding matrix and virus type factor in the model
numeric_group <- cbind(numerical, cluster=kmeans (numerical$pH,3,nstart=25)$cluster)
cluster1n <- subset (numeric_group, cluster == 1)
cluster2n <- subset (numeric_group, cluster == 2)
cluster3n <- subset (numeric_group, cluster == 3)

#histogram with color-coded clusters
#convert cluster to factor
library(dplyr)
numeric_group <- numerical %>%
  mutate(cluster = factor(kmeans(numerical$pH, 3, nstart = 25)$cluster))

ggplot(numeric_group, aes(x = pH, fill = cluster)) +
  geom_histogram(binwidth = 0.3, position = "stack", color = "black", alpha = 0.7) +  
  scale_fill_npg(labels = c("Cluster 1", "Cluster 2", "Cluster 3")) +
  labs(
    x = "pH Level",
    y = "Frequency",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 16) +  # Improve readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size=20,face = "bold"),
    axis.text = element_text(size = 18), legend.text = element_text (size=16),
    legend.title = element_text (size=20, face="bold")
  )

#normality test for kobs (dependent variable)
shapiro.test (log(cluster1n$k))
hist (log(cluster1n$k))
shapiro.test (log(cluster2n$k))
hist (log(cluster2n$k))
shapiro.test (log(cluster3n$k))
hist (log(cluster3n$k))

#change variable NH3 to logarithm form
cluster1n$log_NH3 <- log(cluster1n$NH3)
cluster2n$log_NH3 <- log(cluster2n$NH3)
cluster3n$log_NH3 <- log(cluster3n$NH3)

#mixed model and evaluation
library (lme4)
lmer1a <- lmer (log (k) ~ log_NH3+(1|Virus), data=cluster3n)
lmer2a <- lmer (log (k) ~ log_NH3+Temp+Matrices+(1|Virus), data=cluster3n)
lmer3a <- lmer (log (k) ~ log_NH3+Matrices+(1|Virus), data=cluster3n)
lmer4a <- lmer (log (k) ~ log_NH3+Temp+(1|Virus), data=cluster3n)
lmer5a <- lmer (log (k) ~ log_NH3+log(Temp)+log(Matrices)+(1|Virus), data=cluster3n)
lmer6a <- lmer (log (k) ~ log_NH3+log(Temp)+(Matrices)+(1|Virus), data=cluster3n)
lmer7a <- lmer (log (k) ~ log_NH3+Temp+log(Matrices)+(1|Virus), data=cluster3n)
AIC(lmer1a,lmer2a,lmer3a,lmer4a,lmer5a,lmer6a,lmer7a)

#checking interaction terms between pH and log_NH3, or between matrix and log_NH3
library (car)
lmer8a <- lmer (log (k) ~ log_NH3+Temp+Matrices*log_NH3+(Matrices)+(1|Virus), data=cluster3n)
vif(lmer8a) #multicollinear
lmer9a <- lmer (log (k) ~ log_NH3+Temp+Temp*log_NH3+(Matrices)+(1|Virus), data=cluster3n)
vif(lmer9a) #multicollinear
lmer10a <- lmer (log (k) ~ log_NH3+Temp*Matrices+Temp+(Matrices)+(1|Virus), data=cluster3n)
vif(lmer10a)
AIC(lmer8a,lmer9a,lmer10a)


#checking if pH worth to be included or not
lmer11a <- lmer (log (k) ~ log_NH3+Temp+pH+(Matrices)+(1|Virus), data=cluster3n)
lmer12a <- lmer (log (k) ~ log_NH3+pH+(1|Virus), data=cluster3n)
lmer13a <- lmer (log (k) ~ log_NH3+Matrices+pH+(1|Virus), data=cluster3n)
lmer14a <- lmer (log (k) ~ log_NH3+Temp+pH+(1|Virus), data=cluster3n)
lmer15a <- lmer (log (k) ~ log_NH3+log(Temp)+Matrices+pH+(1|Virus), data=cluster3n)
lmer16a <- lmer (log (k) ~ log_NH3+Temp+log(Matrices)+pH+(1|Virus), data=cluster3n)
AIC(lmer11a, lmer12a, lmer13a,lmer14a,lmer15a,lmer16a)

#checking interaction terms pH and other
lmer17a <- lmer (log (k) ~ log_NH3+Temp+pH*log_NH3+(Matrices)+(1|Virus), data=cluster3n)
vif(lmer17a) #multicollinear
lmer18a <- lmer (log (k) ~ log_NH3+Temp+Temp*pH+(Matrices)+(1|Virus), data=cluster3n)
vif(lmer18a) #multicollinear
lmer19a <- lmer (log (k) ~ log_NH3+pH*Matrices+Temp+(Matrices)+(1|Virus), data=cluster3n)
vif(lmer19a)
AIC(lmer17a,lmer18a,lmer19a)

#linear mixed model final (lmer2a was selected because pH and NH3 might also have correlation)
lmer1 <- lmer (log (k) ~ log_NH3+(Temp)+(Matrices)+(1|Virus), data=cluster1n)
lmer2 <- lmer (log (k) ~ log_NH3+(Temp)+(Matrices)+(1|Virus), data=cluster2n)
lmer3 <- lmer (log (k) ~ log_NH3+(Temp)+(Matrices)+(1|Virus), data=cluster3n)

#evaluation by r2 rmse and mae
library(modelr)
data.frame( 
  R2 = rsquare(lmer1, data = cluster1n),
  RMSE = rmse(lmer1, data = cluster1n),
  MAE = mae(lmer1, data = cluster1n)
)
data.frame( 
  R2 = rsquare(lmer2, data = cluster2n),
  RMSE = rmse(lmer2, data = cluster2n),
  MAE = mae(lmer2, data = cluster2n)
)
data.frame( 
  R2 = rsquare(lmer3, data = cluster3n),
  RMSE = rmse(lmer3, data = cluster3n),
  MAE = mae(lmer3, data = cluster3n)
)

#plot mixed model coefficients
library(jtools)
library (sjPlot)
sumplot <- plot_models (lmer1, lmer2, lmer3, show.values = TRUE,
                        axis.labels = c("Matrix","Temperature", "Ammonia"),legend.title = "Cluster",
                        m.labels = c("at pH 6.5-8.45","at pH 8.47-9.2","at pH 9.3-12.5"),
                        value.size = 5, colors = "viridis")
sumplotfinal <- sumplot + theme (plot.title = element_text (size=15, face = "bold"), axis.title = element_text(size = 15, face = "bold"),
                                 axis.text = element_text(size = 15,face = "bold"),
                                 legend.text = element_text (size=13), legend.title = element_text (size=14))

#table summary
sjPlot::tab_model(lmer1)
sjPlot::tab_model(lmer2)
sjPlot::tab_model(lmer3)

#visualize NH3 effect
#cluster1
facet_labels <- c("1" = "ssRNA", "2" = "dsRNA", "3" = "ssDNA", "4" = "dsDNA")
coefs <- fixef(lmer1)

eq_text <- paste0("Y = ", "0.088", "X", "-6.58", "\n",
                  "R² = 0.577")

p1 <- effect_plot(data=cluster1n, model = lmer1, pred = log_NH3, interval = TRUE, plot.points = TRUE, 
                  jitter = 0.05, colors = "#440154")
p1 +
  annotate("text", x = -0.5, y = 2.5, label = eq_text, size=6, color = "black")+
  theme (axis.title = element_text(size = 16, face = "bold"),
         axis.text = element_text(size = 16))+
  labs (x="Log Ammonia Activity (mM)", y = "Log Rate Constant (L mol⁻¹ day⁻¹)")

#cluster2
coefs2 <- fixef(lmer2)

eq_text2 <- paste0("Y = ", "0.486", "X", "-5.99", "\n",
                   "R² = 0.412")

p2 <- effect_plot(data=cluster2n, model = lmer2, pred = log_NH3, interval = TRUE, plot.points = TRUE, 
                  jitter = 0.05, colors = "#21918c")
p2 +
  annotate("text", x = 3, y = 3, label = eq_text2, size=6, color = "black")+
  theme (axis.title = element_text(size = 16, face = "bold"),
         axis.text = element_text(size = 16))+
  labs (x="Log Ammonia Activity (mM)", y = "Log Rate Constant (L mol⁻¹ day⁻¹)")

#cluster3
coefs3 <- fixef(lmer3)

eq_text3 <- paste0("Y = ", "0.527", "X", "-6.45", "\n",
                   "R² = 0.81")

p3 <- effect_plot(data=cluster3n, model = lmer3, pred = log_NH3, interval = TRUE, plot.points = TRUE, 
                  jitter = 0.05, colors = "#95d840")
p3 +
  annotate("text", x = 2.5, y = 2.5, label = eq_text3, size=6, color = "black")+
  theme (axis.title = element_text(size = 16, face = "bold"),
         axis.text = element_text(size = 16))+
  labs (x="Log Ammonia Activity (mM)", y = "Log Rate Constant (L mol⁻¹ day⁻¹)")

#visualize random effect
library (lattice)
ranef1 <- dotplot(ranef(lmer1), groups=cluster1n$Virus, 
                  par.settings = list(dot.symbol = list(col = "#440154"),
                                      axis.text = list(cex = 1)))[['Virus']]
ranef2 <- dotplot(ranef(lmer2), groups=cluster2n$Virus,
                  par.settings = list(dot.symbol = list(col = "#21918c"),
                                      axis.text = list(cex = 1)))[['Virus']]
ranef3 <- dotplot(ranef(lmer3), groups=cluster3n$Virus,
                  par.settings = list(dot.symbol = list(col = "#95d840"),
                                      axis.text = list(cex = 1)))[['Virus']]

library(gridExtra)
library(grid)   
grid.arrange(ranef1, ranef2, ranef3, ncol = 2,
             top = "Random Effects from Linear Mixed Models")

#Align to EPA guideline requirement and recommended condition
#change in accordance with reference conditions
ref_conditions <- data.frame(
  log_NH3 = log(45),
  Temp = c(15,25,35), 
  Matrices = 0.99, 
  Virus = 4
)

#change model (cluster 1, cluster 2, or cluster 3)
ref_conditions$log_k_pred <- predict(lmer2, newdata = ref_conditions)
ref_conditions$k_pred <- exp(ref_conditions$log_k_pred)

#EPA required k based on treatment time
ref_conditions$k_epa <- 9.21/c(180, 60, 15)

# Compare model predictions with WHO criteria
ref_conditions$meets_epa <- ref_conditions$k_pred >= ref_conditions$k_epa

# Print results
print(ref_conditions)