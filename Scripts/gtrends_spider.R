## ------------------------------------------------------------------------
## 'PAPER TITLE'
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
# 'R script to reproduce the full analysis'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.3.0) and R studio (v. 2023.03.1+446)
# Authors: Stefano Mammola & Andre-Philippe Drapeau Picard

##############
# R packages #
##############

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("dplyr",
               "parameters",
               "performance",
               "see",
               "tidyverse",
               "tidylog")

#################
# plot settings #
#################

theme_set(theme_bw())

theme_update(
  plot.background = element_blank(), #No background
  panel.grid = element_blank(), #No gridlines
  axis.text  = element_text(size = 10, colour = "grey10"),
  axis.title = element_text(size = 12, colour = "grey10")
)

####################
# Data preparation #
####################

# Import data -------------------------------------------------------------

db <- read.csv("Data/spiders_data_metadata_masterfile.csv" , header=TRUE, as.is = FALSE) |>
  dplyr::select(c(-1))

str(db)

#combine bites and deadly bites
db <- db %>% mutate(TypeEvent = Bite + Death)
db$TypeEvent <- as.factor(db$TypeEvent) ; levels(db$TypeEvent) <- c("Encounter","Bite","Deadly Bite")

#combine experts
db <- db %>% mutate(Other_Experts = Expert_doctor + Expert_others)
db$Other_Experts <- ifelse(db$Other_Experts > 0 , 1 , 0) %>% as.factor()

#Total number of Errors
db <- db %>% mutate(TotalErrors = tidyr::replace_na(as.numeric(Taxonomic_error),0) + 
                      tidyr::replace_na(as.numeric(Venom_error),0)     + 
                      tidyr::replace_na(as.numeric(Anatomy_error),0)  + 
                      tidyr::replace_na(as.numeric(Photo_error),0))

db$Errors <- ifelse(db$TotalErrors > 0 , 1 , 0) %>% as.factor()

# Figures

#Total number of Errors
db <- db %>% mutate(Figures = tidyr::replace_na(as.numeric(Figure_species),0) + 
                      tidyr::replace_na(as.numeric(Figure_bite),0))

db$Figures <- ifelse(db$Figures > 0 , 1 , 0) %>% as.factor()

# create a variable period
db$period <- ifelse(db$day < 1 , "before" , "after") %>% as.factor()
db <- within(db, period <- relevel(period, ref = "before"))

#######################
# Analysis for Gtrend #
#######################

# Calculating trend -------------------------------------------------------

db_trend <- db[db$data_source == "GoogleTrends",]

db_trend <- droplevels(db_trend)

levels(db_trend$search_term) <- c(rep("black widow",2),rep("brown recluse",2),rep("spider",2),rep("spider bite",2))

#Here, for each news, we model the temporal trend before and after the publication for each search term

for(i in 1 : nlevels(db_trend$event_id)) {
  
  db_i <- db_trend[db_trend$event_id == levels(db_trend$event_id)[i],] #subdataset for the i news
  
  db_i <- droplevels(db_i)
  
  delta  <- c()
  coef   <- c()
  
  #model trend before and after
  for(j in 1:nlevels(db_i$search_term)) {
    
    db_i_j <- db_i[db_i$search_term == levels(db_i$search_term)[j], ] #extract subdataset for the search term j

    model <- glm(cbind(hits,rep(100,nrow(db_i_j))) ~ period, data = db_i_j, family = "binomial")

    delta <- append(delta, model$coefficients[2])
    
  }
  
  #store data for delta
  db_delta_i <- data.frame(ID             = rep(levels(db$event_id)[i],j),
                           term           = levels(db_i$search_term),
                           trend          = delta,
                           country        = rep(levels(db_i$country),j),
                           circulation    = rep(levels(db_i$Circulation),j),
                           TypeEvent      = rep(levels(db_i$TypeEvent),j),
                           sensationalism = rep(levels(as.factor(db_i$Sensationalism)),j),
                           error          = rep(levels(as.factor(db_i$Errors)),j),
                           expert_spider  = rep(levels(as.factor(db_i$Expert_arachnologist)),j),
                           other_experts  = rep(levels(as.factor(db_i$Other_Experts)),j),
                           Figures        = rep(levels(as.factor(db_i$Figures)),j),
                           Genus          = rep(levels(as.factor(db_i$Genus)),j),
                           Family          = rep(levels(as.factor(db_i$Family)),j))
  
  if(i > 1)
    db_delta <- rbind(db_delta, db_delta_i)
  else
    db_delta <- db_delta_i
  
}

db_delta <- db_delta %>%  mutate_if(is.character, as.factor)

#levels(db_delta$circulation) <- c("(Inter)national", "(Inter)national", "Regional")

# Data analysis: General terms ---------------------------------------

db_delta_general <- db_delta[db_delta$term %in% c("spider", "spider bite"),] ; db_delta_general <- droplevels(db_delta_general)

(plot1a <- ggplot(db_delta_general, aes(x = trend, color = term, fill = term)) +
  geom_density(alpha = 0.7)+
  geom_vline(aes(xintercept=0),color="grey10", linetype="dashed", linewidth=.5)+
  scale_color_manual("Search term", values = c("grey40","orange"))+
  scale_fill_manual("Search term", values = c("grey40","orange"))+
  labs(x = "Intercept change for search volume in gTrend",
       y = "Density")+
  
  annotate("segment", x = 0.7, xend = 1, y = 3, yend = 3,
           color = "grey10",
           arrow = arrow(ends = "last", 
                         angle = 15, 
                         length = unit(.2,"cm")))+
  
  annotate("text", x = 0.3, y = 3, hjust = 0, vjust = 0.5,
           size = 3,
           color = "grey10",
           label = "Greater search intensity\nafter the news")+
  
  annotate("segment", x = -0.7, xend = -1, y = 3, yend = 3,
           color = "grey10",
           arrow = arrow(ends = "last", 
                         angle = 15, 
                         length = unit(.2,"cm")))+
  
  annotate("text", x = -0.3, y = 3, hjust =1, vjust = 0.5,
           size = 3,
           color = "grey10",
           label = "Greater search intensity\nbefore the news")+
  theme(legend.position = c(0.1,0.5))
)

sum(db_delta[db_delta$term == "spider bite",]$trend > 0) / nrow(db_delta[db_delta$term == "spider bite",])
sum(db_delta[db_delta$term == "spider",]$trend > 0) / nrow(db_delta[db_delta$term == "spider",])

#data exploration for lm
table(db_delta$circulation) 
table(db_delta$TypeEvent) 
table(db_delta$sensationalism) 

#linear model
m1 <- lm(trend ~ country + circulation + TypeEvent + 
                 Figures + error + sensationalism + 
                 expert_spider + other_experts, data = db_delta_general[db_delta_general$term == "spider",])
summary(m1)

performance::check_model(m1)
performance::r2(m1)

#plot of the model
(par.M1 <- parameters::parameters(m1))

(table.M1 <- par.M1 %>% dplyr::select(Parameter,
                                     Beta = Coefficient,
                                     SE,
                                     CI_low,
                                     CI_high,
                                     t,
                                     p) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, ~ round(.,3))) ; rm(par.M1)

str(table.M1)

table.M1$Parameter <- as.factor(table.M1$Parameter)

#rename
levels(table.M1$Parameter) <- c("Intercept", 
                                "Circulation [Regional]", 
                                "Spider expert [yes]",
                                "Other experts [yes]",
                                "Sensationalism [yes]",
                                "Search Term [Spider bite]", 
                                "Event type [Encounter]")
  
#sort
table.M1$Parameter <- factor(table.M1$Parameter, rev(c("Intercept", 
                                                     "Search Term [Spider bite]", 
                                                     "Circulation [Regional]", 
                                                     "Event type [Encounter]", 
                                                     "Sensationalism [yes]",
                                                     "Spider expert [yes]",
                                                     "Other experts [yes]")))
  
sign.M1 <- ifelse(table.M1$p > 0.05, "", " *") #Significance

(plot1b <- table.M1 %>%
    ggplot2::ggplot(aes(x = Beta, y = Parameter)) +
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), col = "grey10", width = 0.1)+
    geom_point(col = "grey10", fill = "grey20", size = 3, pch = 21) +
    geom_text(col = "grey10", label = paste0(round(table.M1$Beta, 3), sign.M1, sep = "  "), 
              vjust = - 1, size = 3) +
    labs(x = expression(paste("Estimated beta" %+-% "95% Confidence interval")),
         y = NULL))


pdf(file = "Figure_1.pdf", width = 12, height = 5)
ggpubr::ggarrange(plot1a, plot1b, ncol = 2, nrow = 1, labels = c("A", "B"))
dev.off()

############################### 
##### Analysis by species ##### 
###############################

db_sp <- read.csv("gtrends_per_species.csv" , header=TRUE, as.is = FALSE)

str(db_sp)

#rename variables
db_sp$day            <- db_sp$jour
db_sp$sensationalism <- db_sp$sensationel  
db_sp$error          <- db_sp$erreur
db_sp$event_date     <- db_sp$chevauchement_date
db_sp$hits           <- db_sp$hits_canada

#combine bites and deadly bites
db_sp <- db_sp %>% mutate(TypeEvent = bite + death)
db_sp$TypeEvent <- as.factor(db_sp$TypeEvent) ; levels(db_sp$TypeEvent) <- c("Encounter","Bite","Bite")

#combine experts
db_sp <- db_sp %>% mutate(Other_Experts = Expert_doctor + Expert_others)
db_sp$Other_Experts <- ifelse(db_sp$Other_Experts > 0 , 1 , 0)

#####################
# Data manipulation #
#####################

#Here, for each news, we model the temporal trend before and after the publication

db_sp <- within(db_sp, period <- relevel(period, ref = "before"))

for(i in 1:nlevels(db_sp$ID)) {
  
  db_sp_i <- db_sp[db_sp$ID == levels(db_sp$ID)[i],] #subdataset for the i news
  
  db_sp_i <- droplevels(db_sp_i)
  
  model <- glm(cbind(hits,rep(100,nrow(db_sp_i))) ~ period, data = db_sp_i, family = "binomial")
    
  model$coefficients[2]
    
  #store data for delta
  db_sp_delta_i <- data.frame(ID          = levels(db_sp$ID)[i],
                           species        = levels(db_sp_i$simple_term),
                           trend          = model$coefficients[2],
                           circulation    = levels(db_sp_i$Circulation),
                           TypeEvent      = levels(db_sp_i$TypeEvent),
                           sensationalism = levels(as.factor(db_sp_i$sensationalism)),
                           error          = levels(as.factor(db_sp_i$error)),
                           expert_spider  = levels(as.factor(db_sp_i$Expert_arachnologist)),
                           other_experts  = levels(as.factor(db_sp_i$Other_Experts)))
  
  if(i > 1)
    db_sp_delta <- rbind(db_sp_delta, db_sp_delta_i)
  else
    db_sp_delta <- db_sp_delta_i
  
}

db_sp_delta <- db_sp_delta %>%  mutate_if(is.character, as.factor)

#########################
# Data analysis & plots #
#########################

(plot2 <- ggplot(db_sp_delta, aes(x = trend, color = species, fill = species)) +
    geom_density(alpha = 0.7)+
    geom_vline(aes(xintercept=0),color="grey10", linetype="dashed", linewidth=.5)+
    scale_color_manual("Species", values = c("grey10","darkred"))+
    scale_fill_manual("Species", values = c("grey10","darkred"))+
    labs(x = "Intercept change for search volume in gTrend",
         y = "Density")+
    
    annotate("segment", x = 0.7, xend = 1, y = 6, yend = 6,
             color = "grey10",
             arrow = arrow(ends = "last", 
                           angle = 15, 
                           length = unit(.2,"cm")))+
    
    annotate("text", x = 0.3, y = 6, hjust = 0, vjust = 0.5,
             size = 3,
             color = "grey10",
             label = "Greater search intensity\nafter the news")+
    
    annotate("segment", x = -0.7, xend = -1, y = 6, yend = 6,
             color = "grey10",
             arrow = arrow(ends = "last", 
                           angle = 15, 
                           length = unit(.2,"cm")))+
    
    annotate("text", x = -0.3, y = 6, hjust =1, vjust = 0.5,
             size = 3,
             color = "grey10",
             label = "Greater search intensity\nbefore the news")+
    theme(legend.position = c(0.1,0.5))
)

sum(db_sp_delta[db_sp_delta$species == "black widow",]$trend > 0) / nrow(db_sp_delta[db_sp_delta$species == "black widow",])
sum(db_sp_delta[db_sp_delta$species == "brown recluse",]$trend > 0) / nrow(db_sp_delta[db_sp_delta$species == "brown recluse",])

pdf(file = "Figure_2.pdf", width = 8, height = 5)
plot2
dev.off()


# 
# 
# 
# 
# #extract subdataset for different search terms
# db_spider     <- db[db$short_term == levels(db$short_term)[1], ]
# db_spiderbite <- db[db$short_term == levels(db$short_term)[2], ]
# 
# #régression linéaire
# ##pour l'ensemble des données, indépendemment des événements
# lm_allterm_allpub = lm(gtrends_hits_clean$hits~gtrends_hits_clean$jour, data = gtrends_hits_clean)
# summary_lm_allterm_allpub <- summary(lm_allterm_allpub)
# r_squared_all <- summary_lm_allterm_allpub$r.squared
# cat("Overall R-squared:", overall_r_squared_all, "\n")
# p_value_all <- anova(lm_allterm_allpub)$Pr[1]
# cat("Overall p-value:", overall_p_value_all, "\n")
# 
# ##en tenant compte de chaque événement
# model <- lm(hits~jour + as.factor(n_publication), data = gtrends_hits_clean)
# summary_model <- summary(model)
# overall_r_squared_perpub <- summary_model$r.squared
# cat("Overall R-squared:", overall_r_squared_perpub, "\n")
# overall_p_value_perpub <- anova(model)$Pr[1]
# cat("Overall p-value:", overall_p_value_perpub, "\n")
# 
# #
# ##spider
# spider_data <- subset(gtrends_hits_clean, short_term == "spider")
# hist(spider_data$hits)
# 
# ggplot(spider_data, aes(x = jour, y = hits)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE) +  # Ajouter des tendances linéaires
#   labs(title = "Spider - scatterplot with lm",
#        x = "Day",
#        y = "Number of Hits for 'spider' ") +
#   theme_minimal()
# 
# lm_spider = lm(hits~jour, data = spider_data)
# summary(lm_spider)$r.squared
# coef(summary(lm_spider))[2, 4]
# 
# ##spider bite
# spiderbite_data <- subset(gtrends_hits_clean, short_term == "spider bite")
# hist(spiderbite_data$hits)
# boxplot(spiderbite_data$hits~spiderbite_data$jour)
# 
# spiderbite_data$period = factor(spiderbite_data$period, levels = c("before", "after"))
# boxplot(spiderbite_data$hits~spiderbite_data$period)
# summary(aov(spiderbite_data$hits~spiderbite_data$period))
# 
# 
# ggplot(spiderbite_data, aes(x = jour, y = hits)) +
#   geom_point() +
#   geom_smooth(method = "loess", se = TRUE) +  # Ajouter des tendances linéaires
#   labs(title = "Spider bite - Scatterplot with Loess smoother",
#        x = "Day",
#        y = "Number de hits for 'spider bite' ") +
#   theme_minimal()
# 
# ##look for normality of residuals
# ###comme il y a des 0 dans les valeurs de hits, utilisons la fonction log1p pour transformer les données
# lm_spiderbite = lm(hits~jour, data = spiderbite_data)
# hist(resid(lm_spiderbite))
# summary_lm_spiderbite = summary(lm_spiderbite)
# summary(lm_spiderbite)$r.squared
# coef(summary(lm_spiderbite))[2, 4]
# 
# ##en tenant compte de chaque événement
# model_spiderbite <- lm(hits~jour + as.factor(n_publication), data = spiderbite_data)
# summary_model_spiderbite <- summary(model_spiderbite)
# overall_r_squared <- summary_model_spiderbite$r.squared
# cat("Overall R-squared:", overall_r_squared, "\n")
# overall_p_value <- anova(model_spiderbite)$Pr[1]
# cat("Overall p-value:", overall_p_value, "\n")
# 
# #LOESS curve
# ggplot(spiderbite_data, aes(x = jour, y = hits, group = n_publication, color = as.factor(n_publication))) +
#   geom_point() +
#   geom_smooth(method = "loess", se = TRUE) +  # Ajouter des tendances linéaires
#   labs(title = "Spider bite - Scatterplot with Loess smoother",
#        x = "Day",
#        y = "Number de hits for 'spider bite' ") +
#   theme_minimal()
# 
# #par portée du quotidien (national, regional, international)
# ggplot(spiderbite_data, aes(x = jour, y = hits, group = Circulation, color = as.factor(Circulation))) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE) +  # Ajouter des tendances linéaires
#   labs(title = "Spider bite - Scatterplot with lm",
#        x = "Day",
#        y = "Number de hits for 'spider bite' ") +
#   theme_minimal()
# 
# lm_model_circulation = lm(hits~jour + as.factor(Circulation), data = spiderbite_data)
# summary(lm_model_circulation)
# 
# #frequence
# boxplot(frequence$frequence)
# spider_freq <- frequence[frequence$search_term == "spider", ]
# spiderbite_freq <- frequence[frequence$search_term == "spider_bite", ]
# 
# ###Effectuer un test exact de binôme
# binom_spider <- binom.test(sum(spiderbite_freq$frequence), length(spiderbite_freq$frequence), p = 0.5, alternative = "two.sided")
# print(binom_spider)
# 
# ###calculer les erreurs standard
# # Calculer la fréquence observée de succès (1)
# frequence_obs <- sum(spider_freq$frequence) / length(spider_freq$frequence)
# 
# # Calculer la taille de l'échantillon
# taille_echantillon <- length(spiderbite_freq$frequence)
# 
# # Calculer l'erreur-type de la fréquence
# erreur_type_frequence <- sqrt(frequence_obs * (1 - frequence_obs) / taille_echantillon)
# 
# # Afficher l'erreur-type de la fréquence
# print(erreur_type_frequence)
# 
# ##représentation graphique
# # Créer un tableau de fréquence
# df <- data.frame(
#   search_term = (c("spider", "spider bite")),
#   Frequence = c(mean(spider_freq$frequence), mean(spiderbite_freq$frequence)),
#   SE = c(0.069, 0.063))
# 
# ggplot(df, aes(x = search_term, y = Frequence)) +
#   geom_errorbar(aes(ymin = Frequence - SE, ymax = Frequence + SE), width = 0, color = "black", size = 0.5) +
#   geom_point(stat = "identity", fill = "skyblue", color = "black", size= 5)+
#   labs(title = "Frequency (mean +/- se) of higher GTrends after media story publication",
#        x = "Search term",
#        y = "Frequency") +
#   theme_minimal()+
#   ylim(0.4,0.8)
