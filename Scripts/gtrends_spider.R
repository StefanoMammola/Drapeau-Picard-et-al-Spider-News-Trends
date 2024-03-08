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
               "ggdist",
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
db$TypeEvent <- as.factor(db$TypeEvent) ; levels(db$TypeEvent) <- c("Encounter","Bite","Deadly bite")

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

# check circulation
levels(db$Circulation) <- c("(Inter)national", "(Inter)national", "Regional")

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
                           Family         = rep(levels(as.factor(db_i$Family)),j),
                           Newspaper      = rep(levels(as.factor(db_i$Newspaper)),j),
                           m              = rep(levels(as.factor(db_i$m)),j),
                           yr             = rep(levels(as.factor(db_i$yr)),j))
  
  if(i > 1)
    db_delta_Gtrend <- rbind(db_delta_Gtrend, db_delta_i)
  else
    db_delta_Gtrend <- db_delta_i
  
}

db_delta_Gtrend <- db_delta_Gtrend %>%  mutate_if(is.character, as.factor)

# Plot GTrend ---------------------------------------

db_delta_Gtrend$term <- factor(db_delta_Gtrend$term, rev(c("spider", "spider bite", "brown recluse", "black widow")))

my.colors <- c("black","darkorange", "blue", "purple")

(plot1a <- db_delta_Gtrend %>% ggplot(aes(x = trend, y = term, fill = term, color = term)) +
  xlim(-1.5, 1.5)+
  geom_vline(aes(xintercept=0),color="grey10", linetype="dashed", linewidth=.3)+
  
  scale_color_manual("Search term", values = my.colors)+
  scale_fill_manual("Search term", values = my.colors)+
  
  labs(x = "Intercept change for search volume in gTrend",
       y = "Density of values by search term")+
  
  ggdist::stat_slab(
    # data = ~ .x,
    # aes(fill_ramp = stat(abs(x))),
    #color = "gray15",
    size = .2,
    alpha = 0.5,
    expand = FALSE,
    trim = TRUE,
    height = 2
  ) + 
  
  annotate("segment", x = 1, xend = 1.3, y = 4.7, yend = 4.7,
           color = "grey10",
           arrow = arrow(ends = "last", 
                         angle = 15, 
                         length = unit(.2,"cm")))+
  
  annotate("text", x = 0.6, y = 5, hjust = 0, vjust = 0.5,
           size = 3,
           color = "grey10",
           label = "Greater search intensity\nafter the news")+
  
  annotate("segment", x = -1, xend = -1.3, y = 4.7, yend = 4.7,
           color = "grey10",
           arrow = arrow(ends = "last", 
                         angle = 15, 
                         length = unit(.2,"cm")))+
  
  annotate("text", x = -0.6, y = 5, hjust =1, vjust = 0.5,
           size = 3,
           color = "grey10",
           label = "Greater search intensity\nbefore the news")+
  theme(legend.position = "none")
)

sum(db_delta_Gtrend[db_delta_Gtrend$term == "spider",]$trend > 0) / nrow(db_delta_Gtrend[db_delta_Gtrend$term == "spider",])
sum(db_delta_Gtrend[db_delta_Gtrend$term == "spider bite",]$trend > 0) / nrow(db_delta_Gtrend[db_delta_Gtrend$term == "spider bite",])
sum(db_delta_Gtrend[db_delta_Gtrend$term == "brown recluse",]$trend > 0) / nrow(db_delta_Gtrend[db_delta_Gtrend$term == "brown recluse",])
sum(db_delta_Gtrend[db_delta_Gtrend$term == "black widow",]$trend > 0) / nrow(db_delta_Gtrend[db_delta_Gtrend$term == "black widow",])

# Modelling ---------------------------------------------------------------

# Creating temporal random factor
db_delta_Gtrend$yr_m <- paste(db_delta_Gtrend$yr, db_delta_Gtrend$m, sep ="_")
table(db_delta_Gtrend$yr_m)

# Random factor newspaper
table(db_delta_Gtrend$Newspaper)

# Setting baselines
db_delta_Gtrend <- within(db_delta_Gtrend, Regional <- relevel(circulation, ref = "Regional"))
db_delta_Gtrend <- within(db_delta_Gtrend, TypeEvent <- relevel(TypeEvent, ref = "Encounter"))
db_delta_Gtrend <- within(db_delta_Gtrend, term <- relevel(term, ref = "spider"))

#data exploration: balancing factor levels
table(db_delta_Gtrend$circulation) 
table(db_delta_Gtrend$TypeEvent) 
table(db_delta_Gtrend$sensationalism) 
table(db_delta_Gtrend$Figures)
table(db_delta_Gtrend$error) 
table(db_delta_Gtrend$expert_spider) 
table(db_delta_Gtrend$other_experts) 

# Model
m1 <- lme4::lmer(trend ~ term + country + circulation + 
           TypeEvent + Figures + error + 
           sensationalism + expert_spider + other_experts +
           (1 | yr_m) + (1 | Newspaper), data = db_delta_Gtrend) #db_delta_Gtrend[-c(2,39),]

performance::check_model(m1)

(par.M1 <- parameters::parameters(m1))

#plot of the model
(table.M1 <- par.M1 %>% dplyr::select(Parameter,
                                     Beta = Coefficient,
                                     SE,
                                     CI_low,
                                     CI_high,
                                     t,
                                     p) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, ~ round(.,3)) %>% na.omit()) ; rm(par.M1)

str(table.M1)

table.M1$Parameter <- as.factor(table.M1$Parameter)

#rename
levels(table.M1$Parameter) <- c("Intercept", 
                                "Circulation [Regional]",
                                "Country [USA]",
                                "Errors [yes]",
                                "Spider expert [yes]",
                                "Figures [yes]",
                                "Other experts [yes]",
                                "Sensationalism [yes]",
                                "Search Term [Black widow]",
                                "Search Term [Brown recluse]",
                                "Search Term [Spider bite]", 
                                "Event type [Bite]",
                                "Event type [Deadly bite]")
  
#sort
table.M1$Parameter <- factor(table.M1$Parameter, rev(c("Intercept", 
                                                       "Country [USA]",
                                                       "Circulation [Regional]",
                                                       "Event type [Bite]",
                                                       "Event type [Deadly bite]",
                                                       "Figures [yes]",
                                                       "Sensationalism [yes]",
                                                       "Errors [yes]",
                                                       "Spider expert [yes]",
                                                       "Other experts [yes]",
                                                       "Search Term [Spider bite]",
                                                       "Search Term [Black widow]",
                                                       "Search Term [Brown recluse]")))
  
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

############################### 
##### Analysis Wikipedia ###### 
###############################

# Calculating trend -------------------------------------------------------

db_trend <- db[db$data_source == "Wikipedia",]

db_trend <- db_trend[db_trend$Notes != "behind paywall",]

db_trend <- droplevels(db_trend)

levels(db_trend$search_term) <- c("brown recluse", "Latrodectus","spider","spider bite")

#Here, for each news, we model the temporal trend before and after the publication for each search term

for(i in 1 : nlevels(db_trend$event_id)) {
  
  db_i <- db_trend[db_trend$event_id == levels(db_trend$event_id)[i],] #subdataset for the i news
  
  db_i <- droplevels(db_i)
  
  delta  <- c()
  coef   <- c()
  
  #model trend before and after
  for(j in 1:nlevels(db_i$search_term)) {
    
    db_i_j <- db_i[db_i$search_term == levels(db_i$search_term)[j], ] #extract subdataset for the search term j
    
    model <- MASS::glm.nb(hits ~ period, data = db_i_j)
    
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
                           Family         = rep(levels(as.factor(db_i$Family)),j),
                           Newspaper      = rep(levels(as.factor(db_i$Newspaper)),j),
                           m              = rep(levels(as.factor(db_i$m)),j),
                           yr             = rep(levels(as.factor(db_i$yr)),j))
  
  if(i > 1)
    db_delta_wiki <- rbind(db_delta_wiki, db_delta_i)
  else
    db_delta_wiki <- db_delta_i
  
}

db_delta_wiki <- db_delta_wiki %>%  mutate_if(is.character, as.factor)

# Plot wiki ---------------------------------------

db_delta_wiki$term <- factor(db_delta_wiki$term, rev(c("spider", "spider bite", "brown recluse", "Latrodectus")))

my.colors <- c("black","darkorange", "blue", "purple")

(plot2a <- db_delta_wiki %>% ggplot(aes(x = trend, y = term, fill = term, color = term)) +
    xlim(-1.5, 1.5)+
    geom_vline(aes(xintercept=0),color="grey10", linetype="dashed", linewidth=.3)+
    
    scale_color_manual("Search term", values = my.colors)+
    scale_fill_manual("Search term", values = my.colors)+
    
    labs(x = "Intercept change for search volume in Wikipedia",
         y = "Density of values by search term")+
    
    ggdist::stat_slab(
      # data = ~ .x,
      # aes(fill_ramp = stat(abs(x))),
      #color = "gray15",
      size = .2,
      alpha = 0.5,
      expand = FALSE,
      trim = TRUE,
      height = 2
    ) + 
    
    annotate("segment", x = 1, xend = 1.3, y = 4.7, yend = 4.7,
             color = "grey10",
             arrow = arrow(ends = "last", 
                           angle = 15, 
                           length = unit(.2,"cm")))+
    
    annotate("text", x = 0.6, y = 5, hjust = 0, vjust = 0.5,
             size = 3,
             color = "grey10",
             label = "Greater search intensity\nafter the news")+
    
    annotate("segment", x = -1, xend = -1.3, y = 4.7, yend = 4.7,
             color = "grey10",
             arrow = arrow(ends = "last", 
                           angle = 15, 
                           length = unit(.2,"cm")))+
    
    annotate("text", x = -0.6, y = 5, hjust =1, vjust = 0.5,
             size = 3,
             color = "grey10",
             label = "Greater search intensity\nbefore the news")+
    theme(legend.position = "none")
)

# Modelling ---------------------------------------------------------------

# Creating temporal random factor
db_delta_wiki$yr_m <- paste(db_delta_wiki$yr, db_delta_wiki$m, sep ="_")
table(db_delta_wiki$yr_m)

# Random factor newspaper
table(db_delta_wiki$Newspaper)

# Setting baselines
db_delta_wiki <- within(db_delta_wiki, Regional <- relevel(circulation, ref = "Regional"))
db_delta_wiki <- within(db_delta_wiki, TypeEvent <- relevel(TypeEvent, ref = "Encounter"))
db_delta_wiki <- within(db_delta_wiki, term <- relevel(term, ref = "spider"))

#data exploration: balancing factor levels
table(db_delta_wiki$circulation) 
table(db_delta_wiki$TypeEvent) 
table(db_delta_wiki$sensationalism) 
table(db_delta_wiki$Figures)
table(db_delta_wiki$error) 
table(db_delta_wiki$expert_spider) 
table(db_delta_wiki$other_experts) 

# Model
m2 <- lme4::lmer(trend ~ term + circulation + 
                   TypeEvent + Figures + error + 
                   sensationalism + expert_spider + other_experts +
                   (1 | yr_m) + (1 | Newspaper), data = db_delta_wiki)

performance::check_model(m2)

(par.M2 <- parameters::parameters(m2))

#plot of the model
(table.M2 <- par.M2 %>% dplyr::select(Parameter,
                                      Beta = Coefficient,
                                      SE,
                                      CI_low,
                                      CI_high,
                                      t,
                                      p) %>% 
    data.frame() %>% 
    mutate_if(is.numeric, ~ round(.,3)) %>% na.omit()) ; rm(par.M2)

str(table.M2)

table.M2$Parameter <- as.factor(table.M2$Parameter)

#rename
levels(table.M2$Parameter) <- c("Intercept", 
                                "Circulation [Regional]",
                                "Errors [yes]",
                                "Spider expert [yes]",
                                "Figures [yes]",
                                "Other experts [yes]",
                                "Sensationalism [yes]",
                                "Search Term [Brown recluse]",
                                "Search Term [Latrodectus]",
                                "Search Term [Spider bite]", 
                                "Event type [Bite]",
                                "Event type [Deadly bite]")

#sort
table.M2$Parameter <- factor(table.M2$Parameter, rev(c("Intercept",
                                                       "Circulation [Regional]",
                                                       "Event type [Bite]",
                                                       "Event type [Deadly bite]",
                                                       "Figures [yes]",
                                                       "Sensationalism [yes]",
                                                       "Errors [yes]",
                                                       "Spider expert [yes]",
                                                       "Other experts [yes]",
                                                       "Search Term [Spider bite]",
                                                       "Search Term [Latrodectus]",
                                                       "Search Term [Brown recluse]")))

sign.M2 <- ifelse(table.M2$p > 0.05, "", " *") #Significance

(plot2b <- table.M2 %>%
    ggplot2::ggplot(aes(x = Beta, y = Parameter)) +
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), col = "grey10", width = 0.1)+
    geom_point(col = "grey10", fill = "grey20", size = 3, pch = 21) +
    geom_text(col = "grey10", label = paste0(round(table.M2$Beta, 3), sign.M2, sep = "  "), 
              vjust = - 1, size = 3) +
    labs(x = expression(paste("Estimated beta" %+-% "95% Confidence interval")),
         y = NULL))

############################### 
##### Analysis iNaturalist ####
###############################

# Calculating trend -------------------------------------------------------

db_trend <- db[db$data_source == "iNaturalist",]

#db_trend <- db_trend[db_trend$Notes != "behind paywall",]

db_trend <- droplevels(db_trend)


#Here, for each news, we model the temporal trend before and after the publication for each search term

for(i in 1 : nlevels(db_trend$event_id)) {
  
  db_i <- db_trend[db_trend$event_id == levels(db_trend$event_id)[i],] #subdataset for the i news
  
  db_i <- droplevels(db_i)
  
  #model trend before and after
  model <- glm(hits ~ period, data = db_i, family = "poisson")

  #store data for delta
  db_delta_i <- data.frame(ID             = levels(db$event_id)[i],
                           trend          = model$coefficients[2],
                           country        = levels(db_i$country),
                           circulation    = levels(db_i$Circulation),
                           TypeEvent      = levels(db_i$TypeEvent),
                           sensationalism = levels(as.factor(db_i$Sensationalism)),
                           error          = levels(as.factor(db_i$Errors)),
                           expert_spider  = levels(as.factor(db_i$Expert_arachnologist)),
                           other_experts  = levels(as.factor(db_i$Other_Experts)),
                           Figures        = levels(as.factor(db_i$Figures)),
                           Genus          = levels(as.factor(db_i$Genus)),
                           Family         = levels(as.factor(db_i$Family)),
                           Newspaper      = levels(as.factor(db_i$Newspaper)),
                           m              = levels(as.factor(db_i$m)),
                           yr             = levels(as.factor(db_i$yr)))
  
  if(i > 1)
    db_delta_iNat <- rbind(db_delta_iNat, db_delta_i)
  else
    db_delta_iNat <- db_delta_i
  
}

db_delta_iNat <- db_delta_iNat %>%  mutate_if(is.character, as.factor)

# Plot wiki ---------------------------------------

(plot3a <- db_delta_iNat %>% ggplot(aes(x = trend)) +
    xlim(-1.5, 1.5)+
    geom_vline(aes(xintercept=0),color="grey10", linetype="dashed", linewidth=.3)+
    
    labs(x = "Intercept change for search volume in iNaturalist",
         y = "Density of values by search term")+
    
    ggdist::stat_slab(
      # data = ~ .x,
      # aes(fill_ramp = stat(abs(x))),
      color = "black",
      fill = "black",
      size = .2,
      alpha = 0.5,
      expand = FALSE,
      trim = TRUE,
      height = 2
    ) + 
    
    annotate("segment", x = 1, xend = 1.3, y = 1.9, yend = 1.9,
             color = "grey10",
             arrow = arrow(ends = "last", 
                           angle = 15, 
                           length = unit(.2,"cm")))+
    
    annotate("text", x = 0.6, y = 2, hjust = 0, vjust = 0.5,
             size = 3,
             color = "grey10",
             label = "Greater search intensity\nafter the news")+
    
    annotate("segment", x = -1, xend = -1.3, y = 1.9, yend = 1.9,
             color = "grey10",
             arrow = arrow(ends = "last", 
                           angle = 15, 
                           length = unit(.2,"cm")))+
    
    annotate("text", x = -0.6, y = 2, hjust =1, vjust = 0.5,
             size = 3,
             color = "grey10",
             label = "Greater search intensity\nbefore the news")+
    theme(legend.position = "none")
)

# Modelling ---------------------------------------------------------------

# Creating temporal random factor
db_delta_iNat$yr_m <- paste(db_delta_iNat$yr, db_delta_iNat$m, sep ="_")
table(db_delta_iNat$yr_m)

# Random factor newspaper
table(db_delta_iNat$Newspaper)

# Setting baselines
db_delta_iNat <- within(db_delta_iNat, Regional <- relevel(circulation, ref = "Regional"))
db_delta_iNat <- within(db_delta_iNat, TypeEvent <- relevel(TypeEvent, ref = "Encounter"))

#data exploration: balancing factor levels
table(db_delta_iNat$circulation) 
table(db_delta_iNat$TypeEvent) 
table(db_delta_iNat$sensationalism) 
table(db_delta_iNat$Figures)
table(db_delta_iNat$error) 
table(db_delta_iNat$expert_spider) 
table(db_delta_iNat$other_experts) 

# Model
m3 <- lme4::lmer(trend ~ circulation + country +
                   TypeEvent + Figures + error + 
                   sensationalism + expert_spider + other_experts +
                   (1 | yr_m), data = db_delta_iNat)

performance::check_model(m3)

(par.M3 <- parameters::parameters(m3))

#plot of the model
(table.M3 <- par.M3 %>% dplyr::select(Parameter,
                                      Beta = Coefficient,
                                      SE,
                                      CI_low,
                                      CI_high,
                                      t,
                                      p) %>% 
    data.frame() %>% 
    mutate_if(is.numeric, ~ round(.,3)) %>% na.omit()) ; rm(par.M3)

str(table.M3)

table.M3$Parameter <- as.factor(table.M3$Parameter)

#rename
levels(table.M3$Parameter) <- c("Intercept", 
                                "Circulation [Regional]",
                                "Country [USA]",
                                "Errors [yes]",
                                "Spider expert [yes]",
                                "Figures [yes]",
                                "Other experts [yes]",
                                "Sensationalism [yes]",
                                "Event type [Bite]",
                                "Event type [Deadly bite]")

#sort
table.M3Parameter <- factor(table.M3$Parameter, rev(c("Intercept",
                                                       "Circulation [Regional]",
                                                       "Country [USA]",
                                                       "Event type [Bite]",
                                                       "Event type [Deadly bite]",
                                                       "Figures [yes]",
                                                       "Sensationalism [yes]",
                                                       "Errors [yes]",
                                                       "Spider expert [yes]",
                                                       "Other experts [yes]")))

sign.M3 <- ifelse(table.M3$p > 0.05, "", " *") #Significance

(plot3b <- table.M3 %>%
    ggplot2::ggplot(aes(x = Beta, y = Parameter)) +
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), col = "grey10", width = 0.1)+
    geom_point(col = "grey10", fill = "grey20", size = 3, pch = 21) +
    geom_text(col = "grey10", label = paste0(round(table.M3$Beta, 3), sign.M3, sep = "  "), 
              vjust = - 1, size = 3) +
    labs(x = expression(paste("Estimated beta" %+-% "95% Confidence interval")),
         y = NULL))

# Saving figures ----------------------------------------------------------

pdf(file = "Figures/Figure_1_GTREND.pdf", width = 12, height = 5)
ggpubr::ggarrange(plot1a, plot1b, ncol = 2, nrow = 1, labels = c("A", "B"))
dev.off()

pdf(file = "Figures/Figure_2_WIKI.pdf", width = 12, height = 5)
ggpubr::ggarrange(plot2a, plot2b, ncol = 2, nrow = 1, labels = c("A", "B"))
dev.off()

pdf(file = "Figures/Figure_3_iNAT.pdf", width = 12, height = 5)
ggpubr::ggarrange(plot3a, plot3b, ncol = 2, nrow = 1, labels = c("A", "B"))
dev.off()


