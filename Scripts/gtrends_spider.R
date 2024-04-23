## ------------------------------------------------------------------------
## 'Does the publication of spider-related news stories trigger online searches about spiders?'
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
# 'R script to reproduce the full analysis'
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.3.0) and R studio (v. 2023.03.1+446)
# Authors: Stefano Mammola

##############
# R packages #
##############

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("dplyr",
               "parameters",
               "performance",
               "ggdist",
               "MASS",
               "modelsummary",
               "readxl",
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

my.colors <- c("blue", "purple", "darkorange", "black")

####################
# Data preparation #
####################

# Import data -------------------------------------------------------------

#### Main data ####
db <- read.csv("Data/spiders_data_metadata_masterfile.csv" , header=TRUE, as.is = FALSE) |>
  dplyr::select(c(-1))

str(db)

# How many news?
unique(db$country)

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

#### Volume data ####
vol <- read.csv("Data/volume_search.csv" , header=TRUE, as.is = FALSE, sep = "\t")

str(vol)

colnames(vol)[6] <- "n"

#### Extra vars
var_vol <- db[db$day == 0,]

var_vol$year.month <- paste(var_vol$yr, var_vol$m, sep = "-") |> as.factor()

#english
n_news                 <- c()
Sensationalism         <- c()
Errors                 <- c()
Expert_doctor          <- c()
Expert_arachnologist   <- c()
Bite                   <- c()
Deadly_bite            <- c()

for(i in 1:nlevels(var_vol$year.month)){
  
  var_vol_i <- var_vol[var_vol$year.month == levels(var_vol$year.month)[i],]
  var_vol_i <- droplevels(var_vol_i)
  
  n_news                 <- append(n_news, nrow(var_vol_i))
  Sensationalism         <- append(Sensationalism ,sum(var_vol_i$Sensationalism, na.rm = TRUE)/nrow(var_vol_i))
  Errors                 <- append(Errors ,sum(as.numeric(as.character(var_vol_i$Errors)), na.rm = TRUE)/nrow(var_vol_i))
  Expert_doctor          <- append(Expert_doctor ,sum(as.numeric(as.character(var_vol_i$Expert_doctor)), na.rm = TRUE)/nrow(var_vol_i))
  Expert_arachnologist   <- append(Expert_arachnologist ,sum(as.numeric(as.character(var_vol_i$Expert_arachnologist)), na.rm = TRUE)/nrow(var_vol_i))
  Bite                   <- append(Bite, sum(var_vol_i$Bite, na.rm = TRUE)/nrow(var_vol_i))
  Deadly_bite            <- append(Deadly_bite,sum(var_vol_i$Death, na.rm = TRUE)/nrow(var_vol_i))

}

var_vol.en <- data.frame(year.month = levels(var_vol$year.month), country = rep("english", length(n_news)), n_news, Sensationalism, Errors, Expert_doctor, Expert_arachnologist, Bite, Deadly_bite)

#canada
var_vol.can <- var_vol[var_vol$country == "Canada",] ; var_vol.can <- droplevels(var_vol.can)

n_news                 <- c()
Sensationalism         <- c()
Errors                 <- c()
Expert_doctor          <- c()
Expert_arachnologist   <- c()
Bite                   <- c()
Deadly_bite            <- c()

for(i in 1:nlevels(var_vol.can$year.month)){
  
  var_vol_i <- var_vol.can[var_vol.can$year.month == levels(var_vol.can$year.month)[i],]
  var_vol_i <- droplevels(var_vol_i)
  
  n_news                 <- append(n_news, nrow(var_vol_i))
  Sensationalism         <- append(Sensationalism ,sum(var_vol_i$Sensationalism, na.rm = TRUE)/nrow(var_vol_i))
  Errors                 <- append(Errors ,sum(as.numeric(as.character(var_vol_i$Errors)), na.rm = TRUE)/nrow(var_vol_i))
  Expert_doctor          <- append(Expert_doctor ,sum(as.numeric(as.character(var_vol_i$Expert_doctor)), na.rm = TRUE)/nrow(var_vol_i))
  Expert_arachnologist   <- append(Expert_arachnologist ,sum(as.numeric(as.character(var_vol_i$Expert_arachnologist)), na.rm = TRUE)/nrow(var_vol_i))
  Bite                   <- append(Bite, sum(var_vol_i$Bite, na.rm = TRUE)/nrow(var_vol_i))
  Deadly_bite            <- append(Deadly_bite,sum(var_vol_i$Death, na.rm = TRUE)/nrow(var_vol_i))
  
}

var_vol.can <- data.frame(year.month = levels(var_vol.can$year.month), country = rep("Canada", length(n_news)), 
                          n_news, Sensationalism, Errors, Expert_doctor, Expert_arachnologist, Bite, Deadly_bite)

#USA
var_vol.USA <- var_vol[var_vol$country == "USA",] ; var_vol.USA <- droplevels(var_vol.USA)

n_news                 <- c()
Sensationalism         <- c()
Errors                 <- c()
Expert_doctor          <- c()
Expert_arachnologist   <- c()
Bite                   <- c()
Deadly_bite            <- c()

for(i in 1:nlevels(var_vol.USA$year.month)){
  
  var_vol_i <- var_vol.USA[var_vol.USA$year.month == levels(var_vol.USA$year.month)[i],]
  var_vol_i <- droplevels(var_vol_i)
  
  n_news                 <- append(n_news, nrow(var_vol_i))
  Sensationalism         <- append(Sensationalism ,sum(var_vol_i$Sensationalism, na.rm = TRUE)/nrow(var_vol_i))
  Errors                 <- append(Errors ,sum(as.numeric(as.character(var_vol_i$Errors)), na.rm = TRUE)/nrow(var_vol_i))
  Expert_doctor          <- append(Expert_doctor ,sum(as.numeric(as.character(var_vol_i$Expert_doctor)), na.rm = TRUE)/nrow(var_vol_i))
  Expert_arachnologist   <- append(Expert_arachnologist ,sum(as.numeric(as.character(var_vol_i$Expert_arachnologist)), na.rm = TRUE)/nrow(var_vol_i))
  Bite                   <- append(Bite, sum(var_vol_i$Bite, na.rm = TRUE)/nrow(var_vol_i))
  Deadly_bite            <- append(Deadly_bite,sum(var_vol_i$Death, na.rm = TRUE)/nrow(var_vol_i))
  
}

var_vol.USA <- data.frame(year.month = levels(var_vol.USA$year.month), country = rep("US", length(n_news)), n_news, Sensationalism, Errors, Expert_doctor, Expert_arachnologist, Bite, Deadly_bite)

# merge
var_vol.2 <- rbind(var_vol.en, var_vol.can, var_vol.USA)
var_vol.2$xy <- paste(var_vol.2$year.month, var_vol.2$country, sep = "_")  
var_vol.2 <- var_vol.2 %>%  dplyr::select(-c("year.month","country","n_news"))
vol$xy <- paste(vol$year.month, vol$country, sep = "_")

vol <- vol %>% dplyr::left_join(var_vol.2, by = "xy") 

vol[,8:13][is.na(vol[,8:13])] <- 0

#######################
# Analysis for Gtrend #
#######################

# Calculating trend -------------------------------------------------------

db_trend <- db[db$data_source == "GoogleTrends",]

db_trend <- droplevels(db_trend)

levels(db_trend$search_term) <- c(rep("black widow",2),rep("brown recluse",2),rep("spider",2),rep("spider bite",2))

# Sample size
unique(db_trend$event_id)

# Here, for each news, we model the temporal trend before and after the publication for each search term

for(i in 1 : nlevels(db_trend$event_id)) {
  
  db_i <- db_trend[db_trend$event_id == levels(db_trend$event_id)[i],] #subdataset for the i news
  
  db_i <- droplevels(db_i)
  
  delta  <- c()
  coef   <- c()
  p      <- c()
  
  #model trend before and after
  for(j in 1 : nlevels(db_i$search_term)) {
    
    db_i_j <- db_i[db_i$search_term == levels(db_i$search_term)[j], ] #extract subdataset for the search term j

    model <- glm(cbind(hits,rep(100,nrow(db_i_j))) ~ period, data = db_i_j, family = "binomial")

    delta <- append(delta, model$coefficients[2])
    p     <- append(p, summary(model)$coefficients[2,4])
    
  }
  
  #store data for delta
  db_delta_i <- data.frame(ID             = rep(levels(db$event_id)[i],j),
                           term           = levels(db_i$search_term),
                           trend          = delta,
                           trend.p        = p,
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
                           m              = rep(levels(as.factor(db_i$m)),j) |> as.numeric(),
                           yr             = rep(levels(as.factor(db_i$yr)),j) |> as.numeric())
  
  if(i > 1)
    db_delta_Gtrend <- rbind(db_delta_Gtrend, db_delta_i)
  else
    db_delta_Gtrend <- db_delta_i
  
}

db_delta_Gtrend <- db_delta_Gtrend %>%  mutate_if(is.character, as.factor)

## % of significant comparison
sum(ifelse(db_delta_Gtrend$trend.p < 0.05, 
           ifelse(db_delta_Gtrend$trend > 0, 1, 0), 0))/nrow(db_delta_Gtrend) # 10% are significantly higher

db_delta_Gtrend$trend_sign <- ifelse(db_delta_Gtrend$trend.p < 0.05, 
       ifelse(db_delta_Gtrend$trend > 0, "Sign_plus", "Sign_minus"), "Non_sig") %>% as.factor()

db_delta_Gtrend$trend_alpha<- ifelse(db_delta_Gtrend$trend.p < 0.05, 1, 0.5)

db_delta_Gtrend$trend.01 <- ifelse(db_delta_Gtrend$trend > 0, 1, 0)

## Figure 1a
db_trend$search_term <- factor(db_trend$search_term, c("spider", "spider bite", "brown recluse", "black widow"))

(plot1a <- db_trend %>% 
    ggplot(aes(x = day, y = hits, fill = search_term, color = search_term)) +
    geom_vline(aes(xintercept=0),color="grey10", linetype="dashed", linewidth=.3)+
    geom_smooth(method='gam', formula = y ~ s(x), se = TRUE,
                method.args = list(family = poisson)) +
    scale_x_continuous(breaks = -7:7, labels = as.character(-7:7))+
    scale_color_manual("", values = my.colors)+
    scale_fill_manual("", values = my.colors)+
    labs(x = "Day",
         y = "Hits")+
    theme(legend.position = "top")
)

# Plot GTrend ---------------------------------------

db_delta_Gtrend$term <- factor(db_delta_Gtrend$term, c("spider", "spider bite", "brown recluse", "black widow"))

(plot1b <- db_delta_Gtrend %>% ggplot(aes(x = trend, y = term, fill = term, color = term)) +
  xlim(-1.5, 1.5)+
  geom_vline(aes(xintercept=0),color="grey10", linetype="dashed", linewidth=.3)+
  
  scale_color_manual("Search term", values = my.colors)+
  scale_fill_manual("Search term", values = my.colors)+
  
  labs(x = "Intercept change for search volume in gTrend",
       y = "Density of values by search term")+
  
  ggdist::stat_slab(
    data = ~ .x,
    aes(fill_discrete = stat(abs(x))),
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

# Model tests
# my.formula <- as.formula("trend ~ country + circulation +  TypeEvent + Figures + error +  sensationalism + expert_spider + other_experts + (1 | yr_m)")
# 
# Gtrend_models <- list(
#   "spider"          = lme4::lmer(my.formula, data = db_delta_Gtrend[db_delta_Gtrend$term == "spider",]),
#   "black widow"     = lme4::lmer(my.formula, data = db_delta_Gtrend[db_delta_Gtrend$term == "black widow",]),
#   "brown recluse"   = lme4::lmer(my.formula, data = db_delta_Gtrend[db_delta_Gtrend$term == "brown recluse",]),
#   "spider bite"     = lme4::lmer(my.formula, data = db_delta_Gtrend[db_delta_Gtrend$term == "spider bite",])
# )
# 
# modelsummary::modelsummary(Gtrend_models, statistic = "{std.error} ({p.value})")
# modelsummary::modelplot(Gtrend_models)
# 
# my.formula.01 <- as.formula("trend.01 ~ country + circulation +  TypeEvent + Figures + error +  sensationalism + expert_spider + other_experts + (1 | yr_m)")
# 
# Gtrend_models.01 <- list(
#   "spider"          = glmmTMB::glmmTMB(my.formula.01, data = db_delta_Gtrend[db_delta_Gtrend$term == "spider",], family = "binomial"),
#   "black widow"     = glmmTMB::glmmTMB(my.formula.01, data = db_delta_Gtrend[db_delta_Gtrend$term == "black widow",], family = "binomial"),
#   "brown recluse"   = glmmTMB::glmmTMB(my.formula.01, data = db_delta_Gtrend[db_delta_Gtrend$term == "brown recluse",], family = "binomial"),
#   "spider bite"     = glmmTMB::glmmTMB(my.formula.01, data = db_delta_Gtrend[db_delta_Gtrend$term == "spider bite",], family = "binomial")
# )
# 
# modelsummary::modelsummary(Gtrend_models.01, statistic = "{std.error} ({p.value})")
# 
# modelsummary::modelplot(Gtrend_models.01)

#plot of the model

m1 <- lme4::lmer(trend ~ term + country + circulation + 
                   TypeEvent + Figures + error + 
                   sensationalism + expert_spider + other_experts +
                   (1 | yr_m) + (1 | Newspaper), data = db_delta_Gtrend) #db_delta_Gtrend[-c(2,39),]

performance::check_model(m1)
(par.M1 <- parameters::parameters(m1))

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

(plot1c <- table.M1 %>%
    ggplot2::ggplot(aes(x = Beta, y = Parameter)) +
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), col = "grey10", width = 0.1)+
    geom_point(col = "grey10", fill = "grey20", size = 3, pch = 21) +
    geom_text(col = "grey10", label = sign.M1, vjust = 0, size = 5) +
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
  p      <- c()
  
  #model trend before and after
  for(j in 1:nlevels(db_i$search_term)) {
    
    db_i_j <- db_i[db_i$search_term == levels(db_i$search_term)[j], ] #extract subdataset for the search term j
    
    model <- MASS::glm.nb(hits ~ period, data = db_i_j)
    
    delta <- append(delta, model$coefficients[2])
    p     <- append(p, summary(model)$coefficients[2,4])
    
  }
  
  #store data for delta
  db_delta_i <- data.frame(ID             = rep(levels(db$event_id)[i],j),
                           term           = levels(db_i$search_term),
                           trend          = delta,
                           trend.p.       = p,
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
                           m              = rep(levels(as.factor(db_i$m)),j) |> as.numeric(),
                           yr             = rep(levels(as.factor(db_i$yr)),j) |> as.numeric())
  
  if(i > 1)
    db_delta_wiki <- rbind(db_delta_wiki, db_delta_i)
  else
    db_delta_wiki <- db_delta_i
  
}

db_delta_wiki <- db_delta_wiki %>%  mutate_if(is.character, as.factor)

## % of significant comparison
sum(ifelse(db_delta_wiki$trend.p < 0.05, 
           ifelse(db_delta_wiki$trend > 0, 1, 0), 0))/nrow(db_delta_wiki) # 10% are significantly higher

db_delta_wiki$trend_sign <- ifelse(db_delta_wiki$trend.p < 0.05, 
                                     ifelse(db_delta_wiki$trend > 0, "Sign_plus", "Sign_minus"), "Non_sig") %>% as.factor()

db_delta_wiki$trend_alpha<- ifelse(db_delta_wiki$trend.p < 0.05, 
                                     1, 0.5)

db_delta_wiki$trend.01 <- ifelse(db_delta_wiki$trend > 0, 1, 0)

## plot2a
db_trend$search_term <- factor(db_trend$search_term, c("spider", "spider bite", "brown recluse", "Latrodectus"))

(plot2a <- db_trend %>% 
    ggplot(aes(x = day, y = hits, fill = search_term, color = search_term)) +
    geom_vline(aes(xintercept=0), color = "grey10", linetype = "dashed", linewidth = .3)+
    geom_smooth(method='gam', formula = y ~ s(x), se = TRUE) +
    scale_x_continuous(breaks = -7:7, labels = as.character(-7:7))+
    scale_color_manual("", values = my.colors)+
    scale_fill_manual("", values = my.colors)+
    labs(x = "Day",
         y = "Hits")+
    theme(legend.position = "top")
)

# Plot wiki ---------------------------------------

db_delta_wiki$term <- factor(db_delta_wiki$term, c("spider", "spider bite", "brown recluse", "Latrodectus"))

(plot2b <- db_delta_wiki %>% ggplot(aes(x = trend, y = term, fill = term, color = term)) +
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

# # Test models
# my.formula <- as.formula("trend ~ circulation + Figures + error +  sensationalism + expert_spider + other_experts + (1 | yr_m) + (1 | Newspaper)")
# 
# Wiki_models <- list(
#   "spider"          = lme4::lmer(my.formula, data = db_delta_wiki[db_delta_wiki$term == "spider",]),
#   "Latrodectus"     = lme4::lmer(my.formula, data = db_delta_wiki[db_delta_wiki$term == "Latrodectus",]),
#   "brown recluse"   = lme4::lmer(my.formula, data = db_delta_wiki[db_delta_wiki$term == "brown recluse",]),
#   "spider bite"     = lme4::lmer(my.formula, data = db_delta_wiki[db_delta_wiki$term == "spider bite",])
# )
# 
# 
# modelsummary::modelsummary(Wiki_models, statistic = "{std.error} ({p.value})")
# modelsummary::modelplot(Wiki_models)
# 
# my.formula.01 <- as.formula("trend.01 ~ circulation + Figures + error +  sensationalism + expert_spider + other_experts + (1 | yr_m) + (1 | Newspaper)")
# 
# Wiki_models.01 <- list(
#   "spider"          = glmmTMB::glmmTMB(my.formula.01, data = db_delta_wiki[db_delta_wiki$term == "spider",], family = "binomial"),
#   "Latrodectus"     = glmmTMB::glmmTMB(my.formula.01, data = db_delta_wiki[db_delta_wiki$term == "Latrodectus",], family = "binomial"),
#   "brown recluse"   = glmmTMB::glmmTMB(my.formula.01, data = db_delta_wiki[db_delta_wiki$term == "brown recluse",], family = "binomial"),
#   "spider bite"     = glmmTMB::glmmTMB(my.formula.01, data = db_delta_wiki[db_delta_wiki$term == "spider bite",], family = "binomial")
# )
# 
# modelsummary::modelsummary(Wiki_models.01, statistic = "{std.error} ({p.value})")
# modelsummary::modelplot(Wiki_models.01)

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

(plot2c <- table.M2 %>%
    ggplot2::ggplot(aes(x = Beta, y = Parameter)) +
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), col = "grey10", width = 0.1)+
    geom_point(col = "grey10", fill = "grey20", size = 3, pch = 21) +
    geom_text(col = "grey10", label = sign.M2, vjust = 0, size = 5) +
    labs(x = expression(paste("Estimated beta" %+-% "95% Confidence interval")),
         y = NULL))

############################### 
##### Analysis iNaturalist ####
###############################

# Calculating trend -------------------------------------------------------

db_trend <- db[db$data_source == "iNaturalist",]
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

##
(plot3a <- db_trend %>%
    ggplot(aes(x = day, y = hits)) +
    geom_vline(aes(xintercept=0), color = "grey10", linetype = "dashed", linewidth = .3)+

    geom_smooth(method = 'gam', formula = y ~ s(x), se = TRUE, color="grey10", fill = "grey10") +
    scale_x_continuous(breaks = -7:7, labels = as.character(-7:7))+
    scale_color_manual("", values = my.colors)+
    scale_fill_manual("", values = my.colors)+

    labs(x = "Day",
         y = "Hits")
)


# Plot INat ---------------------------------------

(plot3b <- db_delta_iNat %>% ggplot(aes(x = trend)) +
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
table.M3$Parameter <- factor(table.M3$Parameter, rev(c("Intercept",
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

(plot3c <- table.M3 %>%
    ggplot2::ggplot(aes(x = Beta, y = Parameter)) +
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), col = "grey10", width = 0.1)+
    geom_point(col = "grey10", fill = "grey20", size = 3, pch = 21) +
    geom_text(col = "grey10", label = sign.M3, vjust = 0, size = 5) +
    labs(x = expression(paste("Estimated beta" %+-% "95% Confidence interval")),
         y = NULL))

# Analysis VOLUME ---------------------------------------------------------

### Model

levels(vol$term) <- c("Araneae", "brown recluse", "Latrodectus", "spider", "spider bite")
vol <- within(vol, term <- relevel(term, ref = "spider"))

# iNat
m4 <- glm(hits ~ n, data = vol[vol$data_source == "iNaturalist",], family = "poisson")
performance::check_overdispersion(m4)

m5 <- MASS::glm.nb(hits ~ n, data = vol[vol$data_source == "iNaturalist",])

#wiki
m6 <- glm(hits ~ n * term, data = vol[vol$data_source == "Wikipedia",], family = "poisson")
performance::check_overdispersion(m6)

m7 <- MASS::glm.nb(hits ~ n + term, data = vol[vol$data_source == "Wikipedia",])

summary(m7)

# # proportion models
# vol_wiki <- vol[vol$data_source == "Wikipedia",] |> na.omit()
# 
# levels(vol_wiki$term) <- c("spider", "Araneae", "brown recluse", "Latrodectus", "spider bite")
# vol_wiki <- within(vol_wiki, term <- relevel(term, ref = "spider"))
# 
# m8 <- MASS::glm.nb(hits ~ n + term + Sensationalism + Errors + Expert_doctor + Expert_arachnologist, data = vol_wiki)
# 
# performance::check_collinearity(m8)
# 
# summary(m8)
# 
# m9 <- MASS::glm.nb(hits ~ n + term + Sensationalism + Errors + Expert_doctor + Expert_arachnologist + Bite + Deadly_bite, data = vol_wiki)
# 
# summary(m9)

### plot

vol <- within(vol, term <- relevel(term, ref = "Araneae"))
vol$term <- factor(vol$term, c("Araneae", "spider", "spider bite", "brown recluse", "Latrodectus"))

(plot4 <- vol %>% ggplot2::ggplot(aes(x = n, y = hits, fill = term, col = term)) +
        facet_wrap(vars(data_source), scales = "free") +
        geom_point() +
        geom_smooth(method = "glm.nb")+
        scale_color_manual("Search term", values = c("grey40",my.colors))+
        scale_fill_manual("Search term", values = c("grey40",my.colors))+
        labs(x = "Number of published news (monthly)", y = "Number of hits")
  )

# Saving figures ----------------------------------------------------------

pdf(file = "Figures/Figure_1_GTREND.pdf", width = 18, height = 5)
ggpubr::ggarrange(plot1a, plot1b, plot1c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))
dev.off()

pdf(file = "Figures/Figure_2_WIKI.pdf", width = 18, height = 5)
ggpubr::ggarrange(plot2a, plot2b, plot2c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))
dev.off()

pdf(file = "Figures/Figure_3_iNAT.pdf", width = 18, height = 5)
ggpubr::ggarrange(plot3a, plot3b, plot3c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))
dev.off()

pdf(file = "Figures/Figure_4_volume.pdf", width = 12, height = 5)
plot4
dev.off()

# Saving model outputs ----------------------------------------------------

models <- list(
  "Gtrend"      = m1,
  "Wikipedia"   = m2,
  "iNaturalist" = m3
)

cm <- c("(Intercept)" = "Intercept", 
        "circulationRegional" = "Circulation [Regional]",
        "countryUSA" = "Country [USA]",
        "error1" = "Errors [yes]",
        "expert_spider1" = "Spider expert [yes]",
        "Figures1" = "Figures [yes]",
        "other_experts1" = "Other experts [yes]",
        "sensationalism1" = "Sensationalism [yes]",
        "termbrown recluse" = "Search Term [Brown recluse]",
        "termblack widow" = "Search Term [Black widow]",
        "termLatrodectus" = "Search Term [Latrodectus]",
        "termspider bite" = "Search Term [Spider bite]", 
        "TypeEventBite"= "Event type [Bite]",
        "TypeEventDeadly bite" = "Event type [Deadly bite]")

modelsummary::modelsummary(models, gof_omit = ".*",
             estimate = "{estimate} [{conf.low}, {conf.high}]",
             statistic = NULL, gof_map = NA, coef_map = cm, output = "Tables/Table_S1_models_trend.docx")

models_volume <- list("iNaturalist" = m5, "Wikipedia" = m7)

cm2 <- c("(Intercept)" = "Intercept", 
        "n" = "Number of news",
        "termspider bite" = "Search Term [Spider bite]", 
        "termbrown recluse" = "Search Term [Brown recluse]",
        "termLatrodectus" = "Search Term [Latrodectus]"
        )

modelsummary::modelsummary(models_volume, gof_omit = ".*",
                           estimate = "{estimate} [{conf.low}, {conf.high}]",
                           statistic = NULL, gof_map = NA, coef_map = cm2, output = "Tables/Table_S2_models_volume.docx")
#end
