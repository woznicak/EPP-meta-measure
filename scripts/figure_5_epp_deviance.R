library(EloML)
library(OpenML)
library(ggplot2)
library(dplyr)
library(scales)

data_path <- "./data/elo_results_6_models_400_params/"
list_files <- list.files(data_path, pattern = '.csv')

dst_num <- gsub(pattern = '[^0-9]', replacement = '', x = list_files)

## get OpenML names
dst <- sapply(dst_num, function(x) getOMLDataSet(as.numeric(x))$desc$name)

p_model <- readRDS('data/elo_results_6_models_400_params/Rd_files/epp_openml_glm_models.Rd')
names(p_model) <- list.files(data_path, pattern = '^elo_[0-9].+csv$')

### check that every dataset has 6 algorithms - not at index 31 - we have to remove that
which(sapply(lapply(p_model, function(x) unique(gsub(pattern = '_[0-9]+', replacement = '', rownames(coefficients(x))))), length) != 6)

dst <- dst[-31]
deviance_epp <- sapply(p_model[-31], deviance)


m <- length(coefficients(p_model[[1]])[-1])
degrees_freedom <- m*(m-2)

## we do one-side test for deviance
pvalue_chisquare <- sapply(deviance_epp, function(x)  pchisq(x, df = degrees_freedom, lower.tail = FALSE))
pvalue_norm <- pnorm((deviance_epp - degrees_freedom)/(sqrt(2*degrees_freedom)), lower.tail = FALSE)

stand_deviance <- (deviance_epp - degrees_freedom)/sqrt(2*degrees_freedom)

hist(stand_deviance)


idx_best <- which.min(deviance_epp)
name_best <- dst[idx_best]
idx_worst <- which.max(deviance_epp)
name_worst <- dst[idx_worst]
selected_datasets <- c(names(idx_best), names(idx_worst))

## load all data for the best and the worst dataset
selected_files <- lapply(c('data/elo_results_6_models_400_params/Rd_files/elo_1510.Rd',
                           'data/elo_results_6_models_400_params/Rd_files/elo_1467.Rd'), readRDS)

actual_score_worst <- EloML::plot_wins_ratio(selected_files[[2]],random_sample = 0.3, random_state = 123)
actual_score_best <- EloML::plot_wins_ratio(selected_files[[1]],random_sample = 0.3, random_state = 123)

## extract data to plot and rbind
data_actual_score_worst <- actual_score_worst$data
data_actual_score_worst[['algorithm']] <- gsub(pattern = '_[0-9]+', replacement = '', data_actual_score_worst[['winner']])
data_actual_score_worst[['dataset']] <- name_worst

data_actual_score_best <- actual_score_best$data
data_actual_score_best[['algorithm']] <- gsub(pattern = '_[0-9]+', replacement = '', data_actual_score_best[['winner']])
data_actual_score_best[['dataset']] <- name_best



## rbind results for these datasets

actual_score_bind <- rbind(data_actual_score_worst, data_actual_score_best)
actual_score_bind[['algorithm']] <- gsub('randomForest', 'RF', actual_score_bind[['players']])

p_best <- ggplot(actual_score_bind, aes(x=ratio, y = pred_ratio, color  = algorithm))+
  geom_point(alpha = 0.2)+
  geom_abline(slope=1)+
  facet_grid(~dataset)+
  theme_light()+
  theme(legend.position = 'bottom')+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  labs(x = 'Actual empirical probability of winning', y = 'Predicted probability of winning')+
  scale_color_discrete(name='Algorithm')
ggsave(p_best, filename = 'deviance_best_worst.pdf', device =  'pdf')
ggsave(p_best, filename = 'deviance_best_worst.png', device =  'png', width = 8)
