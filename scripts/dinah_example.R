# chamber experiment 2022 analysis
## r script to analyze the greenhouse and growth chamber experiment from 2022

## load packages
library(plantecophys)
library(R.utils)

## load functions
#sourceDirectory('functions')

## load data
aci_data <- read.csv('../data/example_dinah/Dinah_potato_curves_fullyMerged.csv')
head(aci_data)
colnames(aci_data)

## start visualization, curve fitting, and data frame creation
ids <- levels(as.factor(aci_data$id))
curve_fits <- c()

### plant id1 pre_heatwave
aci_data_id1_pre = subset(aci_data, id == ids[1] & meas.type == 'pre_heatwave')
aci_data_id1_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_pre)
#### fit aci curve
fit_aci_id1_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 300,
                         useRd = FALSE,
                         Tcorrect = FALSE,
                         # fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id1_pre)
summary(fit_aci_id1_pre)
coef_id1_pre <- coef(fit_aci_id1_pre)
#### plot
plot(fit_aci_id1_pre)
#### add to dataframe
aci_data_id1_pre_data <- cbind(aci_data_id1_pre[1, c(10, 284, 9, 14, 16, 19)],
                               mean(aci_data_id1_pre[,30]),
                               mean(aci_data_id1_pre[,118]),
                               fit_aci_id1_pre[[2]][1,1],
                               fit_aci_id1_pre[[2]][1,2],
                               fit_aci_id1_pre[[2]][2,1],
                               fit_aci_id1_pre[[2]][2,2],
                               fit_aci_id1_pre[[2]][3,1],
                               fit_aci_id1_pre[[2]][3,2],
                               # fit_aci_id1_pre[[2]][4,1],
                               # fit_aci_id1_pre[[2]][4,2],
                               fit_aci_id1_pre$RMSE,
                               fit_aci_id1_pre$Ci_transition,
                               fit_aci_id1_pre$citransition,
                               fit_aci_id1_pre$Km,
                               fit_aci_id1_pre$GammaStar,
                               fit_aci_id1_pre$fitmethod,
                               fit_aci_id1_pre$Tcorrect,
                               fit_aci_id1_pre$fitTPU)
colnames(aci_data_id1_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                    'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                    'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                    'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                    'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                    'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id1_pre_data)

### plant id1 post_heatwave
aci_data_id1_post = subset(aci_data, id == ids[1] & meas.type == 'post_heatwave')
aci_data_id1_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id1_post)
#### fit aci curve
fit_aci_id1_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                         # citransition = 350,
                         Tcorrect = FALSE,
                         fitTPU = FALSE,
                         fitmethod = 'bilinear',
                         data = aci_data_id1_post)
summary(fit_aci_id1_post)
coef_id1_post <- coef(fit_aci_id1_post)
#### plot
plot(fit_aci_id1_post)
#### add to dataframe
aci_data_id1_post_data <- cbind(aci_data_id1_post[1, c(10, 284, 9, 14, 16, 19)],
                               mean(aci_data_id1_post[,30]),
                               mean(aci_data_id1_post[,118]),
                               fit_aci_id1_post[[2]][1,1],
                               fit_aci_id1_post[[2]][1,2],
                               fit_aci_id1_post[[2]][2,1],
                               fit_aci_id1_post[[2]][2,2],
                               fit_aci_id1_post[[2]][3,1],
                               fit_aci_id1_post[[2]][3,2],
                               # fit_aci_id1_post[[2]][4,1],
                               # fit_aci_id1_post[[2]][4,2],
                               fit_aci_id1_post$RMSE,
                               fit_aci_id1_post$Ci_transition,
                               fit_aci_id1_post$citransition,
                               fit_aci_id1_post$Km,
                               fit_aci_id1_post$GammaStar,
                               fit_aci_id1_post$fitmethod,
                               fit_aci_id1_post$Tcorrect,
                               fit_aci_id1_post$fitTPU)
colnames(aci_data_id1_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                     'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                     'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                     'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                     'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                     'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id1_post_data)

# ### plant id100 pre_heatwave
# aci_data_id100_pre = subset(aci_data, id == ids[2] & meas.type == 'pre_heatwave')
# aci_data_id100_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100_pre)
# #### fit aci curve
# fit_aci_id100_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                          citransition = 300,
#                          Tcorrect = FALSE,
#                          data = aci_data_id100_pre)
# summary(fit_aci_id100_pre)
# coef_id100_pre <- coef(fit_aci_id100_pre)
# #### plot
# plot(fit_aci_id100_pre)
# #### add to dataframe
# aci_data_id100_pre_data <- cbind(aci_data_id100_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                mean(aci_data_id100_pre[,30]),
#                                mean(aci_data_id100_pre[,118]),
#                                fit_aci_id100_pre[[2]][1,1],
#                                fit_aci_id100_pre[[2]][1,2],
#                                fit_aci_id100_pre[[2]][2,1],
#                                fit_aci_id100_pre[[2]][2,2],
#                                fit_aci_id100_pre[[2]][3,1],
#                                fit_aci_id100_pre[[2]][3,2],
#                                fit_aci_id100_pre$RMSE,
#                                fit_aci_id100_pre$Ci_transition,
#                                fit_aci_id100_pre$citransition,
#                                fit_aci_id100_pre$Km,
#                                fit_aci_id100_pre$GammaStar,
#                                fit_aci_id100_pre$fitmethod,
#                                fit_aci_id100_pre$Tcorrect,
#                                fit_aci_id100_pre$fitTPU)
# colnames(aci_data_id100_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                      'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                      'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                      'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                      'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                      'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100_pre_data)
# 
# ### plant id100 post_heatwave
# aci_data_id100_post = subset(aci_data, id == ids[2] & meas.type == 'post_heatwave')
# aci_data_id100_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100_post)
# #### fit aci curve
# fit_aci_id100_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                           citransition = 350,
#                           Tcorrect = FALSE,
#                           data = aci_data_id100_post)
# summary(fit_aci_id100_post)
# coef_id100_post <- coef(fit_aci_id100_post)
# #### plot
# plot(fit_aci_id100_post)
# #### add to dataframe
# aci_data_id100_post_data <- cbind(aci_data_id100_post[1, c(10, 284, 9, 14, 16, 19)],
#                                 mean(aci_data_id100_post[,30]),
#                                 mean(aci_data_id100_post[,118]),
#                                 fit_aci_id100_post[[2]][1,1],
#                                 fit_aci_id100_post[[2]][1,2],
#                                 fit_aci_id100_post[[2]][2,1],
#                                 fit_aci_id100_post[[2]][2,2],
#                                 fit_aci_id100_post[[2]][3,1],
#                                 fit_aci_id100_post[[2]][3,2],
#                                 fit_aci_id100_post$RMSE,
#                                 fit_aci_id100_post$Ci_transition,
#                                 fit_aci_id100_post$citransition,
#                                 fit_aci_id100_post$Km,
#                                 fit_aci_id100_post$GammaStar,
#                                 fit_aci_id100_post$fitmethod,
#                                 fit_aci_id100_post$Tcorrect,
#                                 fit_aci_id100_post$fitTPU)
# colnames(aci_data_id100_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                       'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100_post_data)

##############################################################################
##############################################################################
###########id100b
##############################################################################
##############################################################################
### plant id100b pre_heatwave
# aci_data_id100b_pre = subset(aci_data, id == ids[3] & meas.type == 'pre_heatwave')
# aci_data_id100b_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100b_pre)
# #### fit aci curve
# fit_aci_id100b_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 300,
#                            Tcorrect = FALSE,
#                            data = aci_data_id100b_pre)
# summary(fit_aci_id100b_pre)
# coef_id100b_pre <- coef(fit_aci_id100b_pre)
# #### plot
# plot(fit_aci_id100b_pre)
# #### add to dataframe
# aci_data_id100b_pre_data <- cbind(aci_data_id100b_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id100b_pre[,30]),
#                                  mean(aci_data_id100b_pre[,118]),
#                                  fit_aci_id100b_pre[[2]][1,1],
#                                  fit_aci_id100b_pre[[2]][1,2],
#                                  fit_aci_id100b_pre[[2]][2,1],
#                                  fit_aci_id100b_pre[[2]][2,2],
#                                  fit_aci_id100b_pre[[2]][3,1],
#                                  fit_aci_id100b_pre[[2]][3,2],
#                                  fit_aci_id100b_pre$RMSE,
#                                  fit_aci_id100b_pre$Ci_transition,
#                                  fit_aci_id100b_pre$citransition,
#                                  fit_aci_id100b_pre$Km,
#                                  fit_aci_id100b_pre$GammaStar,
#                                  fit_aci_id100b_pre$fitmethod,
#                                  fit_aci_id100b_pre$Tcorrect,
#                                  fit_aci_id100b_pre$fitTPU)
# colnames(aci_data_id100b_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100b_pre_data)
# 
# ### plant id100b post_heatwave
# aci_data_id100b_post = subset(aci_data, id == ids[3] & meas.type == 'post_heatwave')
# aci_data_id100b_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id100b_post)
# #### fit aci curve
# fit_aci_id100b_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 350,
#                             Tcorrect = FALSE,
#                             data = aci_data_id100b_post)
# summary(fit_aci_id100b_post)
# coef_id100b_post <- coef(fit_aci_id100b_post)
# #### plot
# plot(fit_aci_id100b_post)
# #### add to dataframe
# aci_data_id100b_post_data <- cbind(aci_data_id100b_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id100b_post[,30]),
#                                   mean(aci_data_id100b_post[,118]),
#                                   fit_aci_id100b_post[[2]][1,1],
#                                   fit_aci_id100b_post[[2]][1,2],
#                                   fit_aci_id100b_post[[2]][2,1],
#                                   fit_aci_id100b_post[[2]][2,2],
#                                   fit_aci_id100b_post[[2]][3,1],
#                                   fit_aci_id100b_post[[2]][3,2],
#                                   fit_aci_id100b_post$RMSE,
#                                   fit_aci_id100b_post$Ci_transition,
#                                   fit_aci_id100b_post$citransition,
#                                   fit_aci_id100b_post$Km,
#                                   fit_aci_id100b_post$GammaStar,
#                                   fit_aci_id100b_post$fitmethod,
#                                   fit_aci_id100b_post$Tcorrect,
#                                   fit_aci_id100b_post$fitTPU)
# colnames(aci_data_id100b_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id100b_post_data)

##############################################################################
##############################################################################
###########id102
##############################################################################
##############################################################################
### plant id102 pre_heatwave
aci_data_id102_pre = subset(aci_data, id == ids[4] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id102_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id102_pre)
#### fit aci curve
fit_aci_id102_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                            data = aci_data_id102_pre)
summary(fit_aci_id102_pre)
#### plot
plot(fit_aci_id102_pre)
#### add to dataframe
aci_data_id102_pre_data <- cbind(aci_data_id102_pre[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id102_pre[,30]),
                                  mean(aci_data_id102_pre[,118]),
                                  fit_aci_id102_pre[[2]][1,1],
                                  fit_aci_id102_pre[[2]][1,2],
                                  fit_aci_id102_pre[[2]][2,1],
                                  fit_aci_id102_pre[[2]][2,2],
                                  fit_aci_id102_pre[[2]][3,1],
                                  fit_aci_id102_pre[[2]][3,2],
                                 # fit_aci_id102_pre[[2]][4,1],
                                 # fit_aci_id102_pre[[2]][4,2],
                                  fit_aci_id102_pre$RMSE,
                                  fit_aci_id102_pre$Ci_transition,
                                  fit_aci_id102_pre$citransition,
                                  fit_aci_id102_pre$Km,
                                  fit_aci_id102_pre$GammaStar,
                                  fit_aci_id102_pre$fitmethod,
                                  fit_aci_id102_pre$Tcorrect,
                                  fit_aci_id102_pre$fitTPU)
colnames(aci_data_id102_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id102_pre_data)

### plant id102 post_heatwave
aci_data_id102_post = subset(aci_data, id == ids[4] & meas.type == 'post_heatwave')
aci_data_id102_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id102_post)
#### fit aci curve
fit_aci_id102_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                             data = aci_data_id102_post)
summary(fit_aci_id102_post)
#### plot
plot(fit_aci_id102_post)
#### add to dataframe
aci_data_id102_post_data <- cbind(aci_data_id102_post[1, c(10, 284, 9, 14, 16, 19)],
                                   mean(aci_data_id102_post[,30]),
                                   mean(aci_data_id102_post[,118]),
                                   fit_aci_id102_post[[2]][1,1],
                                   fit_aci_id102_post[[2]][1,2],
                                   fit_aci_id102_post[[2]][2,1],
                                   fit_aci_id102_post[[2]][2,2],
                                   fit_aci_id102_post[[2]][3,1],
                                   fit_aci_id102_post[[2]][3,2],
                                  # fit_aci_id102_post[[2]][4,1],
                                  # fit_aci_id102_post[[2]][4,2],
                                   fit_aci_id102_post$RMSE,
                                   fit_aci_id102_post$Ci_transition,
                                   fit_aci_id102_post$citransition,
                                   fit_aci_id102_post$Km,
                                   fit_aci_id102_post$GammaStar,
                                   fit_aci_id102_post$fitmethod,
                                   fit_aci_id102_post$Tcorrect,
                                   fit_aci_id102_post$fitTPU)
colnames(aci_data_id102_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                         'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id102_post_data)

##############################################################################
##############################################################################
###########id104
##############################################################################
##############################################################################
### plant id104 pre_heatwave
# aci_data_id104_pre = subset(aci_data, id == ids[5] & meas.type == 'pre_heatwave')
# aci_data_id104_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id104_pre)
# #### fit aci curve
# fit_aci_id104_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 350,
#                            Tcorrect = FALSE,
#                            data = aci_data_id104_pre)
# summary(fit_aci_id104_pre)
# #### plot
# plot(fit_aci_id104_pre)
# #### add to dataframe
# aci_data_id104_pre_data <- cbind(aci_data_id104_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id104_pre[,30]),
#                                  mean(aci_data_id104_pre[,118]),
#                                  fit_aci_id104_pre[[2]][1,1],
#                                  fit_aci_id104_pre[[2]][1,2],
#                                  fit_aci_id104_pre[[2]][2,1],
#                                  fit_aci_id104_pre[[2]][2,2],
#                                  fit_aci_id104_pre[[2]][3,1],
#                                  fit_aci_id104_pre[[2]][3,2],
#                                  fit_aci_id104_pre$RMSE,
#                                  fit_aci_id104_pre$Ci_transition,
#                                  fit_aci_id104_pre$citransition,
#                                  fit_aci_id104_pre$Km,
#                                  fit_aci_id104_pre$GammaStar,
#                                  fit_aci_id104_pre$fitmethod,
#                                  fit_aci_id104_pre$Tcorrect,
#                                  fit_aci_id104_pre$fitTPU)
# colnames(aci_data_id104_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id104_pre_data)
# 
# ### plant id104 post_heatwave
# aci_data_id104_post = subset(aci_data, id == ids[5] & meas.type == 'post_heatwave')
# aci_data_id104_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id104_post)
# #### fit aci curve
# fit_aci_id104_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 350,
#                             Tcorrect = FALSE,
#                             data = aci_data_id104_post)
# summary(fit_aci_id104_post)
# #### plot
# plot(fit_aci_id104_post)
# #### add to dataframe
# aci_data_id104_post_data <- cbind(aci_data_id104_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id104_post[,30]),
#                                   mean(aci_data_id104_post[,118]),
#                                   fit_aci_id104_post[[2]][1,1],
#                                   fit_aci_id104_post[[2]][1,2],
#                                   fit_aci_id104_post[[2]][2,1],
#                                   fit_aci_id104_post[[2]][2,2],
#                                   fit_aci_id104_post[[2]][3,1],
#                                   fit_aci_id104_post[[2]][3,2],
#                                   fit_aci_id104_post$RMSE,
#                                   fit_aci_id104_post$Ci_transition,
#                                   fit_aci_id104_post$citransition,
#                                   fit_aci_id104_post$Km,
#                                   fit_aci_id104_post$GammaStar,
#                                   fit_aci_id104_post$fitmethod,
#                                   fit_aci_id104_post$Tcorrect,
#                                   fit_aci_id104_post$fitTPU)
# colnames(aci_data_id104_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id104_post_data)

##############################################################################
##############################################################################
###########id105
##############################################################################
##############################################################################
### plant id105 pre_heatwave
aci_data_id105_pre = subset(aci_data, id == ids[6] & meas.type == 'pre_heatwave')
aci_data_id105_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id105_pre)
#### fit aci curve
fit_aci_id105_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id105_pre)
summary(fit_aci_id105_pre)
#### plot
plot(fit_aci_id105_pre)
#### add to dataframe
aci_data_id105_pre_data <- cbind(aci_data_id105_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id105_pre[,30]),
                                 mean(aci_data_id105_pre[,118]),
                                 fit_aci_id105_pre[[2]][1,1],
                                 fit_aci_id105_pre[[2]][1,2],
                                 fit_aci_id105_pre[[2]][2,1],
                                 fit_aci_id105_pre[[2]][2,2],
                                 fit_aci_id105_pre[[2]][3,1],
                                 fit_aci_id105_pre[[2]][3,2],
                                 # fit_aci_id105_pre[[2]][4,1],
                                 # fit_aci_id105_pre[[2]][4,2],
                                 fit_aci_id105_pre$RMSE,
                                 fit_aci_id105_pre$Ci_transition,
                                 fit_aci_id105_pre$citransition,
                                 fit_aci_id105_pre$Km,
                                 fit_aci_id105_pre$GammaStar,
                                 fit_aci_id105_pre$fitmethod,
                                 fit_aci_id105_pre$Tcorrect,
                                 fit_aci_id105_pre$fitTPU)
colnames(aci_data_id105_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id105_pre_data)

### plant id105 post_heatwave
aci_data_id105_post = subset(aci_data, id == ids[6] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id105_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id105_post)
#### fit aci curve
fit_aci_id105_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id105_post)
summary(fit_aci_id105_post)
#### plot
plot(fit_aci_id105_post)
#### add to dataframe
aci_data_id105_post_data <- cbind(aci_data_id105_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id105_post[,30]),
                                  mean(aci_data_id105_post[,118]),
                                  fit_aci_id105_post[[2]][1,1],
                                  fit_aci_id105_post[[2]][1,2],
                                  fit_aci_id105_post[[2]][2,1],
                                  fit_aci_id105_post[[2]][2,2],
                                  fit_aci_id105_post[[2]][3,1],
                                  fit_aci_id105_post[[2]][3,2],
                                  # fit_aci_id105_post[[2]][4,1],
                                  # fit_aci_id105_post[[2]][4,2],
                                  fit_aci_id105_post$RMSE,
                                  fit_aci_id105_post$Ci_transition,
                                  fit_aci_id105_post$citransition,
                                  fit_aci_id105_post$Km,
                                  fit_aci_id105_post$GammaStar,
                                  fit_aci_id105_post$fitmethod,
                                  fit_aci_id105_post$Tcorrect,
                                  fit_aci_id105_post$fitTPU)
colnames(aci_data_id105_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id105_post_data)

##############################################################################
##############################################################################
###########id11
##############################################################################
##############################################################################
### plant id11 pre_heatwave
aci_data_id11_pre = subset(aci_data, id == ids[7] & meas.type == 'pre_heatwave')
aci_data_id11_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id11_pre)
#### fit aci curve
fit_aci_id11_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                          # citransition = 300,
                          Tcorrect = FALSE,
                          fitTPU = FALSE,
                          fitmethod = 'bilinear',
                           data = aci_data_id11_pre)
summary(fit_aci_id11_pre)
#### plot
plot(fit_aci_id11_pre)
#### add to dataframe
aci_data_id11_pre_data <- cbind(aci_data_id11_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id11_pre[,30]),
                                 mean(aci_data_id11_pre[,118]),
                                 fit_aci_id11_pre[[2]][1,1],
                                 fit_aci_id11_pre[[2]][1,2],
                                 fit_aci_id11_pre[[2]][2,1],
                                 fit_aci_id11_pre[[2]][2,2],
                                 fit_aci_id11_pre[[2]][3,1],
                                 fit_aci_id11_pre[[2]][3,2],
                                # fit_aci_id11_pre[[2]][4,1],
                                # fit_aci_id11_pre[[2]][4,2],
                                 fit_aci_id11_pre$RMSE,
                                 fit_aci_id11_pre$Ci_transition,
                                 fit_aci_id11_pre$citransition,
                                 fit_aci_id11_pre$Km,
                                 fit_aci_id11_pre$GammaStar,
                                 fit_aci_id11_pre$fitmethod,
                                 fit_aci_id11_pre$Tcorrect,
                                 fit_aci_id11_pre$fitTPU)
colnames(aci_data_id11_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id11_pre_data)

### plant id11 post_heatwave
aci_data_id11_post = subset(aci_data, id == ids[7] & meas.type == 'post_heatwave')
aci_data_id11_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id11_post)
#### fit aci curve
fit_aci_id11_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                            data = aci_data_id11_post)
summary(fit_aci_id11_post)
#### plot
plot(fit_aci_id11_post)
#### add to dataframe
aci_data_id11_post_data <- cbind(aci_data_id11_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id11_post[,30]),
                                  mean(aci_data_id11_post[,118]),
                                  fit_aci_id11_post[[2]][1,1],
                                  fit_aci_id11_post[[2]][1,2],
                                  fit_aci_id11_post[[2]][2,1],
                                  fit_aci_id11_post[[2]][2,2],
                                  fit_aci_id11_post[[2]][3,1],
                                  fit_aci_id11_post[[2]][3,2],
                                 # fit_aci_id11_post[[2]][4,1],
                                 # fit_aci_id11_post[[2]][4,2],
                                  fit_aci_id11_post$RMSE,
                                  fit_aci_id11_post$Ci_transition,
                                  fit_aci_id11_post$citransition,
                                  fit_aci_id11_post$Km,
                                  fit_aci_id11_post$GammaStar,
                                  fit_aci_id11_post$fitmethod,
                                  fit_aci_id11_post$Tcorrect,
                                  fit_aci_id11_post$fitTPU)
colnames(aci_data_id11_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id11_post_data)

##############################################################################
##############################################################################
###########id110
##############################################################################
##############################################################################
### plant id110 pre_heatwave
aci_data_id110_pre = subset(aci_data, id == ids[8] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id110_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id110_pre)
#### fit aci curve
fit_aci_id110_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                          data = aci_data_id110_pre)
summary(fit_aci_id110_pre)
#### plot
plot(fit_aci_id110_pre)
#### add to dataframe
aci_data_id110_pre_data <- cbind(aci_data_id110_pre[1, c(10, 284, 9, 14, 16, 19)],
                                mean(aci_data_id110_pre[,30]),
                                mean(aci_data_id110_pre[,118]),
                                fit_aci_id110_pre[[2]][1,1],
                                fit_aci_id110_pre[[2]][1,2],
                                fit_aci_id110_pre[[2]][2,1],
                                fit_aci_id110_pre[[2]][2,2],
                                fit_aci_id110_pre[[2]][3,1],
                                fit_aci_id110_pre[[2]][3,2],
                                # fit_aci_id110_pre[[2]][4,1],
                                # fit_aci_id110_pre[[2]][4,2],
                                fit_aci_id110_pre$RMSE,
                                fit_aci_id110_pre$Ci_transition,
                                fit_aci_id110_pre$citransition,
                                fit_aci_id110_pre$Km,
                                fit_aci_id110_pre$GammaStar,
                                fit_aci_id110_pre$fitmethod,
                                fit_aci_id110_pre$Tcorrect,
                                fit_aci_id110_pre$fitTPU)
colnames(aci_data_id110_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                      'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                      'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                      'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                      'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                      'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id110_pre_data)

### plant id110 post_heatwave
aci_data_id110_post = subset(aci_data, id == ids[8] & meas.type == 'post_heatwave' & Ci < 500)
aci_data_id110_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id110_post)
#### fit aci curve
fit_aci_id110_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                           data = aci_data_id110_post)
summary(fit_aci_id110_post)
#### plot
plot(fit_aci_id110_post)
#### add to dataframe
aci_data_id110_post_data <- cbind(aci_data_id110_post[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id110_post[,30]),
                                 mean(aci_data_id110_post[,118]),
                                 fit_aci_id110_post[[2]][1,1],
                                 fit_aci_id110_post[[2]][1,2],
                                 fit_aci_id110_post[[2]][2,1],
                                 fit_aci_id110_post[[2]][2,2],
                                 fit_aci_id110_post[[2]][3,1],
                                 fit_aci_id110_post[[2]][3,2],
                                 # fit_aci_id110_post[[2]][4,1],
                                 # fit_aci_id110_post[[2]][4,2],
                                 fit_aci_id110_post$RMSE,
                                 fit_aci_id110_post$Ci_transition,
                                 fit_aci_id110_post$citransition,
                                 fit_aci_id110_post$Km,
                                 fit_aci_id110_post$GammaStar,
                                 fit_aci_id110_post$fitmethod,
                                 fit_aci_id110_post$Tcorrect,
                                 fit_aci_id110_post$fitTPU)
colnames(aci_data_id110_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id110_post_data)

##############################################################################
##############################################################################
###########id112
##############################################################################
##############################################################################
### plant id112 pre_heatwave
aci_data_id112_pre = subset(aci_data, id == ids[9] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id112_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id112_pre)
#### fit aci curve
fit_aci_id112_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id112_pre)
summary(fit_aci_id112_pre)
#### plot
plot(fit_aci_id112_pre)
#### add to dataframe
aci_data_id112_pre_data <- cbind(aci_data_id112_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id112_pre[,30]),
                                 mean(aci_data_id112_pre[,118]),
                                 fit_aci_id112_pre[[2]][1,1],
                                 fit_aci_id112_pre[[2]][1,2],
                                 fit_aci_id112_pre[[2]][2,1],
                                 fit_aci_id112_pre[[2]][2,2],
                                 fit_aci_id112_pre[[2]][3,1],
                                 fit_aci_id112_pre[[2]][3,2],
                                 # fit_aci_id112_pre[[2]][4,1],
                                 # fit_aci_id112_pre[[2]][4,2],
                                 fit_aci_id112_pre$RMSE,
                                 fit_aci_id112_pre$Ci_transition,
                                 fit_aci_id112_pre$citransition,
                                 fit_aci_id112_pre$Km,
                                 fit_aci_id112_pre$GammaStar,
                                 fit_aci_id112_pre$fitmethod,
                                 fit_aci_id112_pre$Tcorrect,
                                 fit_aci_id112_pre$fitTPU)
colnames(aci_data_id112_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id112_pre_data)

### plant id112 post_heatwave
aci_data_id112_post = subset(aci_data, id == ids[9] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id112_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id112_post)
#### fit aci curve
fit_aci_id112_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id112_post)
summary(fit_aci_id112_post)
#### plot
plot(fit_aci_id112_post)
#### add to dataframe
aci_data_id112_post_data <- cbind(aci_data_id112_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id112_post[,30]),
                                  mean(aci_data_id112_post[,118]),
                                  fit_aci_id112_post[[2]][1,1],
                                  fit_aci_id112_post[[2]][1,2],
                                  fit_aci_id112_post[[2]][2,1],
                                  fit_aci_id112_post[[2]][2,2],
                                  fit_aci_id112_post[[2]][3,1],
                                  fit_aci_id112_post[[2]][3,2],
                                  # fit_aci_id112_post[[2]][4,1],
                                  # fit_aci_id112_post[[2]][4,2],
                                  fit_aci_id112_post$RMSE,
                                  fit_aci_id112_post$Ci_transition,
                                  fit_aci_id112_post$citransition,
                                  fit_aci_id112_post$Km,
                                  fit_aci_id112_post$GammaStar,
                                  fit_aci_id112_post$fitmethod,
                                  fit_aci_id112_post$Tcorrect,
                                  fit_aci_id112_post$fitTPU)
colnames(aci_data_id112_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id112_post_data)

##############################################################################
##############################################################################
###########id113
##############################################################################
##############################################################################
### plant id113 pre_heatwave
aci_data_id113_pre = subset(aci_data, id == ids[10] & meas.type == 'pre_heatwave')
aci_data_id113_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id113_pre)
#### fit aci curve
fit_aci_id113_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id113_pre)
summary(fit_aci_id113_pre)
#### plot
plot(fit_aci_id113_pre)
#### add to dataframe
aci_data_id113_pre_data <- cbind(aci_data_id113_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id113_pre[,30]),
                                 mean(aci_data_id113_pre[,118]),
                                 fit_aci_id113_pre[[2]][1,1],
                                 fit_aci_id113_pre[[2]][1,2],
                                 fit_aci_id113_pre[[2]][2,1],
                                 fit_aci_id113_pre[[2]][2,2],
                                 fit_aci_id113_pre[[2]][3,1],
                                 fit_aci_id113_pre[[2]][3,2],
                                 # fit_aci_id113_pre[[2]][4,1],
                                 # fit_aci_id113_pre[[2]][4,2],
                                 fit_aci_id113_pre$RMSE,
                                 fit_aci_id113_pre$Ci_transition,
                                 fit_aci_id113_pre$citransition,
                                 fit_aci_id113_pre$Km,
                                 fit_aci_id113_pre$GammaStar,
                                 fit_aci_id113_pre$fitmethod,
                                 fit_aci_id113_pre$Tcorrect,
                                 fit_aci_id113_pre$fitTPU)
colnames(aci_data_id113_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id113_pre_data)

### plant id113 post_heatwave
aci_data_id113_post = subset(aci_data, id == ids[10] & meas.type == 'post_heatwave')
aci_data_id113_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id113_post)
#### fit aci curve
fit_aci_id113_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id113_post)
summary(fit_aci_id113_post)
#### plot
plot(fit_aci_id113_post)
#### add to dataframe
aci_data_id113_post_data <- cbind(aci_data_id113_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id113_post[,30]),
                                  mean(aci_data_id113_post[,118]),
                                  fit_aci_id113_post[[2]][1,1],
                                  fit_aci_id113_post[[2]][1,2],
                                  fit_aci_id113_post[[2]][2,1],
                                  fit_aci_id113_post[[2]][2,2],
                                  fit_aci_id113_post[[2]][3,1],
                                  fit_aci_id113_post[[2]][3,2],
                                  # fit_aci_id113_post[[2]][4,1],
                                  # fit_aci_id113_post[[2]][4,2],
                                  fit_aci_id113_post$RMSE,
                                  fit_aci_id113_post$Ci_transition,
                                  fit_aci_id113_post$citransition,
                                  fit_aci_id113_post$Km,
                                  fit_aci_id113_post$GammaStar,
                                  fit_aci_id113_post$fitmethod,
                                  fit_aci_id113_post$Tcorrect,
                                  fit_aci_id113_post$fitTPU)
colnames(aci_data_id113_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id113_post_data)

##############################################################################
##############################################################################
###########id114
##############################################################################
##############################################################################
### plant id114 pre_heatwave
aci_data_id114_pre = subset(aci_data, id == ids[11] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id114_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id114_pre)
#### fit aci curve
fit_aci_id114_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id114_pre)
summary(fit_aci_id114_pre)
#### plot
plot(fit_aci_id114_pre)
#### add to dataframe
aci_data_id114_pre_data <- cbind(aci_data_id114_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id114_pre[,30]),
                                 mean(aci_data_id114_pre[,118]),
                                 fit_aci_id114_pre[[2]][1,1],
                                 fit_aci_id114_pre[[2]][1,2],
                                 fit_aci_id114_pre[[2]][2,1],
                                 fit_aci_id114_pre[[2]][2,2],
                                 fit_aci_id114_pre[[2]][3,1],
                                 fit_aci_id114_pre[[2]][3,2],
                                 # fit_aci_id114_pre[[2]][4,1],
                                 # fit_aci_id114_pre[[2]][4,2],
                                 fit_aci_id114_pre$RMSE,
                                 fit_aci_id114_pre$Ci_transition,
                                 fit_aci_id114_pre$citransition,
                                 fit_aci_id114_pre$Km,
                                 fit_aci_id114_pre$GammaStar,
                                 fit_aci_id114_pre$fitmethod,
                                 fit_aci_id114_pre$Tcorrect,
                                 fit_aci_id114_pre$fitTPU)
colnames(aci_data_id114_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id114_pre_data)

### plant id114 post_heatwave
aci_data_id114_post = subset(aci_data, id == ids[11] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id114_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id114_post)
#### fit aci curve
fit_aci_id114_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id114_post)
summary(fit_aci_id114_post)
#### plot
plot(fit_aci_id114_post)
#### add to dataframe
aci_data_id114_post_data <- cbind(aci_data_id114_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id114_post[,30]),
                                  mean(aci_data_id114_post[,118]),
                                  fit_aci_id114_post[[2]][1,1],
                                  fit_aci_id114_post[[2]][1,2],
                                  fit_aci_id114_post[[2]][2,1],
                                  fit_aci_id114_post[[2]][2,2],
                                  fit_aci_id114_post[[2]][3,1],
                                  fit_aci_id114_post[[2]][3,2],
                                  # fit_aci_id114_post[[2]][4,1],
                                  # fit_aci_id114_post[[2]][4,2],
                                  fit_aci_id114_post$RMSE,
                                  fit_aci_id114_post$Ci_transition,
                                  fit_aci_id114_post$citransition,
                                  fit_aci_id114_post$Km,
                                  fit_aci_id114_post$GammaStar,
                                  fit_aci_id114_post$fitmethod,
                                  fit_aci_id114_post$Tcorrect,
                                  fit_aci_id114_post$fitTPU)
colnames(aci_data_id114_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id114_post_data)

##############################################################################
##############################################################################
###########id118
##############################################################################
##############################################################################
### plant id118 pre_heatwave
# aci_data_id118_pre = subset(aci_data, id == ids[12] & meas.type == 'pre_heatwave')
# aci_data_id118_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id118_pre)
# #### fit aci curve
# fit_aci_id118_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 450,
#                            Tcorrect = FALSE,
#                            data = aci_data_id118_pre)
# summary(fit_aci_id118_pre)
# #### plot
# plot(fit_aci_id118_pre)
# #### add to dataframe
# aci_data_id118_pre_data <- cbind(aci_data_id118_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id118_pre[,30]),
#                                  mean(aci_data_id118_pre[,118]),
#                                  fit_aci_id118_pre[[2]][1,1],
#                                  fit_aci_id118_pre[[2]][1,2],
#                                  fit_aci_id118_pre[[2]][2,1],
#                                  fit_aci_id118_pre[[2]][2,2],
#                                  fit_aci_id118_pre[[2]][3,1],
#                                  fit_aci_id118_pre[[2]][3,2],
#                                  fit_aci_id118_pre$RMSE,
#                                  fit_aci_id118_pre$Ci_transition,
#                                  fit_aci_id118_pre$citransition,
#                                  fit_aci_id118_pre$Km,
#                                  fit_aci_id118_pre$GammaStar,
#                                  fit_aci_id118_pre$fitmethod,
#                                  fit_aci_id118_pre$Tcorrect,
#                                  fit_aci_id118_pre$fitTPU)
# colnames(aci_data_id118_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id118_pre_data)
# 
# ### plant id118 post_heatwave
# aci_data_id118_post = subset(aci_data, id == ids[12] & meas.type == 'post_heatwave')
# aci_data_id118_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id118_post)
# #### fit aci curve
# fit_aci_id118_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 400,
#                             Tcorrect = FALSE,
#                             data = aci_data_id118_post)
# summary(fit_aci_id118_post)
# #### plot
# plot(fit_aci_id118_post)
# #### add to dataframe
# aci_data_id118_post_data <- cbind(aci_data_id118_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id118_post[,30]),
#                                   mean(aci_data_id118_post[,118]),
#                                   fit_aci_id118_post[[2]][1,1],
#                                   fit_aci_id118_post[[2]][1,2],
#                                   fit_aci_id118_post[[2]][2,1],
#                                   fit_aci_id118_post[[2]][2,2],
#                                   fit_aci_id118_post[[2]][3,1],
#                                   fit_aci_id118_post[[2]][3,2],
#                                   fit_aci_id118_post$RMSE,
#                                   fit_aci_id118_post$Ci_transition,
#                                   fit_aci_id118_post$citransition,
#                                   fit_aci_id118_post$Km,
#                                   fit_aci_id118_post$GammaStar,
#                                   fit_aci_id118_post$fitmethod,
#                                   fit_aci_id118_post$Tcorrect,
#                                   fit_aci_id118_post$fitTPU)
# colnames(aci_data_id118_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id118_post_data)

##############################################################################
##############################################################################
###########id118
##############################################################################
##############################################################################
### plant id118 pre_heatwave
aci_data_id118_pre = subset(aci_data, id == ids[13] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id118_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id118_pre)
#### fit aci curve
fit_aci_id118_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id118_pre)
summary(fit_aci_id118_pre)
#### plot
plot(fit_aci_id118_pre)
#### add to dataframe
aci_data_id118_pre_data <- cbind(aci_data_id118_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id118_pre[,30]),
                                 mean(aci_data_id118_pre[,118]),
                                 fit_aci_id118_pre[[2]][1,1],
                                 fit_aci_id118_pre[[2]][1,2],
                                 fit_aci_id118_pre[[2]][2,1],
                                 fit_aci_id118_pre[[2]][2,2],
                                 fit_aci_id118_pre[[2]][3,1],
                                 fit_aci_id118_pre[[2]][3,2],
                                 # fit_aci_id118_pre[[2]][4,1],
                                 # fit_aci_id118_pre[[2]][4,2],
                                 fit_aci_id118_pre$RMSE,
                                 fit_aci_id118_pre$Ci_transition,
                                 fit_aci_id118_pre$citransition,
                                 fit_aci_id118_pre$Km,
                                 fit_aci_id118_pre$GammaStar,
                                 fit_aci_id118_pre$fitmethod,
                                 fit_aci_id118_pre$Tcorrect,
                                 fit_aci_id118_pre$fitTPU)
colnames(aci_data_id118_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id118_pre_data)

### plant id118 post_heatwave
aci_data_id118_post = subset(aci_data, id == ids[12] & meas.type == 'post_heatwave' & Ci < 600)
aci_data_id118_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id118_post)
#### fit aci curve
fit_aci_id118_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id118_post)
summary(fit_aci_id118_post)
#### plot
plot(fit_aci_id118_post)
#### add to dataframe
aci_data_id118_post_data <- cbind(aci_data_id118_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id118_post[,30]),
                                  mean(aci_data_id118_post[,118]),
                                  fit_aci_id118_post[[2]][1,1],
                                  fit_aci_id118_post[[2]][1,2],
                                  fit_aci_id118_post[[2]][2,1],
                                  fit_aci_id118_post[[2]][2,2],
                                  fit_aci_id118_post[[2]][3,1],
                                  fit_aci_id118_post[[2]][3,2],
                                  # fit_aci_id118_post[[2]][4,1],
                                  # fit_aci_id118_post[[2]][4,2],
                                  fit_aci_id118_post$RMSE,
                                  fit_aci_id118_post$Ci_transition,
                                  fit_aci_id118_post$citransition,
                                  fit_aci_id118_post$Km,
                                  fit_aci_id118_post$GammaStar,
                                  fit_aci_id118_post$fitmethod,
                                  fit_aci_id118_post$Tcorrect,
                                  fit_aci_id118_post$fitTPU)
colnames(aci_data_id118_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id118_post_data)

##############################################################################
##############################################################################
###########id121
##############################################################################
##############################################################################
### plant id121 pre_heatwave
# aci_data_id121_pre = subset(aci_data, id == ids[14] & meas.type == 'pre_heatwave')
# aci_data_id121_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id121_pre)
# #### fit aci curve
# fit_aci_id121_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            citransition = 350,
#                            Tcorrect = FALSE,
#                            data = aci_data_id121_pre)
# summary(fit_aci_id121_pre)
# #### plot
# plot(fit_aci_id121_pre)
# #### add to dataframe
# aci_data_id121_pre_data <- cbind(aci_data_id121_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id121_pre[,30]),
#                                  mean(aci_data_id121_pre[,118]),
#                                  fit_aci_id121_pre[[2]][1,1],
#                                  fit_aci_id121_pre[[2]][1,2],
#                                  fit_aci_id121_pre[[2]][2,1],
#                                  fit_aci_id121_pre[[2]][2,2],
#                                  fit_aci_id121_pre[[2]][3,1],
#                                  fit_aci_id121_pre[[2]][3,2],
#                                  fit_aci_id121_pre$RMSE,
#                                  fit_aci_id121_pre$Ci_transition,
#                                  fit_aci_id121_pre$citransition,
#                                  fit_aci_id121_pre$Km,
#                                  fit_aci_id121_pre$GammaStar,
#                                  fit_aci_id121_pre$fitmethod,
#                                  fit_aci_id121_pre$Tcorrect,
#                                  fit_aci_id121_pre$fitTPU)
# colnames(aci_data_id121_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id121_pre_data)
# 
# ### plant id121 post_heatwave
# aci_data_id121_post = subset(aci_data, id == ids[14] & meas.type == 'post_heatwave')
# aci_data_id121_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id121_post)
# #### fit aci curve
# fit_aci_id121_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             citransition = 350,
#                             Tcorrect = FALSE,
#                             data = aci_data_id121_post)
# summary(fit_aci_id121_post)
# #### plot
# plot(fit_aci_id121_post)
# #### add to dataframe
# aci_data_id121_post_data <- cbind(aci_data_id121_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id121_post[,30]),
#                                   mean(aci_data_id121_post[,118]),
#                                   fit_aci_id121_post[[2]][1,1],
#                                   fit_aci_id121_post[[2]][1,2],
#                                   fit_aci_id121_post[[2]][2,1],
#                                   fit_aci_id121_post[[2]][2,2],
#                                   fit_aci_id121_post[[2]][3,1],
#                                   fit_aci_id121_post[[2]][3,2],
#                                   fit_aci_id121_post$RMSE,
#                                   fit_aci_id121_post$Ci_transition,
#                                   fit_aci_id121_post$citransition,
#                                   fit_aci_id121_post$Km,
#                                   fit_aci_id121_post$GammaStar,
#                                   fit_aci_id121_post$fitmethod,
#                                   fit_aci_id121_post$Tcorrect,
#                                   fit_aci_id121_post$fitTPU)
# colnames(aci_data_id121_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id121_post_data)

##############################################################################
##############################################################################
###########id122
##############################################################################
##############################################################################
### plant id122 pre_heatwave
# aci_data_id122_pre = subset(aci_data, id == ids[15] & meas.type == 'pre_heatwave' & Ci < 600)
# aci_data_id122_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id122_pre)
# #### fit aci curve
# fit_aci_id122_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id122_pre)
# summary(fit_aci_id122_pre)
# #### plot
# plot(fit_aci_id122_pre)
# #### add to dataframe
# aci_data_id122_pre_data <- cbind(aci_data_id122_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id122_pre[,30]),
#                                  mean(aci_data_id122_pre[,118]),
#                                  fit_aci_id122_pre[[2]][1,1],
#                                  fit_aci_id122_pre[[2]][1,2],
#                                  fit_aci_id122_pre[[2]][2,1],
#                                  fit_aci_id122_pre[[2]][2,2],
#                                  fit_aci_id122_pre[[2]][3,1],
#                                  fit_aci_id122_pre[[2]][3,2],
#                                  # fit_aci_id122_pre[[2]][4,1],
#                                  # fit_aci_id122_pre[[2]][4,2],
#                                  fit_aci_id122_pre$RMSE,
#                                  fit_aci_id122_pre$Ci_transition,
#                                  fit_aci_id122_pre$citransition,
#                                  fit_aci_id122_pre$Km,
#                                  fit_aci_id122_pre$GammaStar,
#                                  fit_aci_id122_pre$fitmethod,
#                                  fit_aci_id122_pre$Tcorrect,
#                                  fit_aci_id122_pre$fitTPU)
# colnames(aci_data_id122_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id122_pre_data)
# 
# ### plant id122 post_heatwave
# aci_data_id122_post = subset(aci_data, id == ids[15] & meas.type == 'post_heatwave')
# aci_data_id122_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id122_post)
# #### fit aci curve
# fit_aci_id122_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id122_post)
# summary(fit_aci_id122_post)
# #### plot
# plot(fit_aci_id122_post)
# #### add to dataframe
# aci_data_id122_post_data <- cbind(aci_data_id122_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id122_post[,30]),
#                                   mean(aci_data_id122_post[,118]),
#                                   fit_aci_id122_post[[2]][1,1],
#                                   fit_aci_id122_post[[2]][1,2],
#                                   fit_aci_id122_post[[2]][2,1],
#                                   fit_aci_id122_post[[2]][2,2],
#                                   fit_aci_id122_post[[2]][3,1],
#                                   fit_aci_id122_post[[2]][3,2],
#                                   fit_aci_id122_post[[2]][4,1],
#                                   fit_aci_id122_post[[2]][4,2],
#                                   fit_aci_id122_post$RMSE,
#                                   fit_aci_id122_post$Ci_transition,
#                                   fit_aci_id122_post$citransition,
#                                   fit_aci_id122_post$Km,
#                                   fit_aci_id122_post$GammaStar,
#                                   fit_aci_id122_post$fitmethod,
#                                   fit_aci_id122_post$Tcorrect,
#                                   fit_aci_id122_post$fitTPU)
# colnames(aci_data_id122_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id122_post_data)

##############################################################################
##############################################################################
###########id122
##############################################################################
##############################################################################
### plant id122 pre_heatwave
# aci_data_id122_pre = subset(aci_data, id == ids[15] & meas.type == 'pre_heatwave' & Ci < 600)
# aci_data_id122_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id122_pre)
# #### fit aci curve
# fit_aci_id122_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id122_pre)
# summary(fit_aci_id122_pre)
# #### plot
# plot(fit_aci_id122_pre)
# #### add to dataframe
# aci_data_id122_pre_data <- cbind(aci_data_id122_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id122_pre[,30]),
#                                  mean(aci_data_id122_pre[,118]),
#                                  fit_aci_id122_pre[[2]][1,1],
#                                  fit_aci_id122_pre[[2]][1,2],
#                                  fit_aci_id122_pre[[2]][2,1],
#                                  fit_aci_id122_pre[[2]][2,2],
#                                  fit_aci_id122_pre[[2]][3,1],
#                                  fit_aci_id122_pre[[2]][3,2],
#                                  # fit_aci_id122_pre[[2]][4,1],
#                                  # fit_aci_id122_pre[[2]][4,2],
#                                  fit_aci_id122_pre$RMSE,
#                                  fit_aci_id122_pre$Ci_transition,
#                                  fit_aci_id122_pre$citransition,
#                                  fit_aci_id122_pre$Km,
#                                  fit_aci_id122_pre$GammaStar,
#                                  fit_aci_id122_pre$fitmethod,
#                                  fit_aci_id122_pre$Tcorrect,
#                                  fit_aci_id122_pre$fitTPU)
# colnames(aci_data_id122_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id122_pre_data)
# 
# ### plant id122 post_heatwave
# aci_data_id122_post = subset(aci_data, id == ids[15] & meas.type == 'post_heatwave')
# aci_data_id122_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id122_post)
# #### fit aci curve
# fit_aci_id122_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id122_post)
# summary(fit_aci_id122_post)
# #### plot
# plot(fit_aci_id122_post)
# #### add to dataframe
# aci_data_id122_post_data <- cbind(aci_data_id122_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id122_post[,30]),
#                                   mean(aci_data_id122_post[,118]),
#                                   fit_aci_id122_post[[2]][1,1],
#                                   fit_aci_id122_post[[2]][1,2],
#                                   fit_aci_id122_post[[2]][2,1],
#                                   fit_aci_id122_post[[2]][2,2],
#                                   fit_aci_id122_post[[2]][3,1],
#                                   fit_aci_id122_post[[2]][3,2],
#                                   fit_aci_id122_post[[2]][4,1],
#                                   fit_aci_id122_post[[2]][4,2],
#                                   fit_aci_id122_post$RMSE,
#                                   fit_aci_id122_post$Ci_transition,
#                                   fit_aci_id122_post$citransition,
#                                   fit_aci_id122_post$Km,
#                                   fit_aci_id122_post$GammaStar,
#                                   fit_aci_id122_post$fitmethod,
#                                   fit_aci_id122_post$Tcorrect,
#                                   fit_aci_id122_post$fitTPU)
# colnames(aci_data_id122_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id122_post_data)



##############################################################################
##############################################################################
###########id129
##############################################################################
##############################################################################
### plant id129 pre_heatwave
# aci_data_id129_pre = subset(aci_data, id == ids[16] & meas.type == 'pre_heatwave' & Ci < 600)
# aci_data_id129_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id129_pre)
# #### fit aci curve
# fit_aci_id129_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id129_pre)
# summary(fit_aci_id129_pre)
# #### plot
# plot(fit_aci_id129_pre)
# #### add to dataframe
# aci_data_id129_pre_data <- cbind(aci_data_id129_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id129_pre[,30]),
#                                  mean(aci_data_id129_pre[,118]),
#                                  fit_aci_id129_pre[[2]][1,1],
#                                  fit_aci_id129_pre[[2]][1,2],
#                                  fit_aci_id129_pre[[2]][2,1],
#                                  fit_aci_id129_pre[[2]][2,2],
#                                  fit_aci_id129_pre[[2]][3,1],
#                                  fit_aci_id129_pre[[2]][3,2],
#                                  fit_aci_id129_pre[[2]][4,1],
#                                  fit_aci_id129_pre[[2]][4,2],
#                                  fit_aci_id129_pre$RMSE,
#                                  fit_aci_id129_pre$Ci_transition,
#                                  fit_aci_id129_pre$citransition,
#                                  fit_aci_id129_pre$Km,
#                                  fit_aci_id129_pre$GammaStar,
#                                  fit_aci_id129_pre$fitmethod,
#                                  fit_aci_id129_pre$Tcorrect,
#                                  fit_aci_id129_pre$fitTPU)
# colnames(aci_data_id129_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id129_pre_data)
# 
# ### plant id129 post_heatwave
# aci_data_id129_post = subset(aci_data, id == ids[16] & meas.type == 'post_heatwave')
# aci_data_id129_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id129_post)
# #### fit aci curve
# fit_aci_id129_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id129_post)
# summary(fit_aci_id129_post)
# #### plot
# plot(fit_aci_id129_post)
# #### add to dataframe
# aci_data_id129_post_data <- cbind(aci_data_id129_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id129_post[,30]),
#                                   mean(aci_data_id129_post[,118]),
#                                   fit_aci_id129_post[[2]][1,1],
#                                   fit_aci_id129_post[[2]][1,2],
#                                   fit_aci_id129_post[[2]][2,1],
#                                   fit_aci_id129_post[[2]][2,2],
#                                   fit_aci_id129_post[[2]][3,1],
#                                   fit_aci_id129_post[[2]][3,2],
#                                   fit_aci_id129_post[[2]][4,1],
#                                   fit_aci_id129_post[[2]][4,2],
#                                   fit_aci_id129_post$RMSE,
#                                   fit_aci_id129_post$Ci_transition,
#                                   fit_aci_id129_post$citransition,
#                                   fit_aci_id129_post$Km,
#                                   fit_aci_id129_post$GammaStar,
#                                   fit_aci_id129_post$fitmethod,
#                                   fit_aci_id129_post$Tcorrect,
#                                   fit_aci_id129_post$fitTPU)
# colnames(aci_data_id129_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id129_post_data)

##############################################################################
##############################################################################
###########id13
##############################################################################
##############################################################################
### plant id13 pre_heatwave
# aci_data_id13_pre = subset(aci_data, id == ids[17] & meas.type == 'pre_heatwave')
# aci_data_id13_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id13_pre)
# #### fit aci curve
# fit_aci_id13_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id13_pre)
# summary(fit_aci_id13_pre)
# #### plot
# plot(fit_aci_id13_pre)
# #### add to dataframe
# aci_data_id13_pre_data <- cbind(aci_data_id13_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id13_pre[,30]),
#                                  mean(aci_data_id13_pre[,118]),
#                                  fit_aci_id13_pre[[2]][1,1],
#                                  fit_aci_id13_pre[[2]][1,2],
#                                  fit_aci_id13_pre[[2]][2,1],
#                                  fit_aci_id13_pre[[2]][2,2],
#                                  fit_aci_id13_pre[[2]][3,1],
#                                  fit_aci_id13_pre[[2]][3,2],
#                                  fit_aci_id13_pre[[2]][4,1],
#                                  fit_aci_id13_pre[[2]][4,2],
#                                  fit_aci_id13_pre$RMSE,
#                                  fit_aci_id13_pre$Ci_transition,
#                                  fit_aci_id13_pre$citransition,
#                                  fit_aci_id13_pre$Km,
#                                  fit_aci_id13_pre$GammaStar,
#                                  fit_aci_id13_pre$fitmethod,
#                                  fit_aci_id13_pre$Tcorrect,
#                                  fit_aci_id13_pre$fitTPU)
# colnames(aci_data_id13_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id13_pre_data)
# 
# ### plant id13 post_heatwave
# aci_data_id13_post = subset(aci_data, id == ids[17] & meas.type == 'post_heatwave')
# aci_data_id13_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id13_post)
# #### fit aci curve
# fit_aci_id13_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id13_post)
# summary(fit_aci_id13_post)
# #### plot
# plot(fit_aci_id13_post)
# #### add to dataframe
# aci_data_id13_post_data <- cbind(aci_data_id13_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id13_post[,30]),
#                                   mean(aci_data_id13_post[,118]),
#                                   fit_aci_id13_post[[2]][1,1],
#                                   fit_aci_id13_post[[2]][1,2],
#                                   fit_aci_id13_post[[2]][2,1],
#                                   fit_aci_id13_post[[2]][2,2],
#                                   fit_aci_id13_post[[2]][3,1],
#                                   fit_aci_id13_post[[2]][3,2],
#                                   fit_aci_id13_post[[2]][4,1],
#                                   fit_aci_id13_post[[2]][4,2],
#                                   fit_aci_id13_post$RMSE,
#                                   fit_aci_id13_post$Ci_transition,
#                                   fit_aci_id13_post$citransition,
#                                   fit_aci_id13_post$Km,
#                                   fit_aci_id13_post$GammaStar,
#                                   fit_aci_id13_post$fitmethod,
#                                   fit_aci_id13_post$Tcorrect,
#                                   fit_aci_id13_post$fitTPU)
# colnames(aci_data_id13_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id13_post_data)

##############################################################################
##############################################################################
###########id131
##############################################################################
##############################################################################
### plant id131 pre_heatwave
aci_data_id131_pre = subset(aci_data, id == ids[18] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id131_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id131_pre)
#### fit aci curve
fit_aci_id131_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id131_pre)
summary(fit_aci_id131_pre)
#### plot
plot(fit_aci_id131_pre)
#### add to dataframe
aci_data_id131_pre_data <- cbind(aci_data_id131_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id131_pre[,30]),
                                 mean(aci_data_id131_pre[,118]),
                                 fit_aci_id131_pre[[2]][1,1],
                                 fit_aci_id131_pre[[2]][1,2],
                                 fit_aci_id131_pre[[2]][2,1],
                                 fit_aci_id131_pre[[2]][2,2],
                                 fit_aci_id131_pre[[2]][3,1],
                                 fit_aci_id131_pre[[2]][3,2],
                                 # fit_aci_id131_pre[[2]][4,1],
                                 # fit_aci_id131_pre[[2]][4,2],
                                 fit_aci_id131_pre$RMSE,
                                 fit_aci_id131_pre$Ci_transition,
                                 fit_aci_id131_pre$citransition,
                                 fit_aci_id131_pre$Km,
                                 fit_aci_id131_pre$GammaStar,
                                 fit_aci_id131_pre$fitmethod,
                                 fit_aci_id131_pre$Tcorrect,
                                 fit_aci_id131_pre$fitTPU)
colnames(aci_data_id131_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id131_pre_data)

### plant id131 post_heatwave
aci_data_id131_post = subset(aci_data, id == ids[18] & meas.type == 'post_heatwave')
aci_data_id131_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id131_post)
#### fit aci curve
fit_aci_id131_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id131_post)
summary(fit_aci_id131_post)
#### plot
plot(fit_aci_id131_post)
#### add to dataframe
aci_data_id131_post_data <- cbind(aci_data_id131_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id131_post[,30]),
                                  mean(aci_data_id131_post[,118]),
                                  fit_aci_id131_post[[2]][1,1],
                                  fit_aci_id131_post[[2]][1,2],
                                  fit_aci_id131_post[[2]][2,1],
                                  fit_aci_id131_post[[2]][2,2],
                                  fit_aci_id131_post[[2]][3,1],
                                  fit_aci_id131_post[[2]][3,2],
                                  # fit_aci_id131_post[[2]][4,1],
                                  # fit_aci_id131_post[[2]][4,2],
                                  fit_aci_id131_post$RMSE,
                                  fit_aci_id131_post$Ci_transition,
                                  fit_aci_id131_post$citransition,
                                  fit_aci_id131_post$Km,
                                  fit_aci_id131_post$GammaStar,
                                  fit_aci_id131_post$fitmethod,
                                  fit_aci_id131_post$Tcorrect,
                                  fit_aci_id131_post$fitTPU)
colnames(aci_data_id131_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id131_post_data)

##############################################################################
##############################################################################
###########id133
##############################################################################
##############################################################################
### plant id133 pre_heatwave
aci_data_id133_pre = subset(aci_data, id == ids[19] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id133_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id133_pre)
#### fit aci curve
fit_aci_id133_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id133_pre)
summary(fit_aci_id133_pre)
#### plot
plot(fit_aci_id133_pre)
#### add to dataframe
aci_data_id133_pre_data <- cbind(aci_data_id133_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id133_pre[,30]),
                                 mean(aci_data_id133_pre[,118]),
                                 fit_aci_id133_pre[[2]][1,1],
                                 fit_aci_id133_pre[[2]][1,2],
                                 fit_aci_id133_pre[[2]][2,1],
                                 fit_aci_id133_pre[[2]][2,2],
                                 fit_aci_id133_pre[[2]][3,1],
                                 fit_aci_id133_pre[[2]][3,2],
                                 # fit_aci_id133_pre[[2]][4,1],
                                 # fit_aci_id133_pre[[2]][4,2],
                                 fit_aci_id133_pre$RMSE,
                                 fit_aci_id133_pre$Ci_transition,
                                 fit_aci_id133_pre$citransition,
                                 fit_aci_id133_pre$Km,
                                 fit_aci_id133_pre$GammaStar,
                                 fit_aci_id133_pre$fitmethod,
                                 fit_aci_id133_pre$Tcorrect,
                                 fit_aci_id133_pre$fitTPU)
colnames(aci_data_id133_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id133_pre_data)

### plant id133 post_heatwave
aci_data_id133_post = subset(aci_data, id == ids[19] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id133_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id133_post)
#### fit aci curve
fit_aci_id133_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id133_post)
summary(fit_aci_id133_post)
#### plot
plot(fit_aci_id133_post)
#### add to dataframe
aci_data_id133_post_data <- cbind(aci_data_id133_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id133_post[,30]),
                                  mean(aci_data_id133_post[,118]),
                                  fit_aci_id133_post[[2]][1,1],
                                  fit_aci_id133_post[[2]][1,2],
                                  fit_aci_id133_post[[2]][2,1],
                                  fit_aci_id133_post[[2]][2,2],
                                  fit_aci_id133_post[[2]][3,1],
                                  fit_aci_id133_post[[2]][3,2],
                                  # fit_aci_id133_post[[2]][4,1],
                                  # fit_aci_id133_post[[2]][4,2],
                                  fit_aci_id133_post$RMSE,
                                  fit_aci_id133_post$Ci_transition,
                                  fit_aci_id133_post$citransition,
                                  fit_aci_id133_post$Km,
                                  fit_aci_id133_post$GammaStar,
                                  fit_aci_id133_post$fitmethod,
                                  fit_aci_id133_post$Tcorrect,
                                  fit_aci_id133_post$fitTPU)
colnames(aci_data_id133_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id133_post_data)

##############################################################################
##############################################################################
###########id136
##############################################################################
##############################################################################
### plant id136 pre_heatwave
# aci_data_id136_pre = subset(aci_data, id == ids[20] & meas.type == 'pre_heatwave')
# aci_data_id136_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id136_pre)
# #### fit aci curve
# fit_aci_id136_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id136_pre)
# summary(fit_aci_id136_pre)
# #### plot
# plot(fit_aci_id136_pre)
# #### add to dataframe
# aci_data_id136_pre_data <- cbind(aci_data_id136_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id136_pre[,30]),
#                                  mean(aci_data_id136_pre[,118]),
#                                  fit_aci_id136_pre[[2]][1,1],
#                                  fit_aci_id136_pre[[2]][1,2],
#                                  fit_aci_id136_pre[[2]][2,1],
#                                  fit_aci_id136_pre[[2]][2,2],
#                                  fit_aci_id136_pre[[2]][3,1],
#                                  fit_aci_id136_pre[[2]][3,2],
#                                  fit_aci_id136_pre[[2]][4,1],
#                                  fit_aci_id136_pre[[2]][4,2],
#                                  fit_aci_id136_pre$RMSE,
#                                  fit_aci_id136_pre$Ci_transition,
#                                  fit_aci_id136_pre$citransition,
#                                  fit_aci_id136_pre$Km,
#                                  fit_aci_id136_pre$GammaStar,
#                                  fit_aci_id136_pre$fitmethod,
#                                  fit_aci_id136_pre$Tcorrect,
#                                  fit_aci_id136_pre$fitTPU)
# colnames(aci_data_id136_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id136_pre_data)
# 
# ### plant id136 post_heatwave
# aci_data_id136_post = subset(aci_data, id == ids[20] & meas.type == 'post_heatwave')
# aci_data_id136_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id136_post)
# #### fit aci curve
# fit_aci_id136_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id136_post)
# summary(fit_aci_id136_post)
# #### plot
# plot(fit_aci_id136_post)
# #### add to dataframe
# aci_data_id136_post_data <- cbind(aci_data_id136_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id136_post[,30]),
#                                   mean(aci_data_id136_post[,118]),
#                                   fit_aci_id136_post[[2]][1,1],
#                                   fit_aci_id136_post[[2]][1,2],
#                                   fit_aci_id136_post[[2]][2,1],
#                                   fit_aci_id136_post[[2]][2,2],
#                                   fit_aci_id136_post[[2]][3,1],
#                                   fit_aci_id136_post[[2]][3,2],
#                                   fit_aci_id136_post[[2]][4,1],
#                                   fit_aci_id136_post[[2]][4,2],
#                                   fit_aci_id136_post$RMSE,
#                                   fit_aci_id136_post$Ci_transition,
#                                   fit_aci_id136_post$citransition,
#                                   fit_aci_id136_post$Km,
#                                   fit_aci_id136_post$GammaStar,
#                                   fit_aci_id136_post$fitmethod,
#                                   fit_aci_id136_post$Tcorrect,
#                                   fit_aci_id136_post$fitTPU)
# colnames(aci_data_id136_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id136_post_data)

##############################################################################
##############################################################################
###########id14
##############################################################################
##############################################################################
### plant id14 pre_heatwave
aci_data_id14_pre = subset(aci_data, id == ids[21] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id14_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id14_pre)
#### fit aci curve
fit_aci_id14_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id14_pre)
summary(fit_aci_id14_pre)
#### plot
plot(fit_aci_id14_pre)
#### add to dataframe
aci_data_id14_pre_data <- cbind(aci_data_id14_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id14_pre[,30]),
                                 mean(aci_data_id14_pre[,118]),
                                 fit_aci_id14_pre[[2]][1,1],
                                 fit_aci_id14_pre[[2]][1,2],
                                 fit_aci_id14_pre[[2]][2,1],
                                 fit_aci_id14_pre[[2]][2,2],
                                 fit_aci_id14_pre[[2]][3,1],
                                 fit_aci_id14_pre[[2]][3,2],
                                 # fit_aci_id14_pre[[2]][4,1],
                                 # fit_aci_id14_pre[[2]][4,2],
                                 fit_aci_id14_pre$RMSE,
                                 fit_aci_id14_pre$Ci_transition,
                                 fit_aci_id14_pre$citransition,
                                 fit_aci_id14_pre$Km,
                                 fit_aci_id14_pre$GammaStar,
                                 fit_aci_id14_pre$fitmethod,
                                 fit_aci_id14_pre$Tcorrect,
                                 fit_aci_id14_pre$fitTPU)
colnames(aci_data_id14_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id14_pre_data)

### plant id14 post_heatwave
aci_data_id14_post = subset(aci_data, id == ids[21] & meas.type == 'post_heatwave')
aci_data_id14_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id14_post)
#### fit aci curve
fit_aci_id14_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id14_post)
summary(fit_aci_id14_post)
#### plot
plot(fit_aci_id14_post)
#### add to dataframe
aci_data_id14_post_data <- cbind(aci_data_id14_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id14_post[,30]),
                                  mean(aci_data_id14_post[,118]),
                                  fit_aci_id14_post[[2]][1,1],
                                  fit_aci_id14_post[[2]][1,2],
                                  fit_aci_id14_post[[2]][2,1],
                                  fit_aci_id14_post[[2]][2,2],
                                  fit_aci_id14_post[[2]][3,1],
                                  fit_aci_id14_post[[2]][3,2],
                                  # fit_aci_id14_post[[2]][4,1],
                                  # fit_aci_id14_post[[2]][4,2],
                                  fit_aci_id14_post$RMSE,
                                  fit_aci_id14_post$Ci_transition,
                                  fit_aci_id14_post$citransition,
                                  fit_aci_id14_post$Km,
                                  fit_aci_id14_post$GammaStar,
                                  fit_aci_id14_post$fitmethod,
                                  fit_aci_id14_post$Tcorrect,
                                  fit_aci_id14_post$fitTPU)
colnames(aci_data_id14_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id14_post_data)

##############################################################################
##############################################################################
###########id140
##############################################################################
##############################################################################
### plant id140 pre_heatwave
aci_data_id140_pre = subset(aci_data, id == ids[22] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id140_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id140_pre)
#### fit aci curve
fit_aci_id140_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                          # citransition = 300,
                          Tcorrect = FALSE,
                          fitTPU = FALSE,
                          fitmethod = 'bilinear',
                          data = aci_data_id140_pre)
summary(fit_aci_id140_pre)
#### plot
plot(fit_aci_id140_pre)
#### add to dataframe
aci_data_id140_pre_data <- cbind(aci_data_id140_pre[1, c(10, 284, 9, 14, 16, 19)],
                                mean(aci_data_id140_pre[,30]),
                                mean(aci_data_id140_pre[,118]),
                                fit_aci_id140_pre[[2]][1,1],
                                fit_aci_id140_pre[[2]][1,2],
                                fit_aci_id140_pre[[2]][2,1],
                                fit_aci_id140_pre[[2]][2,2],
                                fit_aci_id140_pre[[2]][3,1],
                                fit_aci_id140_pre[[2]][3,2],
                                # fit_aci_id140_pre[[2]][4,1],
                                # fit_aci_id140_pre[[2]][4,2],
                                fit_aci_id140_pre$RMSE,
                                fit_aci_id140_pre$Ci_transition,
                                fit_aci_id140_pre$citransition,
                                fit_aci_id140_pre$Km,
                                fit_aci_id140_pre$GammaStar,
                                fit_aci_id140_pre$fitmethod,
                                fit_aci_id140_pre$Tcorrect,
                                fit_aci_id140_pre$fitTPU)
colnames(aci_data_id140_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                      'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                      'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                      'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                      'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                      'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id140_pre_data)

### plant id140 post_heatwave
aci_data_id140_post = subset(aci_data, id == ids[22] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id140_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id140_post)
#### fit aci curve
fit_aci_id140_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id140_post)
summary(fit_aci_id140_post)
#### plot
plot(fit_aci_id140_post)
#### add to dataframe
aci_data_id140_post_data <- cbind(aci_data_id140_post[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id140_post[,30]),
                                 mean(aci_data_id140_post[,118]),
                                 fit_aci_id140_post[[2]][1,1],
                                 fit_aci_id140_post[[2]][1,2],
                                 fit_aci_id140_post[[2]][2,1],
                                 fit_aci_id140_post[[2]][2,2],
                                 fit_aci_id140_post[[2]][3,1],
                                 fit_aci_id140_post[[2]][3,2],
                                 # fit_aci_id140_post[[2]][4,1],
                                 # fit_aci_id140_post[[2]][4,2],
                                 fit_aci_id140_post$RMSE,
                                 fit_aci_id140_post$Ci_transition,
                                 fit_aci_id140_post$citransition,
                                 fit_aci_id140_post$Km,
                                 fit_aci_id140_post$GammaStar,
                                 fit_aci_id140_post$fitmethod,
                                 fit_aci_id140_post$Tcorrect,
                                 fit_aci_id140_post$fitTPU)
colnames(aci_data_id140_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id140_post_data)

##############################################################################
##############################################################################
###########id142
##############################################################################
##############################################################################
### plant id142 pre_heatwave
# aci_data_id142_pre = subset(aci_data, id == ids[23] & meas.type == 'pre_heatwave' & Ci < 700)
# aci_data_id142_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id142_pre)
# #### fit aci curve
# fit_aci_id142_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id142_pre)
# summary(fit_aci_id142_pre)
# #### plot
# plot(fit_aci_id142_pre)
# #### add to dataframe
# aci_data_id142_pre_data <- cbind(aci_data_id142_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id142_pre[,30]),
#                                  mean(aci_data_id142_pre[,118]),
#                                  fit_aci_id142_pre[[2]][1,1],
#                                  fit_aci_id142_pre[[2]][1,2],
#                                  fit_aci_id142_pre[[2]][2,1],
#                                  fit_aci_id142_pre[[2]][2,2],
#                                  fit_aci_id142_pre[[2]][3,1],
#                                  fit_aci_id142_pre[[2]][3,2],
#                                  # fit_aci_id142_pre[[2]][4,1],
#                                  # fit_aci_id142_pre[[2]][4,2],
#                                  fit_aci_id142_pre$RMSE,
#                                  fit_aci_id142_pre$Ci_transition,
#                                  fit_aci_id142_pre$citransition,
#                                  fit_aci_id142_pre$Km,
#                                  fit_aci_id142_pre$GammaStar,
#                                  fit_aci_id142_pre$fitmethod,
#                                  fit_aci_id142_pre$Tcorrect,
#                                  fit_aci_id142_pre$fitTPU)
# colnames(aci_data_id142_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id142_pre_data)
# 
# ### plant id142 post_heatwave
# aci_data_id142_post = subset(aci_data, id == ids[23] & meas.type == 'post_heatwave')
# aci_data_id142_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id142_post)
# #### fit aci curve
# fit_aci_id142_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id142_post)
# summary(fit_aci_id142_post)
# #### plot
# plot(fit_aci_id142_post)
# #### add to dataframe
# aci_data_id142_post_data <- cbind(aci_data_id142_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id142_post[,30]),
#                                   mean(aci_data_id142_post[,118]),
#                                   fit_aci_id142_post[[2]][1,1],
#                                   fit_aci_id142_post[[2]][1,2],
#                                   fit_aci_id142_post[[2]][2,1],
#                                   fit_aci_id142_post[[2]][2,2],
#                                   fit_aci_id142_post[[2]][3,1],
#                                   fit_aci_id142_post[[2]][3,2],
#                                   # fit_aci_id142_post[[2]][4,1],
#                                   # fit_aci_id142_post[[2]][4,2],
#                                   fit_aci_id142_post$RMSE,
#                                   fit_aci_id142_post$Ci_transition,
#                                   fit_aci_id142_post$citransition,
#                                   fit_aci_id142_post$Km,
#                                   fit_aci_id142_post$GammaStar,
#                                   fit_aci_id142_post$fitmethod,
#                                   fit_aci_id142_post$Tcorrect,
#                                   fit_aci_id142_post$fitTPU)
# colnames(aci_data_id142_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id142_post_data)

##############################################################################
##############################################################################
###########id142
##############################################################################
##############################################################################
### plant id142 pre_heatwave
# aci_data_id142_pre = subset(aci_data, id == ids[24] & meas.type == 'pre_heatwave' & Ci < 700)
# aci_data_id142_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id142_pre)
# #### fit aci curve
# fit_aci_id142_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id142_pre)
# summary(fit_aci_id142_pre)
# #### plot
# plot(fit_aci_id142_pre)
# #### add to dataframe
# aci_data_id142_pre_data <- cbind(aci_data_id142_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id142_pre[,30]),
#                                  mean(aci_data_id142_pre[,118]),
#                                  fit_aci_id142_pre[[2]][1,1],
#                                  fit_aci_id142_pre[[2]][1,2],
#                                  fit_aci_id142_pre[[2]][2,1],
#                                  fit_aci_id142_pre[[2]][2,2],
#                                  fit_aci_id142_pre[[2]][3,1],
#                                  fit_aci_id142_pre[[2]][3,2],
#                                  # fit_aci_id142_pre[[2]][4,1],
#                                  # fit_aci_id142_pre[[2]][4,2],
#                                  fit_aci_id142_pre$RMSE,
#                                  fit_aci_id142_pre$Ci_transition,
#                                  fit_aci_id142_pre$citransition,
#                                  fit_aci_id142_pre$Km,
#                                  fit_aci_id142_pre$GammaStar,
#                                  fit_aci_id142_pre$fitmethod,
#                                  fit_aci_id142_pre$Tcorrect,
#                                  fit_aci_id142_pre$fitTPU)
# colnames(aci_data_id142_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id142_pre_data)
# 
# ### plant id142 post_heatwave
# aci_data_id142_post = subset(aci_data, id == ids[24] & meas.type == 'post_heatwave')
# aci_data_id142_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id142_post)
# #### fit aci curve
# fit_aci_id142_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id142_post)
# summary(fit_aci_id142_post)
# #### plot
# plot(fit_aci_id142_post)
# #### add to dataframe
# aci_data_id142_post_data <- cbind(aci_data_id142_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id142_post[,30]),
#                                   mean(aci_data_id142_post[,118]),
#                                   fit_aci_id142_post[[2]][1,1],
#                                   fit_aci_id142_post[[2]][1,2],
#                                   fit_aci_id142_post[[2]][2,1],
#                                   fit_aci_id142_post[[2]][2,2],
#                                   fit_aci_id142_post[[2]][3,1],
#                                   fit_aci_id142_post[[2]][3,2],
#                                   # fit_aci_id142_post[[2]][4,1],
#                                   # fit_aci_id142_post[[2]][4,2],
#                                   fit_aci_id142_post$RMSE,
#                                   fit_aci_id142_post$Ci_transition,
#                                   fit_aci_id142_post$citransition,
#                                   fit_aci_id142_post$Km,
#                                   fit_aci_id142_post$GammaStar,
#                                   fit_aci_id142_post$fitmethod,
#                                   fit_aci_id142_post$Tcorrect,
#                                   fit_aci_id142_post$fitTPU)
# colnames(aci_data_id142_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id142_post_data)

##############################################################################
##############################################################################
###########id142
##############################################################################
##############################################################################
### plant id142 pre_heatwave
aci_data_id142_pre = subset(aci_data, id == ids[25] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id142_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id142_pre)
#### fit aci curve
fit_aci_id142_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id142_pre)
summary(fit_aci_id142_pre)
#### plot
plot(fit_aci_id142_pre)
#### add to dataframe
aci_data_id142_pre_data <- cbind(aci_data_id142_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id142_pre[,30]),
                                 mean(aci_data_id142_pre[,118]),
                                 fit_aci_id142_pre[[2]][1,1],
                                 fit_aci_id142_pre[[2]][1,2],
                                 fit_aci_id142_pre[[2]][2,1],
                                 fit_aci_id142_pre[[2]][2,2],
                                 fit_aci_id142_pre[[2]][3,1],
                                 fit_aci_id142_pre[[2]][3,2],
                                 # fit_aci_id142_pre[[2]][4,1],
                                 # fit_aci_id142_pre[[2]][4,2],
                                 fit_aci_id142_pre$RMSE,
                                 fit_aci_id142_pre$Ci_transition,
                                 fit_aci_id142_pre$citransition,
                                 fit_aci_id142_pre$Km,
                                 fit_aci_id142_pre$GammaStar,
                                 fit_aci_id142_pre$fitmethod,
                                 fit_aci_id142_pre$Tcorrect,
                                 fit_aci_id142_pre$fitTPU)
colnames(aci_data_id142_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id142_pre_data)

### plant id142 post_heatwave
aci_data_id142_post = subset(aci_data, id == ids[25] & meas.type == 'post_heatwave' & Ci < 500)
aci_data_id142_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id142_post)
#### fit aci curve
fit_aci_id142_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id142_post)
summary(fit_aci_id142_post)
#### plot
plot(fit_aci_id142_post)
#### add to dataframe
aci_data_id142_post_data <- cbind(aci_data_id142_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id142_post[,30]),
                                  mean(aci_data_id142_post[,118]),
                                  fit_aci_id142_post[[2]][1,1],
                                  fit_aci_id142_post[[2]][1,2],
                                  fit_aci_id142_post[[2]][2,1],
                                  fit_aci_id142_post[[2]][2,2],
                                  fit_aci_id142_post[[2]][3,1],
                                  fit_aci_id142_post[[2]][3,2],
                                  # fit_aci_id142_post[[2]][4,1],
                                  # fit_aci_id142_post[[2]][4,2],
                                  fit_aci_id142_post$RMSE,
                                  fit_aci_id142_post$Ci_transition,
                                  fit_aci_id142_post$citransition,
                                  fit_aci_id142_post$Km,
                                  fit_aci_id142_post$GammaStar,
                                  fit_aci_id142_post$fitmethod,
                                  fit_aci_id142_post$Tcorrect,
                                  fit_aci_id142_post$fitTPU)
colnames(aci_data_id142_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id142_post_data)

##############################################################################
##############################################################################
###########id145
##############################################################################
##############################################################################
### plant id145 pre_heatwave
# aci_data_id145_pre = subset(aci_data, id == ids[26] & meas.type == 'pre_heatwave' & Ci < 700)
# aci_data_id145_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id145_pre)
# #### fit aci curve
# fit_aci_id145_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id145_pre)
# summary(fit_aci_id145_pre)
# #### plot
# plot(fit_aci_id145_pre)
# #### add to dataframe
# aci_data_id145_pre_data <- cbind(aci_data_id145_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id145_pre[,30]),
#                                  mean(aci_data_id145_pre[,118]),
#                                  fit_aci_id145_pre[[2]][1,1],
#                                  fit_aci_id145_pre[[2]][1,2],
#                                  fit_aci_id145_pre[[2]][2,1],
#                                  fit_aci_id145_pre[[2]][2,2],
#                                  fit_aci_id145_pre[[2]][3,1],
#                                  fit_aci_id145_pre[[2]][3,2],
#                                  # fit_aci_id145_pre[[2]][4,1],
#                                  # fit_aci_id145_pre[[2]][4,2],
#                                  fit_aci_id145_pre$RMSE,
#                                  fit_aci_id145_pre$Ci_transition,
#                                  fit_aci_id145_pre$citransition,
#                                  fit_aci_id145_pre$Km,
#                                  fit_aci_id145_pre$GammaStar,
#                                  fit_aci_id145_pre$fitmethod,
#                                  fit_aci_id145_pre$Tcorrect,
#                                  fit_aci_id145_pre$fitTPU)
# colnames(aci_data_id145_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id145_pre_data)
# 
# ### plant id145 post_heatwave
# aci_data_id145_post = subset(aci_data, id == ids[26] & meas.type == 'post_heatwave')
# aci_data_id145_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id145_post)
# #### fit aci curve
# fit_aci_id145_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id145_post)
# summary(fit_aci_id145_post)
# #### plot
# plot(fit_aci_id145_post)
# #### add to dataframe
# aci_data_id145_post_data <- cbind(aci_data_id145_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id145_post[,30]),
#                                   mean(aci_data_id145_post[,118]),
#                                   fit_aci_id145_post[[2]][1,1],
#                                   fit_aci_id145_post[[2]][1,2],
#                                   fit_aci_id145_post[[2]][2,1],
#                                   fit_aci_id145_post[[2]][2,2],
#                                   fit_aci_id145_post[[2]][3,1],
#                                   fit_aci_id145_post[[2]][3,2],
#                                   # fit_aci_id145_post[[2]][4,1],
#                                   # fit_aci_id145_post[[2]][4,2],
#                                   fit_aci_id145_post$RMSE,
#                                   fit_aci_id145_post$Ci_transition,
#                                   fit_aci_id145_post$citransition,
#                                   fit_aci_id145_post$Km,
#                                   fit_aci_id145_post$GammaStar,
#                                   fit_aci_id145_post$fitmethod,
#                                   fit_aci_id145_post$Tcorrect,
#                                   fit_aci_id145_post$fitTPU)
# colnames(aci_data_id145_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id145_post_data)

##############################################################################
##############################################################################
###########id146
##############################################################################
##############################################################################
### plant id146 pre_heatwave
# aci_data_id146_pre = subset(aci_data, id == ids[27] & meas.type == 'pre_heatwave')
# aci_data_id146_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id146_pre)
# #### fit aci curve
# fit_aci_id146_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id146_pre)
# summary(fit_aci_id146_pre)
# #### plot
# plot(fit_aci_id146_pre)
# #### add to dataframe
# aci_data_id146_pre_data <- cbind(aci_data_id146_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id146_pre[,30]),
#                                  mean(aci_data_id146_pre[,118]),
#                                  fit_aci_id146_pre[[2]][1,1],
#                                  fit_aci_id146_pre[[2]][1,2],
#                                  fit_aci_id146_pre[[2]][2,1],
#                                  fit_aci_id146_pre[[2]][2,2],
#                                  fit_aci_id146_pre[[2]][3,1],
#                                  fit_aci_id146_pre[[2]][3,2],
#                                  # fit_aci_id146_pre[[2]][4,1],
#                                  # fit_aci_id146_pre[[2]][4,2],
#                                  fit_aci_id146_pre$RMSE,
#                                  fit_aci_id146_pre$Ci_transition,
#                                  fit_aci_id146_pre$citransition,
#                                  fit_aci_id146_pre$Km,
#                                  fit_aci_id146_pre$GammaStar,
#                                  fit_aci_id146_pre$fitmethod,
#                                  fit_aci_id146_pre$Tcorrect,
#                                  fit_aci_id146_pre$fitTPU)
# colnames(aci_data_id146_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id146_pre_data)
# 
# ### plant id146 post_heatwave
# aci_data_id146_post = subset(aci_data, id == ids[27] & meas.type == 'post_heatwave')
# aci_data_id146_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id146_post)
# #### fit aci curve
# fit_aci_id146_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id146_post)
# summary(fit_aci_id146_post)
# #### plot
# plot(fit_aci_id146_post)
# #### add to dataframe
# aci_data_id146_post_data <- cbind(aci_data_id146_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id146_post[,30]),
#                                   mean(aci_data_id146_post[,118]),
#                                   fit_aci_id146_post[[2]][1,1],
#                                   fit_aci_id146_post[[2]][1,2],
#                                   fit_aci_id146_post[[2]][2,1],
#                                   fit_aci_id146_post[[2]][2,2],
#                                   fit_aci_id146_post[[2]][3,1],
#                                   fit_aci_id146_post[[2]][3,2],
#                                   # fit_aci_id146_post[[2]][4,1],
#                                   # fit_aci_id146_post[[2]][4,2],
#                                   fit_aci_id146_post$RMSE,
#                                   fit_aci_id146_post$Ci_transition,
#                                   fit_aci_id146_post$citransition,
#                                   fit_aci_id146_post$Km,
#                                   fit_aci_id146_post$GammaStar,
#                                   fit_aci_id146_post$fitmethod,
#                                   fit_aci_id146_post$Tcorrect,
#                                   fit_aci_id146_post$fitTPU)
# colnames(aci_data_id146_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id146_post_data)

##############################################################################
##############################################################################
###########id146
##############################################################################
##############################################################################
### plant id146 pre_heatwave
# aci_data_id146_pre = subset(aci_data, id == ids[28] & meas.type == 'pre_heatwave' & Ci < 700)
# aci_data_id146_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id146_pre)
# #### fit aci curve
# fit_aci_id146_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id146_pre)
# summary(fit_aci_id146_pre)
# #### plot
# plot(fit_aci_id146_pre)
# #### add to dataframe
# aci_data_id146_pre_data <- cbind(aci_data_id146_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id146_pre[,30]),
#                                  mean(aci_data_id146_pre[,118]),
#                                  fit_aci_id146_pre[[2]][1,1],
#                                  fit_aci_id146_pre[[2]][1,2],
#                                  fit_aci_id146_pre[[2]][2,1],
#                                  fit_aci_id146_pre[[2]][2,2],
#                                  fit_aci_id146_pre[[2]][3,1],
#                                  fit_aci_id146_pre[[2]][3,2],
#                                  # fit_aci_id146_pre[[2]][4,1],
#                                  # fit_aci_id146_pre[[2]][4,2],
#                                  fit_aci_id146_pre$RMSE,
#                                  fit_aci_id146_pre$Ci_transition,
#                                  fit_aci_id146_pre$citransition,
#                                  fit_aci_id146_pre$Km,
#                                  fit_aci_id146_pre$GammaStar,
#                                  fit_aci_id146_pre$fitmethod,
#                                  fit_aci_id146_pre$Tcorrect,
#                                  fit_aci_id146_pre$fitTPU)
# colnames(aci_data_id146_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id146_pre_data)
# 
# ### plant id146 post_heatwave
# aci_data_id146_post = subset(aci_data, id == ids[28] & meas.type == 'post_heatwave')
# aci_data_id146_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id146_post)
# #### fit aci curve
# fit_aci_id146_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id146_post)
# summary(fit_aci_id146_post)
# #### plot
# plot(fit_aci_id146_post)
# #### add to dataframe
# aci_data_id146_post_data <- cbind(aci_data_id146_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id146_post[,30]),
#                                   mean(aci_data_id146_post[,118]),
#                                   fit_aci_id146_post[[2]][1,1],
#                                   fit_aci_id146_post[[2]][1,2],
#                                   fit_aci_id146_post[[2]][2,1],
#                                   fit_aci_id146_post[[2]][2,2],
#                                   fit_aci_id146_post[[2]][3,1],
#                                   fit_aci_id146_post[[2]][3,2],
#                                   # fit_aci_id146_post[[2]][4,1],
#                                   # fit_aci_id146_post[[2]][4,2],
#                                   fit_aci_id146_post$RMSE,
#                                   fit_aci_id146_post$Ci_transition,
#                                   fit_aci_id146_post$citransition,
#                                   fit_aci_id146_post$Km,
#                                   fit_aci_id146_post$GammaStar,
#                                   fit_aci_id146_post$fitmethod,
#                                   fit_aci_id146_post$Tcorrect,
#                                   fit_aci_id146_post$fitTPU)
# colnames(aci_data_id146_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id146_post_data)

##############################################################################
##############################################################################
###########id149
##############################################################################
##############################################################################
### plant id149 pre_heatwave
# aci_data_id149_pre = subset(aci_data, id == ids[29] & meas.type == 'pre_heatwave')
# aci_data_id149_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id149_pre)
# #### fit aci curve
# fit_aci_id149_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id149_pre)
# summary(fit_aci_id149_pre)
# #### plot
# plot(fit_aci_id149_pre)
# #### add to dataframe
# aci_data_id149_pre_data <- cbind(aci_data_id149_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id149_pre[,30]),
#                                  mean(aci_data_id149_pre[,118]),
#                                  fit_aci_id149_pre[[2]][1,1],
#                                  fit_aci_id149_pre[[2]][1,2],
#                                  fit_aci_id149_pre[[2]][2,1],
#                                  fit_aci_id149_pre[[2]][2,2],
#                                  fit_aci_id149_pre[[2]][3,1],
#                                  fit_aci_id149_pre[[2]][3,2],
#                                  # fit_aci_id149_pre[[2]][4,1],
#                                  # fit_aci_id149_pre[[2]][4,2],
#                                  fit_aci_id149_pre$RMSE,
#                                  fit_aci_id149_pre$Ci_transition,
#                                  fit_aci_id149_pre$citransition,
#                                  fit_aci_id149_pre$Km,
#                                  fit_aci_id149_pre$GammaStar,
#                                  fit_aci_id149_pre$fitmethod,
#                                  fit_aci_id149_pre$Tcorrect,
#                                  fit_aci_id149_pre$fitTPU)
# colnames(aci_data_id149_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id149_pre_data)
# 
# ### plant id149 post_heatwave
# aci_data_id149_post = subset(aci_data, id == ids[29] & meas.type == 'post_heatwave')
# aci_data_id149_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id149_post)
# #### fit aci curve
# fit_aci_id149_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id149_post)
# summary(fit_aci_id149_post)
# #### plot
# plot(fit_aci_id149_post)
# #### add to dataframe
# aci_data_id149_post_data <- cbind(aci_data_id149_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id149_post[,30]),
#                                   mean(aci_data_id149_post[,118]),
#                                   fit_aci_id149_post[[2]][1,1],
#                                   fit_aci_id149_post[[2]][1,2],
#                                   fit_aci_id149_post[[2]][2,1],
#                                   fit_aci_id149_post[[2]][2,2],
#                                   fit_aci_id149_post[[2]][3,1],
#                                   fit_aci_id149_post[[2]][3,2],
#                                   # fit_aci_id149_post[[2]][4,1],
#                                   # fit_aci_id149_post[[2]][4,2],
#                                   fit_aci_id149_post$RMSE,
#                                   fit_aci_id149_post$Ci_transition,
#                                   fit_aci_id149_post$citransition,
#                                   fit_aci_id149_post$Km,
#                                   fit_aci_id149_post$GammaStar,
#                                   fit_aci_id149_post$fitmethod,
#                                   fit_aci_id149_post$Tcorrect,
#                                   fit_aci_id149_post$fitTPU)
# colnames(aci_data_id149_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id149_post_data)

##############################################################################
##############################################################################
###########id149
##############################################################################
##############################################################################
### plant id149 pre_heatwave
aci_data_id149_pre = subset(aci_data, id == ids[30] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id149_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id149_pre)
#### fit aci curve
fit_aci_id149_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id149_pre)
summary(fit_aci_id149_pre)
#### plot
plot(fit_aci_id149_pre)
#### add to dataframe
aci_data_id149_pre_data <- cbind(aci_data_id149_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id149_pre[,30]),
                                 mean(aci_data_id149_pre[,118]),
                                 fit_aci_id149_pre[[2]][1,1],
                                 fit_aci_id149_pre[[2]][1,2],
                                 fit_aci_id149_pre[[2]][2,1],
                                 fit_aci_id149_pre[[2]][2,2],
                                 fit_aci_id149_pre[[2]][3,1],
                                 fit_aci_id149_pre[[2]][3,2],
                                 # fit_aci_id149_pre[[2]][4,1],
                                 # fit_aci_id149_pre[[2]][4,2],
                                 fit_aci_id149_pre$RMSE,
                                 fit_aci_id149_pre$Ci_transition,
                                 fit_aci_id149_pre$citransition,
                                 fit_aci_id149_pre$Km,
                                 fit_aci_id149_pre$GammaStar,
                                 fit_aci_id149_pre$fitmethod,
                                 fit_aci_id149_pre$Tcorrect,
                                 fit_aci_id149_pre$fitTPU)
colnames(aci_data_id149_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id149_pre_data)

### plant id149 post_heatwave
aci_data_id149_post = subset(aci_data, id == ids[30] & meas.type == 'post_heatwave' & Ci < 550)
aci_data_id149_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id149_post)
#### fit aci curve
fit_aci_id149_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id149_post)
summary(fit_aci_id149_post)
#### plot
plot(fit_aci_id149_post)
#### add to dataframe
aci_data_id149_post_data <- cbind(aci_data_id149_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id149_post[,30]),
                                  mean(aci_data_id149_post[,118]),
                                  fit_aci_id149_post[[2]][1,1],
                                  fit_aci_id149_post[[2]][1,2],
                                  fit_aci_id149_post[[2]][2,1],
                                  fit_aci_id149_post[[2]][2,2],
                                  fit_aci_id149_post[[2]][3,1],
                                  fit_aci_id149_post[[2]][3,2],
                                  # fit_aci_id149_post[[2]][4,1],
                                  # fit_aci_id149_post[[2]][4,2],
                                  fit_aci_id149_post$RMSE,
                                  fit_aci_id149_post$Ci_transition,
                                  fit_aci_id149_post$citransition,
                                  fit_aci_id149_post$Km,
                                  fit_aci_id149_post$GammaStar,
                                  fit_aci_id149_post$fitmethod,
                                  fit_aci_id149_post$Tcorrect,
                                  fit_aci_id149_post$fitTPU)
colnames(aci_data_id149_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id149_post_data)

##############################################################################
##############################################################################
###########id15
##############################################################################
##############################################################################
### plant id15 pre_heatwave
aci_data_id15_pre = subset(aci_data, id == ids[31] & meas.type == 'pre_heatwave')
aci_data_id15_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id15_pre)
#### fit aci curve
fit_aci_id15_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id15_pre)
summary(fit_aci_id15_pre)
#### plot
plot(fit_aci_id15_pre)
#### add to dataframe
aci_data_id15_pre_data <- cbind(aci_data_id15_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id15_pre[,30]),
                                 mean(aci_data_id15_pre[,118]),
                                 fit_aci_id15_pre[[2]][1,1],
                                 fit_aci_id15_pre[[2]][1,2],
                                 fit_aci_id15_pre[[2]][2,1],
                                 fit_aci_id15_pre[[2]][2,2],
                                 fit_aci_id15_pre[[2]][3,1],
                                 fit_aci_id15_pre[[2]][3,2],
                                 # fit_aci_id15_pre[[2]][4,1],
                                 # fit_aci_id15_pre[[2]][4,2],
                                 fit_aci_id15_pre$RMSE,
                                 fit_aci_id15_pre$Ci_transition,
                                 fit_aci_id15_pre$citransition,
                                 fit_aci_id15_pre$Km,
                                 fit_aci_id15_pre$GammaStar,
                                 fit_aci_id15_pre$fitmethod,
                                 fit_aci_id15_pre$Tcorrect,
                                 fit_aci_id15_pre$fitTPU)
colnames(aci_data_id15_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id15_pre_data)

### plant id15 post_heatwave
aci_data_id15_post = subset(aci_data, id == ids[31] & meas.type == 'post_heatwave')
aci_data_id15_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id15_post)
#### fit aci curve
fit_aci_id15_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id15_post)
summary(fit_aci_id15_post)
#### plot
plot(fit_aci_id15_post)
#### add to dataframe
aci_data_id15_post_data <- cbind(aci_data_id15_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id15_post[,30]),
                                  mean(aci_data_id15_post[,118]),
                                  fit_aci_id15_post[[2]][1,1],
                                  fit_aci_id15_post[[2]][1,2],
                                  fit_aci_id15_post[[2]][2,1],
                                  fit_aci_id15_post[[2]][2,2],
                                  fit_aci_id15_post[[2]][3,1],
                                  fit_aci_id15_post[[2]][3,2],
                                  # fit_aci_id15_post[[2]][4,1],
                                  # fit_aci_id15_post[[2]][4,2],
                                  fit_aci_id15_post$RMSE,
                                  fit_aci_id15_post$Ci_transition,
                                  fit_aci_id15_post$citransition,
                                  fit_aci_id15_post$Km,
                                  fit_aci_id15_post$GammaStar,
                                  fit_aci_id15_post$fitmethod,
                                  fit_aci_id15_post$Tcorrect,
                                  fit_aci_id15_post$fitTPU)
colnames(aci_data_id15_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id15_post_data)

##############################################################################
##############################################################################
###########id150
##############################################################################
##############################################################################
### plant id150 pre_heatwave
# aci_data_id150_pre = subset(aci_data, id == ids[32] & meas.type == 'pre_heatwave')
# aci_data_id150_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id150_pre)
# #### fit aci curve
# fit_aci_id150_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id150_pre)
# summary(fit_aci_id150_pre)
# #### plot
# plot(fit_aci_id150_pre)
# #### add to dataframe
# aci_data_id150_pre_data <- cbind(aci_data_id150_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id150_pre[,30]),
#                                  mean(aci_data_id150_pre[,118]),
#                                  fit_aci_id150_pre[[2]][1,1],
#                                  fit_aci_id150_pre[[2]][1,2],
#                                  fit_aci_id150_pre[[2]][2,1],
#                                  fit_aci_id150_pre[[2]][2,2],
#                                  fit_aci_id150_pre[[2]][3,1],
#                                  fit_aci_id150_pre[[2]][3,2],
#                                  # fit_aci_id150_pre[[2]][4,1],
#                                  # fit_aci_id150_pre[[2]][4,2],
#                                  fit_aci_id150_pre$RMSE,
#                                  fit_aci_id150_pre$Ci_transition,
#                                  fit_aci_id150_pre$citransition,
#                                  fit_aci_id150_pre$Km,
#                                  fit_aci_id150_pre$GammaStar,
#                                  fit_aci_id150_pre$fitmethod,
#                                  fit_aci_id150_pre$Tcorrect,
#                                  fit_aci_id150_pre$fitTPU)
# colnames(aci_data_id150_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id150_pre_data)
# 
# ### plant id150 post_heatwave
# aci_data_id150_post = subset(aci_data, id == ids[32] & meas.type == 'post_heatwave')
# aci_data_id150_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id150_post)
# #### fit aci curve
# fit_aci_id150_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id150_post)
# summary(fit_aci_id150_post)
# #### plot
# plot(fit_aci_id150_post)
# #### add to dataframe
# aci_data_id150_post_data <- cbind(aci_data_id150_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id150_post[,30]),
#                                   mean(aci_data_id150_post[,118]),
#                                   fit_aci_id150_post[[2]][1,1],
#                                   fit_aci_id150_post[[2]][1,2],
#                                   fit_aci_id150_post[[2]][2,1],
#                                   fit_aci_id150_post[[2]][2,2],
#                                   fit_aci_id150_post[[2]][3,1],
#                                   fit_aci_id150_post[[2]][3,2],
#                                   # fit_aci_id150_post[[2]][4,1],
#                                   # fit_aci_id150_post[[2]][4,2],
#                                   fit_aci_id150_post$RMSE,
#                                   fit_aci_id150_post$Ci_transition,
#                                   fit_aci_id150_post$citransition,
#                                   fit_aci_id150_post$Km,
#                                   fit_aci_id150_post$GammaStar,
#                                   fit_aci_id150_post$fitmethod,
#                                   fit_aci_id150_post$Tcorrect,
#                                   fit_aci_id150_post$fitTPU)
# colnames(aci_data_id150_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id150_post_data)

##############################################################################
##############################################################################
###########id156
##############################################################################
##############################################################################
### plant id156 pre_heatwave
# aci_data_id156_pre = subset(aci_data, id == ids[33] & meas.type == 'pre_heatwave')
# aci_data_id156_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id156_pre)
# #### fit aci curve
# fit_aci_id156_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id156_pre)
# summary(fit_aci_id156_pre)
# #### plot
# plot(fit_aci_id156_pre)
# #### add to dataframe
# aci_data_id156_pre_data <- cbind(aci_data_id156_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id156_pre[,30]),
#                                  mean(aci_data_id156_pre[,118]),
#                                  fit_aci_id156_pre[[2]][1,1],
#                                  fit_aci_id156_pre[[2]][1,2],
#                                  fit_aci_id156_pre[[2]][2,1],
#                                  fit_aci_id156_pre[[2]][2,2],
#                                  fit_aci_id156_pre[[2]][3,1],
#                                  fit_aci_id156_pre[[2]][3,2],
#                                  # fit_aci_id156_pre[[2]][4,1],
#                                  # fit_aci_id156_pre[[2]][4,2],
#                                  fit_aci_id156_pre$RMSE,
#                                  fit_aci_id156_pre$Ci_transition,
#                                  fit_aci_id156_pre$citransition,
#                                  fit_aci_id156_pre$Km,
#                                  fit_aci_id156_pre$GammaStar,
#                                  fit_aci_id156_pre$fitmethod,
#                                  fit_aci_id156_pre$Tcorrect,
#                                  fit_aci_id156_pre$fitTPU)
# colnames(aci_data_id156_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id156_pre_data)
# 
# ### plant id156 post_heatwave
# aci_data_id156_post = subset(aci_data, id == ids[33] & meas.type == 'post_heatwave')
# aci_data_id156_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id156_post)
# #### fit aci curve
# fit_aci_id156_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id156_post)
# summary(fit_aci_id156_post)
# #### plot
# plot(fit_aci_id156_post)
# #### add to dataframe
# aci_data_id156_post_data <- cbind(aci_data_id156_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id156_post[,30]),
#                                   mean(aci_data_id156_post[,118]),
#                                   fit_aci_id156_post[[2]][1,1],
#                                   fit_aci_id156_post[[2]][1,2],
#                                   fit_aci_id156_post[[2]][2,1],
#                                   fit_aci_id156_post[[2]][2,2],
#                                   fit_aci_id156_post[[2]][3,1],
#                                   fit_aci_id156_post[[2]][3,2],
#                                   # fit_aci_id156_post[[2]][4,1],
#                                   # fit_aci_id156_post[[2]][4,2],
#                                   fit_aci_id156_post$RMSE,
#                                   fit_aci_id156_post$Ci_transition,
#                                   fit_aci_id156_post$citransition,
#                                   fit_aci_id156_post$Km,
#                                   fit_aci_id156_post$GammaStar,
#                                   fit_aci_id156_post$fitmethod,
#                                   fit_aci_id156_post$Tcorrect,
#                                   fit_aci_id156_post$fitTPU)
# colnames(aci_data_id156_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id156_post_data)

##############################################################################
##############################################################################
###########id159
##############################################################################
##############################################################################
### plant id159 pre_heatwave
aci_data_id159_pre = subset(aci_data, id == ids[34] & meas.type == 'pre_heatwave' & Ci < 650)
aci_data_id159_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id159_pre)
#### fit aci curve
fit_aci_id159_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id159_pre)
summary(fit_aci_id159_pre)
#### plot
plot(fit_aci_id159_pre)
#### add to dataframe
aci_data_id159_pre_data <- cbind(aci_data_id159_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id159_pre[,30]),
                                 mean(aci_data_id159_pre[,118]),
                                 fit_aci_id159_pre[[2]][1,1],
                                 fit_aci_id159_pre[[2]][1,2],
                                 fit_aci_id159_pre[[2]][2,1],
                                 fit_aci_id159_pre[[2]][2,2],
                                 fit_aci_id159_pre[[2]][3,1],
                                 fit_aci_id159_pre[[2]][3,2],
                                 # fit_aci_id159_pre[[2]][4,1],
                                 # fit_aci_id159_pre[[2]][4,2],
                                 fit_aci_id159_pre$RMSE,
                                 fit_aci_id159_pre$Ci_transition,
                                 fit_aci_id159_pre$citransition,
                                 fit_aci_id159_pre$Km,
                                 fit_aci_id159_pre$GammaStar,
                                 fit_aci_id159_pre$fitmethod,
                                 fit_aci_id159_pre$Tcorrect,
                                 fit_aci_id159_pre$fitTPU)
colnames(aci_data_id159_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id159_pre_data)

### plant id159 post_heatwave
aci_data_id159_post = subset(aci_data, id == ids[34] & meas.type == 'post_heatwave' & Ci > 0 & Adyn <50 & Ci <600)
aci_data_id159_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id159_post)
#### fit aci curve
fit_aci_id159_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id159_post)
summary(fit_aci_id159_post)
#### plot
plot(fit_aci_id159_post)
#### add to dataframe
aci_data_id159_post_data <- cbind(aci_data_id159_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id159_post[,30]),
                                  mean(aci_data_id159_post[,118]),
                                  fit_aci_id159_post[[2]][1,1],
                                  fit_aci_id159_post[[2]][1,2],
                                  fit_aci_id159_post[[2]][2,1],
                                  fit_aci_id159_post[[2]][2,2],
                                  fit_aci_id159_post[[2]][3,1],
                                  fit_aci_id159_post[[2]][3,2],
                                  # fit_aci_id159_post[[2]][4,1],
                                  # fit_aci_id159_post[[2]][4,2],
                                  fit_aci_id159_post$RMSE,
                                  fit_aci_id159_post$Ci_transition,
                                  fit_aci_id159_post$citransition,
                                  fit_aci_id159_post$Km,
                                  fit_aci_id159_post$GammaStar,
                                  fit_aci_id159_post$fitmethod,
                                  fit_aci_id159_post$Tcorrect,
                                  fit_aci_id159_post$fitTPU)
colnames(aci_data_id159_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id159_post_data)

##############################################################################
##############################################################################
###########id16
##############################################################################
##############################################################################
### plant id16 pre_heatwave
# aci_data_id16_pre = subset(aci_data, id == ids[35] & meas.type == 'pre_heatwave')
# aci_data_id16_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id16_pre)
# #### fit aci curve
# fit_aci_id16_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id16_pre)
# summary(fit_aci_id16_pre)
# #### plot
# plot(fit_aci_id16_pre)
# #### add to dataframe
# aci_data_id16_pre_data <- cbind(aci_data_id16_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id16_pre[,30]),
#                                  mean(aci_data_id16_pre[,118]),
#                                  fit_aci_id16_pre[[2]][1,1],
#                                  fit_aci_id16_pre[[2]][1,2],
#                                  fit_aci_id16_pre[[2]][2,1],
#                                  fit_aci_id16_pre[[2]][2,2],
#                                  fit_aci_id16_pre[[2]][3,1],
#                                  fit_aci_id16_pre[[2]][3,2],
#                                  # fit_aci_id16_pre[[2]][4,1],
#                                  # fit_aci_id16_pre[[2]][4,2],
#                                  fit_aci_id16_pre$RMSE,
#                                  fit_aci_id16_pre$Ci_transition,
#                                  fit_aci_id16_pre$citransition,
#                                  fit_aci_id16_pre$Km,
#                                  fit_aci_id16_pre$GammaStar,
#                                  fit_aci_id16_pre$fitmethod,
#                                  fit_aci_id16_pre$Tcorrect,
#                                  fit_aci_id16_pre$fitTPU)
# colnames(aci_data_id16_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id16_pre_data)
# 
# ### plant id16 post_heatwave
# aci_data_id16_post = subset(aci_data, id == ids[35] & meas.type == 'post_heatwave')
# aci_data_id16_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id16_post)
# #### fit aci curve
# fit_aci_id16_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id16_post)
# summary(fit_aci_id16_post)
# #### plot
# plot(fit_aci_id16_post)
# #### add to dataframe
# aci_data_id16_post_data <- cbind(aci_data_id16_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id16_post[,30]),
#                                   mean(aci_data_id16_post[,118]),
#                                   fit_aci_id16_post[[2]][1,1],
#                                   fit_aci_id16_post[[2]][1,2],
#                                   fit_aci_id16_post[[2]][2,1],
#                                   fit_aci_id16_post[[2]][2,2],
#                                   fit_aci_id16_post[[2]][3,1],
#                                   fit_aci_id16_post[[2]][3,2],
#                                   # fit_aci_id16_post[[2]][4,1],
#                                   # fit_aci_id16_post[[2]][4,2],
#                                   fit_aci_id16_post$RMSE,
#                                   fit_aci_id16_post$Ci_transition,
#                                   fit_aci_id16_post$citransition,
#                                   fit_aci_id16_post$Km,
#                                   fit_aci_id16_post$GammaStar,
#                                   fit_aci_id16_post$fitmethod,
#                                   fit_aci_id16_post$Tcorrect,
#                                   fit_aci_id16_post$fitTPU)
# colnames(aci_data_id16_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id16_post_data)

##############################################################################
##############################################################################
###########id160
##############################################################################
##############################################################################
### plant id160 pre_heatwave
aci_data_id160_pre = subset(aci_data, id == ids[36] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id160_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id160_pre)
#### fit aci curve
fit_aci_id160_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id160_pre)
summary(fit_aci_id160_pre)
#### plot
plot(fit_aci_id160_pre)
#### add to dataframe
aci_data_id160_pre_data <- cbind(aci_data_id160_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id160_pre[,30]),
                                 mean(aci_data_id160_pre[,118]),
                                 fit_aci_id160_pre[[2]][1,1],
                                 fit_aci_id160_pre[[2]][1,2],
                                 fit_aci_id160_pre[[2]][2,1],
                                 fit_aci_id160_pre[[2]][2,2],
                                 fit_aci_id160_pre[[2]][3,1],
                                 fit_aci_id160_pre[[2]][3,2],
                                 # fit_aci_id160_pre[[2]][4,1],
                                 # fit_aci_id160_pre[[2]][4,2],
                                 fit_aci_id160_pre$RMSE,
                                 fit_aci_id160_pre$Ci_transition,
                                 fit_aci_id160_pre$citransition,
                                 fit_aci_id160_pre$Km,
                                 fit_aci_id160_pre$GammaStar,
                                 fit_aci_id160_pre$fitmethod,
                                 fit_aci_id160_pre$Tcorrect,
                                 fit_aci_id160_pre$fitTPU)
colnames(aci_data_id160_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id160_pre_data)

### plant id160 post_heatwave
aci_data_id160_post = subset(aci_data, id == ids[36] & meas.type == 'post_heatwave' & Ci < 600)
aci_data_id160_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id160_post)
#### fit aci curve
fit_aci_id160_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id160_post)
summary(fit_aci_id160_post)
#### plot
plot(fit_aci_id160_post)
#### add to dataframe
aci_data_id160_post_data <- cbind(aci_data_id160_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id160_post[,30]),
                                  mean(aci_data_id160_post[,118]),
                                  fit_aci_id160_post[[2]][1,1],
                                  fit_aci_id160_post[[2]][1,2],
                                  fit_aci_id160_post[[2]][2,1],
                                  fit_aci_id160_post[[2]][2,2],
                                  fit_aci_id160_post[[2]][3,1],
                                  fit_aci_id160_post[[2]][3,2],
                                  # fit_aci_id160_post[[2]][4,1],
                                  # fit_aci_id160_post[[2]][4,2],
                                  fit_aci_id160_post$RMSE,
                                  fit_aci_id160_post$Ci_transition,
                                  fit_aci_id160_post$citransition,
                                  fit_aci_id160_post$Km,
                                  fit_aci_id160_post$GammaStar,
                                  fit_aci_id160_post$fitmethod,
                                  fit_aci_id160_post$Tcorrect,
                                  fit_aci_id160_post$fitTPU)
colnames(aci_data_id160_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id160_post_data)

##############################################################################
##############################################################################
###########id162
##############################################################################
##############################################################################
### plant id162 pre_heatwave
# aci_data_id162_pre = subset(aci_data, id == ids[37] & meas.type == 'pre_heatwave')
# aci_data_id162_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id162_pre)
# #### fit aci curve
# fit_aci_id162_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id162_pre)
# summary(fit_aci_id162_pre)
# #### plot
# plot(fit_aci_id162_pre)
# #### add to dataframe
# aci_data_id162_pre_data <- cbind(aci_data_id162_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id162_pre[,30]),
#                                  mean(aci_data_id162_pre[,118]),
#                                  fit_aci_id162_pre[[2]][1,1],
#                                  fit_aci_id162_pre[[2]][1,2],
#                                  fit_aci_id162_pre[[2]][2,1],
#                                  fit_aci_id162_pre[[2]][2,2],
#                                  fit_aci_id162_pre[[2]][3,1],
#                                  fit_aci_id162_pre[[2]][3,2],
#                                  # fit_aci_id162_pre[[2]][4,1],
#                                  # fit_aci_id162_pre[[2]][4,2],
#                                  fit_aci_id162_pre$RMSE,
#                                  fit_aci_id162_pre$Ci_transition,
#                                  fit_aci_id162_pre$citransition,
#                                  fit_aci_id162_pre$Km,
#                                  fit_aci_id162_pre$GammaStar,
#                                  fit_aci_id162_pre$fitmethod,
#                                  fit_aci_id162_pre$Tcorrect,
#                                  fit_aci_id162_pre$fitTPU)
# colnames(aci_data_id162_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id162_pre_data)
# 
# ### plant id162 post_heatwave
# aci_data_id162_post = subset(aci_data, id == ids[37] & meas.type == 'post_heatwave')
# aci_data_id162_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id162_post)
# #### fit aci curve
# fit_aci_id162_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id162_post)
# summary(fit_aci_id162_post)
# #### plot
# plot(fit_aci_id162_post)
# #### add to dataframe
# aci_data_id162_post_data <- cbind(aci_data_id162_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id162_post[,30]),
#                                   mean(aci_data_id162_post[,118]),
#                                   fit_aci_id162_post[[2]][1,1],
#                                   fit_aci_id162_post[[2]][1,2],
#                                   fit_aci_id162_post[[2]][2,1],
#                                   fit_aci_id162_post[[2]][2,2],
#                                   fit_aci_id162_post[[2]][3,1],
#                                   fit_aci_id162_post[[2]][3,2],
#                                   # fit_aci_id162_post[[2]][4,1],
#                                   # fit_aci_id162_post[[2]][4,2],
#                                   fit_aci_id162_post$RMSE,
#                                   fit_aci_id162_post$Ci_transition,
#                                   fit_aci_id162_post$citransition,
#                                   fit_aci_id162_post$Km,
#                                   fit_aci_id162_post$GammaStar,
#                                   fit_aci_id162_post$fitmethod,
#                                   fit_aci_id162_post$Tcorrect,
#                                   fit_aci_id162_post$fitTPU)
# colnames(aci_data_id162_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id162_post_data)

##############################################################################
##############################################################################
###########id142
##############################################################################
##############################################################################
### plant id142 pre_heatwave
# aci_data_id165_pre = subset(aci_data, id == ids[38] & meas.type == 'pre_heatwave')
# aci_data_id165_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id165_pre)
# #### fit aci curve
# fit_aci_id165_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id165_pre)
# summary(fit_aci_id165_pre)
# #### plot
# plot(fit_aci_id165_pre)
# #### add to dataframe
# aci_data_id165_pre_data <- cbind(aci_data_id165_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id165_pre[,30]),
#                                  mean(aci_data_id165_pre[,118]),
#                                  fit_aci_id165_pre[[2]][1,1],
#                                  fit_aci_id165_pre[[2]][1,2],
#                                  fit_aci_id165_pre[[2]][2,1],
#                                  fit_aci_id165_pre[[2]][2,2],
#                                  fit_aci_id165_pre[[2]][3,1],
#                                  fit_aci_id165_pre[[2]][3,2],
#                                  # fit_aci_id165_pre[[2]][4,1],
#                                  # fit_aci_id165_pre[[2]][4,2],
#                                  fit_aci_id165_pre$RMSE,
#                                  fit_aci_id165_pre$Ci_transition,
#                                  fit_aci_id165_pre$citransition,
#                                  fit_aci_id165_pre$Km,
#                                  fit_aci_id165_pre$GammaStar,
#                                  fit_aci_id165_pre$fitmethod,
#                                  fit_aci_id165_pre$Tcorrect,
#                                  fit_aci_id165_pre$fitTPU)
# colnames(aci_data_id165_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id165_pre_data)
# 
# ### plant id165 post_heatwave
# aci_data_id165_post = subset(aci_data, id == ids[38] & meas.type == 'post_heatwave')
# aci_data_id165_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id165_post)
# #### fit aci curve
# fit_aci_id165_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id165_post)
# summary(fit_aci_id165_post)
# #### plot
# plot(fit_aci_id165_post)
# #### add to dataframe
# aci_data_id165_post_data <- cbind(aci_data_id165_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id165_post[,30]),
#                                   mean(aci_data_id165_post[,118]),
#                                   fit_aci_id165_post[[2]][1,1],
#                                   fit_aci_id165_post[[2]][1,2],
#                                   fit_aci_id165_post[[2]][2,1],
#                                   fit_aci_id165_post[[2]][2,2],
#                                   fit_aci_id165_post[[2]][3,1],
#                                   fit_aci_id165_post[[2]][3,2],
#                                   # fit_aci_id165_post[[2]][4,1],
#                                   # fit_aci_id165_post[[2]][4,2],
#                                   fit_aci_id165_post$RMSE,
#                                   fit_aci_id165_post$Ci_transition,
#                                   fit_aci_id165_post$citransition,
#                                   fit_aci_id165_post$Km,
#                                   fit_aci_id165_post$GammaStar,
#                                   fit_aci_id165_post$fitmethod,
#                                   fit_aci_id165_post$Tcorrect,
#                                   fit_aci_id165_post$fitTPU)
# colnames(aci_data_id165_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id165_post_data)

##############################################################################
##############################################################################
###########id168
##############################################################################
##############################################################################
### plant id168 pre_heatwave
aci_data_id168_pre = subset(aci_data, id == ids[39] & meas.type == 'pre_heatwave' & Ci < 600)
aci_data_id168_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id168_pre)
#### fit aci curve
fit_aci_id168_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id168_pre)
summary(fit_aci_id168_pre)
#### plot
plot(fit_aci_id168_pre)
#### add to dataframe
aci_data_id168_pre_data <- cbind(aci_data_id168_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id168_pre[,30]),
                                 mean(aci_data_id168_pre[,118]),
                                 fit_aci_id168_pre[[2]][1,1],
                                 fit_aci_id168_pre[[2]][1,2],
                                 fit_aci_id168_pre[[2]][2,1],
                                 fit_aci_id168_pre[[2]][2,2],
                                 fit_aci_id168_pre[[2]][3,1],
                                 fit_aci_id168_pre[[2]][3,2],
                                 # fit_aci_id168_pre[[2]][4,1],
                                 # fit_aci_id168_pre[[2]][4,2],
                                 fit_aci_id168_pre$RMSE,
                                 fit_aci_id168_pre$Ci_transition,
                                 fit_aci_id168_pre$citransition,
                                 fit_aci_id168_pre$Km,
                                 fit_aci_id168_pre$GammaStar,
                                 fit_aci_id168_pre$fitmethod,
                                 fit_aci_id168_pre$Tcorrect,
                                 fit_aci_id168_pre$fitTPU)
colnames(aci_data_id168_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id168_pre_data)

### plant id168 post_heatwave
aci_data_id168_post = subset(aci_data, id == ids[39] & meas.type == 'post_heatwave' & Ci < 500)
aci_data_id168_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id168_post)
#### fit aci curve
fit_aci_id168_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id168_post)
summary(fit_aci_id168_post)
#### plot
plot(fit_aci_id168_post)
#### add to dataframe
aci_data_id168_post_data <- cbind(aci_data_id168_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id168_post[,30]),
                                  mean(aci_data_id168_post[,118]),
                                  fit_aci_id168_post[[2]][1,1],
                                  fit_aci_id168_post[[2]][1,2],
                                  fit_aci_id168_post[[2]][2,1],
                                  fit_aci_id168_post[[2]][2,2],
                                  fit_aci_id168_post[[2]][3,1],
                                  fit_aci_id168_post[[2]][3,2],
                                  # fit_aci_id168_post[[2]][4,1],
                                  # fit_aci_id168_post[[2]][4,2],
                                  fit_aci_id168_post$RMSE,
                                  fit_aci_id168_post$Ci_transition,
                                  fit_aci_id168_post$citransition,
                                  fit_aci_id168_post$Km,
                                  fit_aci_id168_post$GammaStar,
                                  fit_aci_id168_post$fitmethod,
                                  fit_aci_id168_post$Tcorrect,
                                  fit_aci_id168_post$fitTPU)
colnames(aci_data_id168_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id168_post_data)

##############################################################################
##############################################################################
###########id169
##############################################################################
##############################################################################
### plant id169 pre_heatwave
aci_data_id169_pre = subset(aci_data, id == ids[40] & meas.type == 'pre_heatwave')
aci_data_id169_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id169_pre)
#### fit aci curve
fit_aci_id169_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id169_pre)
summary(fit_aci_id169_pre)
#### plot
plot(fit_aci_id169_pre)
#### add to dataframe
aci_data_id169_pre_data <- cbind(aci_data_id169_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id169_pre[,30]),
                                 mean(aci_data_id169_pre[,118]),
                                 fit_aci_id169_pre[[2]][1,1],
                                 fit_aci_id169_pre[[2]][1,2],
                                 fit_aci_id169_pre[[2]][2,1],
                                 fit_aci_id169_pre[[2]][2,2],
                                 fit_aci_id169_pre[[2]][3,1],
                                 fit_aci_id169_pre[[2]][3,2],
                                 # fit_aci_id169_pre[[2]][4,1],
                                 # fit_aci_id169_pre[[2]][4,2],
                                 fit_aci_id169_pre$RMSE,
                                 fit_aci_id169_pre$Ci_transition,
                                 fit_aci_id169_pre$citransition,
                                 fit_aci_id169_pre$Km,
                                 fit_aci_id169_pre$GammaStar,
                                 fit_aci_id169_pre$fitmethod,
                                 fit_aci_id169_pre$Tcorrect,
                                 fit_aci_id169_pre$fitTPU)
colnames(aci_data_id169_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id169_pre_data)

### plant id169 post_heatwave
aci_data_id169_post = subset(aci_data, id == ids[40] & meas.type == 'post_heatwave')
aci_data_id169_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id169_post)
#### fit aci curve
fit_aci_id169_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id169_post)
summary(fit_aci_id169_post)
#### plot
plot(fit_aci_id169_post)
#### add to dataframe
aci_data_id169_post_data <- cbind(aci_data_id169_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id169_post[,30]),
                                  mean(aci_data_id169_post[,118]),
                                  fit_aci_id169_post[[2]][1,1],
                                  fit_aci_id169_post[[2]][1,2],
                                  fit_aci_id169_post[[2]][2,1],
                                  fit_aci_id169_post[[2]][2,2],
                                  fit_aci_id169_post[[2]][3,1],
                                  fit_aci_id169_post[[2]][3,2],
                                  # fit_aci_id169_post[[2]][4,1],
                                  # fit_aci_id169_post[[2]][4,2],
                                  fit_aci_id169_post$RMSE,
                                  fit_aci_id169_post$Ci_transition,
                                  fit_aci_id169_post$citransition,
                                  fit_aci_id169_post$Km,
                                  fit_aci_id169_post$GammaStar,
                                  fit_aci_id169_post$fitmethod,
                                  fit_aci_id169_post$Tcorrect,
                                  fit_aci_id169_post$fitTPU)
colnames(aci_data_id169_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id169_post_data)

##############################################################################
##############################################################################
###########id17
##############################################################################
##############################################################################
### plant id17 pre_heatwave
# aci_data_id17_pre = subset(aci_data, id == ids[41] & meas.type == 'pre_heatwave')
# aci_data_id17_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id17_pre)
# #### fit aci curve
# fit_aci_id17_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id17_pre)
# summary(fit_aci_id17_pre)
# #### plot
# plot(fit_aci_id17_pre)
# #### add to dataframe
# aci_data_id17_pre_data <- cbind(aci_data_id17_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id17_pre[,30]),
#                                  mean(aci_data_id17_pre[,118]),
#                                  fit_aci_id17_pre[[2]][1,1],
#                                  fit_aci_id17_pre[[2]][1,2],
#                                  fit_aci_id17_pre[[2]][2,1],
#                                  fit_aci_id17_pre[[2]][2,2],
#                                  fit_aci_id17_pre[[2]][3,1],
#                                  fit_aci_id17_pre[[2]][3,2],
#                                  # fit_aci_id17_pre[[2]][4,1],
#                                  # fit_aci_id17_pre[[2]][4,2],
#                                  fit_aci_id17_pre$RMSE,
#                                  fit_aci_id17_pre$Ci_transition,
#                                  fit_aci_id17_pre$citransition,
#                                  fit_aci_id17_pre$Km,
#                                  fit_aci_id17_pre$GammaStar,
#                                  fit_aci_id17_pre$fitmethod,
#                                  fit_aci_id17_pre$Tcorrect,
#                                  fit_aci_id17_pre$fitTPU)
# colnames(aci_data_id17_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id17_pre_data)
# 
# ### plant id17 post_heatwave
# aci_data_id17_post = subset(aci_data, id == ids[41] & meas.type == 'post_heatwave')
# aci_data_id17_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id17_post)
# #### fit aci curve
# fit_aci_id17_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id17_post)
# summary(fit_aci_id17_post)
# #### plot
# plot(fit_aci_id17_post)
# #### add to dataframe
# aci_data_id17_post_data <- cbind(aci_data_id17_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id17_post[,30]),
#                                   mean(aci_data_id17_post[,118]),
#                                   fit_aci_id17_post[[2]][1,1],
#                                   fit_aci_id17_post[[2]][1,2],
#                                   fit_aci_id17_post[[2]][2,1],
#                                   fit_aci_id17_post[[2]][2,2],
#                                   fit_aci_id17_post[[2]][3,1],
#                                   fit_aci_id17_post[[2]][3,2],
#                                   # fit_aci_id17_post[[2]][4,1],
#                                   # fit_aci_id17_post[[2]][4,2],
#                                   fit_aci_id17_post$RMSE,
#                                   fit_aci_id17_post$Ci_transition,
#                                   fit_aci_id17_post$citransition,
#                                   fit_aci_id17_post$Km,
#                                   fit_aci_id17_post$GammaStar,
#                                   fit_aci_id17_post$fitmethod,
#                                   fit_aci_id17_post$Tcorrect,
#                                   fit_aci_id17_post$fitTPU)
# colnames(aci_data_id17_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id17_post_data)

##############################################################################
##############################################################################
###########id171
##############################################################################
##############################################################################
### plant id171 pre_heatwave
aci_data_id171_pre = subset(aci_data, id == ids[43] & meas.type == 'pre_heatwave')
aci_data_id171_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id171_pre)
#### fit aci curve
fit_aci_id171_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id171_pre)
summary(fit_aci_id171_pre)
#### plot
plot(fit_aci_id171_pre)
#### add to dataframe
aci_data_id171_pre_data <- cbind(aci_data_id171_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id171_pre[,30]),
                                 mean(aci_data_id171_pre[,118]),
                                 fit_aci_id171_pre[[2]][1,1],
                                 fit_aci_id171_pre[[2]][1,2],
                                 fit_aci_id171_pre[[2]][2,1],
                                 fit_aci_id171_pre[[2]][2,2],
                                 fit_aci_id171_pre[[2]][3,1],
                                 fit_aci_id171_pre[[2]][3,2],
                                 # fit_aci_id171_pre[[2]][4,1],
                                 # fit_aci_id171_pre[[2]][4,2],
                                 fit_aci_id171_pre$RMSE,
                                 fit_aci_id171_pre$Ci_transition,
                                 fit_aci_id171_pre$citransition,
                                 fit_aci_id171_pre$Km,
                                 fit_aci_id171_pre$GammaStar,
                                 fit_aci_id171_pre$fitmethod,
                                 fit_aci_id171_pre$Tcorrect,
                                 fit_aci_id171_pre$fitTPU)
colnames(aci_data_id171_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id171_pre_data)

### plant id171 post_heatwave
aci_data_id171_post = subset(aci_data, id == ids[42] & meas.type == 'post_heatwave' & Ci <600)
aci_data_id171_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id171_post)
#### fit aci curve
fit_aci_id171_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id171_post)
summary(fit_aci_id171_post)
#### plot
plot(fit_aci_id171_post)
#### add to dataframe
aci_data_id171_post_data <- cbind(aci_data_id171_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id171_post[,30]),
                                  mean(aci_data_id171_post[,118]),
                                  fit_aci_id171_post[[2]][1,1],
                                  fit_aci_id171_post[[2]][1,2],
                                  fit_aci_id171_post[[2]][2,1],
                                  fit_aci_id171_post[[2]][2,2],
                                  fit_aci_id171_post[[2]][3,1],
                                  fit_aci_id171_post[[2]][3,2],
                                  # fit_aci_id171_post[[2]][4,1],
                                  # fit_aci_id171_post[[2]][4,2],
                                  fit_aci_id171_post$RMSE,
                                  fit_aci_id171_post$Ci_transition,
                                  fit_aci_id171_post$citransition,
                                  fit_aci_id171_post$Km,
                                  fit_aci_id171_post$GammaStar,
                                  fit_aci_id171_post$fitmethod,
                                  fit_aci_id171_post$Tcorrect,
                                  fit_aci_id171_post$fitTPU)
colnames(aci_data_id171_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id171_post_data)

##############################################################################
##############################################################################
###########id172
##############################################################################
##############################################################################
### plant id172 pre_heatwave
# aci_data_id172_pre = subset(aci_data, id == ids[44] & meas.type == 'pre_heatwave')
# aci_data_id172_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id172_pre)
# #### fit aci curve
# fit_aci_id172_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id172_pre)
# summary(fit_aci_id172_pre)
# #### plot
# plot(fit_aci_id172_pre)
# #### add to dataframe
# aci_data_id172_pre_data <- cbind(aci_data_id172_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id172_pre[,30]),
#                                  mean(aci_data_id172_pre[,118]),
#                                  fit_aci_id172_pre[[2]][1,1],
#                                  fit_aci_id172_pre[[2]][1,2],
#                                  fit_aci_id172_pre[[2]][2,1],
#                                  fit_aci_id172_pre[[2]][2,2],
#                                  fit_aci_id172_pre[[2]][3,1],
#                                  fit_aci_id172_pre[[2]][3,2],
#                                  # fit_aci_id172_pre[[2]][4,1],
#                                  # fit_aci_id172_pre[[2]][4,2],
#                                  fit_aci_id172_pre$RMSE,
#                                  fit_aci_id172_pre$Ci_transition,
#                                  fit_aci_id172_pre$citransition,
#                                  fit_aci_id172_pre$Km,
#                                  fit_aci_id172_pre$GammaStar,
#                                  fit_aci_id172_pre$fitmethod,
#                                  fit_aci_id172_pre$Tcorrect,
#                                  fit_aci_id172_pre$fitTPU)
# colnames(aci_data_id172_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id172_pre_data)
# 
# ### plant id172 post_heatwave
# aci_data_id172_post = subset(aci_data, id == ids[44] & meas.type == 'post_heatwave')
# aci_data_id172_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id172_post)
# #### fit aci curve
# fit_aci_id172_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id172_post)
# summary(fit_aci_id172_post)
# #### plot
# plot(fit_aci_id172_post)
# #### add to dataframe
# aci_data_id172_post_data <- cbind(aci_data_id172_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id172_post[,30]),
#                                   mean(aci_data_id172_post[,118]),
#                                   fit_aci_id172_post[[2]][1,1],
#                                   fit_aci_id172_post[[2]][1,2],
#                                   fit_aci_id172_post[[2]][2,1],
#                                   fit_aci_id172_post[[2]][2,2],
#                                   fit_aci_id172_post[[2]][3,1],
#                                   fit_aci_id172_post[[2]][3,2],
#                                   # fit_aci_id172_post[[2]][4,1],
#                                   # fit_aci_id172_post[[2]][4,2],
#                                   fit_aci_id172_post$RMSE,
#                                   fit_aci_id172_post$Ci_transition,
#                                   fit_aci_id172_post$citransition,
#                                   fit_aci_id172_post$Km,
#                                   fit_aci_id172_post$GammaStar,
#                                   fit_aci_id172_post$fitmethod,
#                                   fit_aci_id172_post$Tcorrect,
#                                   fit_aci_id172_post$fitTPU)
# colnames(aci_data_id172_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id172_post_data)

##############################################################################
##############################################################################
###########id175
##############################################################################
##############################################################################
### plant id175 pre_heatwave
# aci_data_id175_pre = subset(aci_data, id == ids[45] & meas.type == 'pre_heatwave' & Ci <600)
# aci_data_id175_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id175_pre)
# #### fit aci curve
# fit_aci_id175_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id175_pre)
# summary(fit_aci_id175_pre)
# #### plot
# plot(fit_aci_id175_pre)
# #### add to dataframe
# aci_data_id175_pre_data <- cbind(aci_data_id175_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id175_pre[,30]),
#                                  mean(aci_data_id175_pre[,118]),
#                                  fit_aci_id175_pre[[2]][1,1],
#                                  fit_aci_id175_pre[[2]][1,2],
#                                  fit_aci_id175_pre[[2]][2,1],
#                                  fit_aci_id175_pre[[2]][2,2],
#                                  fit_aci_id175_pre[[2]][3,1],
#                                  fit_aci_id175_pre[[2]][3,2],
#                                  # fit_aci_id175_pre[[2]][4,1],
#                                  # fit_aci_id175_pre[[2]][4,2],
#                                  fit_aci_id175_pre$RMSE,
#                                  fit_aci_id175_pre$Ci_transition,
#                                  fit_aci_id175_pre$citransition,
#                                  fit_aci_id175_pre$Km,
#                                  fit_aci_id175_pre$GammaStar,
#                                  fit_aci_id175_pre$fitmethod,
#                                  fit_aci_id175_pre$Tcorrect,
#                                  fit_aci_id175_pre$fitTPU)
# colnames(aci_data_id175_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id175_pre_data)
# 
# ### plant id175 post_heatwave
# aci_data_id175_post = subset(aci_data, id == ids[45] & meas.type == 'post_heatwave')
# aci_data_id175_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id175_post)
# #### fit aci curve
# fit_aci_id175_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id175_post)
# summary(fit_aci_id175_post)
# #### plot
# plot(fit_aci_id175_post)
# #### add to dataframe
# aci_data_id175_post_data <- cbind(aci_data_id175_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id175_post[,30]),
#                                   mean(aci_data_id175_post[,118]),
#                                   fit_aci_id175_post[[2]][1,1],
#                                   fit_aci_id175_post[[2]][1,2],
#                                   fit_aci_id175_post[[2]][2,1],
#                                   fit_aci_id175_post[[2]][2,2],
#                                   fit_aci_id175_post[[2]][3,1],
#                                   fit_aci_id175_post[[2]][3,2],
#                                   # fit_aci_id175_post[[2]][4,1],
#                                   # fit_aci_id175_post[[2]][4,2],
#                                   fit_aci_id175_post$RMSE,
#                                   fit_aci_id175_post$Ci_transition,
#                                   fit_aci_id175_post$citransition,
#                                   fit_aci_id175_post$Km,
#                                   fit_aci_id175_post$GammaStar,
#                                   fit_aci_id175_post$fitmethod,
#                                   fit_aci_id175_post$Tcorrect,
#                                   fit_aci_id175_post$fitTPU)
# colnames(aci_data_id175_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id175_post_data)

##############################################################################
##############################################################################
###########id178
##############################################################################
##############################################################################
### plant id178 pre_heatwave
aci_data_id178_pre = subset(aci_data, id == ids[46] & meas.type == 'pre_heatwave')
aci_data_id178_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id178_pre)
#### fit aci curve
fit_aci_id178_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id178_pre)
summary(fit_aci_id178_pre)
#### plot
plot(fit_aci_id178_pre)
#### add to dataframe
aci_data_id178_pre_data <- cbind(aci_data_id178_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id178_pre[,30]),
                                 mean(aci_data_id178_pre[,118]),
                                 fit_aci_id178_pre[[2]][1,1],
                                 fit_aci_id178_pre[[2]][1,2],
                                 fit_aci_id178_pre[[2]][2,1],
                                 fit_aci_id178_pre[[2]][2,2],
                                 fit_aci_id178_pre[[2]][3,1],
                                 fit_aci_id178_pre[[2]][3,2],
                                 # fit_aci_id178_pre[[2]][4,1],
                                 # fit_aci_id178_pre[[2]][4,2],
                                 fit_aci_id178_pre$RMSE,
                                 fit_aci_id178_pre$Ci_transition,
                                 fit_aci_id178_pre$citransition,
                                 fit_aci_id178_pre$Km,
                                 fit_aci_id178_pre$GammaStar,
                                 fit_aci_id178_pre$fitmethod,
                                 fit_aci_id178_pre$Tcorrect,
                                 fit_aci_id178_pre$fitTPU)
colnames(aci_data_id178_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id178_pre_data)

### plant id178 post_heatwave
aci_data_id178_post = subset(aci_data, id == ids[46] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id178_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id178_post)
#### fit aci curve
fit_aci_id178_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id178_post)
summary(fit_aci_id178_post)
#### plot
plot(fit_aci_id178_post)
#### add to dataframe
aci_data_id178_post_data <- cbind(aci_data_id178_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id178_post[,30]),
                                  mean(aci_data_id178_post[,118]),
                                  fit_aci_id178_post[[2]][1,1],
                                  fit_aci_id178_post[[2]][1,2],
                                  fit_aci_id178_post[[2]][2,1],
                                  fit_aci_id178_post[[2]][2,2],
                                  fit_aci_id178_post[[2]][3,1],
                                  fit_aci_id178_post[[2]][3,2],
                                  # fit_aci_id178_post[[2]][4,1],
                                  # fit_aci_id178_post[[2]][4,2],
                                  fit_aci_id178_post$RMSE,
                                  fit_aci_id178_post$Ci_transition,
                                  fit_aci_id178_post$citransition,
                                  fit_aci_id178_post$Km,
                                  fit_aci_id178_post$GammaStar,
                                  fit_aci_id178_post$fitmethod,
                                  fit_aci_id178_post$Tcorrect,
                                  fit_aci_id178_post$fitTPU)
colnames(aci_data_id178_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id178_post_data)

##############################################################################
##############################################################################
###########id180
##############################################################################
##############################################################################
### plant id180 pre_heatwave
# aci_data_id180_pre = subset(aci_data, id == ids[47] & meas.type == 'pre_heatwave')
# aci_data_id180_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id180_pre)
# #### fit aci curve
# fit_aci_id180_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id180_pre)
# summary(fit_aci_id180_pre)
# #### plot
# plot(fit_aci_id180_pre)
# #### add to dataframe
# aci_data_id180_pre_data <- cbind(aci_data_id180_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id180_pre[,30]),
#                                  mean(aci_data_id180_pre[,118]),
#                                  fit_aci_id180_pre[[2]][1,1],
#                                  fit_aci_id180_pre[[2]][1,2],
#                                  fit_aci_id180_pre[[2]][2,1],
#                                  fit_aci_id180_pre[[2]][2,2],
#                                  fit_aci_id180_pre[[2]][3,1],
#                                  fit_aci_id180_pre[[2]][3,2],
#                                  # fit_aci_id180_pre[[2]][4,1],
#                                  # fit_aci_id180_pre[[2]][4,2],
#                                  fit_aci_id180_pre$RMSE,
#                                  fit_aci_id180_pre$Ci_transition,
#                                  fit_aci_id180_pre$citransition,
#                                  fit_aci_id180_pre$Km,
#                                  fit_aci_id180_pre$GammaStar,
#                                  fit_aci_id180_pre$fitmethod,
#                                  fit_aci_id180_pre$Tcorrect,
#                                  fit_aci_id180_pre$fitTPU)
# colnames(aci_data_id180_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id180_pre_data)
# 
# ### plant id180 post_heatwave
# aci_data_id180_post = subset(aci_data, id == ids[47] & meas.type == 'post_heatwave')
# aci_data_id180_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id180_post)
# #### fit aci curve
# fit_aci_id180_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id180_post)
# summary(fit_aci_id180_post)
# #### plot
# plot(fit_aci_id180_post)
# #### add to dataframe
# aci_data_id180_post_data <- cbind(aci_data_id180_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id180_post[,30]),
#                                   mean(aci_data_id180_post[,118]),
#                                   fit_aci_id180_post[[2]][1,1],
#                                   fit_aci_id180_post[[2]][1,2],
#                                   fit_aci_id180_post[[2]][2,1],
#                                   fit_aci_id180_post[[2]][2,2],
#                                   fit_aci_id180_post[[2]][3,1],
#                                   fit_aci_id180_post[[2]][3,2],
#                                   # fit_aci_id180_post[[2]][4,1],
#                                   # fit_aci_id180_post[[2]][4,2],
#                                   fit_aci_id180_post$RMSE,
#                                   fit_aci_id180_post$Ci_transition,
#                                   fit_aci_id180_post$citransition,
#                                   fit_aci_id180_post$Km,
#                                   fit_aci_id180_post$GammaStar,
#                                   fit_aci_id180_post$fitmethod,
#                                   fit_aci_id180_post$Tcorrect,
#                                   fit_aci_id180_post$fitTPU)
# colnames(aci_data_id180_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id180_post_data)

##############################################################################
##############################################################################
###########id181
##############################################################################
##############################################################################
### plant id181 pre_heatwave
aci_data_id181_pre = subset(aci_data, id == ids[48] & meas.type == 'pre_heatwave')
aci_data_id181_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id181_pre)
#### fit aci curve
fit_aci_id181_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id181_pre)
summary(fit_aci_id181_pre)
#### plot
plot(fit_aci_id181_pre)
#### add to dataframe
aci_data_id181_pre_data <- cbind(aci_data_id181_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id181_pre[,30]),
                                 mean(aci_data_id181_pre[,118]),
                                 fit_aci_id181_pre[[2]][1,1],
                                 fit_aci_id181_pre[[2]][1,2],
                                 fit_aci_id181_pre[[2]][2,1],
                                 fit_aci_id181_pre[[2]][2,2],
                                 fit_aci_id181_pre[[2]][3,1],
                                 fit_aci_id181_pre[[2]][3,2],
                                 # fit_aci_id181_pre[[2]][4,1],
                                 # fit_aci_id181_pre[[2]][4,2],
                                 fit_aci_id181_pre$RMSE,
                                 fit_aci_id181_pre$Ci_transition,
                                 fit_aci_id181_pre$citransition,
                                 fit_aci_id181_pre$Km,
                                 fit_aci_id181_pre$GammaStar,
                                 fit_aci_id181_pre$fitmethod,
                                 fit_aci_id181_pre$Tcorrect,
                                 fit_aci_id181_pre$fitTPU)
colnames(aci_data_id181_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id181_pre_data)

### plant id181 post_heatwave
aci_data_id181_post = subset(aci_data, id == ids[48] & meas.type == 'post_heatwave')
aci_data_id181_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id181_post)
#### fit aci curve
fit_aci_id181_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id181_post)
summary(fit_aci_id181_post)
#### plot
plot(fit_aci_id181_post)
#### add to dataframe
aci_data_id181_post_data <- cbind(aci_data_id181_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id181_post[,30]),
                                  mean(aci_data_id181_post[,118]),
                                  fit_aci_id181_post[[2]][1,1],
                                  fit_aci_id181_post[[2]][1,2],
                                  fit_aci_id181_post[[2]][2,1],
                                  fit_aci_id181_post[[2]][2,2],
                                  fit_aci_id181_post[[2]][3,1],
                                  fit_aci_id181_post[[2]][3,2],
                                  # fit_aci_id181_post[[2]][4,1],
                                  # fit_aci_id181_post[[2]][4,2],
                                  fit_aci_id181_post$RMSE,
                                  fit_aci_id181_post$Ci_transition,
                                  fit_aci_id181_post$citransition,
                                  fit_aci_id181_post$Km,
                                  fit_aci_id181_post$GammaStar,
                                  fit_aci_id181_post$fitmethod,
                                  fit_aci_id181_post$Tcorrect,
                                  fit_aci_id181_post$fitTPU)
colnames(aci_data_id181_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id181_post_data)

##############################################################################
##############################################################################
###########id185
##############################################################################
##############################################################################
### plant id185 pre_heatwave
# aci_data_id185_pre = subset(aci_data, id == ids[49] & meas.type == 'pre_heatwave')
# aci_data_id185_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id185_pre)
# #### fit aci curve
# fit_aci_id185_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id185_pre)
# summary(fit_aci_id185_pre)
# #### plot
# plot(fit_aci_id185_pre)
# #### add to dataframe
# aci_data_id185_pre_data <- cbind(aci_data_id185_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id185_pre[,30]),
#                                  mean(aci_data_id185_pre[,118]),
#                                  fit_aci_id185_pre[[2]][1,1],
#                                  fit_aci_id185_pre[[2]][1,2],
#                                  fit_aci_id185_pre[[2]][2,1],
#                                  fit_aci_id185_pre[[2]][2,2],
#                                  fit_aci_id185_pre[[2]][3,1],
#                                  fit_aci_id185_pre[[2]][3,2],
#                                  # fit_aci_id185_pre[[2]][4,1],
#                                  # fit_aci_id185_pre[[2]][4,2],
#                                  fit_aci_id185_pre$RMSE,
#                                  fit_aci_id185_pre$Ci_transition,
#                                  fit_aci_id185_pre$citransition,
#                                  fit_aci_id185_pre$Km,
#                                  fit_aci_id185_pre$GammaStar,
#                                  fit_aci_id185_pre$fitmethod,
#                                  fit_aci_id185_pre$Tcorrect,
#                                  fit_aci_id185_pre$fitTPU)
# colnames(aci_data_id185_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id185_pre_data)
# 
# ### plant id185 post_heatwave
# aci_data_id185_post = subset(aci_data, id == ids[49] & meas.type == 'post_heatwave')
# aci_data_id185_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id185_post)
# #### fit aci curve
# fit_aci_id185_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id185_post)
# summary(fit_aci_id185_post)
# #### plot
# plot(fit_aci_id185_post)
# #### add to dataframe
# aci_data_id185_post_data <- cbind(aci_data_id185_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id185_post[,30]),
#                                   mean(aci_data_id185_post[,118]),
#                                   fit_aci_id185_post[[2]][1,1],
#                                   fit_aci_id185_post[[2]][1,2],
#                                   fit_aci_id185_post[[2]][2,1],
#                                   fit_aci_id185_post[[2]][2,2],
#                                   fit_aci_id185_post[[2]][3,1],
#                                   fit_aci_id185_post[[2]][3,2],
#                                   # fit_aci_id185_post[[2]][4,1],
#                                   # fit_aci_id185_post[[2]][4,2],
#                                   fit_aci_id185_post$RMSE,
#                                   fit_aci_id185_post$Ci_transition,
#                                   fit_aci_id185_post$citransition,
#                                   fit_aci_id185_post$Km,
#                                   fit_aci_id185_post$GammaStar,
#                                   fit_aci_id185_post$fitmethod,
#                                   fit_aci_id185_post$Tcorrect,
#                                   fit_aci_id185_post$fitTPU)
# colnames(aci_data_id185_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id185_post_data)

##############################################################################
##############################################################################
###########id187
##############################################################################
##############################################################################
### plant id187 pre_heatwave
aci_data_id187_pre = subset(aci_data, id == ids[50] & meas.type == 'pre_heatwave')
aci_data_id187_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id187_pre)
#### fit aci curve
fit_aci_id187_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id187_pre)
summary(fit_aci_id187_pre)
#### plot
plot(fit_aci_id187_pre)
#### add to dataframe
aci_data_id187_pre_data <- cbind(aci_data_id187_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id187_pre[,30]),
                                 mean(aci_data_id187_pre[,118]),
                                 fit_aci_id187_pre[[2]][1,1],
                                 fit_aci_id187_pre[[2]][1,2],
                                 fit_aci_id187_pre[[2]][2,1],
                                 fit_aci_id187_pre[[2]][2,2],
                                 fit_aci_id187_pre[[2]][3,1],
                                 fit_aci_id187_pre[[2]][3,2],
                                 # fit_aci_id187_pre[[2]][4,1],
                                 # fit_aci_id187_pre[[2]][4,2],
                                 fit_aci_id187_pre$RMSE,
                                 fit_aci_id187_pre$Ci_transition,
                                 fit_aci_id187_pre$citransition,
                                 fit_aci_id187_pre$Km,
                                 fit_aci_id187_pre$GammaStar,
                                 fit_aci_id187_pre$fitmethod,
                                 fit_aci_id187_pre$Tcorrect,
                                 fit_aci_id187_pre$fitTPU)
colnames(aci_data_id187_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id187_pre_data)

### plant id187 post_heatwave
aci_data_id187_post = subset(aci_data, id == ids[50] & meas.type == 'post_heatwave' & Ci < 550)
aci_data_id187_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id187_post)
#### fit aci curve
fit_aci_id187_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id187_post)
summary(fit_aci_id187_post)
#### plot
plot(fit_aci_id187_post)
#### add to dataframe
aci_data_id187_post_data <- cbind(aci_data_id187_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id187_post[,30]),
                                  mean(aci_data_id187_post[,118]),
                                  fit_aci_id187_post[[2]][1,1],
                                  fit_aci_id187_post[[2]][1,2],
                                  fit_aci_id187_post[[2]][2,1],
                                  fit_aci_id187_post[[2]][2,2],
                                  fit_aci_id187_post[[2]][3,1],
                                  fit_aci_id187_post[[2]][3,2],
                                  # fit_aci_id187_post[[2]][4,1],
                                  # fit_aci_id187_post[[2]][4,2],
                                  fit_aci_id187_post$RMSE,
                                  fit_aci_id187_post$Ci_transition,
                                  fit_aci_id187_post$citransition,
                                  fit_aci_id187_post$Km,
                                  fit_aci_id187_post$GammaStar,
                                  fit_aci_id187_post$fitmethod,
                                  fit_aci_id187_post$Tcorrect,
                                  fit_aci_id187_post$fitTPU)
colnames(aci_data_id187_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id187_post_data)

##############################################################################
##############################################################################
###########id188
##############################################################################
##############################################################################
### plant id188 pre_heatwave
# aci_data_id188_pre = subset(aci_data, id == ids[51] & meas.type == 'pre_heatwave')
# aci_data_id188_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id188_pre)
# #### fit aci curve
# fit_aci_id188_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id188_pre)
# summary(fit_aci_id188_pre)
# #### plot
# plot(fit_aci_id188_pre)
# #### add to dataframe
# aci_data_id188_pre_data <- cbind(aci_data_id188_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id188_pre[,30]),
#                                  mean(aci_data_id188_pre[,118]),
#                                  fit_aci_id188_pre[[2]][1,1],
#                                  fit_aci_id188_pre[[2]][1,2],
#                                  fit_aci_id188_pre[[2]][2,1],
#                                  fit_aci_id188_pre[[2]][2,2],
#                                  fit_aci_id188_pre[[2]][3,1],
#                                  fit_aci_id188_pre[[2]][3,2],
#                                  # fit_aci_id188_pre[[2]][4,1],
#                                  # fit_aci_id188_pre[[2]][4,2],
#                                  fit_aci_id188_pre$RMSE,
#                                  fit_aci_id188_pre$Ci_transition,
#                                  fit_aci_id188_pre$citransition,
#                                  fit_aci_id188_pre$Km,
#                                  fit_aci_id188_pre$GammaStar,
#                                  fit_aci_id188_pre$fitmethod,
#                                  fit_aci_id188_pre$Tcorrect,
#                                  fit_aci_id188_pre$fitTPU)
# colnames(aci_data_id188_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id188_pre_data)
# 
# ### plant id188 post_heatwave
# aci_data_id188_post = subset(aci_data, id == ids[51] & meas.type == 'post_heatwave')
# aci_data_id188_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id188_post)
# #### fit aci curve
# fit_aci_id188_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id188_post)
# summary(fit_aci_id188_post)
# #### plot
# plot(fit_aci_id188_post)
# #### add to dataframe
# aci_data_id188_post_data <- cbind(aci_data_id188_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id188_post[,30]),
#                                   mean(aci_data_id188_post[,118]),
#                                   fit_aci_id188_post[[2]][1,1],
#                                   fit_aci_id188_post[[2]][1,2],
#                                   fit_aci_id188_post[[2]][2,1],
#                                   fit_aci_id188_post[[2]][2,2],
#                                   fit_aci_id188_post[[2]][3,1],
#                                   fit_aci_id188_post[[2]][3,2],
#                                   # fit_aci_id188_post[[2]][4,1],
#                                   # fit_aci_id188_post[[2]][4,2],
#                                   fit_aci_id188_post$RMSE,
#                                   fit_aci_id188_post$Ci_transition,
#                                   fit_aci_id188_post$citransition,
#                                   fit_aci_id188_post$Km,
#                                   fit_aci_id188_post$GammaStar,
#                                   fit_aci_id188_post$fitmethod,
#                                   fit_aci_id188_post$Tcorrect,
#                                   fit_aci_id188_post$fitTPU)
# colnames(aci_data_id188_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id188_post_data)

##############################################################################
##############################################################################
###########id189
##############################################################################
##############################################################################
### plant id189 pre_heatwave
aci_data_id189_pre = subset(aci_data, id == ids[52] & meas.type == 'pre_heatwave')
aci_data_id189_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id189_pre)
#### fit aci curve
fit_aci_id189_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id189_pre)
summary(fit_aci_id189_pre)
#### plot
plot(fit_aci_id189_pre)
#### add to dataframe
aci_data_id189_pre_data <- cbind(aci_data_id189_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id189_pre[,30]),
                                 mean(aci_data_id189_pre[,118]),
                                 fit_aci_id189_pre[[2]][1,1],
                                 fit_aci_id189_pre[[2]][1,2],
                                 fit_aci_id189_pre[[2]][2,1],
                                 fit_aci_id189_pre[[2]][2,2],
                                 fit_aci_id189_pre[[2]][3,1],
                                 fit_aci_id189_pre[[2]][3,2],
                                 # fit_aci_id189_pre[[2]][4,1],
                                 # fit_aci_id189_pre[[2]][4,2],
                                 fit_aci_id189_pre$RMSE,
                                 fit_aci_id189_pre$Ci_transition,
                                 fit_aci_id189_pre$citransition,
                                 fit_aci_id189_pre$Km,
                                 fit_aci_id189_pre$GammaStar,
                                 fit_aci_id189_pre$fitmethod,
                                 fit_aci_id189_pre$Tcorrect,
                                 fit_aci_id189_pre$fitTPU)
colnames(aci_data_id189_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id189_pre_data)

### plant id189 post_heatwave
aci_data_id189_post = subset(aci_data, id == ids[52] & meas.type == 'post_heatwave')
aci_data_id189_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id189_post)
#### fit aci curve
fit_aci_id189_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id189_post)
summary(fit_aci_id189_post)
#### plot
plot(fit_aci_id189_post)
#### add to dataframe
aci_data_id189_post_data <- cbind(aci_data_id189_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id189_post[,30]),
                                  mean(aci_data_id189_post[,118]),
                                  fit_aci_id189_post[[2]][1,1],
                                  fit_aci_id189_post[[2]][1,2],
                                  fit_aci_id189_post[[2]][2,1],
                                  fit_aci_id189_post[[2]][2,2],
                                  fit_aci_id189_post[[2]][3,1],
                                  fit_aci_id189_post[[2]][3,2],
                                  # fit_aci_id189_post[[2]][4,1],
                                  # fit_aci_id189_post[[2]][4,2],
                                  fit_aci_id189_post$RMSE,
                                  fit_aci_id189_post$Ci_transition,
                                  fit_aci_id189_post$citransition,
                                  fit_aci_id189_post$Km,
                                  fit_aci_id189_post$GammaStar,
                                  fit_aci_id189_post$fitmethod,
                                  fit_aci_id189_post$Tcorrect,
                                  fit_aci_id189_post$fitTPU)
colnames(aci_data_id189_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id189_post_data)

##############################################################################
##############################################################################
###########id192
##############################################################################
##############################################################################
### plant id192 pre_heatwave
aci_data_id192_pre = subset(aci_data, id == ids[53] & meas.type == 'pre_heatwave')
aci_data_id192_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id192_pre)
#### fit aci curve
fit_aci_id192_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id192_pre)
summary(fit_aci_id192_pre)
#### plot
plot(fit_aci_id192_pre)
#### add to dataframe
aci_data_id192_pre_data <- cbind(aci_data_id192_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id192_pre[,30]),
                                 mean(aci_data_id192_pre[,118]),
                                 fit_aci_id192_pre[[2]][1,1],
                                 fit_aci_id192_pre[[2]][1,2],
                                 fit_aci_id192_pre[[2]][2,1],
                                 fit_aci_id192_pre[[2]][2,2],
                                 fit_aci_id192_pre[[2]][3,1],
                                 fit_aci_id192_pre[[2]][3,2],
                                 # fit_aci_id192_pre[[2]][4,1],
                                 # fit_aci_id192_pre[[2]][4,2],
                                 fit_aci_id192_pre$RMSE,
                                 fit_aci_id192_pre$Ci_transition,
                                 fit_aci_id192_pre$citransition,
                                 fit_aci_id192_pre$Km,
                                 fit_aci_id192_pre$GammaStar,
                                 fit_aci_id192_pre$fitmethod,
                                 fit_aci_id192_pre$Tcorrect,
                                 fit_aci_id192_pre$fitTPU)
colnames(aci_data_id192_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id192_pre_data)

### plant id192 post_heatwave
aci_data_id192_post = subset(aci_data, id == ids[54] & meas.type == 'post_heatwave')
aci_data_id192_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id192_post)
#### fit aci curve
fit_aci_id192_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id192_post)
summary(fit_aci_id192_post)
#### plot
plot(fit_aci_id192_post)
#### add to dataframe
aci_data_id192_post_data <- cbind(aci_data_id192_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id192_post[,30]),
                                  mean(aci_data_id192_post[,118]),
                                  fit_aci_id192_post[[2]][1,1],
                                  fit_aci_id192_post[[2]][1,2],
                                  fit_aci_id192_post[[2]][2,1],
                                  fit_aci_id192_post[[2]][2,2],
                                  fit_aci_id192_post[[2]][3,1],
                                  fit_aci_id192_post[[2]][3,2],
                                  # fit_aci_id192_post[[2]][4,1],
                                  # fit_aci_id192_post[[2]][4,2],
                                  fit_aci_id192_post$RMSE,
                                  fit_aci_id192_post$Ci_transition,
                                  fit_aci_id192_post$citransition,
                                  fit_aci_id192_post$Km,
                                  fit_aci_id192_post$GammaStar,
                                  fit_aci_id192_post$fitmethod,
                                  fit_aci_id192_post$Tcorrect,
                                  fit_aci_id192_post$fitTPU)
colnames(aci_data_id192_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id192_post_data)

##############################################################################
##############################################################################
###########id194
##############################################################################
##############################################################################
### plant id194 pre_heatwave
aci_data_id194_pre = subset(aci_data, id == ids[55] & meas.type == 'pre_heatwave')
aci_data_id194_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id194_pre)
#### fit aci curve
fit_aci_id194_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id194_pre)
summary(fit_aci_id194_pre)
#### plot
plot(fit_aci_id194_pre)
#### add to dataframe
aci_data_id194_pre_data <- cbind(aci_data_id194_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id194_pre[,30]),
                                 mean(aci_data_id194_pre[,118]),
                                 fit_aci_id194_pre[[2]][1,1],
                                 fit_aci_id194_pre[[2]][1,2],
                                 fit_aci_id194_pre[[2]][2,1],
                                 fit_aci_id194_pre[[2]][2,2],
                                 fit_aci_id194_pre[[2]][3,1],
                                 fit_aci_id194_pre[[2]][3,2],
                                 # fit_aci_id194_pre[[2]][4,1],
                                 # fit_aci_id194_pre[[2]][4,2],
                                 fit_aci_id194_pre$RMSE,
                                 fit_aci_id194_pre$Ci_transition,
                                 fit_aci_id194_pre$citransition,
                                 fit_aci_id194_pre$Km,
                                 fit_aci_id194_pre$GammaStar,
                                 fit_aci_id194_pre$fitmethod,
                                 fit_aci_id194_pre$Tcorrect,
                                 fit_aci_id194_pre$fitTPU)
colnames(aci_data_id194_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id194_pre_data)

### plant id194 post_heatwave
aci_data_id194_post = subset(aci_data, id == ids[55] & meas.type == 'post_heatwave')
aci_data_id194_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id194_post)
#### fit aci curve
fit_aci_id194_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id194_post)
summary(fit_aci_id194_post)
#### plot
plot(fit_aci_id194_post)
#### add to dataframe
aci_data_id194_post_data <- cbind(aci_data_id194_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id194_post[,30]),
                                  mean(aci_data_id194_post[,118]),
                                  fit_aci_id194_post[[2]][1,1],
                                  fit_aci_id194_post[[2]][1,2],
                                  fit_aci_id194_post[[2]][2,1],
                                  fit_aci_id194_post[[2]][2,2],
                                  fit_aci_id194_post[[2]][3,1],
                                  fit_aci_id194_post[[2]][3,2],
                                  # fit_aci_id194_post[[2]][4,1],
                                  # fit_aci_id194_post[[2]][4,2],
                                  fit_aci_id194_post$RMSE,
                                  fit_aci_id194_post$Ci_transition,
                                  fit_aci_id194_post$citransition,
                                  fit_aci_id194_post$Km,
                                  fit_aci_id194_post$GammaStar,
                                  fit_aci_id194_post$fitmethod,
                                  fit_aci_id194_post$Tcorrect,
                                  fit_aci_id194_post$fitTPU)
colnames(aci_data_id194_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id194_post_data)

##############################################################################
##############################################################################
###########id199
##############################################################################
##############################################################################
### plant id199 pre_heatwave
# aci_data_id199_pre = subset(aci_data, id == ids[56] & meas.type == 'pre_heatwave')
# aci_data_id199_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id199_pre)
# #### fit aci curve
# fit_aci_id199_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id199_pre)
# summary(fit_aci_id199_pre)
# #### plot
# plot(fit_aci_id199_pre)
# #### add to dataframe
# aci_data_id199_pre_data <- cbind(aci_data_id199_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id199_pre[,30]),
#                                  mean(aci_data_id199_pre[,118]),
#                                  fit_aci_id199_pre[[2]][1,1],
#                                  fit_aci_id199_pre[[2]][1,2],
#                                  fit_aci_id199_pre[[2]][2,1],
#                                  fit_aci_id199_pre[[2]][2,2],
#                                  fit_aci_id199_pre[[2]][3,1],
#                                  fit_aci_id199_pre[[2]][3,2],
#                                  # fit_aci_id199_pre[[2]][4,1],
#                                  # fit_aci_id199_pre[[2]][4,2],
#                                  fit_aci_id199_pre$RMSE,
#                                  fit_aci_id199_pre$Ci_transition,
#                                  fit_aci_id199_pre$citransition,
#                                  fit_aci_id199_pre$Km,
#                                  fit_aci_id199_pre$GammaStar,
#                                  fit_aci_id199_pre$fitmethod,
#                                  fit_aci_id199_pre$Tcorrect,
#                                  fit_aci_id199_pre$fitTPU)
# colnames(aci_data_id199_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id199_pre_data)
# 
# ### plant id199 post_heatwave
# aci_data_id199_post = subset(aci_data, id == ids[56] & meas.type == 'post_heatwave')
# aci_data_id199_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id199_post)
# #### fit aci curve
# fit_aci_id199_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id199_post)
# summary(fit_aci_id199_post)
# #### plot
# plot(fit_aci_id199_post)
# #### add to dataframe
# aci_data_id199_post_data <- cbind(aci_data_id199_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id199_post[,30]),
#                                   mean(aci_data_id199_post[,118]),
#                                   fit_aci_id199_post[[2]][1,1],
#                                   fit_aci_id199_post[[2]][1,2],
#                                   fit_aci_id199_post[[2]][2,1],
#                                   fit_aci_id199_post[[2]][2,2],
#                                   fit_aci_id199_post[[2]][3,1],
#                                   fit_aci_id199_post[[2]][3,2],
#                                   # fit_aci_id199_post[[2]][4,1],
#                                   # fit_aci_id199_post[[2]][4,2],
#                                   fit_aci_id199_post$RMSE,
#                                   fit_aci_id199_post$Ci_transition,
#                                   fit_aci_id199_post$citransition,
#                                   fit_aci_id199_post$Km,
#                                   fit_aci_id199_post$GammaStar,
#                                   fit_aci_id199_post$fitmethod,
#                                   fit_aci_id199_post$Tcorrect,
#                                   fit_aci_id199_post$fitTPU)
# colnames(aci_data_id199_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id199_post_data)

##############################################################################
##############################################################################
###########id2
##############################################################################
##############################################################################
### plant id2 pre_heatwave
aci_data_id2_pre = subset(aci_data, id == ids[57] & meas.type == 'pre_heatwave')
aci_data_id2_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id2_pre)
#### fit aci curve
fit_aci_id2_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id2_pre)
summary(fit_aci_id2_pre)
#### plot
plot(fit_aci_id2_pre)
#### add to dataframe
aci_data_id2_pre_data <- cbind(aci_data_id2_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id2_pre[,30]),
                                 mean(aci_data_id2_pre[,118]),
                                 fit_aci_id2_pre[[2]][1,1],
                                 fit_aci_id2_pre[[2]][1,2],
                                 fit_aci_id2_pre[[2]][2,1],
                                 fit_aci_id2_pre[[2]][2,2],
                                 fit_aci_id2_pre[[2]][3,1],
                                 fit_aci_id2_pre[[2]][3,2],
                                 # fit_aci_id2_pre[[2]][4,1],
                                 # fit_aci_id2_pre[[2]][4,2],
                                 fit_aci_id2_pre$RMSE,
                                 fit_aci_id2_pre$Ci_transition,
                                 fit_aci_id2_pre$citransition,
                                 fit_aci_id2_pre$Km,
                                 fit_aci_id2_pre$GammaStar,
                                 fit_aci_id2_pre$fitmethod,
                                 fit_aci_id2_pre$Tcorrect,
                                 fit_aci_id2_pre$fitTPU)
colnames(aci_data_id2_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id2_pre_data)

### plant id2 post_heatwave
aci_data_id2_post = subset(aci_data, id == ids[57] & meas.type == 'post_heatwave')
aci_data_id2_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id2_post)
#### fit aci curve
fit_aci_id2_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id2_post)
summary(fit_aci_id2_post)
#### plot
plot(fit_aci_id2_post)
#### add to dataframe
aci_data_id2_post_data <- cbind(aci_data_id2_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id2_post[,30]),
                                  mean(aci_data_id2_post[,118]),
                                  fit_aci_id2_post[[2]][1,1],
                                  fit_aci_id2_post[[2]][1,2],
                                  fit_aci_id2_post[[2]][2,1],
                                  fit_aci_id2_post[[2]][2,2],
                                  fit_aci_id2_post[[2]][3,1],
                                  fit_aci_id2_post[[2]][3,2],
                                  # fit_aci_id2_post[[2]][4,1],
                                  # fit_aci_id2_post[[2]][4,2],
                                  fit_aci_id2_post$RMSE,
                                  fit_aci_id2_post$Ci_transition,
                                  fit_aci_id2_post$citransition,
                                  fit_aci_id2_post$Km,
                                  fit_aci_id2_post$GammaStar,
                                  fit_aci_id2_post$fitmethod,
                                  fit_aci_id2_post$Tcorrect,
                                  fit_aci_id2_post$fitTPU)
colnames(aci_data_id2_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id2_post_data)

##############################################################################
##############################################################################
###########id20
##############################################################################
##############################################################################
### plant id20 pre_heatwave
aci_data_id20_pre = subset(aci_data, id == ids[58] & meas.type == 'pre_heatwave')
aci_data_id20_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id20_pre)
#### fit aci curve
fit_aci_id20_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id20_pre)
summary(fit_aci_id20_pre)
#### plot
plot(fit_aci_id20_pre)
#### add to dataframe
aci_data_id20_pre_data <- cbind(aci_data_id20_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id20_pre[,30]),
                                 mean(aci_data_id20_pre[,118]),
                                 fit_aci_id20_pre[[2]][1,1],
                                 fit_aci_id20_pre[[2]][1,2],
                                 fit_aci_id20_pre[[2]][2,1],
                                 fit_aci_id20_pre[[2]][2,2],
                                 fit_aci_id20_pre[[2]][3,1],
                                 fit_aci_id20_pre[[2]][3,2],
                                 # fit_aci_id20_pre[[2]][4,1],
                                 # fit_aci_id20_pre[[2]][4,2],
                                 fit_aci_id20_pre$RMSE,
                                 fit_aci_id20_pre$Ci_transition,
                                 fit_aci_id20_pre$citransition,
                                 fit_aci_id20_pre$Km,
                                 fit_aci_id20_pre$GammaStar,
                                 fit_aci_id20_pre$fitmethod,
                                 fit_aci_id20_pre$Tcorrect,
                                 fit_aci_id20_pre$fitTPU)
colnames(aci_data_id20_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id20_pre_data)

### plant id20 post_heatwave
aci_data_id20_post = subset(aci_data, id == ids[58] & meas.type == 'post_heatwave')
aci_data_id20_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id20_post)
#### fit aci curve
fit_aci_id20_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id20_post)
summary(fit_aci_id20_post)
#### plot
plot(fit_aci_id20_post)
#### add to dataframe
aci_data_id20_post_data <- cbind(aci_data_id20_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id20_post[,30]),
                                  mean(aci_data_id20_post[,118]),
                                  fit_aci_id20_post[[2]][1,1],
                                  fit_aci_id20_post[[2]][1,2],
                                  fit_aci_id20_post[[2]][2,1],
                                  fit_aci_id20_post[[2]][2,2],
                                  fit_aci_id20_post[[2]][3,1],
                                  fit_aci_id20_post[[2]][3,2],
                                  # fit_aci_id20_post[[2]][4,1],
                                  # fit_aci_id20_post[[2]][4,2],
                                  fit_aci_id20_post$RMSE,
                                  fit_aci_id20_post$Ci_transition,
                                  fit_aci_id20_post$citransition,
                                  fit_aci_id20_post$Km,
                                  fit_aci_id20_post$GammaStar,
                                  fit_aci_id20_post$fitmethod,
                                  fit_aci_id20_post$Tcorrect,
                                  fit_aci_id20_post$fitTPU)
colnames(aci_data_id20_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id20_post_data)

##############################################################################
##############################################################################
###########id21
##############################################################################
##############################################################################
### plant id21 pre_heatwave
# aci_data_id21_pre = subset(aci_data, id == ids[59] & meas.type == 'pre_heatwave')
# aci_data_id21_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id21_pre)
# #### fit aci curve
# fit_aci_id21_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id21_pre)
# summary(fit_aci_id21_pre)
# #### plot
# plot(fit_aci_id21_pre)
# #### add to dataframe
# aci_data_id21_pre_data <- cbind(aci_data_id21_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id21_pre[,30]),
#                                  mean(aci_data_id21_pre[,118]),
#                                  fit_aci_id21_pre[[2]][1,1],
#                                  fit_aci_id21_pre[[2]][1,2],
#                                  fit_aci_id21_pre[[2]][2,1],
#                                  fit_aci_id21_pre[[2]][2,2],
#                                  fit_aci_id21_pre[[2]][3,1],
#                                  fit_aci_id21_pre[[2]][3,2],
#                                  # fit_aci_id21_pre[[2]][4,1],
#                                  # fit_aci_id21_pre[[2]][4,2],
#                                  fit_aci_id21_pre$RMSE,
#                                  fit_aci_id21_pre$Ci_transition,
#                                  fit_aci_id21_pre$citransition,
#                                  fit_aci_id21_pre$Km,
#                                  fit_aci_id21_pre$GammaStar,
#                                  fit_aci_id21_pre$fitmethod,
#                                  fit_aci_id21_pre$Tcorrect,
#                                  fit_aci_id21_pre$fitTPU)
# colnames(aci_data_id21_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id21_pre_data)
# 
# ### plant id21 post_heatwave
# aci_data_id21_post = subset(aci_data, id == ids[59] & meas.type == 'post_heatwave')
# aci_data_id21_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id21_post)
# #### fit aci curve
# fit_aci_id21_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id21_post)
# summary(fit_aci_id21_post)
# #### plot
# plot(fit_aci_id21_post)
# #### add to dataframe
# aci_data_id21_post_data <- cbind(aci_data_id21_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id21_post[,30]),
#                                   mean(aci_data_id21_post[,118]),
#                                   fit_aci_id21_post[[2]][1,1],
#                                   fit_aci_id21_post[[2]][1,2],
#                                   fit_aci_id21_post[[2]][2,1],
#                                   fit_aci_id21_post[[2]][2,2],
#                                   fit_aci_id21_post[[2]][3,1],
#                                   fit_aci_id21_post[[2]][3,2],
#                                   # fit_aci_id21_post[[2]][4,1],
#                                   # fit_aci_id21_post[[2]][4,2],
#                                   fit_aci_id21_post$RMSE,
#                                   fit_aci_id21_post$Ci_transition,
#                                   fit_aci_id21_post$citransition,
#                                   fit_aci_id21_post$Km,
#                                   fit_aci_id21_post$GammaStar,
#                                   fit_aci_id21_post$fitmethod,
#                                   fit_aci_id21_post$Tcorrect,
#                                   fit_aci_id21_post$fitTPU)
# colnames(aci_data_id21_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id21_post_data)

##############################################################################
##############################################################################
###########id22
##############################################################################
##############################################################################
### plant id22 pre_heatwave
aci_data_id22_pre = subset(aci_data, id == ids[60] & meas.type == 'pre_heatwave')
aci_data_id22_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id22_pre)
#### fit aci curve
fit_aci_id22_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id22_pre)
summary(fit_aci_id22_pre)
#### plot
plot(fit_aci_id22_pre)
#### add to dataframe
aci_data_id22_pre_data <- cbind(aci_data_id22_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id22_pre[,30]),
                                 mean(aci_data_id22_pre[,118]),
                                 fit_aci_id22_pre[[2]][1,1],
                                 fit_aci_id22_pre[[2]][1,2],
                                 fit_aci_id22_pre[[2]][2,1],
                                 fit_aci_id22_pre[[2]][2,2],
                                 fit_aci_id22_pre[[2]][3,1],
                                 fit_aci_id22_pre[[2]][3,2],
                                 # fit_aci_id22_pre[[2]][4,1],
                                 # fit_aci_id22_pre[[2]][4,2],
                                 fit_aci_id22_pre$RMSE,
                                 fit_aci_id22_pre$Ci_transition,
                                 fit_aci_id22_pre$citransition,
                                 fit_aci_id22_pre$Km,
                                 fit_aci_id22_pre$GammaStar,
                                 fit_aci_id22_pre$fitmethod,
                                 fit_aci_id22_pre$Tcorrect,
                                 fit_aci_id22_pre$fitTPU)
colnames(aci_data_id22_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id22_pre_data)

### plant id22 post_heatwave
aci_data_id22_post = subset(aci_data, id == ids[61] & meas.type == 'post_heatwave')
aci_data_id22_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id22_post)
#### fit aci curve
fit_aci_id22_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id22_post)
summary(fit_aci_id22_post)
#### plot
plot(fit_aci_id22_post)
#### add to dataframe
aci_data_id22_post_data <- cbind(aci_data_id22_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id22_post[,30]),
                                  mean(aci_data_id22_post[,118]),
                                  fit_aci_id22_post[[2]][1,1],
                                  fit_aci_id22_post[[2]][1,2],
                                  fit_aci_id22_post[[2]][2,1],
                                  fit_aci_id22_post[[2]][2,2],
                                  fit_aci_id22_post[[2]][3,1],
                                  fit_aci_id22_post[[2]][3,2],
                                  # fit_aci_id22_post[[2]][4,1],
                                  # fit_aci_id22_post[[2]][4,2],
                                  fit_aci_id22_post$RMSE,
                                  fit_aci_id22_post$Ci_transition,
                                  fit_aci_id22_post$citransition,
                                  fit_aci_id22_post$Km,
                                  fit_aci_id22_post$GammaStar,
                                  fit_aci_id22_post$fitmethod,
                                  fit_aci_id22_post$Tcorrect,
                                  fit_aci_id22_post$fitTPU)
colnames(aci_data_id22_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id22_post_data)

##############################################################################
##############################################################################
###########id23
##############################################################################
##############################################################################
### plant id23 pre_heatwave
aci_data_id23_pre = subset(aci_data, id == ids[62] & meas.type == 'pre_heatwave' & Ci < 900)
aci_data_id23_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id23_pre)
#### fit aci curve
fit_aci_id23_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id23_pre)
summary(fit_aci_id23_pre)
#### plot
plot(fit_aci_id23_pre)
#### add to dataframe
aci_data_id23_pre_data <- cbind(aci_data_id23_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id23_pre[,30]),
                                 mean(aci_data_id23_pre[,118]),
                                 fit_aci_id23_pre[[2]][1,1],
                                 fit_aci_id23_pre[[2]][1,2],
                                 fit_aci_id23_pre[[2]][2,1],
                                 fit_aci_id23_pre[[2]][2,2],
                                 fit_aci_id23_pre[[2]][3,1],
                                 fit_aci_id23_pre[[2]][3,2],
                                 # fit_aci_id23_pre[[2]][4,1],
                                 # fit_aci_id23_pre[[2]][4,2],
                                 fit_aci_id23_pre$RMSE,
                                 fit_aci_id23_pre$Ci_transition,
                                 fit_aci_id23_pre$citransition,
                                 fit_aci_id23_pre$Km,
                                 fit_aci_id23_pre$GammaStar,
                                 fit_aci_id23_pre$fitmethod,
                                 fit_aci_id23_pre$Tcorrect,
                                 fit_aci_id23_pre$fitTPU)
colnames(aci_data_id23_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id23_pre_data)

### plant id23 post_heatwave
aci_data_id23_post = subset(aci_data, id == ids[62] & meas.type == 'post_heatwave')
aci_data_id23_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id23_post)
#### fit aci curve
fit_aci_id23_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id23_post)
summary(fit_aci_id23_post)
#### plot
plot(fit_aci_id23_post)
#### add to dataframe
aci_data_id23_post_data <- cbind(aci_data_id23_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id23_post[,30]),
                                  mean(aci_data_id23_post[,118]),
                                  fit_aci_id23_post[[2]][1,1],
                                  fit_aci_id23_post[[2]][1,2],
                                  fit_aci_id23_post[[2]][2,1],
                                  fit_aci_id23_post[[2]][2,2],
                                  fit_aci_id23_post[[2]][3,1],
                                  fit_aci_id23_post[[2]][3,2],
                                  # fit_aci_id23_post[[2]][4,1],
                                  # fit_aci_id23_post[[2]][4,2],
                                  fit_aci_id23_post$RMSE,
                                  fit_aci_id23_post$Ci_transition,
                                  fit_aci_id23_post$citransition,
                                  fit_aci_id23_post$Km,
                                  fit_aci_id23_post$GammaStar,
                                  fit_aci_id23_post$fitmethod,
                                  fit_aci_id23_post$Tcorrect,
                                  fit_aci_id23_post$fitTPU)
colnames(aci_data_id23_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id23_post_data)

##############################################################################
##############################################################################
###########id30
##############################################################################
##############################################################################
### plant id30 pre_heatwave
aci_data_id30_pre = subset(aci_data, id == ids[63] & meas.type == 'pre_heatwave')
aci_data_id30_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id30_pre)
#### fit aci curve
fit_aci_id30_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id30_pre)
summary(fit_aci_id30_pre)
#### plot
plot(fit_aci_id30_pre)
#### add to dataframe
aci_data_id30_pre_data <- cbind(aci_data_id30_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id30_pre[,30]),
                                 mean(aci_data_id30_pre[,118]),
                                 fit_aci_id30_pre[[2]][1,1],
                                 fit_aci_id30_pre[[2]][1,2],
                                 fit_aci_id30_pre[[2]][2,1],
                                 fit_aci_id30_pre[[2]][2,2],
                                 fit_aci_id30_pre[[2]][3,1],
                                 fit_aci_id30_pre[[2]][3,2],
                                 # fit_aci_id30_pre[[2]][4,1],
                                 # fit_aci_id30_pre[[2]][4,2],
                                 fit_aci_id30_pre$RMSE,
                                 fit_aci_id30_pre$Ci_transition,
                                 fit_aci_id30_pre$citransition,
                                 fit_aci_id30_pre$Km,
                                 fit_aci_id30_pre$GammaStar,
                                 fit_aci_id30_pre$fitmethod,
                                 fit_aci_id30_pre$Tcorrect,
                                 fit_aci_id30_pre$fitTPU)
colnames(aci_data_id30_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id30_pre_data)

### plant id30 post_heatwave
aci_data_id30_post = subset(aci_data, id == ids[63] & meas.type == 'post_heatwave')
aci_data_id30_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id30_post)
#### fit aci curve
fit_aci_id30_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id30_post)
summary(fit_aci_id30_post)
#### plot
plot(fit_aci_id30_post)
#### add to dataframe
aci_data_id30_post_data <- cbind(aci_data_id30_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id30_post[,30]),
                                  mean(aci_data_id30_post[,118]),
                                  fit_aci_id30_post[[2]][1,1],
                                  fit_aci_id30_post[[2]][1,2],
                                  fit_aci_id30_post[[2]][2,1],
                                  fit_aci_id30_post[[2]][2,2],
                                  fit_aci_id30_post[[2]][3,1],
                                  fit_aci_id30_post[[2]][3,2],
                                  # fit_aci_id30_post[[2]][4,1],
                                  # fit_aci_id30_post[[2]][4,2],
                                  fit_aci_id30_post$RMSE,
                                  fit_aci_id30_post$Ci_transition,
                                  fit_aci_id30_post$citransition,
                                  fit_aci_id30_post$Km,
                                  fit_aci_id30_post$GammaStar,
                                  fit_aci_id30_post$fitmethod,
                                  fit_aci_id30_post$Tcorrect,
                                  fit_aci_id30_post$fitTPU)
colnames(aci_data_id30_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id30_post_data)

##############################################################################
##############################################################################
###########id36
##############################################################################
##############################################################################
### plant id36 pre_heatwave
# aci_data_id36_pre = subset(aci_data, id == ids[64] & meas.type == 'pre_heatwave')
# aci_data_id36_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id36_pre)
# #### fit aci curve
# fit_aci_id36_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id36_pre)
# summary(fit_aci_id36_pre)
# #### plot
# plot(fit_aci_id36_pre)
# #### add to dataframe
# aci_data_id36_pre_data <- cbind(aci_data_id36_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id36_pre[,30]),
#                                  mean(aci_data_id36_pre[,118]),
#                                  fit_aci_id36_pre[[2]][1,1],
#                                  fit_aci_id36_pre[[2]][1,2],
#                                  fit_aci_id36_pre[[2]][2,1],
#                                  fit_aci_id36_pre[[2]][2,2],
#                                  fit_aci_id36_pre[[2]][3,1],
#                                  fit_aci_id36_pre[[2]][3,2],
#                                  # fit_aci_id36_pre[[2]][4,1],
#                                  # fit_aci_id36_pre[[2]][4,2],
#                                  fit_aci_id36_pre$RMSE,
#                                  fit_aci_id36_pre$Ci_transition,
#                                  fit_aci_id36_pre$citransition,
#                                  fit_aci_id36_pre$Km,
#                                  fit_aci_id36_pre$GammaStar,
#                                  fit_aci_id36_pre$fitmethod,
#                                  fit_aci_id36_pre$Tcorrect,
#                                  fit_aci_id36_pre$fitTPU)
# colnames(aci_data_id36_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id36_pre_data)
# 
# ### plant id36 post_heatwave
# aci_data_id36_post = subset(aci_data, id == ids[64] & meas.type == 'post_heatwave')
# aci_data_id36_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id36_post)
# #### fit aci curve
# fit_aci_id36_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id36_post)
# summary(fit_aci_id36_post)
# #### plot
# plot(fit_aci_id36_post)
# #### add to dataframe
# aci_data_id36_post_data <- cbind(aci_data_id36_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id36_post[,30]),
#                                   mean(aci_data_id36_post[,118]),
#                                   fit_aci_id36_post[[2]][1,1],
#                                   fit_aci_id36_post[[2]][1,2],
#                                   fit_aci_id36_post[[2]][2,1],
#                                   fit_aci_id36_post[[2]][2,2],
#                                   fit_aci_id36_post[[2]][3,1],
#                                   fit_aci_id36_post[[2]][3,2],
#                                   # fit_aci_id36_post[[2]][4,1],
#                                   # fit_aci_id36_post[[2]][4,2],
#                                   fit_aci_id36_post$RMSE,
#                                   fit_aci_id36_post$Ci_transition,
#                                   fit_aci_id36_post$citransition,
#                                   fit_aci_id36_post$Km,
#                                   fit_aci_id36_post$GammaStar,
#                                   fit_aci_id36_post$fitmethod,
#                                   fit_aci_id36_post$Tcorrect,
#                                   fit_aci_id36_post$fitTPU)
# colnames(aci_data_id36_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id36_post_data)

##############################################################################
##############################################################################
###########id38
##############################################################################
##############################################################################
### plant id38 pre_heatwave
aci_data_id38_pre = subset(aci_data, id == ids[65] & meas.type == 'pre_heatwave')
aci_data_id38_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id38_pre)
#### fit aci curve
fit_aci_id38_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id38_pre)
summary(fit_aci_id38_pre)
#### plot
plot(fit_aci_id38_pre)
#### add to dataframe
aci_data_id38_pre_data <- cbind(aci_data_id38_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id38_pre[,30]),
                                 mean(aci_data_id38_pre[,118]),
                                 fit_aci_id38_pre[[2]][1,1],
                                 fit_aci_id38_pre[[2]][1,2],
                                 fit_aci_id38_pre[[2]][2,1],
                                 fit_aci_id38_pre[[2]][2,2],
                                 fit_aci_id38_pre[[2]][3,1],
                                 fit_aci_id38_pre[[2]][3,2],
                                 # fit_aci_id38_pre[[2]][4,1],
                                 # fit_aci_id38_pre[[2]][4,2],
                                 fit_aci_id38_pre$RMSE,
                                 fit_aci_id38_pre$Ci_transition,
                                 fit_aci_id38_pre$citransition,
                                 fit_aci_id38_pre$Km,
                                 fit_aci_id38_pre$GammaStar,
                                 fit_aci_id38_pre$fitmethod,
                                 fit_aci_id38_pre$Tcorrect,
                                 fit_aci_id38_pre$fitTPU)
colnames(aci_data_id38_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id38_pre_data)

### plant id38 post_heatwave
aci_data_id38_post = subset(aci_data, id == ids[65] & meas.type == 'post_heatwave')
aci_data_id38_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id38_post)
#### fit aci curve
fit_aci_id38_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id38_post)
summary(fit_aci_id38_post)
#### plot
plot(fit_aci_id38_post)
#### add to dataframe
aci_data_id38_post_data <- cbind(aci_data_id38_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id38_post[,30]),
                                  mean(aci_data_id38_post[,118]),
                                  fit_aci_id38_post[[2]][1,1],
                                  fit_aci_id38_post[[2]][1,2],
                                  fit_aci_id38_post[[2]][2,1],
                                  fit_aci_id38_post[[2]][2,2],
                                  fit_aci_id38_post[[2]][3,1],
                                  fit_aci_id38_post[[2]][3,2],
                                  # fit_aci_id38_post[[2]][4,1],
                                  # fit_aci_id38_post[[2]][4,2],
                                  fit_aci_id38_post$RMSE,
                                  fit_aci_id38_post$Ci_transition,
                                  fit_aci_id38_post$citransition,
                                  fit_aci_id38_post$Km,
                                  fit_aci_id38_post$GammaStar,
                                  fit_aci_id38_post$fitmethod,
                                  fit_aci_id38_post$Tcorrect,
                                  fit_aci_id38_post$fitTPU)
colnames(aci_data_id38_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id38_post_data)

##############################################################################
##############################################################################
###########id39
##############################################################################
##############################################################################
### plant id39 pre_heatwave
aci_data_id39_pre = subset(aci_data, id == ids[66] & meas.type == 'pre_heatwave' & Ci < 900)
aci_data_id39_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id39_pre)
#### fit aci curve
fit_aci_id39_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id39_pre)
summary(fit_aci_id39_pre)
#### plot
plot(fit_aci_id39_pre)
#### add to dataframe
aci_data_id39_pre_data <- cbind(aci_data_id39_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id39_pre[,30]),
                                 mean(aci_data_id39_pre[,118]),
                                 fit_aci_id39_pre[[2]][1,1],
                                 fit_aci_id39_pre[[2]][1,2],
                                 fit_aci_id39_pre[[2]][2,1],
                                 fit_aci_id39_pre[[2]][2,2],
                                 fit_aci_id39_pre[[2]][3,1],
                                 fit_aci_id39_pre[[2]][3,2],
                                 # fit_aci_id39_pre[[2]][4,1],
                                 # fit_aci_id39_pre[[2]][4,2],
                                 fit_aci_id39_pre$RMSE,
                                 fit_aci_id39_pre$Ci_transition,
                                 fit_aci_id39_pre$citransition,
                                 fit_aci_id39_pre$Km,
                                 fit_aci_id39_pre$GammaStar,
                                 fit_aci_id39_pre$fitmethod,
                                 fit_aci_id39_pre$Tcorrect,
                                 fit_aci_id39_pre$fitTPU)
colnames(aci_data_id39_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id39_pre_data)

### plant id39 post_heatwave
aci_data_id39_post = subset(aci_data, id == ids[66] & meas.type == 'post_heatwave')
aci_data_id39_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id39_post)
#### fit aci curve
fit_aci_id39_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id39_post)
summary(fit_aci_id39_post)
#### plot
plot(fit_aci_id39_post)
#### add to dataframe
aci_data_id39_post_data <- cbind(aci_data_id39_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id39_post[,30]),
                                  mean(aci_data_id39_post[,118]),
                                  fit_aci_id39_post[[2]][1,1],
                                  fit_aci_id39_post[[2]][1,2],
                                  fit_aci_id39_post[[2]][2,1],
                                  fit_aci_id39_post[[2]][2,2],
                                  fit_aci_id39_post[[2]][3,1],
                                  fit_aci_id39_post[[2]][3,2],
                                  # fit_aci_id39_post[[2]][4,1],
                                  # fit_aci_id39_post[[2]][4,2],
                                  fit_aci_id39_post$RMSE,
                                  fit_aci_id39_post$Ci_transition,
                                  fit_aci_id39_post$citransition,
                                  fit_aci_id39_post$Km,
                                  fit_aci_id39_post$GammaStar,
                                  fit_aci_id39_post$fitmethod,
                                  fit_aci_id39_post$Tcorrect,
                                  fit_aci_id39_post$fitTPU)
colnames(aci_data_id39_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id39_post_data)

##############################################################################
##############################################################################
###########id43
##############################################################################
##############################################################################
### plant id43 pre_heatwave
aci_data_id43_pre = subset(aci_data, id == ids[67] & meas.type == 'pre_heatwave')
aci_data_id43_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id43_pre)
#### fit aci curve
fit_aci_id43_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id43_pre)
summary(fit_aci_id43_pre)
#### plot
plot(fit_aci_id43_pre)
#### add to dataframe
aci_data_id43_pre_data <- cbind(aci_data_id43_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id43_pre[,30]),
                                 mean(aci_data_id43_pre[,118]),
                                 fit_aci_id43_pre[[2]][1,1],
                                 fit_aci_id43_pre[[2]][1,2],
                                 fit_aci_id43_pre[[2]][2,1],
                                 fit_aci_id43_pre[[2]][2,2],
                                 fit_aci_id43_pre[[2]][3,1],
                                 fit_aci_id43_pre[[2]][3,2],
                                 # fit_aci_id43_pre[[2]][4,1],
                                 # fit_aci_id43_pre[[2]][4,2],
                                 fit_aci_id43_pre$RMSE,
                                 fit_aci_id43_pre$Ci_transition,
                                 fit_aci_id43_pre$citransition,
                                 fit_aci_id43_pre$Km,
                                 fit_aci_id43_pre$GammaStar,
                                 fit_aci_id43_pre$fitmethod,
                                 fit_aci_id43_pre$Tcorrect,
                                 fit_aci_id43_pre$fitTPU)
colnames(aci_data_id43_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id43_pre_data)

### plant id43 post_heatwave
aci_data_id43_post = subset(aci_data, id == ids[67] & meas.type == 'post_heatwave' & Ci <800)
aci_data_id43_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id43_post)
#### fit aci curve
fit_aci_id43_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id43_post)
summary(fit_aci_id43_post)
#### plot
plot(fit_aci_id43_post)
#### add to dataframe
aci_data_id43_post_data <- cbind(aci_data_id43_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id43_post[,30]),
                                  mean(aci_data_id43_post[,118]),
                                  fit_aci_id43_post[[2]][1,1],
                                  fit_aci_id43_post[[2]][1,2],
                                  fit_aci_id43_post[[2]][2,1],
                                  fit_aci_id43_post[[2]][2,2],
                                  fit_aci_id43_post[[2]][3,1],
                                  fit_aci_id43_post[[2]][3,2],
                                  # fit_aci_id43_post[[2]][4,1],
                                  # fit_aci_id43_post[[2]][4,2],
                                  fit_aci_id43_post$RMSE,
                                  fit_aci_id43_post$Ci_transition,
                                  fit_aci_id43_post$citransition,
                                  fit_aci_id43_post$Km,
                                  fit_aci_id43_post$GammaStar,
                                  fit_aci_id43_post$fitmethod,
                                  fit_aci_id43_post$Tcorrect,
                                  fit_aci_id43_post$fitTPU)
colnames(aci_data_id43_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id43_post_data)

##############################################################################
##############################################################################
###########id50
##############################################################################
##############################################################################
### plant id50 pre_heatwave
aci_data_id50_pre = subset(aci_data, id == ids[68] & meas.type == 'pre_heatwave')
aci_data_id50_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id50_pre)
#### fit aci curve
fit_aci_id50_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id50_pre)
summary(fit_aci_id50_pre)
#### plot
plot(fit_aci_id50_pre)
#### add to dataframe
aci_data_id50_pre_data <- cbind(aci_data_id50_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id50_pre[,30]),
                                 mean(aci_data_id50_pre[,118]),
                                 fit_aci_id50_pre[[2]][1,1],
                                 fit_aci_id50_pre[[2]][1,2],
                                 fit_aci_id50_pre[[2]][2,1],
                                 fit_aci_id50_pre[[2]][2,2],
                                 fit_aci_id50_pre[[2]][3,1],
                                 fit_aci_id50_pre[[2]][3,2],
                                 # fit_aci_id50_pre[[2]][4,1],
                                 # fit_aci_id50_pre[[2]][4,2],
                                 fit_aci_id50_pre$RMSE,
                                 fit_aci_id50_pre$Ci_transition,
                                 fit_aci_id50_pre$citransition,
                                 fit_aci_id50_pre$Km,
                                 fit_aci_id50_pre$GammaStar,
                                 fit_aci_id50_pre$fitmethod,
                                 fit_aci_id50_pre$Tcorrect,
                                 fit_aci_id50_pre$fitTPU)
colnames(aci_data_id50_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id50_pre_data)

### plant id50 post_heatwave
aci_data_id50_post = subset(aci_data, id == ids[68] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id50_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id50_post)
#### fit aci curve
fit_aci_id50_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id50_post)
summary(fit_aci_id50_post)
#### plot
plot(fit_aci_id50_post)
#### add to dataframe
aci_data_id50_post_data <- cbind(aci_data_id50_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id50_post[,30]),
                                  mean(aci_data_id50_post[,118]),
                                  fit_aci_id50_post[[2]][1,1],
                                  fit_aci_id50_post[[2]][1,2],
                                  fit_aci_id50_post[[2]][2,1],
                                  fit_aci_id50_post[[2]][2,2],
                                  fit_aci_id50_post[[2]][3,1],
                                  fit_aci_id50_post[[2]][3,2],
                                  # fit_aci_id50_post[[2]][4,1],
                                  # fit_aci_id50_post[[2]][4,2],
                                  fit_aci_id50_post$RMSE,
                                  fit_aci_id50_post$Ci_transition,
                                  fit_aci_id50_post$citransition,
                                  fit_aci_id50_post$Km,
                                  fit_aci_id50_post$GammaStar,
                                  fit_aci_id50_post$fitmethod,
                                  fit_aci_id50_post$Tcorrect,
                                  fit_aci_id50_post$fitTPU)
colnames(aci_data_id50_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id50_post_data)

##############################################################################
##############################################################################
###########id51
##############################################################################
##############################################################################
### plant id51 pre_heatwave
aci_data_id51_pre = subset(aci_data, id == ids[69] & meas.type == 'pre_heatwave' & Ci < 600)
aci_data_id51_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id51_pre)
#### fit aci curve
fit_aci_id51_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id51_pre)
summary(fit_aci_id51_pre)
#### plot
plot(fit_aci_id51_pre)
#### add to dataframe
aci_data_id51_pre_data <- cbind(aci_data_id51_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id51_pre[,30]),
                                 mean(aci_data_id51_pre[,118]),
                                 fit_aci_id51_pre[[2]][1,1],
                                 fit_aci_id51_pre[[2]][1,2],
                                 fit_aci_id51_pre[[2]][2,1],
                                 fit_aci_id51_pre[[2]][2,2],
                                 fit_aci_id51_pre[[2]][3,1],
                                 fit_aci_id51_pre[[2]][3,2],
                                 # fit_aci_id51_pre[[2]][4,1],
                                 # fit_aci_id51_pre[[2]][4,2],
                                 fit_aci_id51_pre$RMSE,
                                 fit_aci_id51_pre$Ci_transition,
                                 fit_aci_id51_pre$citransition,
                                 fit_aci_id51_pre$Km,
                                 fit_aci_id51_pre$GammaStar,
                                 fit_aci_id51_pre$fitmethod,
                                 fit_aci_id51_pre$Tcorrect,
                                 fit_aci_id51_pre$fitTPU)
colnames(aci_data_id51_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id51_pre_data)

### plant id51 post_heatwave
aci_data_id51_post = subset(aci_data, id == ids[69] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id51_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id51_post)
#### fit aci curve
fit_aci_id51_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id51_post)
summary(fit_aci_id51_post)
#### plot
plot(fit_aci_id51_post)
#### add to dataframe
aci_data_id51_post_data <- cbind(aci_data_id51_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id51_post[,30]),
                                  mean(aci_data_id51_post[,118]),
                                  fit_aci_id51_post[[2]][1,1],
                                  fit_aci_id51_post[[2]][1,2],
                                  fit_aci_id51_post[[2]][2,1],
                                  fit_aci_id51_post[[2]][2,2],
                                  fit_aci_id51_post[[2]][3,1],
                                  fit_aci_id51_post[[2]][3,2],
                                  # fit_aci_id51_post[[2]][4,1],
                                  # fit_aci_id51_post[[2]][4,2],
                                  fit_aci_id51_post$RMSE,
                                  fit_aci_id51_post$Ci_transition,
                                  fit_aci_id51_post$citransition,
                                  fit_aci_id51_post$Km,
                                  fit_aci_id51_post$GammaStar,
                                  fit_aci_id51_post$fitmethod,
                                  fit_aci_id51_post$Tcorrect,
                                  fit_aci_id51_post$fitTPU)
colnames(aci_data_id51_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id51_post_data)

##############################################################################
##############################################################################
###########id52
##############################################################################
##############################################################################
### plant id52 pre_heatwave
aci_data_id52_pre = subset(aci_data, id == ids[70] & meas.type == 'pre_heatwave')
aci_data_id52_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id52_pre)
#### fit aci curve
fit_aci_id52_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id52_pre)
summary(fit_aci_id52_pre)
#### plot
plot(fit_aci_id52_pre)
#### add to dataframe
aci_data_id52_pre_data <- cbind(aci_data_id52_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id52_pre[,30]),
                                 mean(aci_data_id52_pre[,118]),
                                 fit_aci_id52_pre[[2]][1,1],
                                 fit_aci_id52_pre[[2]][1,2],
                                 fit_aci_id52_pre[[2]][2,1],
                                 fit_aci_id52_pre[[2]][2,2],
                                 fit_aci_id52_pre[[2]][3,1],
                                 fit_aci_id52_pre[[2]][3,2],
                                 # fit_aci_id52_pre[[2]][4,1],
                                 # fit_aci_id52_pre[[2]][4,2],
                                 fit_aci_id52_pre$RMSE,
                                 fit_aci_id52_pre$Ci_transition,
                                 fit_aci_id52_pre$citransition,
                                 fit_aci_id52_pre$Km,
                                 fit_aci_id52_pre$GammaStar,
                                 fit_aci_id52_pre$fitmethod,
                                 fit_aci_id52_pre$Tcorrect,
                                 fit_aci_id52_pre$fitTPU)
colnames(aci_data_id52_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id52_pre_data)

### plant id52 post_heatwave
aci_data_id52_post = subset(aci_data, id == ids[70] & meas.type == 'post_heatwave')
aci_data_id52_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id52_post)
#### fit aci curve
fit_aci_id52_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id52_post)
summary(fit_aci_id52_post)
#### plot
plot(fit_aci_id52_post)
#### add to dataframe
aci_data_id52_post_data <- cbind(aci_data_id52_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id52_post[,30]),
                                  mean(aci_data_id52_post[,118]),
                                  fit_aci_id52_post[[2]][1,1],
                                  fit_aci_id52_post[[2]][1,2],
                                  fit_aci_id52_post[[2]][2,1],
                                  fit_aci_id52_post[[2]][2,2],
                                  fit_aci_id52_post[[2]][3,1],
                                  fit_aci_id52_post[[2]][3,2],
                                  # fit_aci_id52_post[[2]][4,1],
                                  # fit_aci_id52_post[[2]][4,2],
                                  fit_aci_id52_post$RMSE,
                                  fit_aci_id52_post$Ci_transition,
                                  fit_aci_id52_post$citransition,
                                  fit_aci_id52_post$Km,
                                  fit_aci_id52_post$GammaStar,
                                  fit_aci_id52_post$fitmethod,
                                  fit_aci_id52_post$Tcorrect,
                                  fit_aci_id52_post$fitTPU)
colnames(aci_data_id52_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id52_post_data)

##############################################################################
##############################################################################
###########id54
##############################################################################
##############################################################################
### plant id54 pre_heatwave
# aci_data_id54_pre = subset(aci_data, id == ids[71] & meas.type == 'pre_heatwave')
# aci_data_id54_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id54_pre)
# #### fit aci curve
# fit_aci_id54_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id54_pre)
# summary(fit_aci_id54_pre)
# #### plot
# plot(fit_aci_id54_pre)
# #### add to dataframe
# aci_data_id54_pre_data <- cbind(aci_data_id54_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id54_pre[,30]),
#                                  mean(aci_data_id54_pre[,118]),
#                                  fit_aci_id54_pre[[2]][1,1],
#                                  fit_aci_id54_pre[[2]][1,2],
#                                  fit_aci_id54_pre[[2]][2,1],
#                                  fit_aci_id54_pre[[2]][2,2],
#                                  fit_aci_id54_pre[[2]][3,1],
#                                  fit_aci_id54_pre[[2]][3,2],
#                                  # fit_aci_id54_pre[[2]][4,1],
#                                  # fit_aci_id54_pre[[2]][4,2],
#                                  fit_aci_id54_pre$RMSE,
#                                  fit_aci_id54_pre$Ci_transition,
#                                  fit_aci_id54_pre$citransition,
#                                  fit_aci_id54_pre$Km,
#                                  fit_aci_id54_pre$GammaStar,
#                                  fit_aci_id54_pre$fitmethod,
#                                  fit_aci_id54_pre$Tcorrect,
#                                  fit_aci_id54_pre$fitTPU)
# colnames(aci_data_id54_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id54_pre_data)
# 
# ### plant id54 post_heatwave
# aci_data_id54_post = subset(aci_data, id == ids[71] & meas.type == 'post_heatwave')
# aci_data_id54_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id54_post)
# #### fit aci curve
# fit_aci_id54_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id54_post)
# summary(fit_aci_id54_post)
# #### plot
# plot(fit_aci_id54_post)
# #### add to dataframe
# aci_data_id54_post_data <- cbind(aci_data_id54_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id54_post[,30]),
#                                   mean(aci_data_id54_post[,118]),
#                                   fit_aci_id54_post[[2]][1,1],
#                                   fit_aci_id54_post[[2]][1,2],
#                                   fit_aci_id54_post[[2]][2,1],
#                                   fit_aci_id54_post[[2]][2,2],
#                                   fit_aci_id54_post[[2]][3,1],
#                                   fit_aci_id54_post[[2]][3,2],
#                                   # fit_aci_id54_post[[2]][4,1],
#                                   # fit_aci_id54_post[[2]][4,2],
#                                   fit_aci_id54_post$RMSE,
#                                   fit_aci_id54_post$Ci_transition,
#                                   fit_aci_id54_post$citransition,
#                                   fit_aci_id54_post$Km,
#                                   fit_aci_id54_post$GammaStar,
#                                   fit_aci_id54_post$fitmethod,
#                                   fit_aci_id54_post$Tcorrect,
#                                   fit_aci_id54_post$fitTPU)
# colnames(aci_data_id54_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id54_post_data)

##############################################################################
##############################################################################
###########id56
##############################################################################
##############################################################################
### plant id56 pre_heatwave
aci_data_id56_pre = subset(aci_data, id == ids[72] & meas.type == 'pre_heatwave')
aci_data_id56_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id56_pre)
#### fit aci curve
fit_aci_id56_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id56_pre)
summary(fit_aci_id56_pre)
#### plot
plot(fit_aci_id56_pre)
#### add to dataframe
aci_data_id56_pre_data <- cbind(aci_data_id56_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id56_pre[,30]),
                                 mean(aci_data_id56_pre[,118]),
                                 fit_aci_id56_pre[[2]][1,1],
                                 fit_aci_id56_pre[[2]][1,2],
                                 fit_aci_id56_pre[[2]][2,1],
                                 fit_aci_id56_pre[[2]][2,2],
                                 fit_aci_id56_pre[[2]][3,1],
                                 fit_aci_id56_pre[[2]][3,2],
                                 # fit_aci_id56_pre[[2]][4,1],
                                 # fit_aci_id56_pre[[2]][4,2],
                                 fit_aci_id56_pre$RMSE,
                                 fit_aci_id56_pre$Ci_transition,
                                 fit_aci_id56_pre$citransition,
                                 fit_aci_id56_pre$Km,
                                 fit_aci_id56_pre$GammaStar,
                                 fit_aci_id56_pre$fitmethod,
                                 fit_aci_id56_pre$Tcorrect,
                                 fit_aci_id56_pre$fitTPU)
colnames(aci_data_id56_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id56_pre_data)

### plant id56 post_heatwave
aci_data_id56_post = subset(aci_data, id == ids[72] & meas.type == 'post_heatwave')
aci_data_id56_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id56_post)
#### fit aci curve
fit_aci_id56_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id56_post)
summary(fit_aci_id56_post)
#### plot
plot(fit_aci_id56_post)
#### add to dataframe
aci_data_id56_post_data <- cbind(aci_data_id56_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id56_post[,30]),
                                  mean(aci_data_id56_post[,118]),
                                  fit_aci_id56_post[[2]][1,1],
                                  fit_aci_id56_post[[2]][1,2],
                                  fit_aci_id56_post[[2]][2,1],
                                  fit_aci_id56_post[[2]][2,2],
                                  fit_aci_id56_post[[2]][3,1],
                                  fit_aci_id56_post[[2]][3,2],
                                  # fit_aci_id56_post[[2]][4,1],
                                  # fit_aci_id56_post[[2]][4,2],
                                  fit_aci_id56_post$RMSE,
                                  fit_aci_id56_post$Ci_transition,
                                  fit_aci_id56_post$citransition,
                                  fit_aci_id56_post$Km,
                                  fit_aci_id56_post$GammaStar,
                                  fit_aci_id56_post$fitmethod,
                                  fit_aci_id56_post$Tcorrect,
                                  fit_aci_id56_post$fitTPU)
colnames(aci_data_id56_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id56_post_data)

##############################################################################
##############################################################################
###########id58
##############################################################################
##############################################################################
### plant id58 pre_heatwave
aci_data_id58_pre = subset(aci_data, id == ids[73] & meas.type == 'pre_heatwave')
aci_data_id58_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id58_pre)
#### fit aci curve
fit_aci_id58_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id58_pre)
summary(fit_aci_id58_pre)
#### plot
plot(fit_aci_id58_pre)
#### add to dataframe
aci_data_id58_pre_data <- cbind(aci_data_id58_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id58_pre[,30]),
                                 mean(aci_data_id58_pre[,118]),
                                 fit_aci_id58_pre[[2]][1,1],
                                 fit_aci_id58_pre[[2]][1,2],
                                 fit_aci_id58_pre[[2]][2,1],
                                 fit_aci_id58_pre[[2]][2,2],
                                 fit_aci_id58_pre[[2]][3,1],
                                 fit_aci_id58_pre[[2]][3,2],
                                 # fit_aci_id58_pre[[2]][4,1],
                                 # fit_aci_id58_pre[[2]][4,2],
                                 fit_aci_id58_pre$RMSE,
                                 fit_aci_id58_pre$Ci_transition,
                                 fit_aci_id58_pre$citransition,
                                 fit_aci_id58_pre$Km,
                                 fit_aci_id58_pre$GammaStar,
                                 fit_aci_id58_pre$fitmethod,
                                 fit_aci_id58_pre$Tcorrect,
                                 fit_aci_id58_pre$fitTPU)
colnames(aci_data_id58_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id58_pre_data)

### plant id58 post_heatwave
aci_data_id58_post = subset(aci_data, id == ids[73] & meas.type == 'post_heatwave')
aci_data_id58_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id58_post)
#### fit aci curve
fit_aci_id58_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id58_post)
summary(fit_aci_id58_post)
#### plot
plot(fit_aci_id58_post)
#### add to dataframe
aci_data_id58_post_data <- cbind(aci_data_id58_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id58_post[,30]),
                                  mean(aci_data_id58_post[,118]),
                                  fit_aci_id58_post[[2]][1,1],
                                  fit_aci_id58_post[[2]][1,2],
                                  fit_aci_id58_post[[2]][2,1],
                                  fit_aci_id58_post[[2]][2,2],
                                  fit_aci_id58_post[[2]][3,1],
                                  fit_aci_id58_post[[2]][3,2],
                                  # fit_aci_id58_post[[2]][4,1],
                                  # fit_aci_id58_post[[2]][4,2],
                                  fit_aci_id58_post$RMSE,
                                  fit_aci_id58_post$Ci_transition,
                                  fit_aci_id58_post$citransition,
                                  fit_aci_id58_post$Km,
                                  fit_aci_id58_post$GammaStar,
                                  fit_aci_id58_post$fitmethod,
                                  fit_aci_id58_post$Tcorrect,
                                  fit_aci_id58_post$fitTPU)
colnames(aci_data_id58_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id58_post_data)

##############################################################################
##############################################################################
###########id59
##############################################################################
##############################################################################
### plant id59 pre_heatwave
aci_data_id59_pre = subset(aci_data, id == ids[74] & meas.type == 'pre_heatwave')
aci_data_id59_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id59_pre)
#### fit aci curve
fit_aci_id59_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id59_pre)
summary(fit_aci_id59_pre)
#### plot
plot(fit_aci_id59_pre)
#### add to dataframe
aci_data_id59_pre_data <- cbind(aci_data_id59_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id59_pre[,30]),
                                 mean(aci_data_id59_pre[,118]),
                                 fit_aci_id59_pre[[2]][1,1],
                                 fit_aci_id59_pre[[2]][1,2],
                                 fit_aci_id59_pre[[2]][2,1],
                                 fit_aci_id59_pre[[2]][2,2],
                                 fit_aci_id59_pre[[2]][3,1],
                                 fit_aci_id59_pre[[2]][3,2],
                                 # fit_aci_id59_pre[[2]][4,1],
                                 # fit_aci_id59_pre[[2]][4,2],
                                 fit_aci_id59_pre$RMSE,
                                 fit_aci_id59_pre$Ci_transition,
                                 fit_aci_id59_pre$citransition,
                                 fit_aci_id59_pre$Km,
                                 fit_aci_id59_pre$GammaStar,
                                 fit_aci_id59_pre$fitmethod,
                                 fit_aci_id59_pre$Tcorrect,
                                 fit_aci_id59_pre$fitTPU)
colnames(aci_data_id59_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id59_pre_data)

### plant id59 post_heatwave
aci_data_id59_post = subset(aci_data, id == ids[74] & meas.type == 'post_heatwave')
aci_data_id59_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id59_post)
#### fit aci curve
fit_aci_id59_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id59_post)
summary(fit_aci_id59_post)
#### plot
plot(fit_aci_id59_post)
#### add to dataframe
aci_data_id59_post_data <- cbind(aci_data_id59_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id59_post[,30]),
                                  mean(aci_data_id59_post[,118]),
                                  fit_aci_id59_post[[2]][1,1],
                                  fit_aci_id59_post[[2]][1,2],
                                  fit_aci_id59_post[[2]][2,1],
                                  fit_aci_id59_post[[2]][2,2],
                                  fit_aci_id59_post[[2]][3,1],
                                  fit_aci_id59_post[[2]][3,2],
                                  # fit_aci_id59_post[[2]][4,1],
                                  # fit_aci_id59_post[[2]][4,2],
                                  fit_aci_id59_post$RMSE,
                                  fit_aci_id59_post$Ci_transition,
                                  fit_aci_id59_post$citransition,
                                  fit_aci_id59_post$Km,
                                  fit_aci_id59_post$GammaStar,
                                  fit_aci_id59_post$fitmethod,
                                  fit_aci_id59_post$Tcorrect,
                                  fit_aci_id59_post$fitTPU)
colnames(aci_data_id59_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id59_post_data)

##############################################################################
##############################################################################
###########id6
##############################################################################
##############################################################################
### plant id6 pre_heatwave
aci_data_id6_pre = subset(aci_data, id == ids[75] & meas.type == 'pre_heatwave' & Ci < 700)
aci_data_id6_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id6_pre)
#### fit aci curve
fit_aci_id6_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id6_pre)
summary(fit_aci_id6_pre)
#### plot
plot(fit_aci_id6_pre)
#### add to dataframe
aci_data_id6_pre_data <- cbind(aci_data_id6_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id6_pre[,30]),
                                 mean(aci_data_id6_pre[,118]),
                                 fit_aci_id6_pre[[2]][1,1],
                                 fit_aci_id6_pre[[2]][1,2],
                                 fit_aci_id6_pre[[2]][2,1],
                                 fit_aci_id6_pre[[2]][2,2],
                                 fit_aci_id6_pre[[2]][3,1],
                                 fit_aci_id6_pre[[2]][3,2],
                                 # fit_aci_id6_pre[[2]][4,1],
                                 # fit_aci_id6_pre[[2]][4,2],
                                 fit_aci_id6_pre$RMSE,
                                 fit_aci_id6_pre$Ci_transition,
                                 fit_aci_id6_pre$citransition,
                                 fit_aci_id6_pre$Km,
                                 fit_aci_id6_pre$GammaStar,
                                 fit_aci_id6_pre$fitmethod,
                                 fit_aci_id6_pre$Tcorrect,
                                 fit_aci_id6_pre$fitTPU)
colnames(aci_data_id6_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id6_pre_data)

### plant id6 post_heatwave
aci_data_id6_post = subset(aci_data, id == ids[75] & meas.type == 'post_heatwave' & Ci < 700)
aci_data_id6_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id6_post)
#### fit aci curve
fit_aci_id6_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id6_post)
summary(fit_aci_id6_post)
#### plot
plot(fit_aci_id6_post)
#### add to dataframe
aci_data_id6_post_data <- cbind(aci_data_id6_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id6_post[,30]),
                                  mean(aci_data_id6_post[,118]),
                                  fit_aci_id6_post[[2]][1,1],
                                  fit_aci_id6_post[[2]][1,2],
                                  fit_aci_id6_post[[2]][2,1],
                                  fit_aci_id6_post[[2]][2,2],
                                  fit_aci_id6_post[[2]][3,1],
                                  fit_aci_id6_post[[2]][3,2],
                                  # fit_aci_id6_post[[2]][4,1],
                                  # fit_aci_id6_post[[2]][4,2],
                                  fit_aci_id6_post$RMSE,
                                  fit_aci_id6_post$Ci_transition,
                                  fit_aci_id6_post$citransition,
                                  fit_aci_id6_post$Km,
                                  fit_aci_id6_post$GammaStar,
                                  fit_aci_id6_post$fitmethod,
                                  fit_aci_id6_post$Tcorrect,
                                  fit_aci_id6_post$fitTPU)
colnames(aci_data_id6_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id6_post_data)

##############################################################################
##############################################################################
###########id63
##############################################################################
##############################################################################
### plant id63 pre_heatwave
# aci_data_id63_pre = subset(aci_data, id == ids[76] & meas.type == 'pre_heatwave')
# aci_data_id63_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id63_pre)
# #### fit aci curve
# fit_aci_id63_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id63_pre)
# summary(fit_aci_id63_pre)
# #### plot
# plot(fit_aci_id63_pre)
# #### add to dataframe
# aci_data_id63_pre_data <- cbind(aci_data_id63_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id63_pre[,30]),
#                                  mean(aci_data_id63_pre[,118]),
#                                  fit_aci_id63_pre[[2]][1,1],
#                                  fit_aci_id63_pre[[2]][1,2],
#                                  fit_aci_id63_pre[[2]][2,1],
#                                  fit_aci_id63_pre[[2]][2,2],
#                                  fit_aci_id63_pre[[2]][3,1],
#                                  fit_aci_id63_pre[[2]][3,2],
#                                  # fit_aci_id63_pre[[2]][4,1],
#                                  # fit_aci_id63_pre[[2]][4,2],
#                                  fit_aci_id63_pre$RMSE,
#                                  fit_aci_id63_pre$Ci_transition,
#                                  fit_aci_id63_pre$citransition,
#                                  fit_aci_id63_pre$Km,
#                                  fit_aci_id63_pre$GammaStar,
#                                  fit_aci_id63_pre$fitmethod,
#                                  fit_aci_id63_pre$Tcorrect,
#                                  fit_aci_id63_pre$fitTPU)
# colnames(aci_data_id63_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id63_pre_data)
# 
# ### plant id63 post_heatwave
# aci_data_id63_post = subset(aci_data, id == ids[76] & meas.type == 'post_heatwave')
# aci_data_id63_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id63_post)
# #### fit aci curve
# fit_aci_id63_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id63_post)
# summary(fit_aci_id63_post)
# #### plot
# plot(fit_aci_id63_post)
# #### add to dataframe
# aci_data_id63_post_data <- cbind(aci_data_id63_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id63_post[,30]),
#                                   mean(aci_data_id63_post[,118]),
#                                   fit_aci_id63_post[[2]][1,1],
#                                   fit_aci_id63_post[[2]][1,2],
#                                   fit_aci_id63_post[[2]][2,1],
#                                   fit_aci_id63_post[[2]][2,2],
#                                   fit_aci_id63_post[[2]][3,1],
#                                   fit_aci_id63_post[[2]][3,2],
#                                   # fit_aci_id63_post[[2]][4,1],
#                                   # fit_aci_id63_post[[2]][4,2],
#                                   fit_aci_id63_post$RMSE,
#                                   fit_aci_id63_post$Ci_transition,
#                                   fit_aci_id63_post$citransition,
#                                   fit_aci_id63_post$Km,
#                                   fit_aci_id63_post$GammaStar,
#                                   fit_aci_id63_post$fitmethod,
#                                   fit_aci_id63_post$Tcorrect,
#                                   fit_aci_id63_post$fitTPU)
# colnames(aci_data_id63_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id63_post_data)

##############################################################################
##############################################################################
###########id64
##############################################################################
##############################################################################
### plant id64 pre_heatwave
# aci_data_id64_pre = subset(aci_data, id == ids[77] & meas.type == 'pre_heatwave')
# aci_data_id64_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id64_pre)
# #### fit aci curve
# fit_aci_id64_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id64_pre)
# summary(fit_aci_id64_pre)
# #### plot
# plot(fit_aci_id64_pre)
# #### add to dataframe
# aci_data_id64_pre_data <- cbind(aci_data_id64_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id64_pre[,30]),
#                                  mean(aci_data_id64_pre[,118]),
#                                  fit_aci_id64_pre[[2]][1,1],
#                                  fit_aci_id64_pre[[2]][1,2],
#                                  fit_aci_id64_pre[[2]][2,1],
#                                  fit_aci_id64_pre[[2]][2,2],
#                                  fit_aci_id64_pre[[2]][3,1],
#                                  fit_aci_id64_pre[[2]][3,2],
#                                  # fit_aci_id64_pre[[2]][4,1],
#                                  # fit_aci_id64_pre[[2]][4,2],
#                                  fit_aci_id64_pre$RMSE,
#                                  fit_aci_id64_pre$Ci_transition,
#                                  fit_aci_id64_pre$citransition,
#                                  fit_aci_id64_pre$Km,
#                                  fit_aci_id64_pre$GammaStar,
#                                  fit_aci_id64_pre$fitmethod,
#                                  fit_aci_id64_pre$Tcorrect,
#                                  fit_aci_id64_pre$fitTPU)
# colnames(aci_data_id64_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id64_pre_data)
# 
# ### plant id64 post_heatwave
# aci_data_id64_post = subset(aci_data, id == ids[77] & meas.type == 'post_heatwave')
# aci_data_id64_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id64_post)
# #### fit aci curve
# fit_aci_id64_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id64_post)
# summary(fit_aci_id64_post)
# #### plot
# plot(fit_aci_id64_post)
# #### add to dataframe
# aci_data_id64_post_data <- cbind(aci_data_id64_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id64_post[,30]),
#                                   mean(aci_data_id64_post[,118]),
#                                   fit_aci_id64_post[[2]][1,1],
#                                   fit_aci_id64_post[[2]][1,2],
#                                   fit_aci_id64_post[[2]][2,1],
#                                   fit_aci_id64_post[[2]][2,2],
#                                   fit_aci_id64_post[[2]][3,1],
#                                   fit_aci_id64_post[[2]][3,2],
#                                   # fit_aci_id64_post[[2]][4,1],
#                                   # fit_aci_id64_post[[2]][4,2],
#                                   fit_aci_id64_post$RMSE,
#                                   fit_aci_id64_post$Ci_transition,
#                                   fit_aci_id64_post$citransition,
#                                   fit_aci_id64_post$Km,
#                                   fit_aci_id64_post$GammaStar,
#                                   fit_aci_id64_post$fitmethod,
#                                   fit_aci_id64_post$Tcorrect,
#                                   fit_aci_id64_post$fitTPU)
# colnames(aci_data_id64_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id64_post_data)

##############################################################################
##############################################################################
###########id66
##############################################################################
##############################################################################
### plant id66 pre_heatwave
aci_data_id66_pre = subset(aci_data, id == ids[78] & meas.type == 'pre_heatwave')
aci_data_id66_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id66_pre)
#### fit aci curve
fit_aci_id66_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id66_pre)
summary(fit_aci_id66_pre)
#### plot
plot(fit_aci_id66_pre)
#### add to dataframe
aci_data_id66_pre_data <- cbind(aci_data_id66_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id66_pre[,30]),
                                 mean(aci_data_id66_pre[,118]),
                                 fit_aci_id66_pre[[2]][1,1],
                                 fit_aci_id66_pre[[2]][1,2],
                                 fit_aci_id66_pre[[2]][2,1],
                                 fit_aci_id66_pre[[2]][2,2],
                                 fit_aci_id66_pre[[2]][3,1],
                                 fit_aci_id66_pre[[2]][3,2],
                                 # fit_aci_id66_pre[[2]][4,1],
                                 # fit_aci_id66_pre[[2]][4,2],
                                 fit_aci_id66_pre$RMSE,
                                 fit_aci_id66_pre$Ci_transition,
                                 fit_aci_id66_pre$citransition,
                                 fit_aci_id66_pre$Km,
                                 fit_aci_id66_pre$GammaStar,
                                 fit_aci_id66_pre$fitmethod,
                                 fit_aci_id66_pre$Tcorrect,
                                 fit_aci_id66_pre$fitTPU)
colnames(aci_data_id66_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id66_pre_data)

### plant id66 post_heatwave
aci_data_id66_post = subset(aci_data, id == ids[78] & meas.type == 'post_heatwave')
aci_data_id66_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id66_post)
#### fit aci curve
fit_aci_id66_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id66_post)
summary(fit_aci_id66_post)
#### plot
plot(fit_aci_id66_post)
#### add to dataframe
aci_data_id66_post_data <- cbind(aci_data_id66_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id66_post[,30]),
                                  mean(aci_data_id66_post[,118]),
                                  fit_aci_id66_post[[2]][1,1],
                                  fit_aci_id66_post[[2]][1,2],
                                  fit_aci_id66_post[[2]][2,1],
                                  fit_aci_id66_post[[2]][2,2],
                                  fit_aci_id66_post[[2]][3,1],
                                  fit_aci_id66_post[[2]][3,2],
                                  # fit_aci_id66_post[[2]][4,1],
                                  # fit_aci_id66_post[[2]][4,2],
                                  fit_aci_id66_post$RMSE,
                                  fit_aci_id66_post$Ci_transition,
                                  fit_aci_id66_post$citransition,
                                  fit_aci_id66_post$Km,
                                  fit_aci_id66_post$GammaStar,
                                  fit_aci_id66_post$fitmethod,
                                  fit_aci_id66_post$Tcorrect,
                                  fit_aci_id66_post$fitTPU)
colnames(aci_data_id66_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id66_post_data)

##############################################################################
##############################################################################
###########id68
##############################################################################
##############################################################################
### plant id68 pre_heatwave
aci_data_id68_pre = subset(aci_data, id == ids[79] & meas.type == 'pre_heatwave')
aci_data_id68_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id68_pre)
#### fit aci curve
fit_aci_id68_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id68_pre)
summary(fit_aci_id68_pre)
#### plot
plot(fit_aci_id68_pre)
#### add to dataframe
aci_data_id68_pre_data <- cbind(aci_data_id68_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id68_pre[,30]),
                                 mean(aci_data_id68_pre[,118]),
                                 fit_aci_id68_pre[[2]][1,1],
                                 fit_aci_id68_pre[[2]][1,2],
                                 fit_aci_id68_pre[[2]][2,1],
                                 fit_aci_id68_pre[[2]][2,2],
                                 fit_aci_id68_pre[[2]][3,1],
                                 fit_aci_id68_pre[[2]][3,2],
                                 # fit_aci_id68_pre[[2]][4,1],
                                 # fit_aci_id68_pre[[2]][4,2],
                                 fit_aci_id68_pre$RMSE,
                                 fit_aci_id68_pre$Ci_transition,
                                 fit_aci_id68_pre$citransition,
                                 fit_aci_id68_pre$Km,
                                 fit_aci_id68_pre$GammaStar,
                                 fit_aci_id68_pre$fitmethod,
                                 fit_aci_id68_pre$Tcorrect,
                                 fit_aci_id68_pre$fitTPU)
colnames(aci_data_id68_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id68_pre_data)

### plant id68 post_heatwave
aci_data_id68_post = subset(aci_data, id == ids[79] & meas.type == 'post_heatwave')
aci_data_id68_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id68_post)
#### fit aci curve
fit_aci_id68_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id68_post)
summary(fit_aci_id68_post)
#### plot
plot(fit_aci_id68_post)
#### add to dataframe
aci_data_id68_post_data <- cbind(aci_data_id68_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id68_post[,30]),
                                  mean(aci_data_id68_post[,118]),
                                  fit_aci_id68_post[[2]][1,1],
                                  fit_aci_id68_post[[2]][1,2],
                                  fit_aci_id68_post[[2]][2,1],
                                  fit_aci_id68_post[[2]][2,2],
                                  fit_aci_id68_post[[2]][3,1],
                                  fit_aci_id68_post[[2]][3,2],
                                  # fit_aci_id68_post[[2]][4,1],
                                  # fit_aci_id68_post[[2]][4,2],
                                  fit_aci_id68_post$RMSE,
                                  fit_aci_id68_post$Ci_transition,
                                  fit_aci_id68_post$citransition,
                                  fit_aci_id68_post$Km,
                                  fit_aci_id68_post$GammaStar,
                                  fit_aci_id68_post$fitmethod,
                                  fit_aci_id68_post$Tcorrect,
                                  fit_aci_id68_post$fitTPU)
colnames(aci_data_id68_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id68_post_data)

##############################################################################
##############################################################################
###########id72
##############################################################################
##############################################################################
### plant id72 pre_heatwave
# aci_data_id72_pre = subset(aci_data, id == ids[80] & meas.type == 'pre_heatwave')
# aci_data_id72_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id72_pre)
# #### fit aci curve
# fit_aci_id72_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id72_pre)
# summary(fit_aci_id72_pre)
# #### plot
# plot(fit_aci_id72_pre)
# #### add to dataframe
# aci_data_id72_pre_data <- cbind(aci_data_id72_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id72_pre[,30]),
#                                  mean(aci_data_id72_pre[,118]),
#                                  fit_aci_id72_pre[[2]][1,1],
#                                  fit_aci_id72_pre[[2]][1,2],
#                                  fit_aci_id72_pre[[2]][2,1],
#                                  fit_aci_id72_pre[[2]][2,2],
#                                  fit_aci_id72_pre[[2]][3,1],
#                                  fit_aci_id72_pre[[2]][3,2],
#                                  # fit_aci_id72_pre[[2]][4,1],
#                                  # fit_aci_id72_pre[[2]][4,2],
#                                  fit_aci_id72_pre$RMSE,
#                                  fit_aci_id72_pre$Ci_transition,
#                                  fit_aci_id72_pre$citransition,
#                                  fit_aci_id72_pre$Km,
#                                  fit_aci_id72_pre$GammaStar,
#                                  fit_aci_id72_pre$fitmethod,
#                                  fit_aci_id72_pre$Tcorrect,
#                                  fit_aci_id72_pre$fitTPU)
# colnames(aci_data_id72_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id72_pre_data)
# 
# ### plant id72 post_heatwave
# aci_data_id72_post = subset(aci_data, id == ids[80] & meas.type == 'post_heatwave')
# aci_data_id72_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id72_post)
# #### fit aci curve
# fit_aci_id72_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id72_post)
# summary(fit_aci_id72_post)
# #### plot
# plot(fit_aci_id72_post)
# #### add to dataframe
# aci_data_id72_post_data <- cbind(aci_data_id72_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id72_post[,30]),
#                                   mean(aci_data_id72_post[,118]),
#                                   fit_aci_id72_post[[2]][1,1],
#                                   fit_aci_id72_post[[2]][1,2],
#                                   fit_aci_id72_post[[2]][2,1],
#                                   fit_aci_id72_post[[2]][2,2],
#                                   fit_aci_id72_post[[2]][3,1],
#                                   fit_aci_id72_post[[2]][3,2],
#                                   # fit_aci_id72_post[[2]][4,1],
#                                   # fit_aci_id72_post[[2]][4,2],
#                                   fit_aci_id72_post$RMSE,
#                                   fit_aci_id72_post$Ci_transition,
#                                   fit_aci_id72_post$citransition,
#                                   fit_aci_id72_post$Km,
#                                   fit_aci_id72_post$GammaStar,
#                                   fit_aci_id72_post$fitmethod,
#                                   fit_aci_id72_post$Tcorrect,
#                                   fit_aci_id72_post$fitTPU)
# colnames(aci_data_id72_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id72_post_data)

##############################################################################
##############################################################################
###########id76
##############################################################################
##############################################################################
### plant id76 pre_heatwave
# aci_data_id76_pre = subset(aci_data, id == ids[81] & meas.type == 'pre_heatwave')
# aci_data_id76_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id76_pre)
# #### fit aci curve
# fit_aci_id76_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id76_pre)
# summary(fit_aci_id76_pre)
# #### plot
# plot(fit_aci_id76_pre)
# #### add to dataframe
# aci_data_id76_pre_data <- cbind(aci_data_id76_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id76_pre[,30]),
#                                  mean(aci_data_id76_pre[,118]),
#                                  fit_aci_id76_pre[[2]][1,1],
#                                  fit_aci_id76_pre[[2]][1,2],
#                                  fit_aci_id76_pre[[2]][2,1],
#                                  fit_aci_id76_pre[[2]][2,2],
#                                  fit_aci_id76_pre[[2]][3,1],
#                                  fit_aci_id76_pre[[2]][3,2],
#                                  # fit_aci_id76_pre[[2]][4,1],
#                                  # fit_aci_id76_pre[[2]][4,2],
#                                  fit_aci_id76_pre$RMSE,
#                                  fit_aci_id76_pre$Ci_transition,
#                                  fit_aci_id76_pre$citransition,
#                                  fit_aci_id76_pre$Km,
#                                  fit_aci_id76_pre$GammaStar,
#                                  fit_aci_id76_pre$fitmethod,
#                                  fit_aci_id76_pre$Tcorrect,
#                                  fit_aci_id76_pre$fitTPU)
# colnames(aci_data_id76_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id76_pre_data)
# 
# ### plant id76 post_heatwave
# aci_data_id76_post = subset(aci_data, id == ids[81] & meas.type == 'post_heatwave')
# aci_data_id76_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id76_post)
# #### fit aci curve
# fit_aci_id76_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id76_post)
# summary(fit_aci_id76_post)
# #### plot
# plot(fit_aci_id76_post)
# #### add to dataframe
# aci_data_id76_post_data <- cbind(aci_data_id76_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id76_post[,30]),
#                                   mean(aci_data_id76_post[,118]),
#                                   fit_aci_id76_post[[2]][1,1],
#                                   fit_aci_id76_post[[2]][1,2],
#                                   fit_aci_id76_post[[2]][2,1],
#                                   fit_aci_id76_post[[2]][2,2],
#                                   fit_aci_id76_post[[2]][3,1],
#                                   fit_aci_id76_post[[2]][3,2],
#                                   # fit_aci_id76_post[[2]][4,1],
#                                   # fit_aci_id76_post[[2]][4,2],
#                                   fit_aci_id76_post$RMSE,
#                                   fit_aci_id76_post$Ci_transition,
#                                   fit_aci_id76_post$citransition,
#                                   fit_aci_id76_post$Km,
#                                   fit_aci_id76_post$GammaStar,
#                                   fit_aci_id76_post$fitmethod,
#                                   fit_aci_id76_post$Tcorrect,
#                                   fit_aci_id76_post$fitTPU)
# colnames(aci_data_id76_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id76_post_data)

##############################################################################
##############################################################################
###########id77
##############################################################################
##############################################################################
### plant id77 pre_heatwave
# aci_data_id77_pre = subset(aci_data, id == ids[82] & meas.type == 'pre_heatwave')
# aci_data_id77_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id77_pre)
# #### fit aci curve
# fit_aci_id77_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id77_pre)
# summary(fit_aci_id77_pre)
# #### plot
# plot(fit_aci_id77_pre)
# #### add to dataframe
# aci_data_id77_pre_data <- cbind(aci_data_id77_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id77_pre[,30]),
#                                  mean(aci_data_id77_pre[,118]),
#                                  fit_aci_id77_pre[[2]][1,1],
#                                  fit_aci_id77_pre[[2]][1,2],
#                                  fit_aci_id77_pre[[2]][2,1],
#                                  fit_aci_id77_pre[[2]][2,2],
#                                  fit_aci_id77_pre[[2]][3,1],
#                                  fit_aci_id77_pre[[2]][3,2],
#                                  # fit_aci_id77_pre[[2]][4,1],
#                                  # fit_aci_id77_pre[[2]][4,2],
#                                  fit_aci_id77_pre$RMSE,
#                                  fit_aci_id77_pre$Ci_transition,
#                                  fit_aci_id77_pre$citransition,
#                                  fit_aci_id77_pre$Km,
#                                  fit_aci_id77_pre$GammaStar,
#                                  fit_aci_id77_pre$fitmethod,
#                                  fit_aci_id77_pre$Tcorrect,
#                                  fit_aci_id77_pre$fitTPU)
# colnames(aci_data_id77_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id77_pre_data)
# 
# ### plant id77 post_heatwave
# aci_data_id77_post = subset(aci_data, id == ids[82] & meas.type == 'post_heatwave')
# aci_data_id77_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id77_post)
# #### fit aci curve
# fit_aci_id77_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id77_post)
# summary(fit_aci_id77_post)
# #### plot
# plot(fit_aci_id77_post)
# #### add to dataframe
# aci_data_id77_post_data <- cbind(aci_data_id77_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id77_post[,30]),
#                                   mean(aci_data_id77_post[,118]),
#                                   fit_aci_id77_post[[2]][1,1],
#                                   fit_aci_id77_post[[2]][1,2],
#                                   fit_aci_id77_post[[2]][2,1],
#                                   fit_aci_id77_post[[2]][2,2],
#                                   fit_aci_id77_post[[2]][3,1],
#                                   fit_aci_id77_post[[2]][3,2],
#                                   # fit_aci_id77_post[[2]][4,1],
#                                   # fit_aci_id77_post[[2]][4,2],
#                                   fit_aci_id77_post$RMSE,
#                                   fit_aci_id77_post$Ci_transition,
#                                   fit_aci_id77_post$citransition,
#                                   fit_aci_id77_post$Km,
#                                   fit_aci_id77_post$GammaStar,
#                                   fit_aci_id77_post$fitmethod,
#                                   fit_aci_id77_post$Tcorrect,
#                                   fit_aci_id77_post$fitTPU)
# colnames(aci_data_id77_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id77_post_data)

##############################################################################
##############################################################################
###########id8
##############################################################################
##############################################################################
### plant id8 pre_heatwave
aci_data_id8_pre = subset(aci_data, id == ids[83] & meas.type == 'pre_heatwave')
aci_data_id8_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id8_pre)
#### fit aci curve
fit_aci_id8_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id8_pre)
summary(fit_aci_id8_pre)
#### plot
plot(fit_aci_id8_pre)
#### add to dataframe
aci_data_id8_pre_data <- cbind(aci_data_id8_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id8_pre[,30]),
                                 mean(aci_data_id8_pre[,118]),
                                 fit_aci_id8_pre[[2]][1,1],
                                 fit_aci_id8_pre[[2]][1,2],
                                 fit_aci_id8_pre[[2]][2,1],
                                 fit_aci_id8_pre[[2]][2,2],
                                 fit_aci_id8_pre[[2]][3,1],
                                 fit_aci_id8_pre[[2]][3,2],
                                 # fit_aci_id8_pre[[2]][4,1],
                                 # fit_aci_id8_pre[[2]][4,2],
                                 fit_aci_id8_pre$RMSE,
                                 fit_aci_id8_pre$Ci_transition,
                                 fit_aci_id8_pre$citransition,
                                 fit_aci_id8_pre$Km,
                                 fit_aci_id8_pre$GammaStar,
                                 fit_aci_id8_pre$fitmethod,
                                 fit_aci_id8_pre$Tcorrect,
                                 fit_aci_id8_pre$fitTPU)
colnames(aci_data_id8_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id8_pre_data)

### plant id8 post_heatwave
aci_data_id8_post = subset(aci_data, id == ids[83] & meas.type == 'post_heatwave')
aci_data_id8_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id8_post)
#### fit aci curve
fit_aci_id8_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id8_post)
summary(fit_aci_id8_post)
#### plot
plot(fit_aci_id8_post)
#### add to dataframe
aci_data_id8_post_data <- cbind(aci_data_id8_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id8_post[,30]),
                                  mean(aci_data_id8_post[,118]),
                                  fit_aci_id8_post[[2]][1,1],
                                  fit_aci_id8_post[[2]][1,2],
                                  fit_aci_id8_post[[2]][2,1],
                                  fit_aci_id8_post[[2]][2,2],
                                  fit_aci_id8_post[[2]][3,1],
                                  fit_aci_id8_post[[2]][3,2],
                                  # fit_aci_id8_post[[2]][4,1],
                                  # fit_aci_id8_post[[2]][4,2],
                                  fit_aci_id8_post$RMSE,
                                  fit_aci_id8_post$Ci_transition,
                                  fit_aci_id8_post$citransition,
                                  fit_aci_id8_post$Km,
                                  fit_aci_id8_post$GammaStar,
                                  fit_aci_id8_post$fitmethod,
                                  fit_aci_id8_post$Tcorrect,
                                  fit_aci_id8_post$fitTPU)
colnames(aci_data_id8_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id8_post_data)

##############################################################################
##############################################################################
###########id83
##############################################################################
##############################################################################
### plant id83 pre_heatwave
aci_data_id83_pre = subset(aci_data, id == ids[84] & meas.type == 'pre_heatwave')
aci_data_id83_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id83_pre)
#### fit aci curve
fit_aci_id83_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id83_pre)
summary(fit_aci_id83_pre)
#### plot
plot(fit_aci_id83_pre)
#### add to dataframe
aci_data_id83_pre_data <- cbind(aci_data_id83_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id83_pre[,30]),
                                 mean(aci_data_id83_pre[,118]),
                                 fit_aci_id83_pre[[2]][1,1],
                                 fit_aci_id83_pre[[2]][1,2],
                                 fit_aci_id83_pre[[2]][2,1],
                                 fit_aci_id83_pre[[2]][2,2],
                                 fit_aci_id83_pre[[2]][3,1],
                                 fit_aci_id83_pre[[2]][3,2],
                                 # fit_aci_id83_pre[[2]][4,1],
                                 # fit_aci_id83_pre[[2]][4,2],
                                 fit_aci_id83_pre$RMSE,
                                 fit_aci_id83_pre$Ci_transition,
                                 fit_aci_id83_pre$citransition,
                                 fit_aci_id83_pre$Km,
                                 fit_aci_id83_pre$GammaStar,
                                 fit_aci_id83_pre$fitmethod,
                                 fit_aci_id83_pre$Tcorrect,
                                 fit_aci_id83_pre$fitTPU)
colnames(aci_data_id83_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id83_pre_data)

### plant id83 post_heatwave
aci_data_id83_post = subset(aci_data, id == ids[84] & meas.type == 'post_heatwave')
aci_data_id83_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id83_post)
#### fit aci curve
fit_aci_id83_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id83_post)
summary(fit_aci_id83_post)
#### plot
plot(fit_aci_id83_post)
#### add to dataframe
aci_data_id83_post_data <- cbind(aci_data_id83_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id83_post[,30]),
                                  mean(aci_data_id83_post[,118]),
                                  fit_aci_id83_post[[2]][1,1],
                                  fit_aci_id83_post[[2]][1,2],
                                  fit_aci_id83_post[[2]][2,1],
                                  fit_aci_id83_post[[2]][2,2],
                                  fit_aci_id83_post[[2]][3,1],
                                  fit_aci_id83_post[[2]][3,2],
                                  # fit_aci_id83_post[[2]][4,1],
                                  # fit_aci_id83_post[[2]][4,2],
                                  fit_aci_id83_post$RMSE,
                                  fit_aci_id83_post$Ci_transition,
                                  fit_aci_id83_post$citransition,
                                  fit_aci_id83_post$Km,
                                  fit_aci_id83_post$GammaStar,
                                  fit_aci_id83_post$fitmethod,
                                  fit_aci_id83_post$Tcorrect,
                                  fit_aci_id83_post$fitTPU)
colnames(aci_data_id83_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id83_post_data)

##############################################################################
##############################################################################
###########id86
##############################################################################
##############################################################################
### plant id86 pre_heatwave
# aci_data_id86_pre = subset(aci_data, id == ids[85] & meas.type == 'pre_heatwave')
# aci_data_id86_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id86_pre)
# #### fit aci curve
# fit_aci_id86_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id86_pre)
# summary(fit_aci_id86_pre)
# #### plot
# plot(fit_aci_id86_pre)
# #### add to dataframe
# aci_data_id86_pre_data <- cbind(aci_data_id86_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id86_pre[,30]),
#                                  mean(aci_data_id86_pre[,118]),
#                                  fit_aci_id86_pre[[2]][1,1],
#                                  fit_aci_id86_pre[[2]][1,2],
#                                  fit_aci_id86_pre[[2]][2,1],
#                                  fit_aci_id86_pre[[2]][2,2],
#                                  fit_aci_id86_pre[[2]][3,1],
#                                  fit_aci_id86_pre[[2]][3,2],
#                                  # fit_aci_id86_pre[[2]][4,1],
#                                  # fit_aci_id86_pre[[2]][4,2],
#                                  fit_aci_id86_pre$RMSE,
#                                  fit_aci_id86_pre$Ci_transition,
#                                  fit_aci_id86_pre$citransition,
#                                  fit_aci_id86_pre$Km,
#                                  fit_aci_id86_pre$GammaStar,
#                                  fit_aci_id86_pre$fitmethod,
#                                  fit_aci_id86_pre$Tcorrect,
#                                  fit_aci_id86_pre$fitTPU)
# colnames(aci_data_id86_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id86_pre_data)
# 
# ### plant id86 post_heatwave
# aci_data_id86_post = subset(aci_data, id == ids[85] & meas.type == 'post_heatwave')
# aci_data_id86_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id86_post)
# #### fit aci curve
# fit_aci_id86_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id86_post)
# summary(fit_aci_id86_post)
# #### plot
# plot(fit_aci_id86_post)
# #### add to dataframe
# aci_data_id86_post_data <- cbind(aci_data_id86_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id86_post[,30]),
#                                   mean(aci_data_id86_post[,118]),
#                                   fit_aci_id86_post[[2]][1,1],
#                                   fit_aci_id86_post[[2]][1,2],
#                                   fit_aci_id86_post[[2]][2,1],
#                                   fit_aci_id86_post[[2]][2,2],
#                                   fit_aci_id86_post[[2]][3,1],
#                                   fit_aci_id86_post[[2]][3,2],
#                                   # fit_aci_id86_post[[2]][4,1],
#                                   # fit_aci_id86_post[[2]][4,2],
#                                   fit_aci_id86_post$RMSE,
#                                   fit_aci_id86_post$Ci_transition,
#                                   fit_aci_id86_post$citransition,
#                                   fit_aci_id86_post$Km,
#                                   fit_aci_id86_post$GammaStar,
#                                   fit_aci_id86_post$fitmethod,
#                                   fit_aci_id86_post$Tcorrect,
#                                   fit_aci_id86_post$fitTPU)
# colnames(aci_data_id86_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id86_post_data)

##############################################################################
##############################################################################
###########id88
##############################################################################
##############################################################################
### plant id88 pre_heatwave
# aci_data_id88_pre = subset(aci_data, id == ids[86] & meas.type == 'pre_heatwave')
# aci_data_id88_pre[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id88_pre)
# #### fit aci curve
# fit_aci_id88_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                            # citransition = 300,
#                            Tcorrect = FALSE,
#                            fitTPU = FALSE,
#                            fitmethod = 'bilinear',
#                            data = aci_data_id88_pre)
# summary(fit_aci_id88_pre)
# #### plot
# plot(fit_aci_id88_pre)
# #### add to dataframe
# aci_data_id88_pre_data <- cbind(aci_data_id88_pre[1, c(10, 284, 9, 14, 16, 19)],
#                                  mean(aci_data_id88_pre[,30]),
#                                  mean(aci_data_id88_pre[,118]),
#                                  fit_aci_id88_pre[[2]][1,1],
#                                  fit_aci_id88_pre[[2]][1,2],
#                                  fit_aci_id88_pre[[2]][2,1],
#                                  fit_aci_id88_pre[[2]][2,2],
#                                  fit_aci_id88_pre[[2]][3,1],
#                                  fit_aci_id88_pre[[2]][3,2],
#                                  # fit_aci_id88_pre[[2]][4,1],
#                                  # fit_aci_id88_pre[[2]][4,2],
#                                  fit_aci_id88_pre$RMSE,
#                                  fit_aci_id88_pre$Ci_transition,
#                                  fit_aci_id88_pre$citransition,
#                                  fit_aci_id88_pre$Km,
#                                  fit_aci_id88_pre$GammaStar,
#                                  fit_aci_id88_pre$fitmethod,
#                                  fit_aci_id88_pre$Tcorrect,
#                                  fit_aci_id88_pre$fitTPU)
# colnames(aci_data_id88_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                        'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id88_pre_data)
# 
# ### plant id88 post_heatwave
# aci_data_id88_post = subset(aci_data, id == ids[86] & meas.type == 'post_heatwave')
# aci_data_id88_post[, c(6,9:10)]
# #### plot raw data and remove values if needed
# plot(Adyn ~ Ci, data = aci_data_id88_post)
# #### fit aci curve
# fit_aci_id88_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
#                             # citransition = 300,
#                             Tcorrect = FALSE,
#                             fitTPU = FALSE,
#                             fitmethod = 'bilinear',
#                             data = aci_data_id88_post)
# summary(fit_aci_id88_post)
# #### plot
# plot(fit_aci_id88_post)
# #### add to dataframe
# aci_data_id88_post_data <- cbind(aci_data_id88_post[1, c(10, 284, 9, 14, 16, 19)],
#                                   mean(aci_data_id88_post[,30]),
#                                   mean(aci_data_id88_post[,118]),
#                                   fit_aci_id88_post[[2]][1,1],
#                                   fit_aci_id88_post[[2]][1,2],
#                                   fit_aci_id88_post[[2]][2,1],
#                                   fit_aci_id88_post[[2]][2,2],
#                                   fit_aci_id88_post[[2]][3,1],
#                                   fit_aci_id88_post[[2]][3,2],
#                                   # fit_aci_id88_post[[2]][4,1],
#                                   # fit_aci_id88_post[[2]][4,2],
#                                   fit_aci_id88_post$RMSE,
#                                   fit_aci_id88_post$Ci_transition,
#                                   fit_aci_id88_post$citransition,
#                                   fit_aci_id88_post$Km,
#                                   fit_aci_id88_post$GammaStar,
#                                   fit_aci_id88_post$fitmethod,
#                                   fit_aci_id88_post$Tcorrect,
#                                   fit_aci_id88_post$fitTPU)
# colnames(aci_data_id88_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
#                                         'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
#                                         'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
#                                         'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
#                                         'aci_km', 'aci_gammastar', 'aci_fitmethod',
#                                         'aci_tcorrect', 'aci_fittpu')
# curve_fits <- rbind(curve_fits, aci_data_id88_post_data)

##############################################################################
##############################################################################
###########id9
##############################################################################
##############################################################################
### plant id9 pre_heatwave
aci_data_id9_pre = subset(aci_data, id == ids[87] & meas.type == 'pre_heatwave' & Ci < 900)
aci_data_id9_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id9_pre)
#### fit aci curve
fit_aci_id9_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id9_pre)
summary(fit_aci_id9_pre)
#### plot
plot(fit_aci_id9_pre)
#### add to dataframe
aci_data_id9_pre_data <- cbind(aci_data_id9_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id9_pre[,30]),
                                 mean(aci_data_id9_pre[,118]),
                                 fit_aci_id9_pre[[2]][1,1],
                                 fit_aci_id9_pre[[2]][1,2],
                                 fit_aci_id9_pre[[2]][2,1],
                                 fit_aci_id9_pre[[2]][2,2],
                                 fit_aci_id9_pre[[2]][3,1],
                                 fit_aci_id9_pre[[2]][3,2],
                                 # fit_aci_id9_pre[[2]][4,1],
                                 # fit_aci_id9_pre[[2]][4,2],
                                 fit_aci_id9_pre$RMSE,
                                 fit_aci_id9_pre$Ci_transition,
                                 fit_aci_id9_pre$citransition,
                                 fit_aci_id9_pre$Km,
                                 fit_aci_id9_pre$GammaStar,
                                 fit_aci_id9_pre$fitmethod,
                                 fit_aci_id9_pre$Tcorrect,
                                 fit_aci_id9_pre$fitTPU)
colnames(aci_data_id9_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id9_pre_data)

### plant id9 post_heatwave
aci_data_id9_post = subset(aci_data, id == ids[87] & meas.type == 'post_heatwave')
aci_data_id9_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id9_post)
#### fit aci curve
fit_aci_id9_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id9_post)
summary(fit_aci_id9_post)
#### plot
plot(fit_aci_id9_post)
#### add to dataframe
aci_data_id9_post_data <- cbind(aci_data_id9_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id9_post[,30]),
                                  mean(aci_data_id9_post[,118]),
                                  fit_aci_id9_post[[2]][1,1],
                                  fit_aci_id9_post[[2]][1,2],
                                  fit_aci_id9_post[[2]][2,1],
                                  fit_aci_id9_post[[2]][2,2],
                                  fit_aci_id9_post[[2]][3,1],
                                  fit_aci_id9_post[[2]][3,2],
                                  # fit_aci_id9_post[[2]][4,1],
                                  # fit_aci_id9_post[[2]][4,2],
                                  fit_aci_id9_post$RMSE,
                                  fit_aci_id9_post$Ci_transition,
                                  fit_aci_id9_post$citransition,
                                  fit_aci_id9_post$Km,
                                  fit_aci_id9_post$GammaStar,
                                  fit_aci_id9_post$fitmethod,
                                  fit_aci_id9_post$Tcorrect,
                                  fit_aci_id9_post$fitTPU)
colnames(aci_data_id9_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id9_post_data)

##############################################################################
##############################################################################
###########id92
##############################################################################
##############################################################################
### plant id92 pre_heatwave
aci_data_id92_pre = subset(aci_data, id == ids[88] & meas.type == 'pre_heatwave')
aci_data_id92_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id92_pre)
#### fit aci curve
fit_aci_id92_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id92_pre)
summary(fit_aci_id92_pre)
#### plot
plot(fit_aci_id92_pre)
#### add to dataframe
aci_data_id92_pre_data <- cbind(aci_data_id92_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id92_pre[,30]),
                                 mean(aci_data_id92_pre[,118]),
                                 fit_aci_id92_pre[[2]][1,1],
                                 fit_aci_id92_pre[[2]][1,2],
                                 fit_aci_id92_pre[[2]][2,1],
                                 fit_aci_id92_pre[[2]][2,2],
                                 fit_aci_id92_pre[[2]][3,1],
                                 fit_aci_id92_pre[[2]][3,2],
                                 # fit_aci_id92_pre[[2]][4,1],
                                 # fit_aci_id92_pre[[2]][4,2],
                                 fit_aci_id92_pre$RMSE,
                                 fit_aci_id92_pre$Ci_transition,
                                 fit_aci_id92_pre$citransition,
                                 fit_aci_id92_pre$Km,
                                 fit_aci_id92_pre$GammaStar,
                                 fit_aci_id92_pre$fitmethod,
                                 fit_aci_id92_pre$Tcorrect,
                                 fit_aci_id92_pre$fitTPU)
colnames(aci_data_id92_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id92_pre_data)

### plant id92 post_heatwave
aci_data_id92_post = subset(aci_data, id == ids[88] & meas.type == 'post_heatwave')
aci_data_id92_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id92_post)
#### fit aci curve
fit_aci_id92_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id92_post)
summary(fit_aci_id92_post)
#### plot
plot(fit_aci_id92_post)
#### add to dataframe
aci_data_id92_post_data <- cbind(aci_data_id92_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id92_post[,30]),
                                  mean(aci_data_id92_post[,118]),
                                  fit_aci_id92_post[[2]][1,1],
                                  fit_aci_id92_post[[2]][1,2],
                                  fit_aci_id92_post[[2]][2,1],
                                  fit_aci_id92_post[[2]][2,2],
                                  fit_aci_id92_post[[2]][3,1],
                                  fit_aci_id92_post[[2]][3,2],
                                  # fit_aci_id92_post[[2]][4,1],
                                  # fit_aci_id92_post[[2]][4,2],
                                  fit_aci_id92_post$RMSE,
                                  fit_aci_id92_post$Ci_transition,
                                  fit_aci_id92_post$citransition,
                                  fit_aci_id92_post$Km,
                                  fit_aci_id92_post$GammaStar,
                                  fit_aci_id92_post$fitmethod,
                                  fit_aci_id92_post$Tcorrect,
                                  fit_aci_id92_post$fitTPU)
colnames(aci_data_id92_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id92_post_data)

##############################################################################
##############################################################################
###########id94
##############################################################################
##############################################################################
### plant id94 pre_heatwave
aci_data_id94_pre = subset(aci_data, id == ids[89] & meas.type == 'pre_heatwave')
aci_data_id94_pre[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id94_pre)
#### fit aci curve
fit_aci_id94_pre = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                           # citransition = 300,
                           Tcorrect = FALSE,
                           fitTPU = FALSE,
                           fitmethod = 'bilinear',
                           data = aci_data_id94_pre)
summary(fit_aci_id94_pre)
#### plot
plot(fit_aci_id94_pre)
#### add to dataframe
aci_data_id94_pre_data <- cbind(aci_data_id94_pre[1, c(10, 284, 9, 14, 16, 19)],
                                 mean(aci_data_id94_pre[,30]),
                                 mean(aci_data_id94_pre[,118]),
                                 fit_aci_id94_pre[[2]][1,1],
                                 fit_aci_id94_pre[[2]][1,2],
                                 fit_aci_id94_pre[[2]][2,1],
                                 fit_aci_id94_pre[[2]][2,2],
                                 fit_aci_id94_pre[[2]][3,1],
                                 fit_aci_id94_pre[[2]][3,2],
                                 # fit_aci_id94_pre[[2]][4,1],
                                 # fit_aci_id94_pre[[2]][4,2],
                                 fit_aci_id94_pre$RMSE,
                                 fit_aci_id94_pre$Ci_transition,
                                 fit_aci_id94_pre$citransition,
                                 fit_aci_id94_pre$Km,
                                 fit_aci_id94_pre$GammaStar,
                                 fit_aci_id94_pre$fitmethod,
                                 fit_aci_id94_pre$Tcorrect,
                                 fit_aci_id94_pre$fitTPU)
colnames(aci_data_id94_pre_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                       'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                       'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                       'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                       'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                       'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id94_pre_data)

### plant id94 post_heatwave
aci_data_id94_post = subset(aci_data, id == ids[89] & meas.type == 'post_heatwave')
aci_data_id94_post[, c(6,9:10)]
#### plot raw data and remove values if needed
plot(Adyn ~ Ci, data = aci_data_id94_post)
#### fit aci curve
fit_aci_id94_post = fitaci(varnames = list(ALEAF = 'Adyn', Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
                            # citransition = 300,
                            Tcorrect = FALSE,
                            fitTPU = FALSE,
                            fitmethod = 'bilinear',
                            data = aci_data_id94_post)
summary(fit_aci_id94_post)
#### plot
plot(fit_aci_id94_post)
#### add to dataframe
aci_data_id94_post_data <- cbind(aci_data_id94_post[1, c(10, 284, 9, 14, 16, 19)],
                                  mean(aci_data_id94_post[,30]),
                                  mean(aci_data_id94_post[,118]),
                                  fit_aci_id94_post[[2]][1,1],
                                  fit_aci_id94_post[[2]][1,2],
                                  fit_aci_id94_post[[2]][2,1],
                                  fit_aci_id94_post[[2]][2,2],
                                  fit_aci_id94_post[[2]][3,1],
                                  fit_aci_id94_post[[2]][3,2],
                                  # fit_aci_id94_post[[2]][4,1],
                                  # fit_aci_id94_post[[2]][4,2],
                                  fit_aci_id94_post$RMSE,
                                  fit_aci_id94_post$Ci_transition,
                                  fit_aci_id94_post$citransition,
                                  fit_aci_id94_post$Km,
                                  fit_aci_id94_post$GammaStar,
                                  fit_aci_id94_post$fitmethod,
                                  fit_aci_id94_post$Tcorrect,
                                  fit_aci_id94_post$fitTPU)
colnames(aci_data_id94_post_data) <- c('id', 'heatwave_time', 'machine', 'anet_420', 'ci_420', 'gs_420',
                                        'vpd_leaf', 'temperature_leaf', 'vcmax_tleaf', 'vcmax_tleaf_se',
                                        'jmax_tleaf', 'jmax_tleaf_se', 'rd_tleaf', 'rd_tleaf_se',
                                        'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                                        'aci_km', 'aci_gammastar', 'aci_fitmethod',
                                        'aci_tcorrect', 'aci_fittpu')
curve_fits <- rbind(curve_fits, aci_data_id94_post_data)

nrow(curve_fits)
# write.csv(curve_fits, 'aci_fits/aci_fits.csv')