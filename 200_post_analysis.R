# ------------------------------------------------------------------------------
# post analysis of boruta'd survey important variables
# what is the median (IQR) of the number of important variables?
# ------------------------------------------------------------------------------


rm(list=ls())
gc()



all_results <- read.dta("output/MF_validation_ALL_WRF_inv_prev.dta")
all_results <- read.dta("output/MF_validation_ALL_pre_WRF.dta")
# analysis of prediction: Se, Sp, PPV, NPV
surv_results <- all_results[ , names(all_results)[!(names(all_results) %in% c("BORUTA"))] ] # drop BORUTA
surv_results <- unique(surv_results) # and get rid of repeated rows (used to be 1 per important var)
surv_results <- surv_results[order(-1*surv_results$PREVALENCE_MURHO), ] # ordered by prevalence

# take a look at results
head(surv_results)
summary(surv_results)
# summary of only those with meaningful predictions (ie: Sensitivty not 0, or 1)
meaningful_results <- surv_results[!(surv_results$SE %in% c(1,0)),]
meaningful_results$SEX <- factor(meaningful_results$SEX)
summary(meaningful_results)
nrow(meaningful_results)
# how many surveys represented?
length(table(meaningful_results$SURVEY)[table(meaningful_results$SURVEY)!=0])
# CreateTableOne(vars = names(meaningful_results), strata = c("SEX"), data = meaningful_results)

# total population possible
sum(meaningful_results$N_HAS_HIV_DATA)
# total population analyzed
sum(meaningful_results$N_HAS_HIV_DATA_MURHO)
# total population HIV+
sum(meaningful_results$N_HIV_POS_MURHO)

# feature list from Boruta ---------------------------


# read in labels for variables (from zm71)
labels <- read.csv("/Users/echow/QSU/DHS_boruta/data/dictionary.csv", sep=",")
head(labels)
labels$var <- as.character(labels$var)
labels$label <- as.character(labels$label)

# how many features were selected
meaningful_features <- all_results[!(all_results$SE %in% c(0,1)), ]
meaningful_features$SURVEY <- factor(meaningful_features$SURVEY)
# how many features selected per survey?
summary(as.numeric(table(meaningful_features$SURVEY)))

# frequency of features
num_meaningful_surveys <- nrow(unique(meaningful_features[,c("SURVEY", "SEX")]))
num_meaningful_surveys

# FEATURES OVERALL
features <- data.frame(table(meaningful_features$BORUTA)/num_meaningful_surveys) # frequency percent
features <- features[order(-1*features$Freq), ] #sorted
features$var <- as.character(features$Var1)
features <- join(features, labels, by=c("var"), type="left", match="first")
head(features, 15) # list important features


# FEATURES FOR MALES
meaningful_features_m <- meaningful_features[meaningful_features$SEX == 1, ]
num_meaningful_surveys_m <- nrow(unique(meaningful_features_m[,c("SURVEY", "SEX")]))
features_m <- data.frame(table(meaningful_features_m$BORUTA)/num_meaningful_surveys_m) # frequency percent
features_m <- features_m[order(-1*features_m$Freq), ] #sorted
features_m$var <- as.character(features_m$Var1)
features_m <- join(features_m, labels, by=c("var"), type="left", match="first")
head(features_m, 30)
features_m

# FEATURES FOR FEMALES
meaningful_features_f <- meaningful_features[meaningful_features$SEX == 2, ]
num_feaningful_surveys_f <- nrow(unique(meaningful_features_f[,c("SURVEY", "SEX")]))
features_f <- data.frame(table(meaningful_features_f$BORUTA)/num_feaningful_surveys_f) # frequency percent
features_f <- features_f[order(-1*features_f$Freq), ] #sorted
features_f$var <- as.character(features_f$Var1)
features_f <- join(features_f, labels, by=c("var"), type="left", match="first")
head(features_f, 60)
features_f



# ------------------------------------------------------------------------------
# PPV and Se, Sp analysis
# ROC curves
roc_ <- read.dta("data/roc_ALL.dta")
meaning_ <- meaningful_results[,c("SURVEY", "SEX")]
meaning_$survey <- meaning_$SURVEY
meaning_$sex <- meaning_$SEX
meaning_$flag <- 1
meaning_ <- meaning_[,c("survey", "sex", "flag")]

roc <- join(roc_, meaning_, by=c("survey", "sex"), type="left", match="first")
roc <- roc[!is.na(roc$flag), ]
head(roc)
summary(roc)
unique(roc$survey)

# ROC plot
roc_p <- roc(roc$ref, roc$prob)
plot(roc_p, col=0, lty=c(2), main="", grid=c(0.2, 0.2))
legend(x=0.1, y=0.85,unique(roc$survey),pch=15, col=1:length(unique(roc$survey)), bty="n", cex=0.6)
legend("bottomright",c("male","female"),lty=1:2, col="black", bty="n", cex=0.6)
text(0.85,0.80, srt=45, "max AUC=0.87")
text(0.65,0.5, srt=45, "min AUC=0.65")
i = 1
for (survey in unique(roc$survey)) {
    roc_t <- roc[roc$survey == survey & roc$sex == 1, ]
    if (nrow(roc_t) != 0) {
        roc_p <- roc(roc_t$ref, roc_t$prob)
        plot(roc_p, col=i, lty=1, lwd=0.8, add=TRUE)
    }
    i = i+1
}
i = 1
for (survey in unique(roc$survey)) {
    roc_t <- roc[roc$survey == survey & roc$sex == 2, ]
    if (nrow(roc_t) != 0) {
        roc_p <- roc(roc_t$ref, roc_t$prob)
        plot(roc_p, col=i, lty=2, lwd=0.8, add=TRUE)
    }
    i = i+1
}


# get survey list for year
survey_list <- read.delim("/Users/echow/QSU/DHS_boruta/survey_list/survey_list.csv", sep=",")
head(survey_list)
survey_list$SURVEY <- substr(survey_list$filename, 1, 4)

mr <- join(meaningful_results, survey_list[,c("SURVEY","year","country")], by=c("SURVEY"), type="left", match="first")
ar <- join(all_results, survey_list[,c("SURVEY","year")], by=c("SURVEY"), type="left", match="first")
head(mr)
ggplot(mr, aes(year, AUC)) + geom_smooth(method="lm") + geom_point(color="blue")
auc_time <- lm(AUC ~ year, data=mr)
summary(auc_time)
cor(mr$year, mr$AUC)

# get Se/Sp that maximizes the PPV ...

# survey <- unique(roc$survey)[1]

result <- data.frame(survey = NA, sex=NA, se=NA, sp=NA, ppv=NA, thresh=NA, youden=NA)
i <- 1
for (survey in unique(roc$survey)) {
    for (sex in 1:2) {
        roc_t <- roc[roc$survey == survey & roc$sex == sex, ]
        if (nrow(roc_t) != 0) {
            J <- 0          # reset Youden's
            J_max <- 0
            opt_thresh <- 0

            # SOLVE FOR BEST YOUDEN'S
            for (j in 1:500) {
                # thresh <- i/100
                # subset predictions to this survey
                # calculate and apply threshold to probability
                thresh <- j/500 # table(roc_t$ref)[2]/nrow(roc_t) # set the threshold to the prevalence}
                roc_t$class <- factor(as.numeric(roc_t$prob >= thresh))
                # make the same factor levels
                levels(roc_t$class) <- levels(roc_t$ref)
                # get Se, Sp, PPV, NPV
                cm <- confusionMatrix(data = roc_t$class, reference = roc_t$ref, positive = as.character(levels(roc_t$ref)[2])) # use confusion matrix to EVALUATE
                se <- cm$byClass["Sensitivity"]
                sp <- cm$byClass["Specificity"]
                ppv <- cm$byClass["Pos Pred Value"]
                npv <- cm$byClass["Neg Pred Value"]

                # Youden's index
                J <- se + sp - 1
                if (J > J_max) {
                    J_max <- J
                    opt_thresh <- thresh
                }
            } # finish solving for best Youden's

            # --------------------------------------------
            # calcuate the Se/Sp with best Youden's
            thresh <- opt_thresh # table(roc_t$ref)[2]/nrow(roc_t) # set the threshold to the prevalence}
            roc_t$class <- factor(as.numeric(roc_t$prob >= thresh))
            # make the same factor levels
            levels(roc_t$class) <- levels(roc_t$ref)
            # get Se, Sp, PPV, NPV
            cm <- confusionMatrix(data = roc_t$class, reference = roc_t$ref, positive = as.character(levels(roc_t$ref)[2])) # use confusion matrix to EVALUATE
            se <- cm$byClass["Sensitivity"]
            sp <- cm$byClass["Specificity"]
            ppv <- cm$byClass["Pos Pred Value"]
            npv <- cm$byClass["Neg Pred Value"]

            # ---------------------------------
            # set results se; sp; ppv; npv
            result[i, "survey"] <- survey
            result[i, "se"] <- se
            result[i, "sp"] <- sp
            result[i, "ppv"] <- ppv
            result[i, "thresh"] <- thresh
            result[i, "youden"] <- J_max
            result[i, "sex"] <- sex
            i = i + 1
        } # end if nrow
    } # end sex
} # end survey loop

result
abstract_surveys <- meaningful_results[,c("SURVEY", "SEX")]
abstract_surveys$survey <- abstract_surveys$SURVEY
abstract_surveys$sex <- abstract_surveys$SEX

result_abstract <- join(abstract_surveys, result, by=c("survey", "sex"), type="left", match="first")
result_abstract
nrow(result_abstract)
summary(result_abstract)
#
#
#
#
# # *****************************************************
# # cycle through a bunch of Mu and Rhos, and for each
# # apply the restrictions and see how many are lost
#
# # an array to store results
# sumstats <- data.frame(mu=NA, rho=NA, vars=NA, cat_vars=NA, obs=NA, tn=NA, fn=NA, tp=NA, fp=NA, se=NA, sp=NA, ppv=NA, npv=NA)
#
# # index var for storing meta results
# i = 1
# m = 5; r = 2
# set.seed(314)
# # takes about ... um ... some time to run! 5:26 -
# # loop through Mu and Rho's
# for (m in 1:9) {
#     for (r in 1:4) {
#         mu = m/20; rho = r/10
#         d    <- NULL
#         dtrn <- NULL
#         dtst <- NULL
#         modelFit <- NULL
#         predictions <-NULL
#         cm <- NULL
#
#         try ({
#             # apply mu and rho -------------------------------------
#
#             # append train and test set so can murho (and get same levels)
#             # but then split again
#             survhivt$train <- 1
#             survhivv$train <- 0
#
#             # recombine the train and test sets
#             survhiva <- rbind(survhivt, survhivv)
#             # apply murho to both train and test
#             da <- murho(data = survhiva, mu = mu, rho = rho)
#             # resplit into train and test
#             d  <- da[da$train==1, ]
#             dv <- da[da$train==0, ]
#
#             # count factors
#             factor_i = 0
#             for (var in names(d)) {
#                 if (is.factor(d[,var])) {
#                     factor_i <- factor_i + 1
#                 }
#             }
#             # do I sample within the dataset after mu/rho, or use survhivv, the validation set?
#
#             # YES, do THIS one! use the validation data that was set aside prior to murho
#             if (do_OOS_valid) { # use OOS validation, the CORRECT way
#                 # don't partition the data -----------------------------------
#                 # use whole murho's dataset to train
#                 # train <- createDataPartition(y = d$hiv03, p=0.80, list=FALSE)
#                 # subset using partition
#                 dtrn  <- d      # used for fitting the model, and Mu and Rho etc
#                 dtst  <- dv  # THE VALIDATION SET,  survhivv (validation) was murho'd
#
#                 # build a predictor ------------------------------------
#                 message("running randomForest ...")
#                 modelFit <- randomForest(hiv03 ~ . , data = dtrn, importance = TRUE)  # will auto center and scale
#
#                 # do OUT OF sample validation --------------------------
#                 message("RF complete! predicting from test set ...")
#                 predictions <- predict(modelFit, newdata=dtst)
#                 # compare to actual
#                 cm <- confusionMatrix(predictions, dtst$hiv03) # use confusion matrix to EVALUATE
#
#                 # store meta results
#                 sumstats[i,"mu"] <- mu
#                 sumstats[i,"rho"] <- rho
#                 sumstats[i,"obs"] <- dim(d)[1]
#                 sumstats[i,"vars"] <- dim(d)[2]
#                 sumstats[i,"cat_vars"] <- factor_i
#
#                 sumstats[i,"tn"] <- cm$table[1,1]
#                 sumstats[i,"fn"] <- cm$table[1,2]
#                 sumstats[i,"tp"] <- cm$table[2,2]
#                 sumstats[i,"fp"] <- cm$table[2,1]
#
#                 sumstats[i,"se"] <- cm$byClass["Sensitivity"]
#                 sumstats[i,"sp"] <- cm$byClass["Specificity"]
#                 sumstats[i,"ppv"] <- cm$byClass["Pos Pred Value"]
#                 sumstats[i,"npv"] <- cm$byClass["Neg Pred Value"]
#                 message("DONE!\n")
#             }
#             i = i + 1
#         })
#     }
# }
#
# # show results
# sumstats
#
# # ------------------------------------------------------------------------------
# # plot mu vs. # vars
# pdf("figs/mu_vars_oos.pdf", height=4, width=4.7)
#     ggplot(sumstats, aes(x=mu, y=vars)) + geom_point(aes(size=obs))
# dev.off()
#
# # plot mu vs. Sensitivity
# pdf("figs/mu_se_oos.pdf", height=4, width=4.7)
#     ggplot(sumstats[1:32,], aes(x=mu, y=se)) + geom_jitter(width=0.005, height=0.001) + geom_smooth(method="loess")
# dev.off()
#
#
#
#
#
#
#
#
#
#
# #      ~ fin ~
