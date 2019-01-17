# ------------------------------------------------------------
# this function selects a survey, runs random forest, and
# does Boruta to select some features.
#
# Jan 14, 2019 NOTES -- TODO! cforest, w/o replacement, permutation importance
# [ ] use cforest instead of ranForest - conditional inference tree doesn't
#     bias selection towards continuous variables, or those w more levels
# [ ] set subsampling without replacement, bootstraping sampling induces bias
# [ ] use permutation importance, not Gini importance
# [ ] use the conditional permutation importance, not marginal importance
#
# Eric Chow, 2018-10-18
# ------------------------------------------------------------

# ------------------------------------------------------------------------------
# THIS IS WHERE FUNCTION WOULD START
# ------------------------------------------------------------------------------
do_cforest <- function(survey_filepath, mtadat_filepath, sex, this_survey, skip_cforest = TRUE, w_neg=1, w_pos=1, seed) {

    # RETURN VARIABLES:
    # PERCENT_W_HIV_DATA - the percent of the original data rows that has HIV results (pre-murho)
    # N_HAS_HIV_DATA     - the number of obs w HIV test result  (pre-murho)
    # N_HIV_POS          - the number of obs that are HIV positive  (pre-murho)
    # PREVALENCE         - the prevalence of HIV+  (pre-murho)
    # N_HAS_HIV_DATA_MURHO     - the number of obs w HIV test result  (pre-murho)
    # N_HIV_POS_MURHO          - the number of obs that are HIV positive  (pre-murho)
    # PREVALENCE_MURHO         - the prevalence of HIV+  (pre-murho)

    survey  <- readRDS(survey_filepath) # survey
    meta    <- data.frame(read_dta(mtadat_filepath)) # meta data
    sex     <- sex


    seed    <- 314
    mu      <- 0.15
    rho     <- 0.2
    split   <- 0.80

    # -------------------------------------------
    # Keep ONLY SEX == sex here
    survey <- survey[((as.numeric(survey$hv104) == sex) & !is.na(survey$hv104)),  ]
    SEX <- sex
    # -------------------------------------------

    # -------------------------------------------
    # keep observations with HIV test result data
    survhiv <- survey[!is.na(survey$hiv03), ]
    # keep only if HIV status == negative or positive
    survhiv <- survhiv[as.numeric(survhiv$hiv03) %in% c(1,2), ]
    survhiv$hiv03 <- factor(survhiv$hiv03)
    # dim(survhiv)                # 32,192 obs
    PERCENT_W_HIV_DATA <- nrow(survhiv)/nrow(survey)  # 73.7% of obs had HIV

    # what percent had HIV data?
    # if (nrow(survey) != nrow(survhiv)) {message( nrow(survey) - nrow(survhiv)," (", round(100*(nrow(survey)-nrow(survhiv))/nrow(survey)), "%) observations were dropped." )}

    N_HAS_HIV_DATA     <- nrow(survhiv)               # still 32,192 obs - so they were all 0,1
    N_HIV_POS  <- table(survhiv$hiv03)[2]
    PREVALENCE <- table(survhiv$hiv03)[2]/nrow(survhiv)

    # how many men/women?  HV104 is sex
        #
        # table(survhiv$hv104)/nrow(survhiv)
        # table(survhiv$hiv03, survhiv$hv104)  # 6.8% of men are hiv[+], 10.3% of females are hiv[+]
        # table(survhiv$hiv03, survhiv$hv104)[2,]/table(survhiv$hv104) # proportion of men/women HIV+
        # prop.test(table(survhiv$hv104, survhiv$hiv03))
        #
    # put survhiv into another dataset to preserve survhiv for later
    # survhiv_ <- survhiv

    # -------------------------------------------
    # subset out a validation set
    set.seed(seed)
    train <- createDataPartition(y = survhiv$hiv03, p = split, list=FALSE)

    # Subset to an out-of-sample validation set, and dataset for training
    survhivt <- survhiv[train, ]    # used for fitting the model, and Mu and Rho etc
    dim(survhivt)               # 25,690 obs (~80%)
    survhivv <- survhiv[-train, ]  # THE OOS VALIDATION SET
    dim(survhivv)               # 6,422 obs  (~20%)

    # pre-process with mu and rho
    survhivt$train <- 1
    survhivv$train <- 0
    # recombine the train and test sets
    survhiva <- rbind(survhivt, survhivv)

    # ----------------------------------------------
    # apply murho to both train and test
    # ----------------------------------------------
    da <- murho(data = survhiva, mu = mu, rho = rho, meta = meta)
    # da$hiv03 <- factor(da$hiv03)

    # ----------------------------------------------
    # drop variables that have too many levels (char)
    # ----------------------------------------------
    da <- da[,!(names(da) %in% c("shdist","sdist","_append"))]

    # -----------------------------------------------
    # drop variables that would perfectly predict HIV
    # -----------------------------------------------
    # shiv51 - confirmed HIV status
    # sh279  - result of determine hiv rdt
    # sh279a - result of unigold hiv rdt
    # sh278  - result measurement code of rapid hiv test
    # sh235  - result of determine hiv rdt
    # sh235a - result of unigold hiv rdt
    # sh234  - result measurement code of rapid hiv test
    #
    # sh224 - read consent statement - rapid hiv test

    sh279_vars <- names(da)[isin("sh279", names(da))]
    sh278_vars <- names(da)[isin("sh278", names(da))]

    sh235_vars <- names(da)[isin("sh235", names(da))]
    sh234_vars <- names(da)[isin("sh234", names(da))]

    sh224_vars <- names(da)[isin("sh224", names(da))]

    hiv06_vars <- names(da)[isin("hiv06", names(da))]
    hiv07_vars <- names(da)[isin("hiv07", names(da))]
    hiv08_vars <- names(da)[isin("hiv08", names(da))]

    sprelim_vars <- names(da)[isin("sprelim", names(da))]
    srecent_vars <- names(da)[isin("srecent", names(da))]

    da <- da[,!(names(da) %in% c("shiv51", sh279_vars, sh278_vars, sh235_vars, sh234_vars, sh224_vars, hiv06_vars, hiv07_vars, hiv08_vars, sprelim_vars, srecent_vars))]


    # resplit into train and test
    survhivt_murhoed <- da[da$train==1, ]
    survhivv_murhoed <- da[da$train==0, ]

    N_HAS_HIV_DATA_MURHO <- nrow(survhivt_murhoed)
    N_HIV_POS_MURHO <- table(survhivt_murhoed$hiv03)[2]           # 91% HIV (-), 8.7% HIV (+)
    n_hiv_neg_murho <- table(survhivt_murhoed$hiv03)[1]           # HIV negative - for class weights
    PREVALENCE_MURHO <- table(survhivt_murhoed$hiv03)[2]/nrow(survhivt_murhoed)
    N_VARS_MURHO <- ncol(survhivt_murhoed)

    #prevalence of HIV in VALIDATION
    # table(survhivv_murhoed$hiv03)
    # table(survhivv$hiv03)/nrow(survhivv)

    # how many men/women?  HV104 is sex
    # table(survhivt_murhoed$hv104)/nrow(survhivt_murhoed)
    # table(survhivt_murhoed$hiv03, survhivt_murhoed$hv104)  # 6.8% of men are hiv[+], 10.3% of females are hiv[+]
    # table(survhivt_murhoed$hiv03, survhivt_murhoed$hv104)[2,]/table(survhivt_murhoed$hv104) # proportion of men/women HIV+
    # prop.test(table(survhivt_murhoed$hv104, survhivt_murhoed$hiv03))



        # -----------------------------------------------------
        # 1. tweak the max # of vars - sqrt, % of vars, any number, mtry
        # 2. minium leaf size - avoid overfitting ie: node size
        # 3. number of trees, ntree
         # will auto center and scale
        nvars    <- ncol(survhivt_murhoed) # sample about half of the variables (200/435)
        nobs     <- round(nrow(survhivt_murhoed),0)
        nodesize <- 12 # min number in each leaf

        nvars # number of vars
        nobs # number of obseravations
        nodesize

        # set ntree and mtry
        ntree = round(nobs * nvars * 0.000131,0)  # eyeball around 150-200 trees for 3563*430 obs*vars
        mtry  = round(nvars / 2, 0) #

        ntree
        mtry

    # --------------------------------------------------------------------------
    # RUN RANDOM FOREST TRAINING
    cat("random foresting...  ")
    # Run a random forest with all features
    rf_fit <- NULL
    # 10/19/2018 - try classwt (1,2) ie: (negative, positive) = (PREVALENCE_MURHO, 1/PREVALENCE_MURHO)

          # ---------------------
          # SET CLASS WEIGHT
          w_neg <- w_neg #1 #as.numeric(PREVALENCE_MURHO) # oh shit, actually set it to 1.  because there are 99 X 1 majority cases, and 1 X (1/0.01 ie: 100) minority cases
          w_pos <- w_pos #(as.numeric(n_hiv_neg_murho) / as.numeric(N_HIV_POS_MURHO)) # used to be 1/PREVALENCE_MURHO
          # ---------------------

    rf_fit <- randomForest(hiv03 ~ . , data = survhivt_murhoed, importance = TRUE,
        ntree=ntree, mtry=mtry, nodesize=nodesize, classwt=c(w_neg, w_pos)) #, strata = hv104)
    rf_fit
    rf_fit$type
    NTREE <- rf_fit$ntree
    MTRY  <- rf_fit$mtry

    # calculate an ROC
    # head(rf_fit$votes[,2]) # the HIV+ votes
    # rf_fit.roc <- roc(survhivt_murhoed$hiv03, rf_fit$votes[,2])
    # plot(rf_fit.roc)
    # auc(rf_fit.roc)

    # list important vars sorted
    imp_rf <- rf_fit$importance[,3]; imp_rf <- data.frame(imp_rf[order(-imp_rf)])
    names(imp_rf) <- "importance"        # clean up importance name
    imp_rf$rank <- 1:nrow(imp_rf)        # give a rank
    imp_rf$ttl_feature_n <- nrow(imp_rf) # total number of features in importance list
    imp_rf$varname <- row.names(imp_rf)  # get var names for merging
    head(imp_rf,15)

    # get the OOB Se
    get_se(rf_fit)

    # --------------------------------------------------------------------------
    # PREDICTION STEP USING VALIDATION DATSET
    cat("predicting...  ")
    # do validation
    predictions <- predict(rf_fit, newdata = survhivv_murhoed)              # classification
    predi_probs <- predict(rf_fit, newdata = survhivv_murhoed, type="prob") # predicted probabilities
    length(predictions)

    # make an roc dataset from validation set
    roc.data <- data.frame(ref=survhivv_murhoed$hiv03, prob=predi_probs[,2])
    roc.data$survey <- this_survey
    roc.data$sex    <- sex
    # write.dta(roc.data, str_c("data/roc_zm63_2.dta"))
    write.dta(roc.data, str_c("data/roc_",this_survey,"_",sex,".dta"))
    # head(roc.data)
    predicted.roc <- roc(roc.data$ref, roc.data$prob)
    # plot(predicted.roc)

    # compare to actual
    cm <- confusionMatrix(data = predictions, reference = survhivv_murhoed$hiv03, positive = as.character(levels(survhivv_murhoed$hiv03)[2])) # use confusion matrix to EVALUATE

    # the outcomes
    SE <- cm$byClass["Sensitivity"]
    SP <- cm$byClass["Specificity"]
    PPV <- cm$byClass["Pos Pred Value"]
    NPV <- cm$byClass["Neg Pred Value"]
    AUC <- auc(predicted.roc)

    tn <- cm$table[1,1]
    tp <- cm$table[2,2]
    fn <- cm$table[1,2]
    fp <- cm$table[2,1]

    # ------------------------------------------------------------------------------
    # Run Boruta to get important features
    imprtnt_vars <- NA
    if (skip_cforest ==FALSE) {
        cat("boruting.... \n\n")
        br_fit <- Boruta(hiv03 ~ . , data = survhivt_murhoed) # , doTrace=2) #, strata=hv104)
        summary(br_fit)
        br_fit
        br_fit$pValue
        br_fit$impSource
        br_fit$mcAdj

        # print out Boruta plot
        pdf(file = str_c("figs/",this_survey,"_",sex,".pdf"), width=14)
            plot(br_fit)
        dev.off()

        # get important vars
        imprtnt_vars <- c(getSelectedAttributes(br_fit, withTentative=FALSE),"hiv03")
        br_imp_vars <- data.frame(varname = imprtnt_vars)
        # and merge attributes from the single-run random forest
        br_imp_vars <- join(br_imp_vars, imp_rf, by="varname", type="left", match="first")
    }
    else {
      cat("skipped Boruta! \n\n")
    }
                    # # --------------------------------------------------------------------------
                    # # RANDOM FOREST WITH ONLY BORUTA-SELECTED VARIABLES - jsut testing it out
                    # if (1==0) {
                    #     # Get the selected attributes
                    #     imprtnt_vars <- c(getSelectedAttributes(br_fit, withTentative=FALSE),"hiv03")
                    #     survhivt_murhoed_b <- survhivt_murhoed[ ,imprtnt_vars]
                    #
                    #     nvars_b <- ncol(survhivt_murhoed_b)
                    #     nobs_b  <- round(nrow(survhivt_murhoed_b),0)
                    #     mtry = nvars_b # round(nvars_b/2,0)
                    #     ntree = round(nobs_b * nvars_b * 0.000131,0)
                    #
                    #     # RANDOM FOREST WITH ONLY BORUTA-SELECTED VARIABLES
                    #     modelFit <- NULL
                    #     modelFit <- randomForest(hiv03 ~ . , data = survhivt_murhoed_b, importance = TRUE,
                    #                 ntree = ntree, mtry = mtry, nodesize=12)
                    #
                    #     get_se(modelFit)
                    #     modelFit
                    #
                    #     # do validation
                    #     predictions <- predict(modelFit, newdata = survhivv_murhoed)
                    #     length(predictions)
                    #     # compare to actual
                    #     cm <- confusionMatrix(data = predictions, reference = survhivv_murhoed$hiv03, positive = "[1]hiv  positive") # use confusion matrix to EVALUATE
                    # } # ------------------------------------------------------------------------
                    #


    return(
        data.frame(
        PERCENT_W_HIV_DATA = rep(PERCENT_W_HIV_DATA, length(imprtnt_vars)),
        N_HAS_HIV_DATA = rep(N_HAS_HIV_DATA, length(imprtnt_vars)),
        N_HIV_POS= rep(N_HIV_POS, length(imprtnt_vars)),
        PREVALENCE = rep(PREVALENCE, length(imprtnt_vars)),
        N_HAS_HIV_DATA_MURHO = rep(N_HAS_HIV_DATA_MURHO, length(imprtnt_vars)),
        N_VARS_MURHO = rep(N_VARS_MURHO, length(imprtnt_vars)),
        N_HIV_POS_MURHO= rep(N_HIV_POS_MURHO, length(imprtnt_vars)),
        PREVALENCE_MURHO = rep(PREVALENCE_MURHO, length(imprtnt_vars)),
        SEX = rep(SEX, length(imprtnt_vars)),
        SE = rep(SE, length(imprtnt_vars)),
        SP = rep(SP, length(imprtnt_vars)),
        PPV = rep(PPV, length(imprtnt_vars)),
        NPV = rep(NPV, length(imprtnt_vars)),
        AUC = rep(AUC, length(imprtnt_vars)),
        tn  = rep(tn, length(imprtnt_vars)),
        tp  = rep(tp, length(imprtnt_vars)),
        fn  = rep(fn, length(imprtnt_vars)),
        fp  = rep(fp, length(imprtnt_vars)),
        NTREE = rep(NTREE, length(imprtnt_vars)),
        MTRY = rep(MTRY, length(imprtnt_vars)),
        br_imp_vars
        # BORUTA = imprtnt_vars
        )
    )
} # end do_cforest



# #      ~ fin ~
