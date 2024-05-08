# Analysis - Wave 12 Data
# ---- Cleaning ----
indaf <- index_analysis_full
indaf$sex_dv <- as.factor(ifelse(indaf$sex_dv == 2, 1, 0))
indaf$hiqual_dv <-  as.factor(ifelse(indaf$hiqual_dv == 1, "Degree",
                                     ifelse(indaf$hiqual_dv == 2, "Higher", 
                                            ifelse(indaf$hiqual_dv == 3, "A-Levels",
                                                   ifelse(indaf$hiqual_dv == 4, "GCSE",
                                                          ifelse(indaf$hiqual == 5, "OtherQual", "NoQual"))))))
indaf$ethn_dv <- factor(indaf$ethn_dv,
                        levels = c(1,2, 3, 4, 5 ,6 ,7 ,8, 9, 10,
                                   11, 12, 13, 14, 15, 16, 17, 97),
                        labels = c("White", "White", "White", "White",
                                   "Mixed", "Mixed", "Mixed", "Mixed",
                                   "Asian", "Asian", "Asian", "Asian", "Asian",
                                   "Black", "Black", "Black",
                                   "OtherEthn", "OtherEthn"))
indaf$jbstat <- factor(indaf$jbstat,
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 97),
                       labels = c("Self", "Empl", "Unemp", "Retired", "OtherJb",
                                  "OtherJb", "Student", "OtherJb", "OtherJb", "OtherJb",
                                  "OtherJb", "OtherJb", "OtherJb", "OtherJb", "OtherJb"))
indaf$unemployed <- ifelse(indaf$jbstat == "Unemp", 1, 0)
indaf$retired <- ifelse(indaf$jbstat == "Retired", 1, 0)
indaf$tenure_dv <- factor(indaf$tenure_dv,
                          levels = c(1, 2, 3, 4, 5 ,6 ,7 ,8),
                          labels = c("Outr", "Mort", "Social", 
                                     "Social", "OtherTen", "Priv",
                                     "Priv", "OtherTen"))
indaf$own <- ifelse(indaf$tenure_dv == "Outr" | indaf$tenure_dv == "Mort", 1, 0)
# Equivalised household income 
indaf$hhead <- indaf$hhsize-indaf$nchoecd_dv-1
indaf$eqw <- 1+indaf$hhead*0.5+indaf$nchoecd_dv*0.3
indaf$eqhhmnnet <- indaf$fihhmnnet1_dv/indaf$eqw
indaf$eqhhmnnet <- indaf$eqhhmnnet/1000

indaf$jbnssec3_dv <- ifelse(indaf$jbnssec3_dv == 1, "M&P",
                            ifelse(indaf$jbnssec3_dv == 2, "I", "R"))
# Would vote for tomorrow
indaf$vote3 <- factor(indaf$vote3,
                      levels = c(1, 2, 3, 4, 5 ,6 ,7, 8, 9, 10, 11, 12, 14, 15, 95, 96, 97),
                      labels = c("Cons", "Lab", "LD", "SNP", "PC", "GP", 
                                 "UU", "sdlp", "AP", "DU", "SF",
                                 "UKIP", "TBP", "CUK", "None", "Cant", "Other"))
indaf$urban_dv <- as.factor(ifelse(indaf$urban_dv == 1, 1, 0))
# Recode opsoc to be increasing in leftist ideology
# opsoca removed because ambiguous left/right
indaf$opsocb <- as.numeric(factor(indaf$opsocb),
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("5", "4", "3", "2", "1"))
indaf$opsocc <- as.numeric(factor(indaf$opsocc),
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("1", "2" , "3", "4", "5"))
indaf$opsocd <- as.numeric(factor(indaf$opsocd),
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("5", "4", "3", "2", "1"))
indaf$opsoce <- as.numeric(factor(indaf$opsocd),
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("5", "4", "3", "2", "1"))
indaf$opsocf <- as.numeric(factor(indaf$opsocf),
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("5", "4", "3", "2", "1"))
indaf$left <- indaf$opsocb + indaf$opsocc + indaf$opsocd +
  indaf$opsoce + indaf$opsocf

indaf$invest <- indaf$fimninvnet_dv/1000
indaf$hhinvest <- indaf$fihhmninv_dv/1000
indaf$hvalue <- indaf$hsval/10000

# Material deprivation - indexed; Don't need coded as not deprived as not part of individual considerations
indaf$matdepa <- ifelse(indaf$matdepa == 2, 1, 0)
indaf$matdepd <- ifelse(indaf$matdepd == 2, 1, 0)
indaf$matdepe <- ifelse(indaf$matdepe == 2, 1, 0)
indaf$matdepf <- ifelse(indaf$matdepf == 2, 1, 0)
indaf$matdepg <- ifelse(indaf$matdepg == 2, 1, 0)
indaf$matdeph <- ifelse(indaf$matdeph == 2, 1, 0)
indaf$matdepi <- ifelse(indaf$matdepi == 2, 1, 0)
indaf$matdepj <- ifelse(indaf$matdepj == 2, 1, 0)

indaf$deprivation <- indaf$matdepa + indaf$matdepd + indaf$matdepe + indaf$matdepf +
  indaf$matdepg + indaf$matdeph + indaf$matdepi + indaf$matdepj

# ---- END Cleaning ----

# ---- Obtaining Tenure Change ----
tenure_data <- data.frame()
vars_tenure <- c("hidp")
for (wn in c(10, 11, 12)) {
  wl <- paste0(letters[wn],"_")
  var_names <- paste0(wl, vars_tenure) 
  var_names <- c("pidp", var_names)
  
  filepath <- paste0("~/Desktop/Dissertation/Data/Main Survey/ukhls/", wl,
                     "indresp.dta")
  
  wave_data <- read_dta(filepath) %>%
    select(all_of(var_names)) %>%
    rename_with(~str_replace(.x, paste0("^", wl), ""), .cols = matches(paste0("^", wl))) %>%
    mutate(wave = wn)
  
  tenure_data <- rbind(tenure_data, wave_data)
}

tenure_hh <- data.frame()
vars_hh <- c("hidp", "tenure_dv")
for (wn in c(10, 11, 12)) {
  wl <- paste0(letters[wn],"_")
  var_names <- paste0(wl, vars_hh) 
  
  filepath <- paste0("~/Desktop/Dissertation/Data/Main Survey/ukhls/", wl,
                     "hhresp.dta")
  
  wave_data <- read_dta(filepath) %>%
    select(all_of(var_names)) %>%
    rename_with(~str_replace(.x, paste0("^", wl), ""), .cols = matches(paste0("^", wl))) 
  
  tenure_hh <- rbind(tenure_hh, wave_data)
}

tenure_hind <- merge(tenure_data, tenure_hh, by = "hidp")

tenure_hind$tenure_dv <- factor(tenure_hind$tenure_dv,
                                levels = c(1, 2, 3, 4, 5 ,6 ,7 ,8),
                                labels = c("Outr", "Mort", "Social", 
                                           "Social", "OtherTen", "Priv",
                                           "Priv", "OtherTen"))

tenure_hind$own <- ifelse(tenure_hind$tenure_dv == "Outr" | tenure_hind$tenure_dv == "Mort", 1, 0)

# Step 1: Filter individuals present in both waves 10 and 12, or waves 11 and 12
tenure_hind <- tenure_hind %>%
  group_by(pidp) %>%
  filter((any(wave == 10) & any(wave == 12)) | (any(wave == 11) & any(wave == 12)))

# Step 2: Subset data for waves 10, 11, and 12
wave_10 <- tenure_hind %>% filter(wave == 10)
wave_11 <- tenure_hind %>% filter(wave == 11)
wave_12 <- tenure_hind %>% filter(wave == 12)

# Step 3: Identify individuals who became homeowners between waves 10 and 12, and between waves 11 and 12
new_homeowners_10_12 <- merge(wave_10, wave_12, by = "pidp", all.x = TRUE) %>%
  filter(own.x == 0 & own.y == 1) %>%
  select(pidp)

new_homeowners_11_12 <- merge(wave_11, wave_12, by = "pidp", all.x = TRUE) %>%
  filter(own.x == 0 & own.y == 1) %>%
  select(pidp)

# Step 4: Combine new homeowner information from both pairs of waves
new_homeowners <- unique(c(new_homeowners_10_12$pidp, new_homeowners_11_12$pidp))

# Step 5: Create a variable indicating whether an individual became a homeowner
tenure_hind <- tenure_hind %>%
  mutate(new_owner = ifelse(pidp %in% new_homeowners, 1, 0))

tenure_hind <- tenure_hind[tenure_hind$wave == 12, c(2,6)]

# ---- END Tenure Change ----

indaf2 <- merge(indaf, tenure_hind, by = "pidp")

# ---- 2021 Census Data ----

msoa <- read.csv("~/Desktop/Dissertation/Data/Census21/lsoa_to_msoa.csv")
msoa <- msoa[,c(2,4)]
msoa <- unique(msoa)
colnames(msoa) <- c("LSOA21CD", "MSOA21CD")

# Education
educ <- read.csv("~/Desktop/Dissertation/Data/Census21/education_lsoa.csv")
educ <- educ[,c(2, 4:8)]
colnames(educ) <- c("LSOA21CD", "NoQual", "levelOne", "levelTwo", "levelThree", "levelFourP")
# Tenure
# Shared ownership will be considered as ownership with mortgage, quote source
tenure <- read.csv("~/Desktop/Dissertation/Data/Census21/tenure_lsoa.csv")
tenure <- tenure[,c(2, 4:10)]
tenure$mortg <- tenure$Owned..Owns.with.a.mortgage.or.loan + tenure$Shared.ownership
colnames(tenure) <- c("LSOA21CD", "Owned", "OwnedOutr", "mortOld", "sharedOld", 
                      "Social", "Private", "OtherTenure", "OwnedMort")
tenure <- tenure[,c(1:3, 6:9)]
tenure$LOwn <- tenure$Social + tenure$Private + tenure$Other

# Equivalized household income - Caveat: 2020 Financial year
eqnet_msoas <- read.csv("~/Desktop/Dissertation/Data/MSOA income estimates/neteqinc.csv", sep = ";")
eqnet_msoas <- eqnet_msoas[,c(1,7)]
colnames(eqnet_msoas) <- c("MSOA21CD", "EqNetTsd")
eqnet_msoas$EqNetTsd <- eqnet_msoas$EqNetTsd/12
# Economic activity status - Ignore for now on LSOA level - no clear link to attitudes
econ_lsoa <- read.csv("~/Desktop/Dissertation/Data/Census21/econ_lsoa.csv")
econ_lsoa <- econ_lsoa[,c(2,4,7,8)]
colnames(econ_lsoa) <- c("LSOA21CD", "Empl", "Unempl", "Retired")

# Merging all files into one LSOA file
eqnet_msoas <- merge(eqnet_msoas, msoa, by = "MSOA21CD")
lsoa_dfs <- list(eqnet_msoas, cars, ethn, educ, nssec3, tenure, econ_lsoa)
Census21.lsoa <- reduce(lsoa_dfs, full_join, by = "LSOA21CD")

# Wave 12: Merge with UKHLS file - excluding NS-SeC for now
Census21.lsoa <- read.csv("~/Desktop/Dissertation/Data/census21.csv")

# ---- END Census Data ----
indaf.cen <- as.data.frame(merge(indaf, Census21.lsoa, by = "LSOA21CD"))
write.csv(indaf.cen, "~/Desktop/Dissertation/New data/indaf_cen.csv", row.names = F)
# Preparation for PCA
# ---- Preparation for PCA ----
# Create scores through PCA (entire dataset)
indaf.cen <- read.csv("~/Desktop/Dissertation/New data/indaf_cen.csv")
indaf.pca <- indaf.cen[c(which(colnames(indaf.cen)=="pidp"),
                         which(colnames(indaf.cen)=="hiqual_dv"),
                         which(colnames(indaf.cen)=="jbstat"),
                         which(colnames(indaf.cen)=="eqhhmnnet"),
                         which(colnames(indaf.cen)=="tenure_dv"),
                         which(colnames(indaf.cen)=="indscui_xw"))]
indaf.pca <- indaf.pca[complete.cases(indaf.pca),]

indaf.pca$own <- ifelse(indaf.pca$tenure_dv == "Outr" | indaf.pca$tenure == "Mort", 1, 0)
indaf.pca$retired <- ifelse(indaf.pca$jbstat == "Retired", 1, 0)
indaf.pca$unemployed <- ifelse(indaf.pca$jbstat == "Unemp", 1, 0)
indaf.pca$level4p <- ifelse(indaf.pca$hiqual_dv == "Higher" | indaf.pca$hiqual_dv == "Degree", 1,0)
indaf.pca$noqual <- ifelse(indaf.pca$hiqual_dv == "NoQual", 1, 0)
indaf.pca$gcse <- ifelse(indaf.pca$hiqual_dv == "GCSE", 1, 0)
indaf.pca$alevel <- ifelse(indaf.pca$hiqual_dv == "A-Levels", 1, 0)

indaf.pca$own <- (indaf.pca$own-mean(indaf.pca$own))/sd(indaf.pca$own)
indaf.pca$retired <- (indaf.pca$retired-mean(indaf.pca$retired))/sd(indaf.pca$retired)
indaf.pca$unemployed <- (indaf.pca$unemployed-mean(indaf.pca$unemployed))/sd(indaf.pca$unemployed)
indaf.pca$level4p <- (indaf.pca$level4p-mean(indaf.pca$level4p))/sd(indaf.pca$level4p)
#indaf.pca$degree <- (indaf.pca$degree-mean(indaf.pca$degree))/sd(indaf.pca$degree)
#indaf.pca$higher <- (indaf.pca$higher-mean(indaf.pca$higher))/sd(indaf.pca$higher)
indaf.pca$noqual <- (indaf.pca$noqual-mean(indaf.pca$noqual))/sd(indaf.pca$noqual)
indaf.pca$gcse <- (indaf.pca$gcse-mean(indaf.pca$gcse))/sd(indaf.pca$gcse)
indaf.pca$alevel <- (indaf.pca$alevel-mean(indaf.pca$alevel))/sd(indaf.pca$alevel)
indaf.pca$eqhhmnnet <- (indaf.pca$eqhhmnnet-mean(indaf.pca$eqhhmnnet))/sd(indaf.pca$eqhhmnnet)

# Correlation plot of compoonents
corrplot.mixed(
  cor(indaf.pca[,c(4, 7:13)], use="complete.obs"),
  upper="ellipse",tl.cex=0.3)

# ---- END Preparation for PCA ----

# ---- PCA ----
indaf.pca <- indaf.pca[indaf.pca$indscui_xw > 0,]

pcafit <- PCA(indaf.pca[,c(4, 7, 9:13)], row.w = indaf.pca[,which(colnames(indaf.pca) == "indscui_xw")])
screeplot(pcafit)
eigenvalues <- pcafit$eig
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Screeplot of PCA - Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="grey")
# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")
summary(pcafit)
indaf.pca$score <- pcafit$ind$coord[,1]
# Extract and merge back with original dataframe
indaf.scores <- indaf.pca[,c(1,14)]
indaf.cen <- merge(indaf.cen, indaf.scores, by = "pidp", all = F)
indaf.cen2 <- merge(indaf.cen2, indaf.scores, by = "pidp", all = F)

# Apply PCA to census data
census <- Census21.lsoa
census$EqNetTsd <- (Census21.lsoa$EqNetTsd-mean(Census21.lsoa$EqNetTsd, na.rm = T))/sd(Census21.lsoa$EqNetTsd, na.rm = T)
census$NoQual <- (census$NoQual-mean(census$NoQual, na.rm = T))/sd(census$NoQual, na.rm = T)
census$levelTwo <- (census$levelTwo-mean(census$levelTwo, na.rm = T))/sd(census$levelTwo, na.rm = T)
census$levelThree <- (census$levelThree-mean(census$levelThree, na.rm = T))/sd(census$levelThree, na.rm = T)
census$levelFourP <- (census$levelFourP-mean(census$levelFourP, na.rm = T))/sd(census$levelFourP, na.rm = T)
census$Owned <- (census$Owned-mean(census$Owned, na.rm = T))/sd(census$Owned, na.rm = T)
census$Ret <- (census$Ret-mean(census$Ret, na.rm = T))/sd(census$Ret, na.rm = T)
census$Unempl <- (census$Unempl-mean(census$Unempl, na.rm = T))/sd(census$Unempl, na.rm = T)

# Binding the results with the original data
result <- cbind(census[3], mode_ethn = mode_ethn)
census <- merge(census, result, by = "LSOA21CD")

census$score.ls <- census$EqNetTsd*pcafit$var$coord[1] + census$Owned*pcafit$var$coord[2] +
  census$Unempl*pcafit$var$coord[3] + census$levelFourP*pcafit$var$coord[4] + 
  census$NoQual*pcafit$var$coord[5] + census$levelTwo*pcafit$var$coord[6] + census$levelThree*pcafit$var$coord[7] 
census <- census[,c(1,30:31)]
indaf.cen.p <- merge(indaf.cen, census, by = "LSOA21CD")
indaf.cen.p$status <- indaf.cen.p$score-indaf.cen.p$score.ls

# ---- END Preparation ----

indaf.cen.p <- read.csv("~/Desktop/Dissertation/Data/Final/indaf_score.csv")

# ---- Selecting columns for indaf.stat ----
indaf.stat <- indaf.cen.p[,c(which(colnames(indaf.cen.p)=="pidp"),
                             which(colnames(indaf.cen.p)=="wave"),
                             which(colnames(indaf.cen.p)=="indscui_xw"),
                             which(colnames(indaf.cen.p)=="sex_dv"),
                             which(colnames(indaf.cen.p)=="fimngrs_dv"),
                             which(colnames(indaf.cen.p)=="hiqual_dv"),
                             which(colnames(indaf.cen.p)=="age_dv"),
                             which(colnames(indaf.cen.p)=="ethn_dv"),
                             which(colnames(indaf.cen.p)=="vote3lr"),
                             which(colnames(indaf.cen.p)=="vote3"),
                             which(colnames(indaf.cen.p)=="votetxspnd"),
                             which(colnames(indaf.cen.p)=="urban_dv"),
                             which(colnames(indaf.cen.p)=="gor_dv"),
                             which(colnames(indaf.cen.p)=="unemployed"),
                             which(colnames(indaf.cen.p)=="retired"),
                             which(colnames(indaf.cen.p)=="student"),
                             which(colnames(indaf.cen.p)=="own"),
                             which(colnames(indaf.cen.p)=="eqhhmnnet"),
                             which(colnames(indaf.cen.p)=="invest"),
                             which(colnames(indaf.cen.p)=="hhinvest"),
                             which(colnames(indaf.cen.p)=="EqNetTsd"),
                             which(colnames(indaf.cen.p)=="Mean"),
                             which(colnames(indaf.cen.p)=="standing"),
                             which(colnames(indaf.cen.p)=="status"),
                             which(colnames(indaf.cen.p)=="mode_ethn"),
                             which(colnames(indaf.cen.p)=="score"),
                             which(colnames(indaf.cen.p)=="left"),
                             which(colnames(indaf.cen.p)=="nbrcoh_dv"),
                             which(colnames(indaf.cen.p)=="llknbrd"),
                             which(colnames(indaf.cen.p)=="scopngbhb"),
                             which(colnames(indaf.cen.p)=="scopngbhg"),
                             which(colnames(indaf.cen.p)=="scopngbhh"),
                             which(colnames(indaf.cen.p)=="smlook"),
                             which(colnames(indaf.cen.p)=="smpost"),
                             which(colnames(indaf.cen.p)=="opsoce"))]
# ----
indaf.stat <- indaf.stat[complete.cases(indaf.stat),]
indaf.stat$inc.stat <- indaf.stat$eqhhmnnet - indaf.stat$EqNetTsd
str(indaf.stat)
indaf.stat$hiqual_dv <- as.factor(indaf.stat$hiqual_dv)
indaf.stat$ethn_dv <- as.factor(indaf.stat$ethn_dv)
indaf.stat$vote3lr <- as.factor(indaf.stat$vote3lr)
indaf.stat <- indaf.stat[!indaf.stat$vote3lr == "None",]
indaf.stat <- indaf.stat[!indaf.stat$vote3lr == "Cant",]
indaf.stat <- indaf.stat[!indaf.stat$vote3 == "None",]
indaf.stat <- indaf.stat[!indaf.stat$vote3 == "Cant",]
indaf.stat$vote3 <- as.factor(indaf.stat$vote3)
indaf.stat$mode_ethn <- as.factor(indaf.stat$mode_ethn)
indaf.stat$urban_dv <- as.factor(indaf.stat$urban_dv)
indaf.stat$retired <- as.factor(indaf.stat$retired)
indaf.stat$own <- as.factor(indaf.stat$own)
indaf.stat$llknbrd <- ifelse(indaf.stat$llknbrd == 1, 1, 0)
indaf.stat$freq.use <- ifelse(indaf.stat$smlook == 1 | indaf.stat$smlook == 2, 1, 0)
indaf.stat$freq.post <- ifelse(indaf.stat$smpost == 1 | indaf.stat$smpost == 2, 1, 0)
indaf.stat$friends <- ifelse(indaf.stat$scopngbhb == 1 | indaf.stat$scopngbhb == 2, 1, 0)
indaf.stat$talk <- ifelse(indaf.stat$scopngbhh == 1 | indaf.stat$scopngbhh == 2, 1, 0)
indaf.stat$similar <- ifelse(indaf.stat$scopngbhg == 1 | indaf.stat$scopngbhg == 2, 1, 0)
indaf.stat$cohesive <- ifelse(indaf.stat$nbrcoh_dv > (mean(indaf.stat$nbrcoh_dv) + sd(indaf.stat$nbrcoh_dv)), 1, 0)
indaf.stat$fimngrs_dv <- indaf.stat$fimngrs_dv/1000
indaf.stat$status.low <- ifelse(indaf.stat$status < 0, 1, 0)
indaf.stat$lowinc <- ifelse(indaf.stat$inc.stat < 0, 1, 0)
indaf.stat$gor_dv <- as.factor(indaf.stat$gor_dv)
indaf.stat$vote3 <- as.factor(ifelse(indaf.stat$vote3 == "Lab", "Labour",
                           ifelse(indaf.stat$vote3 == "Cons", "Conservative",
                                  ifelse(indaf.stat$vote3 == "CUK" | indaf.stat$vote3 == "TBP" | 
                                           indaf.stat$vote3 == "UKIP", "Other Right",
                                         ifelse(indaf.stat$vote3 == "GP" | indaf.stat$vote3 == "LD" | 
                                                  indaf.stat$vote3 == "PC" | indaf.stat$vote3 == "SNP", 
                                                "Other Left", "Other")))))

indaf.stat$inc.stat <- indaf.stat$eqhhmnnet - indaf.stat$EqNetTsd
indaf.stat$vote3 <- relevel(indaf.stat$vote3, ref = "Conservative")
indaf.stat$ethn_dv <- relevel(indaf.stat$ethn_dv, ref = "White")

write.csv(indaf.stat, "~/Desktop/Dissertation/New data/indaf.stat.csv", row.names = F)
# For robustness if time, consider repeating all models with "left" as dependent
# Models: Hypothesis 1 and 3: Relative status to others predictor of attitudes & SES > Rel.Inc. ----
# Also justify including scopnbgh measures over nbrcoh to avoid ecological fallacy (perhaps)
# Also, see if sensible to add one variable to other to check whether significant in presence of other,
# because inc.stat becomes insignficant when status part of the model.
# Simple models without social controls
ses.h1a <- lm(votetxspnd ~ status + eqhhmnnet + own + urban_dv + sex_dv + 
                age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                vote3 + left + gor_dv, data = indaf.stat, weights = indaf.stat$indscui_xw)
summary(ses.h1a)
AIC(ses.h1a)
inc.h1a <- lm(votetxspnd ~ inc.stat + eqhhmnnet + own + urban_dv + sex_dv + 
                age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                vote3 + left + gor_dv, data = indaf.stat, weights = indaf.stat$indscui_xw)
summary(inc.h1a)
AIC(inc.h1a)
# Simple models with social controls
ses.h1b <- lm(votetxspnd ~ status + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat, weights = indaf.stat$indscui_xw)
summary(ses.h1b)

inc.h1b <- lm(votetxspnd ~ inc.stat + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat, weights = indaf.stat$indscui_xw)
summary(inc.h1b)
# Models with friends interaction
ses.h1c <- lm(votetxspnd ~ status + left + eqhhmnnet + status*friends + similar + talk + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat, weights = indaf.stat$indscui_xw)
summary(ses.h1c)
AIC(ses.h1c)
inc.h1d <- lm(votetxspnd ~ inc.stat + eqhhmnnet + inc.stat*friends + similar + talk + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat, weights = indaf.stat$indscui_xw)
summary(inc.h1d)
AIC(inc.h1d)
# Model with interaction terms and inc.stat combined

ses.h1f <- lm(votetxspnd ~ status + inc.stat + status*friends + inc.stat*friends + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat, weights = indaf.stat$indscui_xw)
summary(ses.h1f)

models <- list(ses.h1a, ses.h1b, ses.h1c, inc.h1a, inc.h1b, inc.h1d, ses.h1f)
stargazer(models, star.cutoffs = c(0.05, 0.01, 0.001))
# END H1 & H3 ----

# Models: Hypothesis 2: Owners are more affected by rel. SES than non-owners ----
indaf.stat.own <- indaf.stat[indaf.stat$own == 1,]
hsval <- indaf.cen.p[,c(which(colnames(indaf.cen.p) == "pidp"), which(colnames(indaf.cen.p) == "hvalue"))]
indaf.stat.own <- merge(indaf.stat.own, hsval, by = "pidp")
indaf.stat.own <- merge(indaf.stat.own, tenure_hind, by = "pidp")

# Simple model
ses.h2a <- lm(votetxspnd ~ status  + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv,
              data = indaf.stat.own, weights = indaf.stat.own$indscui_xw)
summary(ses.h2a)

# Hypothesis 2 with friends interaction term
ses.h2b <- lm(votetxspnd ~ status + eqhhmnnet + hvalue + status*friends + talk + similar + freq.use + freq.post +
                urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv + new_owner, 
              data = indaf.stat.own, weights = indaf.stat.own$indscui_xw)
summary(ses.h2b)

ses.h2c <- lm(votetxspnd ~ status + eqhhmnnet + hvalue + status*friends + talk + similar + freq.use + freq.post +
                urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv + status*new_owner, 
              data = indaf.stat.own, weights = indaf.stat.own$indscui_xw)
summary(ses.h2c)
h.models <- list(ses.h2a, ses.h2b, ses.h2c)
stargazer(h.models, star.cutoffs = c(0.05, 0.01, 0.001))
# Model for renters
# Simple model
indaf.stat.nown <- indaf.stat[indaf.stat$own == 0,]
ses.h2aa <- lm(votetxspnd ~ status  + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat.nown, weights = indaf.stat.nown$indscui_xw)
summary(ses.h2aa)
stargazer(ses.h2aa, ses.h2bb)
# Hypothesis 2 for renters with friends interaction term
ses.h2bb <- lm(votetxspnd ~ status + eqhhmnnet + status*friends + talk + similar + freq.use + freq.post +
                urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat.nown, weights = indaf.stat.nown$indscui_xw)
summary(ses.h2bb)
stargazer(h.models, ses.h2aa, ses.h2bb, star.cutoffs = c(0.05, 0.01, 0.001))
# ----

# Models: Hypothesis 4a and 4b ----
# First, split by quntile Second model will estimate with material deprivation
deciles <- quantile(indaf.stat$eqhhmnnet, probs = seq(0.2, 0.8, by = 0.2))
print(deciles)

indaf.stat.rich <- indaf.stat[indaf.stat$eqhhmnnet >= 2.877535,]
indaf.stat.poor <- indaf.stat[indaf.stat$eqhhmnnet <= 1.270307,]
indaf.stat.poor$status <- (indaf.stat.poor$status - mean(indaf.stat.poor$status))/sd(indaf.stat.poor$status)
indaf.stat.poor$inc.stat <- (indaf.stat.poor$inc.stat - mean(indaf.stat.poor$inc.stat))/sd(indaf.stat.poor$inc.stat)

indaf.stat.rich$status <- (indaf.stat.rich$status - mean(indaf.stat.rich$status))/sd(indaf.stat.rich$status)
indaf.stat.rich$inc.stat <- (indaf.stat.rich$inc.stat - mean(indaf.stat.rich$inc.stat))/sd(indaf.stat.rich$inc.stat)
# Hypothesis 4a ----
# Simple models
ses.h4aa <- lm(votetxspnd ~ status + eqhhmnnet + own + urban_dv + sex_dv + 
                age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                vote3 + left + gor_dv, data = indaf.stat.poor, 
               weights = indaf.stat.poor$indscui_xw)
summary(ses.h4aa)

inc.h4aa <- lm(votetxspnd ~ inc.stat + eqhhmnnet +own + urban_dv + sex_dv + 
                 age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                 vote3 + left + gor_dv, data = indaf.stat.poor, 
               weights = indaf.stat.poor$indscui_xw)
summary(inc.h4aa)

# Models with friends interaction term
ses.h4ab <- lm(votetxspnd ~ status*friends + eqhhmnnet + talk + similar+ own + urban_dv + sex_dv + 
                age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + freq.use + freq.post +
                vote3 + left + gor_dv, data = indaf.stat.poor, 
               weights = indaf.stat.poor$indscui_xw)
summary(ses.h4ab)

inc.h4ab <- lm(votetxspnd ~ inc.stat*friends + eqhhmnnet + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat.poor, 
               weights = indaf.stat.poor$indscui_xw)
summary(inc.h4ab)

ses.h4ac <- lm(votetxspnd ~ status*friends + inc.stat*friends + eqhhmnnet + talk + similar + freq.use + freq.post +
                 own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                 invest + vote3 + left + gor_dv, data = indaf.stat.poor, 
               weights = indaf.stat.poor$indscui_xw)
summary(ses.h4ac)

models.p <- list(ses.h4aa, inc.h4aa, ses.h4ab, inc.h4ab, ses.h4ac)
stargazer(models.p, star.cutoffs = c(0.05, 0.01, 0.001))
# ----

# Hypothesis 4b: ----
# Simple models
ses.h4ba <- lm(votetxspnd ~ status + eqhhmnnet + own + urban_dv + sex_dv + 
                age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                vote3 + left + gor_dv, data = indaf.stat.rich, 
               weights = indaf.stat.rich$indscui_xw)
summary(ses.h4ba)

inc.h4ba <- lm(votetxspnd ~ inc.stat + eqhhmnnet + own + urban_dv + sex_dv + 
                 age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                 vote3 + left + gor_dv, data = indaf.stat.rich, 
               weights = indaf.stat.rich$indscui_xw)
summary(inc.h4ba)

# Models with friends interaction term
ses.h4bb <- lm(votetxspnd ~ status*friends + friends + eqhhmnnet + talk + similar + own + urban_dv + sex_dv + 
                age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + freq.use + freq.post +
                vote3 + left + gor_dv, data = indaf.stat.rich, 
               weights = indaf.stat.rich$indscui_xw)
summary(ses.h4bb)
# Add to table?

inc.h4bb <- lm(votetxspnd ~ inc.stat*friends + eqhhmnnet + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + vote3 + left + gor_dv, data = indaf.stat.rich, 
               weights = indaf.stat.rich$indscui_xw)
summary(inc.h4bb)

ses.h4bc <- lm(votetxspnd ~ status*friends + inc.stat*friends + eqhhmnnet + talk + similar + own + urban_dv + sex_dv + 
                 age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + freq.use + freq.post +
                 vote3 + left + gor_dv, data = indaf.stat.rich, 
               weights = indaf.stat.rich$indscui_xw)
summary(ses.h4bc)

models.r <- list(ses.h4ba, inc.h4ba, ses.h4bb, inc.h4bb, ses.h4bc)
stargazer(models.r, star.cutoffs = c(0.05, 0.01, 0.001))
# ----

# Hypothesis 4a with deprivation
# Columns for deprivation ----
indaf.stat.dep <- indaf.cen.p[,c(which(colnames(indaf.cen.p)=="pidp"),
                             which(colnames(indaf.cen.p)=="wave"),
                             which(colnames(indaf.cen.p)=="indscui_xw"),
                             which(colnames(indaf.cen.p)=="sex_dv"),
                             which(colnames(indaf.cen.p)=="fimngrs_dv"),
                             which(colnames(indaf.cen.p)=="hiqual_dv"),
                             which(colnames(indaf.cen.p)=="age_dv"),
                             which(colnames(indaf.cen.p)=="ethn_dv"),
                             which(colnames(indaf.cen.p)=="vote3lr"),
                             which(colnames(indaf.cen.p)=="vote3"),
                             which(colnames(indaf.cen.p)=="votetxspnd"),
                             which(colnames(indaf.cen.p)=="urban_dv"),
                             which(colnames(indaf.cen.p)=="gor_dv"),
                             which(colnames(indaf.cen.p)=="unemployed"),
                             which(colnames(indaf.cen.p)=="retired"),
                             which(colnames(indaf.cen.p)=="student"),
                             which(colnames(indaf.cen.p)=="own"),
                             which(colnames(indaf.cen.p)=="eqhhmnnet"),
                             which(colnames(indaf.cen.p)=="invest"),
                             which(colnames(indaf.cen.p)=="hhinvest"),
                             which(colnames(indaf.cen.p)=="EqNetTsd"),
                             which(colnames(indaf.cen.p)=="Mean"),
                             which(colnames(indaf.cen.p)=="standing"),
                             which(colnames(indaf.cen.p)=="status"),
                             which(colnames(indaf.cen.p)=="mode_ethn"),
                             which(colnames(indaf.cen.p)=="score"),
                             which(colnames(indaf.cen.p)=="left"),
                             which(colnames(indaf.cen.p)=="nbrcoh_dv"),
                             which(colnames(indaf.cen.p)=="llknbrd"),
                             which(colnames(indaf.cen.p)=="scopngbhb"),
                             which(colnames(indaf.cen.p)=="scopngbhg"),
                             which(colnames(indaf.cen.p)=="scopngbhh"),
                             which(colnames(indaf.cen.p)=="smlook"),
                             which(colnames(indaf.cen.p)=="smpost"),
                             which(colnames(indaf.cen.p)=="matdepj"),
                             which(colnames(indaf.cen.p)=="matdepi"),
                             which(colnames(indaf.cen.p)=="matdepb"),
                             which(colnames(indaf.cen.p)=="deprivation"),
                             which(colnames(indaf.cen.p)=="smpost"),
                             which(colnames(indaf.cen.p)=="opsoce"))]
indaf.stat.dep <- indaf.stat.dep[complete.cases(indaf.stat.dep),]
# ----
indaf.stat.dep <- indaf.stat.dep[complete.cases(indaf.stat.dep),]
indaf.stat.dep$inc.stat <- indaf.stat.dep$eqhhmnnet - indaf.stat.dep$EqNetTsd
str(indaf.stat.dep)
indaf.stat.dep$hiqual_dv <- as.factor(indaf.stat.dep$hiqual_dv)
indaf.stat.dep$ethn_dv <- as.factor(indaf.stat.dep$ethn_dv)
indaf.stat.dep$vote3lr <- as.factor(indaf.stat.dep$vote3lr)
indaf.stat.dep <- indaf.stat.dep[!indaf.stat.dep$vote3lr == "None",]
indaf.stat.dep <- indaf.stat.dep[!indaf.stat.dep$vote3lr == "Cant",]
indaf.stat.dep <- indaf.stat.dep[!indaf.stat.dep$vote3 == "None",]
indaf.stat.dep <- indaf.stat.dep[!indaf.stat.dep$vote3 == "Cant",]
indaf.stat.dep$vote3 <- as.factor(indaf.stat.dep$vote3)
indaf.stat.dep$mode_ethn <- as.factor(indaf.stat.dep$mode_ethn)
indaf.stat.dep$urban_dv <- as.factor(indaf.stat.dep$urban_dv)
indaf.stat.dep$retired <- as.factor(indaf.stat.dep$retired)
indaf.stat.dep$own <- as.factor(indaf.stat.dep$own)
indaf.stat.dep$llknbrd <- ifelse(indaf.stat.dep$llknbrd == 1, 1, 0)
indaf.stat.dep$freq.use <- ifelse(indaf.stat.dep$smlook == 1 | indaf.stat.dep$smlook == 2, 1, 0)
indaf.stat.dep$freq.post <- ifelse(indaf.stat.dep$smpost == 1 | indaf.stat.dep$smpost == 2, 1, 0)
indaf.stat.dep$friends <- ifelse(indaf.stat.dep$scopngbhb == 1 | indaf.stat.dep$scopngbhb == 2, 1, 0)
indaf.stat.dep$talk <- ifelse(indaf.stat.dep$scopngbhh == 1 | indaf.stat.dep$scopngbhh == 2, 1, 0)
indaf.stat.dep$similar <- ifelse(indaf.stat.dep$scopngbhg == 1 | indaf.stat.dep$scopngbhg == 2, 1, 0)
indaf.stat.dep$cohesive <- ifelse(indaf.stat.dep$nbrcoh_dv > (mean(indaf.stat.dep$nbrcoh_dv) + sd(indaf.stat.dep$nbrcoh_dv)), 1, 0)
indaf.stat.dep$fimngrs_dv <- indaf.stat.dep$fimngrs_dv/1000
indaf.stat.dep$status.low <- ifelse(indaf.stat.dep$status < 0, 1, 0)
indaf.stat.dep$lowinc <- ifelse(indaf.stat.dep$inc.stat < 0, 1, 0)
indaf.stat.dep$gor_dv <- as.factor(indaf.stat.dep$gor_dv)
indaf.stat.dep$poor <- ifelse(indaf.stat.dep$eqhhmnnet <= 0.9905267, 1, 0)
indaf.stat.dep$rich <- ifelse(indaf.stat.dep$eqhhmnnet >= 3.5782834, 1, 0)
indaf.stat.dep$deprived <- ifelse(indaf.stat.dep$deprivation >= 2, 1, 0)

indaf.stat.dep$inc.stat <- indaf.stat.dep$eqhhmnnet - indaf.stat.dep$EqNetTsd
indaf.stat.dep$vote3lr <- relevel(indaf.stat.dep$vote3lr, ref = "Right")
indaf.stat.dep$ethn_dv <- relevel(indaf.stat.dep$ethn_dv, ref = "White")
indaf.stat.dep$vote3 <- relevel(indaf.stat.dep$vote3, ref = "Lab")
summary(indaf.stat.dep$mode_ethn)
indaf.stat.dep$maj.ethn <- ifelse((indaf.stat.dep$ethn_dv == "White" & indaf.stat.dep$mode_ethn == "White") |
                                (indaf.stat.dep$ethn_dv == "Black" & indaf.stat.dep$mode_ethn == "Black") |
                                (indaf.stat.dep$ethn_dv == "Asian" & indaf.stat.dep$mode_ethn == "Asian"), 1, 0)
write.csv(indaf.stat.dep, "~/Desktop/Dissertation/New data/indaf.stat.dep.csv", row.names = F)
# ----
ses.h4a2 <- lm(votetxspnd ~ status + deprivation + eqhhmnnet + own + urban_dv + sex_dv + 
                age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                vote3 + left + gor_dv, data = indaf.stat.dep, 
               weights = indaf.stat.dep$indscui_xw)
summary(ses.h4a2)

ses.h4ab2 <- lm(votetxspnd ~ status + deprivation + eqhhmnnet + own + urban_dv + sex_dv + 
                 age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + friends +
                  similar + talk + vote3 + left + gor_dv + freq.use + freq.post, 
                data = indaf.stat.dep, weights = indaf.stat.dep$indscui_xw)
summary(ses.h4ab2)

ses.h4ac2 <- lm(votetxspnd ~ status*friends + deprivation + eqhhmnnet + own + urban_dv + sex_dv + 
                  age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + friends +
                  similar + talk + vote3 + left + gor_dv + freq.use + freq.post, 
                data = indaf.stat.dep, weights = indaf.stat.dep$indscui_xw)
summary(ses.h4ac2)

ses.h4ad2 <- lm(votetxspnd ~ status + inc.stat +  deprivation + eqhhmnnet + own + urban_dv + sex_dv + 
                  age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + status*friends +
                  similar + talk + vote3 + left + gor_dv + freq.use + freq.post, 
                data = indaf.stat.dep, weights = indaf.stat.dep$indscui_xw)
summary(ses.h4ad2)


# Sample split by vote ----
indaf.stat.left <- indaf.stat[indaf.stat$vote3lr == "Left",]
indaf.stat.right <- indaf.stat[indaf.stat$vote3lr == "Right",]

ses.rbl <- lm(votetxspnd ~ status + status*friends + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + gor_dv + left, 
              data = indaf.stat.left, weights = indaf.stat.left$indscui_xw)
summary(ses.rbl)

ses.rbr <- lm(votetxspnd ~ status + status*friends + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + gor_dv + left, 
              data = indaf.stat.right, weights = indaf.stat.right$indscui_xw)
summary(ses.rbr)

indaf.stat.lab <- indaf.stat[indaf.stat$vote3 == "Lab",]
indaf.stat.con <- indaf.stat[indaf.stat$vote3 == "Cons",]

# Partisan split
ses.rblab <- lm(votetxspnd ~ status + status*friends + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + gor_dv + left, data = indaf.stat.lab, weights = indaf.stat.lab$indscui_xw)
summary(ses.rblab)

ses.rbcon <- lm(votetxspnd ~ status + status*friends + talk + similar + freq.use + freq.post +
                own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                invest + gor_dv + left, data = indaf.stat.con, weights = indaf.stat.con$indscui_xw)
summary(ses.rbcon)

# Same, but with full employment question
ses.rblab.emp <- lm(opsoce ~ status + status*friends + talk + similar + freq.use + freq.post +
                  own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                  invest + gor_dv, data = indaf.stat.lab, weights = indaf.stat.lab$indscui_xw)
summary(ses.rblab.emp)

ses.rbcon.emp <- lm(opsoce ~ status + status*friends + talk + similar + freq.use + freq.post +
                  own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                  invest + gor_dv, data = indaf.stat.con, weights = indaf.stat.con$indscui_xw)
summary(ses.rbcon.emp)
# ----


# ---- Robustness checks ----
# Manual robustness checks
# Cooks Distances
# Hypotheses 1-3 ----
cooksD <- cooks.distance(ses.h1a)
threshold <- 4 / (nrow(indaf.stat) - length(coef(ses.h1a)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh1a <-indaf.stat[-influential_points,]

cd.ses.h1a <- lm(votetxspnd ~ status + eqhhmnnet + own + urban_dv + sex_dv + 
                   age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                   vote3 + left + gor_dv, data = indaf.stat.sesh1a, weights = indaf.stat.sesh1a$indscui_xw)

cooksD <- cooks.distance(inc.h1a)
threshold <- 4 / (nrow(indaf.stat) - length(coef(inc.h1a)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch1a <-indaf.stat[-influential_points,]
cd.inc.h1a <- lm(votetxspnd ~ inc.stat + eqhhmnnet + own + urban_dv + sex_dv + 
                   age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                   vote3 + left + gor_dv, data = indaf.stat.inch1a, weights = indaf.stat.inch1a$indscui_xw)

cooksD <- cooks.distance(ses.h1b)
threshold <- 4 / (nrow(indaf.stat) - length(coef(ses.h1b)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh1b <-indaf.stat[-influential_points,]
cd.ses.h1b <- lm(votetxspnd ~ status + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                   own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv, data = indaf.stat.sesh1b, weights = indaf.stat.sesh1b$indscui_xw)

cooksD <- cooks.distance(inc.h1b)
threshold <- 4 / (nrow(indaf.stat) - length(coef(inc.h1b)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch1b <-indaf.stat[-influential_points,]
cd.inc.h1b <- lm(votetxspnd ~ inc.stat + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                   own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv, data = indaf.stat.inch1b, weights = indaf.stat.inch1b$indscui_xw)

cooksD <- cooks.distance(ses.h1c)
threshold <- 4 / (nrow(indaf.stat) - length(coef(ses.h1c)))
influential_points <- which(cooksD > threshold)
inf.sesh1c <- indaf.stat[influential_points,]
indaf.stat.sesh1c <-indaf.stat[-influential_points,]
cd.ses.h1c <- lm(votetxspnd ~ status + left + eqhhmnnet + status*friends + similar + talk + freq.use + freq.post +
                   own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv, data = indaf.stat.sesh1c, weights = indaf.stat.sesh1c$indscui_xw)

cooksD <- cooks.distance(inc.h1d)
threshold <- 4 / (nrow(indaf.stat) - length(coef(inc.h1d)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch1d <-indaf.stat[-influential_points,]
cd.inc.h1d <- lm(votetxspnd ~ inc.stat + eqhhmnnet + inc.stat*friends + similar + talk + freq.use + freq.post +
                   own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv, data = indaf.stat.inch1d, weights = indaf.stat.inch1d$indscui_xw)

cooksD <- cooks.distance(ses.h1f)
threshold <- 4 / (nrow(indaf.stat) - length(coef(ses.h1f)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh1f <-indaf.stat[-influential_points,]
cd.ses.h1f <- lm(votetxspnd ~ status + inc.stat + status*friends + inc.stat*friends + talk + similar + freq.use + freq.post +
                   own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv, data = indaf.stat.sesh1f, weights = indaf.stat.sesh1f$indscui_xw)

cd.h1 <- list(cd.ses.h1a, cd.ses.h1b, cd.ses.h1c, cd.inc.h1a, cd.inc.h1b, cd.inc.h1d, cd.ses.h1f)
stargazer(cd.h1, star.cutoffs = c(0.05, 0.01, 0.001))
# ----

# Hypothesis 2 ----
cooksD <- cooks.distance(ses.h2a)
threshold <- 4 / (nrow(indaf.stat.own) - length(coef(ses.h2a)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh2a <-indaf.stat.own[-influential_points,]

cd.ses.h2a <- lm(votetxspnd ~ status  + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                   urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv,
                 data = indaf.stat.sesh2a, weights = indaf.stat.sesh2a$indscui_xw)

cooksD <- cooks.distance(ses.h2b)
threshold <- 4 / (nrow(indaf.stat.own) - length(coef(ses.h2b)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh2b <-indaf.stat.own[-influential_points,]

cd.ses.h2b <- lm(votetxspnd ~ status + eqhhmnnet + hvalue + status*friends + talk + similar + freq.use + freq.post +
                   urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv + new_owner, 
                 data = indaf.stat.sesh2b, weights = indaf.stat.sesh2b$indscui_xw)

cooksD <- cooks.distance(ses.h2c)
threshold <- 4 / (nrow(indaf.stat.own) - length(coef(ses.h2c)))
influential_points <- which(cooksD > threshold)
inf.sesh2c <- indaf.stat.own[influential_points,]
indaf.stat.sesh2c <-indaf.stat.own[-influential_points,]

cd.ses.h2c <- lm(votetxspnd ~ status + eqhhmnnet + hvalue + status*friends + talk + similar + freq.use + freq.post +
                   urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                   invest + vote3 + left + gor_dv + status*new_owner, 
                 data = indaf.stat.sesh2c, weights = indaf.stat.sesh2c$indscui_xw)

cooksD <- cooks.distance(ses.h2aa)
threshold <- 4 / (nrow(indaf.stat.nown) - length(coef(ses.h2aa)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh2aa <-indaf.stat.nown[-influential_points,]

cd.ses.h2aa <- lm(votetxspnd ~ status  + eqhhmnnet + friends + talk + similar + freq.use + freq.post +
                    urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                    invest + vote3 + left + gor_dv, data = indaf.stat.sesh2aa, weights = indaf.stat.sesh2aa$indscui_xw)

cooksD <- cooks.distance(ses.h2bb)
threshold <- 4 / (nrow(indaf.stat.nown) - length(coef(ses.h2bb)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh2bb <-indaf.stat.nown[-influential_points,]
inf.sesh2bb <- indaf.stat.nown[influential_points,]
cd.ses.h2bb <- lm(votetxspnd ~ status + eqhhmnnet + status*friends + talk + similar + freq.use + freq.post +
                    urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                    invest + vote3 + left + gor_dv, data = indaf.stat.sesh2bb, weights = indaf.stat.sesh2bb$indscui_xw)

cd.h2 <- list(cd.ses.h2a, cd.ses.h2b, cd.ses.h2c, cd.ses.h2aa, cd.ses.h2bb)
stargazer(cd.h2, star.cutoffs = c(0.05, 0.01, 0.001)) 
# ----
# Hypothesis 4a ----
cooksD <- cooks.distance(ses.h4aa)
threshold <- 4 / (nrow(indaf.stat.poor) - length(coef(ses.h4aa)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh4aa <-indaf.stat.poor[-influential_points,]

cd.ses.h4aa <- lm(votetxspnd ~ status + eqhhmnnet + own + urban_dv + sex_dv + 
                    age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                    vote3 + left + gor_dv, data = indaf.stat.sesh4aa, 
                  weights = indaf.stat.sesh4aa$indscui_xw)

cooksD <- cooks.distance(inc.h4aa)
threshold <- 4 / (nrow(indaf.stat.poor) - length(coef(inc.h4aa)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch4aa <-indaf.stat.poor[-influential_points,]

cd.inc.h4aa <- lm(votetxspnd ~ inc.stat + eqhhmnnet +own + urban_dv + sex_dv + 
                    age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                    vote3 + left + gor_dv, data = indaf.stat.inch4aa, 
                  weights = indaf.stat.inch4aa$indscui_xw)

cooksD <- cooks.distance(ses.h4ab)
threshold <- 4 / (nrow(indaf.stat.poor) - length(coef(ses.h4ab)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh4ab <-indaf.stat.poor[-influential_points,]

cd.ses.h4ab <- lm(votetxspnd ~ status*friends + eqhhmnnet + talk + similar+ own + urban_dv + sex_dv + 
                    age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + freq.use + freq.post +
                    vote3 + left + gor_dv, data = indaf.stat.sesh4ab, 
                  weights = indaf.stat.sesh4ab$indscui_xw)


cooksD <- cooks.distance(inc.h4ab)
threshold <- 4 / (nrow(indaf.stat.poor) - length(coef(inc.h4ab)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch4ab <-indaf.stat.poor[-influential_points,]

cd.inc.h4ab <- lm(votetxspnd ~ inc.stat*friends + eqhhmnnet + talk + similar + freq.use + freq.post +
                    own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                    invest + vote3 + left + gor_dv, data = indaf.stat.inch4ab, 
                  weights = indaf.stat.inch4ab$indscui_xw)

cooksD <- cooks.distance(ses.h4ac)
threshold <- 4 / (nrow(indaf.stat.poor) - length(coef(ses.h4ac)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh4ac <-indaf.stat.poor[-influential_points,]

cd.ses.h4ac <- lm(votetxspnd ~ status*friends + inc.stat*friends + eqhhmnnet + talk + similar + freq.use + freq.post +
                    own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                    invest + vote3 + left + gor_dv, data = indaf.stat.sesh4ac, 
                  weights = indaf.stat.sesh4ac$indscui_xw)

cd.h4a <- list(cd.ses.h4aa, cd.inc.h4aa, cd.ses.h4ab, cd.inc.h4ab, cd.ses.h4ac)
stargazer(cd.h4a, star.cutoffs = c(0.05, 0.01, 0.001))

#  Hypothesis 4b ----
cooksD <- cooks.distance(ses.h4ba)
threshold <- 4 / (nrow(indaf.stat.rich) - length(coef(ses.h4ba)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh4ba <-indaf.stat.rich[-influential_points,]

cd.ses.h4ba <- lm(votetxspnd ~ status + eqhhmnnet + own + urban_dv + sex_dv + 
                    age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                    vote3 + left + gor_dv, data = indaf.stat.sesh4ba, 
                  weights = indaf.stat.sesh4ba$indscui_xw)

cooksD <- cooks.distance(inc.h4ba)
threshold <- 4 / (nrow(indaf.stat.rich) - length(coef(inc.h4ba)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch4ba <-indaf.stat.rich[-influential_points,]

cd.inc.h4ba <- lm(votetxspnd ~ inc.stat + eqhhmnnet + own + urban_dv + sex_dv + 
                    age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + 
                    vote3 + left + gor_dv, data = indaf.stat.inch4ba, 
                  weights = indaf.stat.inch4ba$indscui_xw)

cooksD <- cooks.distance(ses.h4bb)
threshold <- 4 / (nrow(indaf.stat.rich) - length(coef(ses.h4bb)))
influential_points <- which(cooksD > threshold)
indaf.stat.sesh4bb <-indaf.stat.rich[-influential_points,]

cd.ses.h4bb <- lm(votetxspnd ~ status*friends + eqhhmnnet + talk + similar+ own + urban_dv + sex_dv + 
                    age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + freq.use + freq.post +
                    vote3 + left + gor_dv, data = indaf.stat.sesh4bb, 
                  weights = indaf.stat.sesh4bb$indscui_xw)


cooksD <- cooks.distance(inc.h4bb)
threshold <- 4 / (nrow(indaf.stat.rich) - length(coef(inc.h4bb)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch4bb <-indaf.stat.rich[-influential_points,]

cd.inc.h4bb <- lm(votetxspnd ~ inc.stat*friends + eqhhmnnet + talk + similar + freq.use + freq.post +
                    own + urban_dv + sex_dv + age_dv + hiqual_dv + ethn_dv + unemployed + retired +
                    invest + vote3 + left + gor_dv, data = indaf.stat.inch4bb, 
                  weights = indaf.stat.inch4bb$indscui_xw)

cooksD <- cooks.distance(ses.h4bc)
threshold <- 4 / (nrow(indaf.stat.rich) - length(coef(ses.h4bc)))
influential_points <- which(cooksD > threshold)
indaf.stat.inch4bc <-indaf.stat.rich[-influential_points,]

cd.ses.h4bc <- lm(votetxspnd ~ status*friends + inc.stat*friends + eqhhmnnet + talk + similar + own + urban_dv + sex_dv + 
                    age_dv + hiqual_dv + ethn_dv + unemployed + retired + invest + freq.use + freq.post +
                    vote3 + left + gor_dv, data = indaf.stat.inch4bc, 
                  weights = indaf.stat.inch4bc$indscui_xw)
cd.h4b <- list(cd.ses.h4ba, cd.inc.h4ba, cd.ses.h4bb, cd.inc.h4bb, cd.ses.h4bc)
stargazer(cd.h4b, star.cutoffs = c(0.05, 0.01, 0.001))

