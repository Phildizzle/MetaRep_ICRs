library(readxl)
library(tidycomm)
library(dplyr)
library(tidyr)

df <- read_excel("H:\\Meta-Rep\\R Code\\ICR Tests\\ICR_Data set.xlsx", na = "0")

# replace first row since it is a semantic header, and delete ID variable 
df <- df[-1, ]
df$CASE <- NULL
df$QUESTNNR <- NULL

# test_icr() can only handle integer ids in the post_id, so we remove all letters from our ID column
df$A009_01 <- gsub("[^0-9.-]", "", df$A009_01)

# delete columns with all NAs
df <- df[,colSums(is.na(df))<nrow(df)]

# transform all NAs to make it easier for tidycomm
df[is.na(df)] <- "NA"

# calculate ICRs

ICR_tibble <- test_icr(df, A009_01, A011, na.omit = TRUE, kripp_alpha = TRUE, cohens_kappa = TRUE, holsti = TRUE) #post_id is for , coder_id is for c, , A007, A007_01, A007_02, A008x01
ICR_tibble

# Retro Sprintf statements for old time's sake ;)

sprintf("Mean Agreement/SD: %s/%s.", round(mean(ICR_tibble$Agreement),4), round(sd(ICR_tibble$Agreement),4))
sprintf("Mean Krippendorf/SD: %s/%s", round(mean(ICR_tibble$Krippendorffs_Alpha),4), round(sd(ICR_tibble$Krippendorffs_Alpha),4))
sprintf("Mean Cohen's Kappa/SD: %s/%s.", round(mean(ICR_tibble$Holstis_CR),4), round(sd(ICR_tibble$Holstis_CR),4))

# Categories in which we performed bad:
# Units of analysis: We'll discuss this in a coder meeting but it is overall a very difficult category to codify since there are usually
# multiple units of analysis in a paper and the coder has to make an individual decision which one is the most important one.
# 


#####################
# Sensitivity Side Note
#
# One paper about the introduction of a chatbot tool causes significant turbulence in our coding
# That paper is "Conversational Agent Research Toolkit" by Theo Araujo. We already discussed the problems this paper caused since it is
# a non-empirical paper in which a toolkit for chatbot creation is explained. If we exclude this paper our ICRs are way better.
# w/o araujo paper

# Same preprocessing
df_1 <- read_excel("H:\\Meta-Rep\\R Code\\ICR Tests\\ICR_Data set.xlsx", na = "0")

df_1 <- df_1[-1, ]

df_1$A009_01 <- gsub("[^0-9.-]", "", df_1$A009_01)

df_1 <- df_1[,colSums(is.na(df_1))<nrow(df_1)]

df_1[is.na(df_1)] <- "NA"
df_1 <- df_1[-c(1:2),]

test_tibble_1 <- test_icr(df_1, A009_01, A011, na.omit = TRUE, kripp_alpha = TRUE, cohens_kappa = TRUE, holsti = TRUE) #post_id is for , coder_id is for c, , A007, A007_01, A007_02, A008x01

sprintf("Mean Agreement/SD: %s/%s.", round(mean(test_tibble_1$Agreement),4), round(sd(test_tibble_1$Agreement),4))
sprintf("Mean Krippendorf/SD: %s/%s", round(mean(test_tibble_1$Krippendorffs_Alpha),4), round(sd(test_tibble_1$Krippendorffs_Alpha),4))
sprintf("Mean Cohen's Kappa/SD: %s/%s.", round(mean(test_tibble_1$Holstis_CR),4), round(sd(test_tibble_1$Holstis_CR),4))

# This really shows how sensitive to very small changes ICRs are in our case, as most of the data exhibits extremely low variance, 
# i.e. most categories consist mainly of one value with only minor variance. 

# Second Sensitivity side note:
# If we exclude all perfectly scored categories, our Krippendorf drops by a lot and the SD is still really high.
test_tibble_2 <- ICR_tibble

test_tibble_no_1s <- subset(test_tibble_2, Krippendorffs_Alpha != 1.0)

sprintf("Mean Agreement/SD: %s/%s.", round(mean(test_tibble_no_1s$Agreement),4), round(sd(test_tibble_no_1s$Agreement),4))
sprintf("Mean Krippendorf/SD: %s/%s", round(mean(test_tibble_no_1s$Krippendorffs_Alpha),4), round(sd(test_tibble_no_1s$Krippendorffs_Alpha),4))
sprintf("Mean Cohen's Kappa/SD: %s/%s.", round(mean(test_tibble_no_1s$Holstis_CR),4), round(sd(test_tibble_no_1s$Holstis_CR),4))

# Third sensitivity side note:























