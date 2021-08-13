library(tidyverse)
library(Hmisc)
library(climdex.pcic) # Multithreading

# * Replace outliers 
# * Grouped mean imputation
# * Combine all data

df_impute <- function(col, df_file) {
    # Single column mean filling
    tmp <- impute(df_file[, col], mean)
    # print(col)
    # print(tmp)
    if (unique(tmp)[1] == "NaN" & length(unique(tmp)) == 1) { # When the column is all empty, fill it with NAN
        tmp <- as.numeric(rep(NA, nrow(df_file)))
    } else {
        tmp <- as.numeric(tmp)
    }
    return(tmp)
}

df_file_process <- function(file, set = "A") {
    # file <- files_A[1]
    # Read a single file
    df_file <- read.csv(
        paste0("train/training_set", set, "/", file), 
        stringsAsFactors = FALSE, sep = "|", na.strings = "NaN", 
        colClasses = rep("numeric", 41)
    )
    
    # Outliers changed to NA========================================================
    df_file <- df_file %>% 
        mutate(HR = ifelse((is.na(HR) | (HR >= 25 & HR <= 230)), HR, NA)) %>%
        mutate(O2Sat = ifelse((is.na(O2Sat) | (O2Sat >= 80 & O2Sat <= 100)), O2Sat, NA)) %>%
        mutate(Temp = ifelse((is.na(Temp) | (Temp >= 25 & Temp <= 47)), Temp, NA)) %>%
        mutate(SBP = ifelse((is.na(SBP) | (SBP >= 60 & SBP <= 231)), SBP, NA)) %>%
        mutate(MAP = ifelse((is.na(MAP) | (MAP >= 39 & MAP <= 155)), MAP, NA)) %>%
        mutate(DBP = ifelse((is.na(DBP) | (DBP >= 27 & DBP <= 126)), DBP, NA)) %>%
        mutate(Resp = ifelse((is.na(Resp) | (Resp >= 5 & Resp <= 41)), Resp, NA)) %>%
        mutate(EtCO2 = ifelse((is.na(EtCO2) | (EtCO2 >= 13 & EtCO2 <= 68)), EtCO2, NA)) %>%
        mutate(BaseExcess = ifelse((is.na(BaseExcess) | (BaseExcess >= -15 & BaseExcess <= 13)), BaseExcess, NA)) %>%
        mutate(HCO3 = ifelse((is.na(HCO3) | (HCO3 >= 7 & HCO3 <= 41)), HCO3, NA)) %>%
        # mutate(FiO2 = ifelse((is.na(FiO2) | (FiO2 >=  & FiO2 <= )), FiO2, NA)) %>%
        mutate(pH = ifelse((is.na(pH) | (pH >= 7 & pH <= 7.7)), pH, NA)) %>%
        mutate(PaCO2 = ifelse((is.na(PaCO2) | (PaCO2 >= 5 & PaCO2 <= 75)), PaCO2, NA)) %>%
        mutate(SaO2 = ifelse((is.na(SaO2) | (SaO2 >= 82 & SaO2 <= 100)), SaO2, NA)) %>%
        # mutate(AST = ifelse((is.na(AST) | (AST >=  & AST <= )), AST, NA)) %>%
        mutate(BUN = ifelse((is.na(BUN) | (BUN >= 0 & BUN <= 76)), BUN, NA)) %>%
        mutate(Alkalinephos = ifelse((is.na(Alkalinephos) | (Alkalinephos >= 20 & Alkalinephos <= 200)), Alkalinephos, NA)) %>%
        mutate(Calcium = ifelse((is.na(Calcium) | (Calcium >= 4.7 & Calcium <= 11.7)), Calcium, NA)) %>%
        mutate(Chloride = ifelse((is.na(Chloride) | (Chloride >= 81 & Chloride <= 130)), Chloride, NA)) %>%
        mutate(Creatinine = ifelse((is.na(Creatinine) | (Creatinine >= 0 & Creatinine <= 3.5)), Creatinine, NA)) %>%
        mutate(Bilirubin_direct = ifelse((is.na(Bilirubin_direct) | (Bilirubin_direct >= 0 & Bilirubin_direct <= 3.95)), Bilirubin_direct, NA)) %>%
        mutate(Glucose = ifelse((is.na(Glucose) | (Glucose >= 35 & Glucose <= 224)), Glucose, NA)) %>%
        # mutate(Lactate = ifelse((is.na(Lactate) | (Lactate >=  & Lactate <= )), Lactate, NA)) %>%
        mutate(Magnesium = ifelse((is.na(Magnesium) | (Magnesium >= 0.6 & Magnesium <= 3.4)), Magnesium, NA)) %>%
        mutate(Phosphate = ifelse((is.na(Phosphate) | (Phosphate >= 0 & Phosphate <= 8.6)), Phosphate, NA)) %>%
        mutate(Potassium = ifelse((is.na(Potassium) | (Potassium >= 1.6 & Potassium <= 6.5)), Potassium, NA)) %>%
        mutate(Bilirubin_total = ifelse((is.na(Bilirubin_total) | (Bilirubin_total >= 0 & Bilirubin_total <= 5.3)), Bilirubin_total, NA)) %>%
        mutate(TroponinI = ifelse((is.na(TroponinI) | (TroponinI >= 0 & TroponinI <= 16)), TroponinI, NA)) %>%
        mutate(Hct = ifelse((is.na(Hct) | (Hct >= 6 & Hct <= 55)), Hct, NA)) %>%
        mutate(Hgb = ifelse((is.na(Hgb) | (Hgb >= 1.3 & Hgb <= 19.5)), Hgb, NA)) %>%
        mutate(PTT = ifelse((is.na(PTT) | (PTT >= 5 & PTT <= 65)), PTT, NA)) %>%
        mutate(WBC = ifelse((is.na(WBC) | (WBC >= 0 & WBC <= 32)), WBC, NA)) %>%
        mutate(Fibrinogen = ifelse((is.na(Fibrinogen) | (Fibrinogen >= 0 & Fibrinogen <= 596)), Fibrinogen, NA)) %>%
        mutate(Platelets = ifelse((is.na(Platelets) | (Platelets >= 0 & Platelets <= 421)), Platelets, NA))

    # ==========================================================================
    
    cols <- colnames(df_file)
    # Mean value filling for 'SepsisLabel = 0' in a single file
    df_file_0 <- df_file %>% filter(SepsisLabel == "0")
    if (nrow(df_file_0) > 0) {
        for (col in cols) {
            if (!(col %in% c("Age", "Gender", "HospAdmTime", "ICULOS", "SepsisLabel"))) {
                df_file_0[, col] <- df_impute(col, df_file_0)
            }
        }
    }
    
    # Mean value filling for 'SepsisLabel = 1' in a single file
    df_file_1 <- df_file %>% filter(SepsisLabel == "1")
    if (nrow(df_file_1) > 0) {
        for (col in cols) {
            if (!(col %in% c("Age", "Gender", "HospAdmTime", "ICULOS", "SepsisLabel"))) {
                df_file_1[, col] <- df_impute(col, df_file_1)
            }
        }
    }
    
    df_file <- rbind(df_file_0, df_file_1)
    df_file <- df_file %>% mutate(person = unlist(str_split(file, pattern = "[.]"))[1], set = set)
    return(df_file)
}

# Combine all data of training_setA
files_A <- dir("train/training_setA/")
# df_file <- df_file_process(files[1])
system.time({
    df_file_setA <- lapply(1:length(files_A), function(i){
        df_file <- df_file_process(files_A[i], set = "A")
    })
    df_file_setA <- do.call("rbind", df_file_setA)
})

# Combine all data of training_setB
files_B <- dir("train/training_setB/")
# df_file <- df_file_process(files[1])
system.time({
    df_file_setB <- lapply(1:length(files_B), function(i){
        df_file <- df_file_process(files_B[i], set = "B")
    })
    df_file_setB <- do.call("rbind", df_file_setB)
})

save(df_file_setA, df_file_setB, file = "training_set.RData")

