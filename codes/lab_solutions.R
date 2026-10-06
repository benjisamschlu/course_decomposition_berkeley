
##==============================================================================
##
##  Course: Decomposition methods for Social Epi (Berkeley)
##
##  Solutions to the lab.
##
## 
##  Author: Benjamin Schlüter
##  Date: October 2026
##==============================================================================
##
##  Notes
## ------
## 1. 
## 
##==============================================================================





##==============================================================================
## LOAD PACKAGES 
##==============================================================================

# Install/load packages
packages <- c("tidyverse", "here")
for(p in packages){
        if(!require(p,character.only = TRUE)) install.packages(p)
        library(p,character.only = TRUE)
}





##==============================================================================
## SECTION A
##==============================================================================

## ----- Load data

# Data comes from HMD (https://www.mortality.org/) 
# and HFD (https://www.humanfertility.org/).

# Age-specific death counts
df.dth <- read.table(
        here(
                "inputs",
                "data",
                "jap_dth_1x1.txt"),
        header = TRUE,
        sep = "",
        dec = "."
) |>
        mutate(
                # Remove + in variable Age (i.e 110+)
                Age = gsub("[^0-9]", "", Age) |> as.numeric(),
                across(Age:Total, as.numeric)
        )

head(df.dth, 10)

# Age-specific exposures to risk
df.exp <- read.table(
        here(
                "inputs",
                "data",
                "jap_exp_1x1.txt"),
        header = TRUE,
        sep = "",
        dec = "."
) |>
        mutate(
                # Remove + in variable Age (i.e 110+)
                Age = gsub("[^0-9]", "", Age) |> as.numeric(),
                across(Age:Total, as.numeric)
        )

head(df.exp, 10)

# Age-specific mortality rates
df.mx <-  read.table(
        here(
                "inputs",
                "data",
                "jap_mx_1x1.txt"),
        header = TRUE,
        sep = "",
        dec = "."
) |>
        mutate(
                # Remove + in variable Age (i.e 110+)
                Age = gsub("[^0-9]", "", Age) |> as.numeric(),
                across(Age:Total, as.numeric)
        )

head(df.mx, 10)


## ----- Exercises

# 1. CDR of Japan, 2000 vs 2020

df.cdr <-
        df.dth |> 
        dplyr::select(
                Year, Age, Deaths = Female
        ) |> 
        left_join(
                df.exp |>
                        dplyr::select(
                                Year, Age, Exp = Female
                        ),
                
                by = c("Age", "Year")
        )

# Now compute the CDR for both years using df.cdr
df.cdr |> 
        filter(
                Year %in% c(2000, 2020)
        ) |> 
        summarise(
                .by = Year,
                
                cdr = sum(Deaths) / sum(Exp)
        )

# 2. SMR of Japan, 2000 vs 2020

# I will use matrix operations for efficiency

Mx2000 <- 
        df.mx |> 
        filter(
                Year == 2000
        ) |> 
        pull(
                Female
        )

Mx2020 <- 
        df.mx |> 
        filter(
                Year == 2020
        ) |> 
        pull(
                Female
        )

df.cx <- 
        df.exp |> 
        filter(
                Year %in% c(2000, 2020)
        ) |> 
        mutate(
                .by = Year,
                
                cx = Female / sum(Female)
        )
        
Cx2000 <- 
        df.cx |> 
        filter(
                Year == 2000
        ) |> 
        pull(
                cx
                )

smr <- Mx2000 %*% Cx2000 / Mx2020 %*% Cx2000

# 3. Kitagawa decomposition

Cx2020 <- 
        df.cx |> 
        filter(
                Year == 2020
        ) |> 
        pull(
                cx
        )

comp.effect <- sum( ((Mx2020 + Mx2000)/2) * (Cx2020 - Cx2000) )
rate.effect <- sum( ((Cx2020 + Cx2000)/2) * (Mx2020 - Mx2000) )
        
comp.effect + rate.effect





##==============================================================================
## SECTION B
##==============================================================================

## ----- Load data

# Data comes from HMD (https://www.mortality.org/) 
# and HFD (https://www.humanfertility.org/).

# Age-specific birth counts
df.bth <- bind_rows(
        read.table(
                here(
                        "inputs",
                        "data",
                        "chl_bth_1x1.txt"),
                header = TRUE,
                sep = "",
                dec = "."
        ) |> 
                mutate(
                        ctry = "Chile"
                ),
        read.table(
                here(
                        "inputs",
                        "data",
                        "irl_bth_1x1.txt"),
                header = TRUE,
                sep = "",
                dec = "."
        ) |> 
                mutate(
                        ctry = "Ireland"
                )
        ) |>
                mutate(
                        # Remove + in variable Age (i.e 110+)
                        Age = gsub("[^0-9]", "", Age) |> as.numeric(),
                        across(Age:Total, as.numeric)
                )

head(df.bth, 10)

# Age-specific exposures to risk
df.exp <- 
        bind_rows(
                read.table(
                        here(
                                "inputs",
                                "data",
                                "chl_exp_f_1x1.txt"),
                        header = TRUE,
                        sep = "",
                        dec = "."
                ) |> 
                        mutate(
                                ctry = "Chile"
                        ),
                read.table(
                        here(
                                "inputs",
                                "data",
                                "irl_exp_f_1x1.txt"),
                        header = TRUE,
                        sep = "",
                        dec = "."
                ) |> 
                        mutate(
                                ctry = "Ireland"
                        )
        ) |>
        mutate(
                # Remove + in variable Age (i.e 110+)
                Age = gsub("[^0-9]", "", Age) |> as.numeric(),
                across(Age:Exposure, as.numeric)
        )

head(df.exp, 10)

# Age-specific fertilty rates
df.fx <- 
        bind_rows(
                read.table(
                        here(
                                "inputs",
                                "data",
                                "chl_fx_1x1.txt"),
                        header = TRUE,
                        sep = "",
                        dec = "."
                ) |> 
                        mutate(
                                ctry = "Chile"
                        ),
                read.table(
                        here(
                                "inputs",
                                "data",
                                "irl_fx_1x1.txt"),
                        header = TRUE,
                        sep = "",
                        dec = "."
                ) |> 
                        mutate(
                                ctry = "Ireland"
                        )
        ) |>
        mutate(
                # Remove + in variable Age (i.e 110+)
                Age = gsub("[^0-9]", "", Age) |> as.numeric()
        )

head(df.fx, 10)


## ----- Exercises

# 1. GFR of Chile and Ireland, 2020
# Focus on ages 15 to 49 years old!

df.gfr <-
        df.bth |> 
        filter(
                Year == 2020,
                Age %in% 15:49
        ) |> 
        dplyr::select(
                ctry, Year, Age, Births = Total
        ) |> 
        left_join(
                df.exp |>
                        filter(
                                Year == 2020,
                                Age %in% 15:49
                        ) |> 
                        dplyr::select(
                                ctry, Year, Age, Exposure
                        ),
                
                by = c("ctry", "Year", "Age")
        )

# Now compute the GFR for both countries using df.gfr
df.gfr |> 
        summarise(
                
                .by = "ctry", 
                
                gfr = sum(Births) / sum(Exposure)
        )

# 2. Kitagawa for GFR

FxIreland <- 
        df.fx |> 
        filter(
                Year == 2020,
                Age %in% 15:49,
                ctry == "Ireland"
        ) |> 
        pull(
                ASFR
        )

FxChile <- 
        df.fx |> 
        filter(
                Year == 2020,
                Age %in% 15:49,
                ctry == "Chile"
        ) |> 
        pull(
                ASFR
        )


df.cx <- 
        df.exp |> 
        filter(
                Year == 2020,
                Age %in% 15:49
        ) |> 
        mutate(
                .by = c(ctry, Year),
                
                cx = Exposure / sum(Exposure)
        )

CxIreland <- 
        df.cx |> 
        filter(
                ctry == "Ireland"
        ) |> 
        pull(
                cx
        )

CxChile <- 
        df.cx |> 
        filter(
                ctry == "Chile"
        ) |> 
        pull(
                cx
        )
                

comp.effect <- sum( ((FxIreland + FxChile)/2) * (CxIreland - CxChile) )
rate.effect <- sum( ((CxIreland + CxChile)/2) * (FxIreland - FxChile) )

comp.effect + rate.effect



