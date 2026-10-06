
##==============================================================================
##
##  Course: Decomposition methods for Social Epi (Berkeley)
##
##  Script for the lab.
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




# 2. SMR of Japan, 2000 vs 2020

# Below the mortality rates for 2000. 
Mx2000 <- 
        df.mx |> 
        filter(
                Year == 2000
        ) |> 
        pull(
                Female
        )

# Compute the other elements you need to get the SMR.





# 3. Kitagawa decomposition







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

# Compute the other elements you need to do the decomposition.



