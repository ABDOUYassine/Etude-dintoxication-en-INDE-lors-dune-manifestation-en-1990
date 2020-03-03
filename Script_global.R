rm(list = ls())
#   ____________________________________________________________________________
#   Packages                                                                ####
library(dplyr)
library(broom)
library(ggplot2)
library(kableExtra)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(tidyverse)
library(naniar)
library(mice)

#   ____________________________________________________________________________
#   II. Vector of colours                                                   ####
Pamplemousse_colour <-
  c("#0218a2", "#ffb703", "#f76f73", "#027fdc", "#07c4c5")
Nueva_colour <-
  c("#012345", "#aa2345", "#ffa500", "#abcdef", "#d7a0e1")


#   ____________________________________________________________________________
#   Importation des fonctions                                               ####
source("Fonctions/NA_percentage.r")
source("Fonctions/NA_percentage_col.r")
source("Fonctions/Precision.r")
source("Fonctions/err_prediction.r")

#   ____________________________________________________________________________
#   Importation du jeu de données                                           ####

Intoxications <-
  read.table(file = "Datas/Intoxications.csv", sep = ";", header = T) %>% 
  dplyr::select(-1)

colnames(Intoxications) <- Intoxications %>% 
  names() %>% 
  str_replace_all("é","e") %>% 
  str_remove("\\..")


corrplot::corrplot(cor(Intoxications), method = "number")

df <- Intoxications %>% 
  dplyr::select(-age) %>% 
  replace_with_na_all(condition = ~ .x  %in%  c(90, 9)) %>% 
  mutate(age = Intoxications$age) %>% 
  replace_with_na(replace = list(age = 99))

df %>% 
  attach()

df$eclair <- ifelse(eclair == 80, median(na.omit(eclair)), eclair)

df %>% 
  detach()

#   ____________________________________________________________________________
#   Imputation des NA's par la méthode cart                                 ####

tempData_cart <- mice(
  df,
  m = 1,
  maxit = 7,
  meth = "cart",
  seed = 500
)

intox_cart <- mice::complete(tempData_cart, 1) %>% 
  select(Intoxications %>% names())

intox_cart %>% is.na() %>% sum()
#   ____________________________________________________________________________
#   Codage des variables (Variable dichotomique)                            ####

# Si l'une des variables de symptôme est égale 1 alors le patient est malade.

Intox_dicho <- intox_cart %>% 
  mutate(malade = ifelse(Diarree + Douleur + Vomissements + Nausee == 0, 0,1)) %>% 
  dplyr::select(-c('Diarree', 'Douleur', 'Vomissements', 'Nausee'))
