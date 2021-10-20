Código 1: Importar datos y generar descriptivos
================
dacarras
Octubre 20, 2021

# Librerías

## Librerías en uso para el presente código

``` r
#------------------------------------------------------------------------------
# librerías
#------------------------------------------------------------------------------

# ---------------------------------------------------------
# librerías para este código
# ---------------------------------------------------------

install.packages('tidyverse')
# librería que instala diversar librería para el manejo de datos

install.packages('haven')     
# librería para abrir datos SPSS, STATA y SAS

install.packages('devtools')  
# librería para instalar librerias en desarollo

devtools::install_github("dacarras/r4sda") 
# librería experimental con funciones auxiliares al analisis de datos secundarios

install.packages("RALSA", dependencies = TRUE)
# librería para convertir datos de PISA 2006 a datos en formato R

install.packages("instvy")
# librería para estimar resultados con estudios de gran escala

install.packages("survey")
# librería para estimar resultados con estudios que incluyen muestras complejas

install.packages("srvyr")
# librería para estimar resultados con estudios que incluyen muestras complejas

install.packages("mitools")
# librería para combinar resultados de datos imputados
```

# Problema 1: Importación de datos

-   Los archivos de PISA 2006 se encuentran en formato texto plano
-   Para ser importados el usuario debiera emplear SPSS o SAS
-   Estos datos pueden ser importados de manera manual, pero esto
    requiere que el usuario asigne los nombres de variables y otras
    propiedades a cada columna importada
-   Una manera de resolver este problema es emplear la librería `RALSA`,
    la cual lee los syntax de SPSS y genera objetos en el entorno R
    ahorrando al usuario secundario el tiempo que implicaría realizar
    todo este trabajo de forma manual.

## Código 1: Importación de datos en texto plano

-   Para resolver este problema empleamos la librería RALSA, en
    particular la función `RALSA::lsa.convert.data()`
-   La secuencia de este código es la siguiente
    -   Primero colocamos los archivos de PISA 2006 en una carpeta, en
        este caso en la carpeta:
        -   `'/Users/d/Dropbox (Personal)/_data/PISA 2006/data/'`
        -   En esta misma carpeta, incluimos los archivos SPSS que
            contienen los nombres y formatos de los archvios de texto
            plano
    -   Segundo, específicamos los argumentos respectivos de la función
        -   indicamos la carpeta que contiene los archivos de datos de
            PISA 2006 en `inp.folder`
        -   indicamos la carpeta que alojará los archivos que se van a
            generar en `out.folder`
        -   indicamos que los archivos que serán procesados son previos
            a 2015 con el argumento `PISApre15 = TRUE`
        -   Finalmente ejecutamos el código.
    -   Tercero, convertimos todos los archivos `Rdata` a `rds` de modo
        que puedan tomar cualquier nombre al ser abiertos.
    -   Cuarto, inspreccionamos los archivos generados en la carpeta
        `rds_folder`

``` r
# -----------------------------------------------------------------------------
# import all data
# -----------------------------------------------------------------------------

# -----------------------------------------------
# relative folder
# -----------------------------------------------

folder_within <- function(x){
paste0(tools::file_path_as_absolute(x),'/')
}

# Note: this requires to set the working directory to the current
#       folder in which is the r syntax folder.
#       it allows to open a folder within the working directory
#       in a relative way.

# -----------------------------------------------
# location folder
# -----------------------------------------------

data_folder    <- folder_within(paste0(getwd(),'/data/'))
rds_folder     <- folder_within(paste0(getwd(),'/rds/'))

# -----------------------------------------------
# convert txt files into R data sets using RALSA
# -----------------------------------------------

RALSA::lsa.convert.data(
                 inp.folder = data_folder,
                 PISApre15 = TRUE, 
                 out.folder = rds_folder
                 )
```

    ## 
    ## 5  datasets selected for conversion. Some datasets can be rather large. Please be patient.

    ##        (1/5)  PISA2006_SPSS_cognitive_item.txt         converted in 00:00:03.950

    ## 

    ##        (3/5)  PISA2006_SPSS_school.txt                 converted in 00:00:00.832

    ##        (4/5)  PISA2006_SPSS_scored_cognitive_item.txt  converted in 00:00:02.555

    ##        (5/5)  PISA2006_SPSS_student.txt                converted in 00:00:05.213

    ## 
    ##  All 5 found files successfully converted in 00:00:12.863

``` r
# -----------------------------------------------
# check files
# -----------------------------------------------

list.files(rds_folder)
```

    ##  [1] "pisa_2006_cognitive_item.rds"             
    ##  [2] "pisa_2006_parent.rds"                     
    ##  [3] "pisa_2006_school.rds"                     
    ##  [4] "pisa_2006_scored_cognitive_item.rds"      
    ##  [5] "pisa_2006_student.rds"                    
    ##  [6] "pisa2006_spss_cognitive_item.RData"       
    ##  [7] "pisa2006_spss_parent.RData"               
    ##  [8] "pisa2006_spss_school.RData"               
    ##  [9] "pisa2006_spss_scored_cognitive_item.RData"
    ## [10] "pisa2006_spss_student.RData"

``` r
# -----------------------------------------------
# load data and save data in rds
# -----------------------------------------------

# schools data
load(paste0(rds_folder,'pisa2006_spss_school.RData'))

pisa2006_spss_school %>%
saveRDS(paste0(rds_folder,'pisa_2006_school.rds'))

# parents data
load(paste0(rds_folder,'pisa2006_spss_parent.RData'))

pisa2006_spss_parent %>%
saveRDS(paste0(rds_folder,'pisa_2006_parent.rds'))

# students data
load(paste0(rds_folder,'pisa2006_spss_student.RData'))

pisa2006_spss_student %>%
saveRDS(paste0(rds_folder,'pisa_2006_student.rds'))

# items score data
load(paste0(rds_folder,'pisa2006_spss_scored_cognitive_item.RData'))

pisa2006_spss_scored_cognitive_item %>%
saveRDS(paste0(rds_folder,'pisa_2006_scored_cognitive_item.rds'))

# items responses
load(paste0(rds_folder,'pisa2006_spss_cognitive_item.RData'))

pisa2006_spss_cognitive_item %>%
saveRDS(paste0(rds_folder,'pisa_2006_cognitive_item.rds'))

# -----------------------------------------------
# check rds generated files
# -----------------------------------------------

list.files(rds_folder) %>%
stringr::str_subset('.rds') %>%
knitr::kable()
```

| x                                       |
|:----------------------------------------|
| pisa\_2006\_cognitive\_item.rds         |
| pisa\_2006\_parent.rds                  |
| pisa\_2006\_school.rds                  |
| pisa\_2006\_scored\_cognitive\_item.rds |
| pisa\_2006\_student.rds                 |

# Problema 2: Descriptivos empleando valores plausibles

-   Los valores plausibles son realizaciones de un modelo de respuesta
    condicionado
-   Esto implica que son valores imputados basados en un modelo
-   Como tales, se deben generar los cálculos con cada uno de estos
    valores plausibles
-   Luego estas estimaciones requieren ser combinadas empleando las
    reglas de Rubin (1987)
-   Lo anterior puede ser resuelto de al menos dos maneras:
    -   Emplear una libreria diseñada para datos de gran escala capaz de
        procesar valores plausibles (e.g., instvy, RALSA)
    -   Emplear una librería genérica que pueda interactual con
        `mitools` (e.g., survey)
-   En el siguiente ejemplo ilustramos como emplear `instvy` y `survey`

# Percentiles (Table 2.1c, p28)

## intsvy

-   <https://cran.r-project.org/web/packages/intsvy/intsvy.pdf>

``` r
#------------------------------------------------------------------------------
# percentiles with pv variables
#------------------------------------------------------------------------------

#------------------------------------------------
# load data
#------------------------------------------------

library(dplyr)
pisa_2006_student <- readRDS(paste0(rds_folder, 'pisa_2006_student.rds')) %>%
                     dplyr::filter(COUNTRY %in% c(
                      'Uruguay'
                      ))

#------------------------------------------------
# get percentiles
#------------------------------------------------

percentile_1 <- intsvy::pisa.per.pv(
                    pvlabel = "SCIE",
                    per = c(5, 10, 25, 75, 90, 95),
                    by = "CNT", 
                    data = pisa_2006_student)

#------------------------------------------------
# display results
#------------------------------------------------

percentile_1 %>%
knitr::kable(., digits = c(0, 0, 0, 1))
```

| CNT     | Percentiles | Score | Std. err. |
|:--------|------------:|------:|----------:|
| Uruguay |           5 |   274 |       6.8 |
| Uruguay |          10 |   306 |       4.9 |
| Uruguay |          25 |   363 |       4.1 |
| Uruguay |          75 |   493 |       3.3 |
| Uruguay |          90 |   550 |       3.6 |
| Uruguay |          95 |   583 |       4.2 |

## survey

-   <https://cran.r-project.org/web/packages/survey/survey.pdf>

``` r
#------------------------------------------------------------------------------
# percentiles with pv variables
#------------------------------------------------------------------------------

#------------------------------------------------
# create common clustering variables
#------------------------------------------------

data_example <- pisa_2006_student %>%
                dplyr::filter(COUNTRY == 'Uruguay') %>%
                mutate(id_j = as.numeric(as.factor(paste0(COUNTRY, "_", SCHOOLID)))) %>%
                mutate(id_i = seq(1:nrow(.)))

#------------------------------------------------
# separate plausible values
#------------------------------------------------

plausible_values <- dplyr::select(data_example, PV1SCIE, PV2SCIE, PV3SCIE, PV4SCIE, PV5SCIE)


non_plausible_values  <- dplyr::select(data_example, -one_of(names(plausible_values)))


# -----------------------------------------------
# create a data frame per plausible value
# -----------------------------------------------

data_1 <- non_plausible_values %>%
          dplyr::left_join(., dplyr::select(data_example, id_i, PV1SCIE), by = 'id_i') %>% 
          rename(scie = PV1SCIE)

data_2 <- non_plausible_values %>%
          dplyr::left_join(., dplyr::select(data_example, id_i, PV2SCIE), by = 'id_i') %>% 
          rename(scie = PV2SCIE)

data_3 <- non_plausible_values %>%
          dplyr::left_join(., dplyr::select(data_example, id_i, PV3SCIE), by = 'id_i') %>% 
          rename(scie = PV3SCIE)

data_4 <- non_plausible_values %>%
          dplyr::left_join(., dplyr::select(data_example, id_i, PV4SCIE), by = 'id_i') %>% 
          rename(scie = PV4SCIE)

data_5 <- non_plausible_values %>%
          dplyr::left_join(., dplyr::select(data_example, id_i, PV5SCIE), by = 'id_i') %>% 
          rename(scie = PV5SCIE)


#------------------------------------------------
# create a list of imputed data object
#------------------------------------------------

data_imputed <- mitools::imputationList(
                list(data_1, data_2, data_3, data_4, data_5)
                )


#------------------------------------------------
# specify survey design
#------------------------------------------------

library(survey)
data_svy <- survey::svrepdesign(
                data    = data_imputed,
                type    = 'Fay', 
                rho     = .5,
                weights = ~W_FSTUWT,
                repweights = "W_FSTR[0-9]+",
                combined.weights = TRUE
                )
#------------------------------------------------
# estimate percentiles with imputed data
#------------------------------------------------

percentiles_imp <- mitools::MIcombine(
                with(data_svy, 
                svyquantile(~scie, 
                  design=data_svy, 
                  quantile=c(.05, .10, .25, .75, .90, .95)
                    )
                  )
                )

#------------------------------------------------
# create table of estimates
#------------------------------------------------

percentile_2 <- summary(percentiles_imp) %>%
                tibble::rownames_to_column("percentiles") %>%
                dplyr::rename(scie = results, ,scie_se = se) %>%
                dplyr::select(percentiles, scie, scie_se)
```

    ## Multiple imputation results:
    ##       with(data_svy, svyquantile(~scie, design = data_svy, quantile = c(0.05, 
    ##     0.1, 0.25, 0.75, 0.9, 0.95)))
    ##       MIcombine.default(with(data_svy, svyquantile(~scie, design = data_svy, 
    ##     quantile = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95))))
    ##             results         se     (lower     upper) missInfo
    ## scie.0.05 273.58800 6.84972051 259.643124 287.532876     39 %
    ## scie.0.1  306.05676 4.91581844 296.296580 315.816940     22 %
    ## scie.0.25 363.47844 4.04017904 355.411824 371.545056     27 %
    ## scie.0.75 492.77530 3.30926733 486.027911 499.522689     40 %
    ## scie.0.9  549.69350 3.60868712 542.540053 556.846947     21 %
    ## scie.0.95 583.43040 4.24944976 575.023257 591.837543     19 %

``` r
# -----------------------------------------------
# display table
# -----------------------------------------------

knitr::kable(percentile_2, digits = 2)
```

| percentiles |   scie | scie\_se |
|:------------|-------:|---------:|
| scie.0.05   | 273.59 |     6.85 |
| scie.0.1    | 306.06 |     4.92 |
| scie.0.25   | 363.48 |     4.04 |
| scie.0.75   | 492.78 |     3.31 |
| scie.0.9    | 549.69 |     3.61 |
| scie.0.95   | 583.43 |     4.25 |

# Problema 3: Descriptivos empleando variables categóricas

-   Los cuestionarios de contexto incluyen diferentes variables
    categoricas
-   En general, estas variables son dicotomizadas para expresar un
    porcentaje o proporción
-   en el siguiente ejemplo emplearemos el grado de escolaridad máximo
    de los padres (`HISCED`)
-   Los valores originales los vamos a dicotomizar para distinguir entre
    aquellos padres que poseen educación terciaria (ISCED 5 y 6), y el
    resto de los padres.
-   Lo anterior puede ser resuelto de al menos dos maneras:
    -   Emplear una libreria diseñada para datos de gran escala (e.g.,
        instvy, RALSA)
    -   Emplear una librería genérica, diseñada para estudios de
        muestras complejas (e.g., srvyr)
-   En el siguiente ejemplo ilustramos como emplear `instvy` y `srvyr`

# Porcentajes (Table 4.7a, p141)

## intsvy

-   <https://cran.r-project.org/web/packages/intsvy/intsvy.pdf>

``` r
#------------------------------------------------------------------------------
# percentages
#------------------------------------------------------------------------------

#------------------------------------------------
# load data and create variables
#------------------------------------------------

library(dplyr)
pisa_2006_student <- readRDS(paste0(rds_folder, 'pisa_2006_student.rds')) %>%
                     dplyr::filter(COUNTRY %in% c(
                      'Uruguay'
                      )) %>%
                     mutate(ter = case_when(
                      as.numeric(HISCED) == 1 ~ 0,
                      as.numeric(HISCED) == 2 ~ 0,
                      as.numeric(HISCED) == 3 ~ 0,
                      as.numeric(HISCED) == 4 ~ 0,
                      as.numeric(HISCED) == 5 ~ 0,
                      as.numeric(HISCED) == 6 ~ 1,
                      as.numeric(HISCED) == 7 ~ 1
                      ))

#------------------------------------------------
# calculate percentages
#------------------------------------------------

percentage_1 <- intsvy::pisa.table(
                variable = 'ter', 
                by = 'CNT', 
                data = pisa_2006_student)

#------------------------------------------------
# display estimates
#------------------------------------------------

percentage_1 %>%
knitr::kable(., digits = c(0, 0, 0, 1, 1))
```

| CNT     | ter | Freq | Percentage | Std.err. |
|:--------|:----|-----:|-----------:|---------:|
| Uruguay | 0   | 2050 |       45.7 |        1 |
| Uruguay | 1   | 2712 |       54.3 |        1 |

## srvyr

-   <https://cran.r-project.org/web/packages/srvyr/srvyr.pdf>

``` r
#------------------------------------------------------------------------------
# percentages
#------------------------------------------------------------------------------


#------------------------------------------------
# load data and create variables
#------------------------------------------------

library(dplyr)
pisa_2006_student <- readRDS(paste0(rds_folder, 'pisa_2006_student.rds')) %>%
                     dplyr::filter(COUNTRY %in% c(
                      'Uruguay'
                      )) %>%
                     mutate(ter = case_when(
                      as.numeric(HISCED) == 1 ~ 0,
                      as.numeric(HISCED) == 2 ~ 0,
                      as.numeric(HISCED) == 3 ~ 0,
                      as.numeric(HISCED) == 4 ~ 0,
                      as.numeric(HISCED) == 5 ~ 0,
                      as.numeric(HISCED) == 6 ~ 1,
                      as.numeric(HISCED) == 7 ~ 1
                      ))

#------------------------------------------------
# specify survey design
#------------------------------------------------

library(srvyr)
data_svy <- pisa_2006_student %>%
            r4sda::remove_labels() %>%
            as_survey_rep(type = 'Fay', 
            repweights = starts_with('W_FSTR'),
            weights = 'W_FSTUWT',
            combined_weights = TRUE,
            rho = .5,
            mse = TRUE)

#------------------------------------------------
# calculate percentages
#------------------------------------------------

percentage_2 <- data_svy %>%
             group_by(CNT) %>%
             summarize(
               e = survey_mean(ter, 
                   na.rm = TRUE,
                   vartype = c('se','ci'),
                   level = .95,
                   proportion = TRUE,
                   prop_method = 'logit')
               )

#------------------------------------------------
# display results
#------------------------------------------------

percentage_2 %>%
knitr::kable(., digits = c(0, 3, 3, 3, 3))
```

| CNT     |     e | e\_se | e\_low | e\_upp |
|:--------|------:|------:|-------:|-------:|
| Uruguay | 0.543 |  0.01 |  0.523 |  0.563 |

# Estimated times

``` r
# ---------------------------------------- 
# estimated times
# ---------------------------------------- 

difftime(end_time, start_time, units="mins")
```

    ## Time difference of 0.317007383 mins
