
# Capstoneproject

## Purpose
This repository holds the files of the Coursera Data Science Specialization CapStone project.

## Shiny App
Under TextPredictor you will find the files for the Shiny App
The Shiny app is available here: https://jraniero.shinyapps.io/TextPredictor/

## Preprocessing scripts
In the main directory you will find
- The Report.Rmd file that shows the exploratory data analysis and how the training data was obtained from the .txt project files
- The W3StoreNGrams.R script, that was used to create the n-gram dictionnary used by the shiny app
- The make_test_set.R script, used to create the test set from the project .txt files
- The test_perf.R script, used to measure the performance of the prediction algorithm
- The Analysis_Performance_Results.Rmd, that reports on the performance of the algorithm
- Different unix scripts used for exploratory analysis, sampling and cleanup

## Environment
R version 3.3.2 (2016-10-31)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: OS X El Capitan 10.11.4

locale:
[1] fr_BE.UTF-8/fr_BE.UTF-8/fr_BE.UTF-8/C/fr_BE.UTF-8/fr_BE.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] wordcloud_2.5      RColorBrewer_1.1-2 shiny_0.14.2       data.table_1.9.6  
[5] SnowballC_0.5.1    tm_0.6-2           NLP_0.1-9          stringr_1.1.0     

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.5     knitr_1.14      magrittr_1.5    xtable_1.8-2    R6_2.1.2       
 [6] tools_3.3.2     parallel_3.3.2  htmltools_0.3.5 yaml_2.1.13     rprojroot_1.2  
[11] digest_0.6.9    RJSONIO_1.3-0   bitops_1.0-6    RCurl_1.95-4.8  rsconnect_0.7  
[16] evaluate_0.9    slam_0.1-40     mime_0.4        rmarkdown_1.3   stringi_1.1.2  
[21] backports_1.0.5 jsonlite_1.2    httpuv_1.3.3    chron_2.3-47 
