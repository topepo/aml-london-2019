# `aml-london-2019`

Course materials for _Applied Machine Learning_ course in 2019 in London

* November 18, 2019—November 19, 2019 9:00 AM-5:00 PM

* Hilton London Paddington (146 Praed Street, London, LND W2 1EE, [map](https://goo.gl/maps/LtTu8g4ChX4Dc8XaA))


## Materials

These will be posted a day before the workshop. The slides are in HTML, so browser that uses javascript is required. 

## Software

We will have RStudio server pro instances will all of the packages installed as well as the above GitHub repository available. 

If you would like to run R locally, the installation instructions are:

```r
install.packages(
  c(
    'AmesHousing',
    'C50',
    'devtools',
    'discrim',
    'earth',
    'ggthemes',
    'glmnet',   # See important note below
    'klaR',
    'lubridate',
    'party',
    'pROC',
    'rpart',
    'stringr',
    'textfeatures',
    'tidymodels'
  ),
  repos = "http://cran.rstudio.com"
)
devtools::install_github("tidymodels/recipes")
devtools::install_github("tidymodels/dials")
devtools::install_github("tidymodels/textrecipes")
devtools::install_github("r-lib/cli")
devtools::install_github("koalaverse/vip")
devtools::install_github("tidymodels/tune")
```

** Important note! ** A new version of `glmnet` was released on 2019-11-09. Although it states that it depends on R (≥ 3.5.0), it may not install on R versions < 3.6.0. 

We will be on-site at least 30min before the workshop commences in case you need any help getting packages installed. Prior to this, you can email `max@rstudio.com` with questions. 

 