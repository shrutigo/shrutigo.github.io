pkgs <- c('vitae', 'tidyverse', 'lubridate', 'here', 'readxl', 'glue', 'pdftools', 'ggplot2', 'devtools', 'rmarkdown')
install.packages(pkgs)

devtools::install_github("laresbernardo/lares")

rmarkdown::render('shruti-hegde/shruti-hegde.Rmd')
rmarkdown::render_site(encoding = 'UTF-8')
