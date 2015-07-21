# copy_webapps_templates.r

# INSTRUCTIONS 
# Run this script one time only. This will download the WebApps'
# template files to your local repository. You will then edit the files using
# RStudio. When editing, save, commit, and push all files normally and they will
# automatically be updated on the WebApp at 00:00 GMT every day.

# To edit the equations displayed on the WebApp's goals page, you will edit
# conf/goals.Rmd. The equations are written in a format similar to LaTex; use
# current equations as a syntax guide. To view the rendered equations, press the
# 'Knit HTML' button when working with goals.Rmd.
# Learn more about LaTex syntax at https://en.wikibooks.org/wiki/LaTeX/Mathematics.
# Learn about .Rmd formatting at http://shiny.rstudio.com/articles/rmarkdown.html

# To edit the text content that is displayed on your WebApp, edit only the text
# headers and descriptions; all other information is automatically rendered with
# the most recent content in rgn_labels.csv, layers.csv, goals.Rmd, and
# scores.csv and your changes will be overwritten. Do not change any of the
# formatting or spacing.


# install packages to render goals.Rmd
devtools:install_github("rmarkdown", "rstudio")

# setup for dopying WebApp template files
library(httr)
dir_gh  = '~/github/chn/gh-pages_templates'
url_loc = 'https://raw.githubusercontent.com/OHI-Science/ohi-webapps/master/results'

# create a gh-pages_templates folder
dir.create(dir_gh, recursive=T, showWarnings=F)

# download template files
for (f in c('regions.brew.md',   # ohi-science.org/chn/regions -> renders with layers/rgn_labels.csv
            'layers.brew.md',    # ohi-science.org/chn/layers  -> renders with layers.csv
            'goals.brew.md',     # ohi-science.org/chn/goals   -> renders with conf/goals.Rmd
            'scores.brew.md'     # ohi-science.org/chn/scores  -> renders with scores.csv
)){
  
  url_in = file.path(url_loc, f)
  f_out  = file.path(dir_gh, f)
  writeBin(httr::content(GET(url_in)), f_out)
  
}

