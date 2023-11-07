# Trackman Live Dashboard

# Features

* Trackman Live Interface with Pitch Movement, Velocity, and Plate Locations during a throwing session.
* Stuff+ and xERA in real time.
* Stuff+ Rolling Averages Per Pitch Type.
* Group by Selected Pitches and/or Pitch Types for Average Movement, Velocity, and Pitch Quality metrics

## Primer

All R code is located within ```trackman_live_dash/app.R```, this includes sections for the UI and the server-side of Shiny. 

```trackman_live_dash/www``` contains all necessary style sheets and fonts.

Most Pitch and Stuff Models are also housed within the ```trackman_live_dash/www``` with the exception of:

* mov_x_dif_model.rds
* mov_z_dif_model.rds
* fastball_R_xgb1.rds

## Setup

### R

Note that this example was written when the current version of R was 4.0.4.

If you have R, but it is not the current version, run the following commands in R.exe

```r
install.packages("installr"); library(installr)

updateR()
```

You'll need to have the following packages installed (which you can load in with the following): 

```r
install.packages(c("shiny", "shinydashboard", "shinyjs", "tablerDash", "shinyWidgets", "shinyEffects", "pushbar", "waiter", "ggplot2", "sysfonts", "showtext", "DT", "reactable", "htmltools", "httr", "RMySQL", "zoo", "dplyr", "readr", "tidyr", "tidyverse", "tidymodels", "workflows", "dials", "tune", "xgboost"))
```
### SQL Database Table

The ```trackman_live_dash/app.R``` contains functions that write new values (Stuff+, xERA) and select inputs (Pitch Type, Batter Handedness) into a table.

```trackman_live_dash/tm_live_dash_inputs_sql_table_set_up.xlsx``` is the Excel sheet that details the structure of the SQL table that contains the output from the Shiny app.

```trackman_live_dash/tm_live_dash_inputs_sql_table_set_up.py``` is the python script that creates the SQL table that the app writes to using the Excel sheet mentioned above.
