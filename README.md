# Rapid shifts in migration routes and breeding latitude in North American bluebirds

*Submitted to Ecosphere 2021*

Jared Sonnleitner^1, [Steffi LaZerte](https://steffilazerte.ca)^2, Ann E. McKellar^3, Nancy J. Flood^1, and Matthew W. Reudink^1*

^* Corresponding Author  
^1 Department of Biological Sciences, Thompson Rivers University, Kamloops, BC V2C 0C8, Canada  
^2 Department of Biology, Brandon University, Brandon, MB R7A 6A9, Canada  
^3 Wildlife Research Division, Environment and Climate Change Canada, Saskatoon, SK S7N 0X4, Canada  


# Contents

This repository contains the code used to summarize, transform, and analyze 
eBird data for this manuscript. 

We are extremely grateful to Dr. Sarah Supp and colleagues for sharing their [well-annotated code](https://github.com/sarahsupp/hb-migration) from their publication [Supp et al. (2015)](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/ES15-00239.1).


## Format

The scripts are stored int the `Scripts` folder and can be run in sequence
starting with the `01_setup.R`. Note that scripts are designed to produce formatted
RMarkdown reports, so can be run interactively (by hand) or with the `rmarkdown::render()` 
code contained at the top of each script. If running this code, an html report
of the script will be produced in the `Results` folder, any data produced will
be saved to a `Data` folder. 

Because scripts were developed off GitHub and separately by different team members, 
we have not included reproducible controls for packages and package versions
(like [`renv`](https://rstudio.github.io/renv) for example). 
Please see the manuscript for package versions used, and the header of each script
for packages required. Apologies for the inconvenience! 
Please contact @steffilazerte if you run into any problems.



### `Data` folder

Subfolders contain the following:

- `Intermediate` - Intermediate data files created in the process of summarizing
ebird data
- `Datasets` - Full datasets created by the scripts. Although not in the
intermediate folder, some of these datasets are intermediate in nature. For
example, the `birds_01_hex_all.rds` file contains all observations separately
linked to hex. But we will only every used the data which is summarized by hex. 
However, this data may be useful for other things so it's stored here.

> Note that the original, eBird data is not stored here, but can be obtained from
> eBird directly. See details in `Scripts/03_initial_data_ebird.R`.

## `Results` folder

Compiled scripts will be saved to the `Results` folder and dated.

Figures are usually embedded in the Report, but final manuscript figures may be
saved to the `Results/Figures` folder.

## `Scripts` folder

All the scripts used to summarize the data and conduct the analyses are
contained in the **Scripts** folder. The scripts are numbered in the order that
they are meant to be compiled. For example, `04_ebird_samples.R` creates figures
exploring the data samples and relies on data created by
`03_initial_data_ebird.R`.

### `01_setup.R`
This script makes sure the folder structure is set up and that you have all the packages you need to run the analysis.

### `02_initial_data_hex.R`
This script creates the hex grid we use for summarizing the eBird data and
prepares a map of North and South America for plotting.

**Output:**

- Spatial hex grids (`hex_res6.rds`, `hex_res7.rds`)
- Map of the Americas (`americas.rds`)

### `03_initial_data_ebird.R`
This script takes the eBird data on checklists and sampling and filters it, and
summarizes it by hex. The eBird data needs to be downloaded from [eBird](https://ebird.org) and is in
the format of, for example: `ebd_chiswi_200501_201901_relNov-2019.txt`

**Output:**

- The complete data file with all observations aligned by hex (`birds_01_hex_all.rds`)
- The summarized data file with summed checklists by hex by species (`birds_02_hex_sum.rds`)

### `04_ebird_samples.R`
This script explores the data samples. There is no output for this script (just figures in the Report)

### `05_gam.R`
This script calculates mean population locations based on hex locations weighted by the proportion of birds observed. 
It then calculates a GAM model of the population migration routes and predicts daily locations. 

**Output:**

- For each day in each year for each species, average population location, and predicted location from the GAM model (`birds_03_mean.csv`)

### `06_migration_dates.R`
This script uses the predicted GAM locations to calculate dates of spring and fall migrations.
BOTH the start and end of spring and the start and end of fall are calculated 
A threshold method is used to find an approximate date, and then a segmented 
regression method is used to find a breakpoint (i.e. the final date).

**Output:**

- Dates and seasonal categories are added ot the average/predicted population locations 
  (`birds_04_migration.csv`)
- A simple data set of migration dates for each species in each year 
  (`migration_details.csv`)
    - Start and End dates of spring migration (`spring_begin_yday`, `spring_end_yday`)
    - Start and End dates of fall migration (`fall_begin_yday`, `fall_end_yday`)
    - Date the first time the maximum latitude was reached (`max_lat_yday`)
    - Maximum latitude (`max_lat`) reached

### `07_migration_dist_speed.R`
This script calculates migration distances and daily speeds for each seasonal category. 

**Output:**

- Speeds for each seasonal category are added to the migration details file
  (`migration_details`)
  - Median of the top 5 Daily speeds (km/day) for each spring, breeding and fall
    (`speed_spring`, `speed_breeding`, `speed_fall`)

### `08_migration_routes.R`
This script looks at migration routes and calculates longitudinal variation

**Output:**

- Different measures of seasonal longitude are added to the migration details file
  (`migration_details`)
  - Min, Mean, Median, Mid-point, and Max longitude for non-breeding, spring, 
  breeding, and fall 
   (`non-breeding_min_lon`, `non-breeding_mean_lon`, `non-breeding_max_lon`, etc.)

### `09_figures.R`
- Large Figure 1 combo

### `10_stats.R`
- Statistical analyses of blue bird data
