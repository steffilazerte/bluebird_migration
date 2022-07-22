#+ echo = FALSE
# Setup -------------------------------------------------------------------

# Create folders for output

dir.create("Data/Intermediate")
dir.create("Data/Datasets")
dir.create("Results")
dir.create("Results/Figures")


# Install specific package versions:

# dggridR was archived on CRAN, can either use specific version, or GitHub version
#devtools::install_version("dggridR", version = "2.0.4", repos = "http://cran.us.r-project.org")

# SDMTools was archived on CRAN
#devtools::install_version("SDMTools", version = "1.1-221", repos = "http://cran.us.r-project.org")

# We require a specific version of segmented to reproduce results exactly
#devtools::install_version("segmented", version = "1.2-0", repos = "http://cran.us.r-project.org")
