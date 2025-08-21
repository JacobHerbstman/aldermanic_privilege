rm(list = ls())

packages <- c(
  "DBI", "readr","fixest","haven","stringr", "ipumsr", "tidycensus","dplyr","sf", "furrr", "purrr", "nngeo", "data.table", 
  "tigris", "zoo", "patchwork", "glue", "writexl", "arrow", "duckdb", "ptaxsim", "ggplot2", "here", "tidyr", "sfarrow", "geoarrow", 
  "tibble", "rdrobust"
)

# Create a directory for  packages
dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)

i = 1
output <- NA
for (package in packages) {
  if (require(package, character.only = TRUE) == FALSE) {

    message(paste("Installing ", package, sep = ""))


        install.packages(package, lib = Sys.getenv("R_LIBS_USER"), repos = "http://cran.us.r-project.org")
    
 
    attempt <- 1

    # I am leaving the debugging code within this section, because this section will
    # only run if something goes wrong and causes a need for debugging.
    while(package %in% rownames(installed.packages()) == FALSE){
      if (attempt == 6){
      try(install.packages(package))

      writeLines(paste("\n"))
      writeLines(paste("Exceeded 6 attempts without success when attempting to install", package))
      break
      }
 
      attempt <- attempt + 1

      writeLines(paste("\n"))
      writeLines(paste("Installing ", package, sep = ""))
      writeLines(paste("attempt number: ", attempt))


      writeLines(paste("\n"))
      writeLines(paste("Standard Install Attempt"))

      tryCatch({
        install.packages(package, lib = Sys.getenv("R_LIBS_USER"), repos = "http://cran.us.r-project.org")
        }, error = function(e) {
        writeLines(paste("Error message: ", e$message)) 
        }, warning = function(w) {
        writeLines(paste("Warning message: ", w$message))
        }
      )

      writeLines(paste("\n"))
      writeLines(paste("Alternative Install Attempt"))
      tryCatch({
        install.packages(package, dependencies=TRUE, repos='http://cran.rstudio.com/')
        }, error = function(e) {
        writeLines(paste("Error message: ", e$message)) 
        }, warning = function(w) {
        writeLines(paste("Warning message: ", w$message))
        }
      )      

      }


  }


  version <- packageDescription(package, fields = "Version")
  output[i] <- paste(package, version, sep = " : ")
  print(output)

  i = i + 1
}

output <- paste(output, collapse = "\n")
output <- paste("Packages installed: ", output, sep = "\n")

# write.table(output, "../output/R_packages.txt",
#             col.names = FALSE, row.names = FALSE)