##downloads renthub data from dewey

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# devtools::install_github("Dewey-Data/deweydatar")

my_api_key <- Sys.getenv("DEWEY_API_KEY")

my_product_path <- "https://api.deweydata.io/api/v1/external/data/cdst_uytkqpdft8tbhbhc"

file_list <- get_file_list(
  apikey = my_api_key,
  product_path = my_product_path
)

download_files(
  files_df = file_list,
  dest_folder = "../output/"  # Note: Argument name is 'dest_folder', not 'output_dir'
)
