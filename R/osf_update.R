
library(osfr)

# Load project and components
dog_spatial_choice_project <- osf_retrieve_node("https://osf.io/eb5m3/")
dog_spatial_choice_data <- osf_retrieve_node("https://osf.io/yqkp5/")
dog_spatial_choice_figures <- osf_retrieve_node("https://osf.io/tmpyg/")
dog_spatial_choice_preprint <- osf_retrieve_node("https://osf.io/d5gfk/")

# Upload data
data_files <- osf_upload(dog_spatial_choice_data, path = c("stevens_etal_2022_data1.csv", "stevens_etal_2022_data2.csv"), conflicts = "overwrite")

# Upload R script and README
readme <- osf_upload(dog_spatial_choice_data, path = "README.md", conflicts = "overwrite")
rscript <- osf_upload(dog_spatial_choice_data, path = "stevens_etal_2022_rcode.R", conflicts = "overwrite")

# Upload figures
figures <- osf_upload(dog_spatial_choice_figures, path = list.files(path = "figures", pattern = "png", full.names = TRUE), conflicts = "overwrite")
osf_upload(dog_spatial_choice_figures, path = "stevens_etal_2022.mp4", conflicts = "overwrite")

# Upload preprint
preprint <- osf_upload(dog_spatial_choice_preprint, path = c("stevens_etal_2022.Rmd", "stevens_etal_2022.pdf", "stevens_etal_2022_SM.Rmd", "stevens_etal_2022_SM.pdf"), conflicts = "overwrite")
