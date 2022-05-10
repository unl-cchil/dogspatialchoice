library(readr)
library(stringr)
library(pdftools)

# Copy files from project directory
file.copy("~/active_sync/projects/dog_spatial_choice/data/stevens_etal_2022_data1.csv", "stevens_etal_2022_data1.csv", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/data/stevens_etal_2022_data2.csv", "stevens_etal_2022_data2.csv", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/R/stevens_etal_2022_rcode.R", "R/stevens_etal_2022_rcode.R", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/docs/stevens_etal_2022.Rmd", "docs/stevens_etal.Rmd", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/figures/setup_study.png", "figures/setup_study.png", overwrite = TRUE)
# file.copy("~/active_sync/projects/dog_spatial_choice/figures/setup_study1.png", "figures/setup_study1.png", overwrite = TRUE)
# file.copy("~/active_sync/projects/dog_spatial_choice/figures/setup_study2.png", "figures/setup_study2.png", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/media/videos/dog_spatial_choice.mp4", "stevens_etal_2022.mp4", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/docs/dog_spatial_choice.bib", "stevens_etal_2022.bib", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/docs/r-references.bib", "r-references.bib", overwrite = TRUE)
file.copy("~/active_sync/projects/dog_spatial_choice/docs/apa7_chron.csl", "stevens_etal_2022.csl", overwrite = TRUE)

# Edit R script
r_code <- read_file("R/stevens_etal_2022_rcode.R") %>%
  # str_replace("stevens_etal_", "stevens_etal_2021_") %>%
  str_replace("library\\(here\\)", "") %>% 
  str_replace('read_csv\\(here\\("data/stevens_etal_2022_data1.csv"\\)\\)', 'read_csv\\("stevens_etal_2022_data1.csv"\\)') %>% 
  str_replace('read_csv\\(here\\("data/stevens_etal_2022_data2.csv"\\)\\)', 'read_csv\\("stevens_etal_2022_data2.csv"\\)') %>% 
  str_replace_all("\\(here\\(", "\\(") %>% 
  str_replace_all('.png"\\),', '.png",') %>% 
  str_replace_all('.RData"\\)', '.RData"') %>% 
  str_replace_all('save.image', '# save.image') %>% 
  write_file("stevens_etal_2022_rcode.R")
source("stevens_etal_2022_rcode.R")

# Edit manuscript
manuscript <- read_file("docs/stevens_etal.Rmd") %>%
  str_replace_all("dog_spatial_choice.bib", "stevens_etal_2022.bib") %>%
  str_replace_all("apa7_chron.csl", "stevens_etal_2022.csl") %>%
  str_replace('# source\\(here\\("R/stevens_etal_2022_rcode.R"\\)\\)', 'source\\("stevens_etal_2022_rcode.R"\\)') %>% 
  str_replace('load\\(here\\("dog_spatial_workspace.RData"\\)\\)', '# load\\(here\\("dog_spatial_workspace.RData"\\)\\)') %>% 
  str_replace_all(" Figure used with permission under a CC-BY4.0 license: Stevens, et al., 2022; available at https://doi.org/10.31234/osf.io/hyvdq.", "") %>%
  str_replace_all("Table used with permission under a CC-BY4.0 license: Stevens, et al., 2022; available at https://doi.org/10.31234/osf.io/hyvdq.", "") %>%
  str_replace_all("linenumbers       : yes", "linenumbers       : no") %>%
  str_replace("authornote: \\|", paste("authornote: \\|\n\  **Note: This is a pre-print and has not been peer reviewed.**\n\n\  PsyArXiv: https://doi.org/10.31234/osf.io/hyvdq\n\n\  Version:", Sys.Date(), "\n")) %>% 
  str_replace_all(": \"man\"", ": \"pub\"") %>%
  str_replace_all('fig.align = "center"', 'fig.align = "center", fig.env = \"figure*\"') %>%
  # str_replace_all('out.width = "33\\%"', 'out.width = "30\\%"') %>% 
  str_replace_all("\\\\newpage", "") %>% 
  # str_replace_all("\\\\clearpage", "") %>% 
  str_replace("# References", "# References\n\\\\scriptsize") %>% 
  write_file("stevens_etal_2022.Rmd")
# rmdfile <- read_lines("stevens_etal_2022.Rmd")
# sm_line <- which(rmdfile == "# Supplementary Materials") - 1
# max_line <- length(rmdfile)
# rmd_file <- rmdfile[-(sm_line:max_line)]
# write_lines(rmd_file, "stevens_etal_2022.Rmd")
rmarkdown::render("stevens_etal_2022.Rmd")
file.copy("stevens_etal_2022.pdf", "stevens_etal_2022_all.pdf", overwrite = TRUE)
pdf_subset("stevens_etal_2022_all.pdf", pages = 1:14, output = "stevens_etal_2022.pdf")

# Edit supplementary materials
rmdfile <- read_lines("stevens_etal_2022.Rmd")
sm_line <- which(rmdfile == "# Supplementary Materials") - 1
max_line <- length(rmdfile)
rmd_file <- rmdfile[c(1:24, 68:92, sm_line:max_line)]
rmd_file <- str_replace(rmd_file, ": \"pub\"", ": \"doc\"") %>% 
  str_replace("\\\\singlespacing", "") %>% 
  str_replace('kableExtra::footnote\\(general = "", threeparttable = TRUE\\)', "") %>% 
  str_replace('pack_rows\\("Household income", 11, 16\\) %>%', 'pack_rows\\("Household income", 11, 16\\)')
write_lines(rmd_file, "stevens_etal_2022_SM.Rmd")
rmarkdown::render("stevens_etal_2022_SM.Rmd")

+# Create zip file
zip(zipfile = "stevens_etal_2022_rr", files = c("r-references.bib", "README.md", "stevens_etal_2022_data1.csv", "stevens_etal_2022_data2.csv", "stevens_etal_2022_rcode.R", "stevens_etal_2022_SM.pdf", "stevens_etal_2022.bib", "stevens_etal_2022.csl", "stevens_etal_2022.pdf", "stevens_etal_2022.Rmd"))
