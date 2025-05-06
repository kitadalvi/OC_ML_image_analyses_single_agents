library(dplyr)
library(tidyr)
library(stringr)

file_dir <- getwd()

file_dir

# List all files in the directory
Susi.meta <- 
  read.csv(file.path(file_dir, "data", "OC_ORG66BR2_annotation_Dox.csv")) %>% 
  select(3,5,6)

OC.meta <- 
  read.csv(file.path(file_dir, "data", "Well annotations Endpoint single agent.csv")) %>% 
  separate(Annotation, into = c("Concentration", "Compound_ID"), sep = " ", extra = "merge") %>%
  mutate(
    Compound_ID = gsub("_Endpt_R1_BF", "", Compound_ID),
    Concentration = gsub("uM", "", Concentration)
  ) %>% 
  mutate(Concentration = as.numeric(Concentration))%>%
  mutate(Compound_ID = str_extract(Compound_ID, "^[^_]+"))%>%
  mutate(Compound_ID = str_replace(Compound_ID, "_Endpt_R2_BF", ""))



# Assuming susi.meta and OC.meta are data frames
overlap_rows <- intersect(Susi.meta, OC.meta)

# Check how many rows overlap
n_overlap <- nrow(overlap_rows)
print(n_overlap)

# View the overlapping rows
print(overlap_rows)



Susi.meta.org <- 
  read.csv(file.path(file_dir, "data", "OC_ORG66BR2_annotation_Dox.csv"))



hanger <- 
  Susi.meta.org %>% 
  select(3,4,7) %>% 
  right_join(OC.meta) %>% 
  mutate(Patient = "Org73",
         Description = case_when(Compound_ID %in% c("Media", "DMSO", "TritonX") ~ "negative_control",
                                 Compound_ID %in% c("Staurosporine", "Doxorubicin", "Mitomycin C") ~ "positive_control",
                                 TRUE ~ "Sample"))

# add CTG
library(tibble)
library(readxl)
files <- file.path(file_dir, "data", "ORG73_CTG.xlsx")


##Excell reading might have to change if the folder has only the raw CTG reading
##Cytation instrument data requires me to skip X lines

Raw.CTG <- lapply(files, function(x){
  
  read_excel(path = files, 
             skip = 29) %>% #the skip argument changes depending on each batch
    filter(across(last_col(), ~ . == "Lum")) %>%    #use this line to control which read do we want
    column_to_rownames(1) %>%
    select_if(is.numeric) %>%
    rownames_to_column("Row") %>%
    pivot_longer(c(everything(), -Row),
                 names_to = "Col",
                 values_to = "CTG") %>%
    mutate(Col= as.numeric(Col)) 
})


names(Raw.CTG) <- basename(files)

#here you want to insert the plate number/ID via excel names
colnames(CTG.dt)

CTG.dt <- 
  data.table::rbindlist(Raw.CTG, idcol=TRUE) %>% 
  drop_na(CTG) %>% 
  mutate(Well_ID = paste0(Row, Col)) %>% 
  select(Well_ID, Row, Col,CTG) %>%
  left_join(hanger) %>%
  mutate(Concentration = replace_na(Concentration, 0))%>%
  mutate(Units = replace_na(Units, "uM"))


write.csv(CTG.dt,file.path(file_dir, "data", "OC_ORG73_annotatedCTG.csv"), row.names = FALSE)
