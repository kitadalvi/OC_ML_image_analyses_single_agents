library(dplyr)
library(pheatmap)

### Load raw data for ORG70BR2
df <- read_csv('output/ORG70BR1/all_data_with_bins.csv')
df <- as.data.frame(df)

### Load msot important features
features_cystic <- c("Texture_AngularSecondMoment_CropDAPI_3_03_256",
              "Intensity_IntegratedIntensityEdge_CropDAPI",
              "Texture_InverseDifferenceMoment_CropDAPI_3_03_256",
              "Texture_DifferenceVariance_CropDAPI_3_03_256",
              "Intensity_IntegratedIntensity_CropDAPI",
              "RadialDistribution_MeanFrac_CropBF_2of4",
              "Texture_InfoMeas2_CropDAPI_3_02_256",
              "Intensity_StdIntensityEdge_CropDAPI",
              "AreaShape_Area",
              "Texture_Entropy_CropBF_3_01_256",
              "Intensity_IntegratedIntensity_CropBF",
              "Neighbors_SecondClosestObjectNumber_Expanded",
              "RadialDistribution_FracAtD_CropBF_2of4",
              "RadialDistribution_MeanFrac_CropBF_1of4",
              "Intensity_MADIntensity_CropDAPI")
metadata_cols <- c('Bin','Plate_ID','ObjectNumber','WELL_ID','Concentration','Compound_ID','CTG')

metadata <- df[,metadata_cols]
feature_data <- df[,features_cystic]
df <- cbind(metadata,feature_data)

### calculate the sd and mean of controls
control_df <- df[df$Compound_ID == "DMSO" |df$Compound_ID=="Media", ]

### compute control and mean for each feature
control_mean <- apply(control_df[, features_cystic], 2, mean, na.rm = TRUE)
control_sd   <- apply(control_df[, features_cystic], 2, sd, na.rm = TRUE)

### Compute Z-scores
df_z <- df
df_z[, features_cystic] <- scale(df[, features_cystic],
                              center = control_mean,
                              scale  = control_sd)

### Calualte mean zscore per drug
drug_feature_matrix <- df_z %>%
  mutate(condition = paste(Compound_ID, Concentration, sep = '_')) %>%
  group_by(condition) %>%
  summarize(across(all_of(features_cystic), mean, na.rm = TRUE))

df <- as.data.frame(drug_feature_matrix)
rownames(df) <- paste(drug_feature_matrix$condition)

rows_of_interest <- c("5-Fluouracil_0.1",
                      "Carboplatin_0.1",
                      "Cisplatin_0.1",
                      "Docetaxel_0.1",
                      "Doxorubicin_0.1",
                      "Gemcitabine_0.1",
                      "Irinotecan_0.1",
                      "Leucovorin_0.1",
                      "Mitomycin C_0.1",
                      "Oxaliplatin_0.1",
                      "Paclitaxel_0.1",
                      "Staurosporine_0.1",
                      "Topotecan_0.1")

df <- df[rows_of_interest,features_cystic[1:5]]



### create heatmap of z scores
pheatmap(df,
         scale = "none",    # already Z-scored
         color = hcl.colors(50, "BluYl"),
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "complete",
         main = "Z-score Heatmap of 5 most important features \nby Drug (ORG70BR2)")
