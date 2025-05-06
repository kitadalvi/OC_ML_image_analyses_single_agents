

metadata_heatmap <- function(metadata_file, legend = "show", output_file = NULL) {

  metadata <- metadata_file


  if (!"Well_ID" %in% colnames(metadata)) {
    stop("The metadata is missing a 'Well_ID' column.")
  }

  valid_well_id <- grepl("^[A-P](0[1-9]|1[0-9]|2[0-4])$", metadata$Well_ID)
  if (!all(valid_well_id)) {
    stop("The 'Well_ID' column contains invalid entries.")
  }

  metadata <- metadata %>%
    mutate(
      row = as.numeric(factor(str_extract(metadata$Well_ID, "^[A-P]"))),
      col = as.numeric(str_extract(metadata$Well_ID, "\\d+"))
    )

  if (!"Plate_ID" %in% colnames(metadata)) {
    stop("The metadata is missing a 'Plate_ID' column.")
  }

  annotation_cols <- setdiff(names(metadata), c("Well_ID", "row", "col", "Row", "Column", "Plate_ID"))
  if (length(annotation_cols) == 0) {
    stop("No annotation columns found to generate heatmaps. Please check your input data.")
  }

  hide_legends <- sapply(annotation_cols, function(col) {
    length(unique(metadata[[col]])) > 20
  })

  unique_plate_ids <- unique(metadata$Plate_ID)
  for (plate_id in unique_plate_ids) {
    plate_data <- metadata %>% filter(Plate_ID == plate_id)

    heatmaps <- lapply(seq_along(annotation_cols), function(i) {
      annotation <- annotation_cols[i]
      scale_func <- if (is.numeric(plate_data[[annotation]])) {
        scale_fill_viridis_c(option = "C", na.value = "grey80")
      } else {
        scale_fill_viridis_d(option = "C", na.value = "grey80")
      }

      legend_setting <- if (legend == "show" && !hide_legends[i]) "right" else "none"

      ggplot(plate_data, aes(x = col, y = row, fill = .data[[annotation]])) +
        geom_tile(color = "white") +
        scale_func +
        scale_y_reverse() +
        labs(title = annotation, x = "Column", y = "Row") +
        coord_fixed() +
        theme_minimal(base_size = 8) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
          axis.text.y = element_text(size = 5),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 5),
          legend.key.size = unit(0.4, "cm"),
          legend.position = legend_setting,
          panel.border = element_rect(color = "black", fill = NA),
          plot.background = element_rect(color = "black")
        )
    })

    if (length(heatmaps) == 0) {
      stop("No heatmaps were generated. Please check the input metadata and annotation columns.")
    }

    combined_plot <- wrap_plots(heatmaps, ncol = 3) +
      plot_annotation(
        title = paste("Metadata Heatmap Grid - Plate ID:", plate_id),
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 8, face = "bold")
        )
      )

    if (!is.null(output_file)) {
      dir_name <- dirname(output_file)
      if (!dir.exists(dir_name)) {
        stop("The directory for the output file does not exist: ", dir_name)
      }

      file_name <- normalizePath(paste0(
        sub(".(png|jpg|pdf)$", "", output_file),
        "_Plate_", plate_id, ".", tools::file_ext(output_file)
      ), mustWork = FALSE)

      message("Saving plot to: ", file_name)
      ggsave(file_name, combined_plot, width = 10, height = 6 * ceiling(length(heatmaps) / 3), units = "in")
      message("Saved plot for Plate ID: ", plate_id, " to ", file_name)
    } else {
      return(combined_plot)
    }
  }
}
