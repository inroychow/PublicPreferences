
#--- List of functions we can call from other scripts

library(magick)
library(tinytex)

# Just so it's easier to read tex results as an image
tex_table_to_png <- function(tabular_tex_path, output_prefix, dpi = 300) {
  # Read LaTeX table (assumed to contain tabular only)
  tabular_code <- readLines(tabular_tex_path)
  
  # Wrap in a minimal standalone LaTeX document
  full_tex <- c(
    "\\documentclass{standalone}",
    "\\usepackage{booktabs}",
    "\\begin{document}",
    tabular_code,
    "\\end{document}"
  )
  
  # Save wrapped .tex file
  tex_file <- paste0(output_prefix, "_temp.tex")
  writeLines(full_tex, tex_file)
  
  # Compile to PDF using tinytex (no 'quiet' argument!)
  tinytex::latexmk(tex_file, clean = TRUE, engine = "pdflatex")
  
  # Convert to PNG using magick
  pdf_file <- sub("\\.tex$", ".pdf", tex_file)
  png_file <- sub("\\.tex$", ".png", tex_file)
  
  img <- magick::image_read_pdf(pdf_file, density = dpi)
  magick::image_write(img, png_file)
  
  message("PNG saved to: ", png_file)
}




#--- function to add spacers in ggplot
add_spacers_between_groups <- function(df, group_var, sort_var = NULL, spacer_label = "", spacer_prefix = "---spacer") {
  group_var <- rlang::ensym(group_var)
  if (!is.null(sort_var)) sort_var <- rlang::ensym(sort_var)
  
  # Prepare empty tibble for output
  df_out <- tibble()
  group_list <- df %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::arrange(if (!is.null(sort_var)) !!sort_var else dplyr::row_number(), .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    split(.[[rlang::as_string(group_var)]])
  
  spacer_id <- 1
  for (group_df in group_list) {
    df_out <- dplyr::bind_rows(df_out, group_df)
    
    # Add unique spacer row
    spacer_row <- tibble::as_tibble(matrix(NA, nrow = 1, ncol = ncol(df)))
    colnames(spacer_row) <- colnames(df)
    spacer_row$term_clean <- paste0(spacer_prefix, spacer_id)
    spacer_id <- spacer_id + 1
    
    df_out <- dplyr::bind_rows(df_out, spacer_row)
  }
  
  # Drop the final spacer row (at bottom of plot)
  df_out <- df_out[1:(nrow(df_out)-1), ]
  
  # Set y-axis order (reversed)
  df_out$term_clean <- factor(df_out$term_clean, levels = rev(df_out$term_clean))
  return(df_out)
}

