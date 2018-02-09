# Create a scale using the minimum and maximum values
min_max_scale <- function(x) {
  breaks <- c(ceiling(min(x)), floor(max(x)))
  breaks <- as.numeric(breaks)
  names(breaks) <- attr(breaks, "labels")

  breaks
}

# Add a ggplot-style circle of correlations plot for the variables on PC1 and PC2
# Code based on:
# http://gastonsanchez.com/blog/how-to/2012/06/17/PCA-in-R.html



# Create a ggplot2 graph of the PCA
ggplot_pca2 <- function(x, pc1=1, pc2=2, info) {
  # Make the axis labels
  prop_var <- lapply(x$eig[c(pc1, pc2), 2], function(x) sprintf("%.2f", x))

  labels <- paste(
    "PC", c(pc1, pc2),
    "\n(", prop_var, "%)", sep = ""
  )

  # TASK: Pull out the percentage of variation explained by each variable


  # Build the plot
  x$ind$coord %>%
    as.data.frame() %>%
    cbind_rownames("sample_id") %>%
    tbl_df() %>%
    inner_join(info) %>%
    distinct() %>%
    ggplot(aes_string(
      x = paste("Dim.", pc1, sep = ""),
      y = paste("Dim.", pc2, sep = "")
    )) +
    geom_hline(yintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    geom_vline(xintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    geom_point(aes(fill = group), shape = 21, color = "black", size = 3) +
    #         geom_point(aes(fill=group, shape = group), color="black", size = 3) +
    labs(x = labels[1], y = labels[2]) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line()
    )
}


# Create a ggplot2 graph of the circle of correlations
ggplot_coc <- function(x, pc1, pc2, labels=c(pc1, pc2)) {
  corcir <- circle(c(0, 0), npoints = 100)

  # create data frame with correlations between variables and PCs
  correlations <-
    x$var$cor %>%
    as.data.frame() %>%
    cbind_rownames("layer_region") %>%
    separate(layer_region, c("seg_layer", "region"), sep = "_", remove = FALSE) %>%
    mutate_(
      x1 = 0, y1 = 0,
      x2 = pc1, y2 = pc2
    ) %>%
    mutate(seg_layer = factor(seg_layer, levels = layer_order))

  correlations_mean <-
    correlations %>%
    group_by(seg_layer) %>%
    summarize(
      x1 = mean(x1), x2 = mean(x2),
      y1 = mean(y1), y2 = mean(y2)
    ) %>%
    ungroup()

  # Plot
  ggplot() +
    geom_hline(yintercept = 0, color = "gray65") +
    geom_vline(xintercept = 0, color = "gray65") +
    geom_path(
      data = corcir, aes(x = x, y = y),
      colour = "gray65"
    ) +
    geom_segment(
      data = correlations,
      aes(
        x = x1, y = y1,
        xend = x2, yend = y2,
        color = seg_layer
      ),
      alpha = 0.4
    ) +
    geom_text(
      data = correlations_mean,
      aes(
        x = x2, y = y2,
        label = seg_layer,
        color = seg_layer
      ),
      fontface = "bold"
    ) +
    # scale_color_brewer(palette = "Paired") +
    xlim(-1.1, 1.1) +
    ylim(-1.1, 1.1) +
    labs(x = labels[1], y = labels[2]) +
    # theme_bw() +
    coord_fixed()
}

# Create a ggplot2 graph of the PCA
ggplot_pca2_caret <- function(x, pc1=1, pc2=2, info) {
  # Make the axis labels
  prop_var <- lapply(x$eig[c(pc1, pc2), 2], function(x) sprintf("%.2f", x))

  labels <- paste(
    "PC", c(pc1, pc2),
    "\n(", prop_var, "%)", sep = ""
  )

  # TASK: Pull out the percentage of variation explained by each variable


  # Build the plot
  x$ind$coord %>%
    as.data.frame() %>%
    cbind_rownames("sample_id") %>%
    tbl_df() %>%
    inner_join(info) %>%
    distinct() %>%
    ggplot(aes_string(
      x = paste("Dim.", pc1, sep = ""),
      y = paste("Dim.", pc2, sep = "")
    )) +
    geom_hline(yintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    geom_vline(xintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    geom_point(aes(fill = group), shape = 21, color = "black", size = 3) +
    # geom_point(aes(fill=group, shape = group), color="black", size = 3) +
    labs(x = labels[1], y = labels[2]) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line()
    )
}

# Create a ggplot2 graph of the circle of correlations
ggplot_coc_caret <- function(x, pcs, pc1, pc2, labels=c(pc1, pc2)) {
  corcir <- circle(c(0, 0), npoints = 100)

  # create data frame with correlations between variables and PCs
  correlations <-
    cor(x, pcs) %>%
    as.data.frame() %>%
    cbind_rownames("layer_region") %>%
    tbl_df() %>%
    separate(layer_region, c("seg_layer", "region"), sep = "_", remove = FALSE) %>%
    mutate_(
      x1 = 0, y1 = 0,
      x2 = pc1, y2 = pc2
    ) %>%
    mutate(seg_layer = factor(seg_layer, levels = layer_order))

  correlations_mean <-
    correlations %>%
    group_by(seg_layer) %>%
    summarize(
      x1 = mean(x1), x2 = mean(x2),
      y1 = mean(y1), y2 = mean(y2)
    ) %>%
    ungroup()

  # Plot
  ggplot() +
    geom_hline(yintercept = 0, color = "gray65") +
    geom_vline(xintercept = 0, color = "gray65") +
    geom_path(
      data = corcir, aes(x = x, y = y),
      colour = "gray65"
    ) +
    geom_segment(
      data = correlations,
      aes(
        x = x1, y = y1,
        xend = x2, yend = y2,
        color = seg_layer
      ),
      alpha = 0.4
    ) +
    geom_text(
      data = correlations_mean,
      aes(
        x = x2, y = y2,
        label = seg_layer,
        color = seg_layer
      ),
      fontface = "bold"
    ) +
    # scale_color_brewer(palette = "Paired") +
    xlim(-1.1, 1.1) +
    ylim(-1.1, 1.1) +
    labs(x = labels[1], y = labels[2]) +
    # theme_bw() +
    coord_fixed()
}

# Create a ggplot2 graph of the PCA
ggplot_pca2_mod <- function(x, pc1=1, pc2=2, info) {
  # Make the axis labels
  prop_var <- lapply(x$eig[c(pc1, pc2), 2], function(x) sprintf("%.2f", x))

  labels <- paste(
    "PC", c(pc1, pc2),
    "\n(", prop_var, "%)", sep = ""
  )

  # TASK: Pull out the percentage of variation explained by each variable

  # Build the plot
  x$ind$coord %>%
    as.data.frame() %>%
    cbind_rownames("sample_id") %>%
    tbl_df() %>%
    inner_join(info) %>%
    distinct() %>%
    ggplot(aes_string(
      x = paste("Dim.", pc1, sep = ""),
      y = paste("Dim.", pc2, sep = "")
    )) +
    geom_hline(yintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    geom_vline(xintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    labs(x = labels[1], y = labels[2]) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line()
    )
}







# Create a ggplot2 graph of the PCA
ggplot_pca3_mod <- function(x, y, pc1=1, pc2=2, info) {
  # Make the axis labels
  prop_var <- lapply(y[c(pc1, pc2), 4] * 100, function(i) sprintf("%.2f", i))

  labels <- paste(
    "PC", c(pc1, pc2),
    "\n(", prop_var, "%)", sep = ""
  )

  # Build the plot
  x %>%
    tbl_df() %>%
    inner_join(info) %>%
    distinct() %>%
    ggplot(aes_string(
      x = paste("PC", pc1, sep = ""),
      y = paste("PC", pc2, sep = "")
    )) +
    geom_hline(yintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    geom_vline(xintercept = 0, color = "gray65", alpha = 0.5, size = 0.5) +
    labs(x = labels[1], y = labels[2]) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line()
    )
}
