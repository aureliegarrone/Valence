##AVERAGING RATINGS FOR REPEATEDLY PRESENTED WORDS - WORD COUNT NOT INCLUDING REPEAT WORDS

valmeans_avg <- valmeans_all_groups %>%
  group_by(word_name) %>%
  summarise(
    mean_val = mean(mean, na.rm = TRUE)
  )

emomeans_avg <- emomeans_all_groups %>%
  group_by(word_name) %>%
  summarise(
    mean_emo = mean(mean_emo, na.rm = TRUE)
  )

##VALENCE HISTOGRAM

ggplot(valence_polarity_norms, aes(x = mean_valence)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  scale_x_continuous(breaks = seq(0,100, by = 10)) +
  scale_y_continuous(breaks = seq(0,650, by = 100)) +
  labs(
    title = "",
    x = "Emotional valence ratings",
    y = "Number of words"
  ) +
  theme_classic()

##EMOTIONALITY HISTOGRAM

ggplot(valence_polarity_norms, aes(x = mean_polarity)) +
  geom_histogram(binwidth = 5, fill = "#CC99FF", color = "black") +
  scale_x_continuous(breaks = seq(0,100, by = 10)) +
  scale_y_continuous(breaks = seq(0,1000, by = 100)) +
  labs(
    title = "",
    x = "Emotional polarity ratings",
    y = "Number of words"
  ) +
  theme_classic()

##CORRELATION MATRIX

custom_ggcorrplot <- function (
    corr,
    method = c("square", "circle"),
    type = c("full", "lower", "upper"),
    ggtheme = ggplot2::theme_minimal,
    title = "",
    show.legend = TRUE,
    legend.title = "Corr",
    show.diag = NULL,
    colors = c("blue", "white", "red"),
    outline.color = "gray",
    hc.order = FALSE,
    hc.method = "complete",
    lab = FALSE,
    lab_col = "black",
    lab_size = 4,
    # new label controls
    lab_nudge = c(0, 0),  # x, y offsets for numeric labels
    lab_alpha = 1,        # label opacity (top layer)
    lab_fmt   = NULL,     # e.g., "%.2f" for sprintf-style control
    p.mat = NULL,
    sig.level = 0.05,
    insig = c("pch", "blank"),
    tl.cex = 12,
    tl.col = "black",
    tl.srt = 45,
    digits = 2,
    as.is = FALSE,
    # star controls (default: up to three stars)
    star_levels = c(0.05, 0.01, 0.001),
    star_labels = c("*", "**", "***"),
    star_col = "black",
    star_size = 5,
    star_nudge = c(0.40, 0.20)  # x, y offset for star placement
) {
  
  # ---------- internal helpers ----------
  .remove_diag <- function(m) { diag(m) <- NA; m }
  .get_lower_tri <- function(m, show.diag = TRUE) {
    m[upper.tri(m)] <- NA; if (!show.diag) diag(m) <- NA; m
  }
  .get_upper_tri <- function(m, show.diag = TRUE) {
    m[lower.tri(m)] <- NA; if (!show.diag) diag(m) <- NA; m
  }
  .hc_cormat_order <- function(corr, hc.method = "complete") {
    d <- stats::as.dist(1 - abs(corr)); stats::hclust(d, method = hc.method)$order
  }
  .tibble_to_matrix <- function(df) {
    df <- as.data.frame(df)
    rn <- df[[1]]
    m  <- as.matrix(df[, -1, drop = FALSE])
    rownames(m) <- rn; colnames(m) <- colnames(df)[-1]; m
  }
  # corrected: keeps level/label mapping stable, smallest thresholds win
  .stars_from_p <- function(p, levels, labels) {
    stopifnot(length(levels) == length(labels))
    o <- order(levels)          # ascending (e.g., 0.001, 0.01, 0.05)
    levels <- levels[o]
    labels <- labels[o]
    out <- rep("", length(p))
    for (i in seq_along(levels)) {
      hit <- !is.na(p) & p <= levels[i] & out == ""
      out[hit] <- labels[i]
    }
    out
  }
  # --------------------------------------
  
  type   <- match.arg(type)
  method <- match.arg(method)
  insig  <- match.arg(insig)
  
  if (is.null(show.diag)) show.diag <- (type == "full")
  
  # Accept rstatix-like cor_mat
  if (inherits(corr, "cor_mat")) {
    cor.mat <- corr
    corr    <- .tibble_to_matrix(cor.mat)
    p.mat   <- .tibble_to_matrix(attr(cor.mat, "pvalue"))
  }
  
  if (!is.matrix(corr) && !is.data.frame(corr)) stop("Need a matrix or data frame for 'corr'.")
  corr <- as.matrix(corr)
  corr <- base::round(corr, digits = digits)
  
  # Reorder if requested
  if (hc.order) {
    ord  <- .hc_cormat_order(corr, hc.method = hc.method)
    corr <- corr[ord, ord, drop = FALSE]
    if (!is.null(p.mat)) p.mat <- as.matrix(p.mat)[ord, ord, drop = FALSE]
  }
  
  # Diagonal handling
  if (!show.diag) {
    corr <- .remove_diag(corr)
    if (!is.null(p.mat)) p.mat <- .remove_diag(as.matrix(p.mat))
  }
  
  # Upper/lower selection
  if (type == "lower") {
    corr <- .get_lower_tri(corr, show.diag)
    if (!is.null(p.mat)) p.mat <- .get_lower_tri(as.matrix(p.mat), show.diag)
  } else if (type == "upper") {
    corr <- .get_upper_tri(corr, show.diag)
    if (!is.null(p.mat)) p.mat <- .get_upper_tri(as.matrix(p.mat), show.diag)
  }
  
  # Melt correlation -> rename value column to avoid future name clashes
  corr_df <- reshape2::melt(corr, na.rm = TRUE, as.is = as.is)
  colnames(corr_df) <- c("Var1", "Var2", "corr_value")
  corr_df <- as.data.frame(corr_df, check.names = TRUE)
  corr_df$Var1 <- as.character(corr_df$Var1)
  corr_df$Var2 <- as.character(corr_df$Var2)
  corr_df$..rowid <- seq_len(nrow(corr_df))  # preserve order
  
  # Attach p-values safely and build stars
  if (!is.null(p.mat)) {
    p_all <- reshape2::melt(as.matrix(p.mat), na.rm = TRUE)
    colnames(p_all) <- c("Var1", "Var2", "p_value")
    p_all$Var1 <- as.character(p_all$Var1)
    p_all$Var2 <- as.character(p_all$Var2)
    
    df <- merge(corr_df, p_all, by = c("Var1", "Var2"), all.x = TRUE, sort = FALSE)
    df <- df[order(df$..rowid), , drop = FALSE]
    df$..rowid <- NULL
    
    # hide non-significant cells if requested
    if (insig == "blank") {
      df$corr_value[is.na(df$p_value) | df$p_value > sig.level] <- NA
    }
    
    # stars (never on diagonal)
    df$stars <- .stars_from_p(df$p_value, star_levels, star_labels)
    df$stars[is.na(df$p_value)] <- ""
    df$stars[df$Var1 == df$Var2] <- ""
    
  } else {
    df <- corr_df
    df$p_value <- NA_real_
    df$stars <- ""
  }
  
  # keep matrix layout order
  df$Var1 <- factor(df$Var1, levels = rownames(corr))
  df$Var2 <- factor(df$Var2, levels = colnames(corr))
  
  # Size aesthetic for circles
  df$abs_corr <- abs(df$corr_value) * 10
  
  # Base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Var1, y = Var2, fill = corr_value))
  if (method == "square") {
    p <- p + ggplot2::geom_tile(color = outline.color)
  } else {
    p <- p + ggplot2::geom_point(
      shape = 21, color = outline.color,
      ggplot2::aes(size = abs_corr)
    ) +
      ggplot2::scale_size(range = c(4, 10)) +
      ggplot2::guides(size = "none")
  }
  
  p <- p + ggplot2::scale_fill_gradient2(
    low = colors[1], mid = colors[2], high = colors[3],
    midpoint = 0, limit = c(-1, 1), space = "Lab", name = legend.title
  )
  
  # Theme & layout (extra room, avoid clipping)
  if (inherits(ggtheme, "function")) p <- p + ggtheme() else p <- p + ggtheme
  p <- p +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = tl.srt, vjust = 1, hjust = 1, size = tl.cex, colour = tl.col,
        margin = ggplot2::margin(t = 6)
      ),
      axis.text.y = ggplot2::element_text(
        size = tl.cex, colour = tl.col,
        margin = ggplot2::margin(r = 6)
      ),
      axis.ticks.length = grid::unit(2, "pt"),
      plot.margin = ggplot2::margin(6, 10, 6, 6)
    )
  

  
  # numeric labels with halo + optional formatting and nudging
  if (lab) {
    lab_vals <- if (is.null(digits)) df$corr_value else round(df$corr_value, digits)
    lab_text <- if (!is.null(lab_fmt)) sprintf(lab_fmt, lab_vals) else lab_vals
    
    # actual label on top
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = lab_text),
      color = lab_col, size = lab_size, alpha = lab_alpha,
      position = ggplot2::position_nudge(x = lab_nudge[1], y = lab_nudge[2]),
      na.rm = TRUE
    )
  }
  
  # stars (only if insig == "pch")
  if (insig == "pch" && any(nzchar(df$stars))) {
    p <- p + ggplot2::geom_text(
      data = df[nzchar(df$stars), , drop = FALSE],
      ggplot2::aes(label = stars),
      size = star_size, color = star_col,
      position = ggplot2::position_nudge(x = star_nudge[1], y = star_nudge[2])
    )
  }
  
  if (title != "") p <- p + ggplot2::ggtitle(title)
  if (!show.legend) p <- p + ggplot2::theme(legend.position = "none")
  
  p
}


cor_matrix <- All_variables_val_pol %>%
  dplyr::select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

cor_matrix <- as.data.frame((cor_matrix))

p_matrix <- All_variables_val_pol %>%
  dplyr::select(where(is.numeric)) %>%
  cor.mtest(use = "pairwise.complete.obs")

desired_order <- c("Reaction Time", "Frequency", "Length","Imageability", "Emotional Valence", "Emotional Polarity")
cor_matrix <- cor_matrix[desired_order, desired_order]
p_matrix$p <- p_matrix$p[desired_order, desired_order]


library(ggplot2)
library(reshape2)

matrix <- custom_ggcorrplot(
  corr      = cor_matrix,
  p.mat     = p_matrix$p,
  lab       = TRUE,
  lab_size  = 6,
  sig.level = 0.05,
  show.diag = TRUE,                   # keep diagonal numbers (1s)
  insig     = "pch",                  # draw *, **, ***
  star_levels = c(0.05, 0.01, 0.001),
  star_labels = c("*","**","***"),
  colors = c("lightgreen","white","purple")
) +
  scale_fill_gradient2(midpoint = 0, low = "lightgreen", mid = "white",
                       high = "purple", limits = c(-1, 1)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

matrix
