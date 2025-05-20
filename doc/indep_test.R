## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(minerva)  # For Data
library(FORD)     # Our package
library(XICOR)    # For comparison
library(ggplot2)  # For visualization

## -----------------------------------------------------------------------------
# Load yeast gene expression data
yeast_genes_data <- as.data.frame(Spellman)
gene_names <- colnames(yeast_genes_data)[-1]
time_points <- yeast_genes_data$time
n <- length(time_points)

## -----------------------------------------------------------------------------
xi_vals <- numeric(ncol(yeast_genes_data) - 1)
xi_pvals <- numeric(ncol(yeast_genes_data) - 1)
ird_vals <- numeric(ncol(yeast_genes_data) - 1)
ird_pvals <- numeric(ncol(yeast_genes_data) - 1)

## -----------------------------------------------------------------------------
for (i in 1:(ncol(yeast_genes_data) - 1)) {
  y <- as.numeric(yeast_genes_data[, i + 1])

  # XICOR
  xi_pvals[i] <- xicor(x = time_points , y = y, pvalue = T)$pval
  
  # IRDC
  ird <- irdc_simple(Y = y, X = time_points)
  ird_vals[i] <- ird
  ird_pvals[i] <-  1 - pnorm(ird, mean = 2/n , sd = sqrt((pi^2 / 3 - 3)/n))
}

## -----------------------------------------------------------------------------
xi_fdr <- p.adjust(xi_pvals, method = "BH")
ird_fdr <- p.adjust(ird_pvals, method = "BH")

sig_xi <- gene_names[xi_fdr < 0.05]
sig_ird <- gene_names[ird_fdr < 0.05]
common_genes <- intersect(sig_xi, sig_ird)

cat("All genes:", length(gene_names) , "\n")
cat("XICOR significant genes:", length(sig_xi), "\n")
cat("Simple IRDC significant genes:", length(sig_ird), "\n")
cat("Overlap:", length(common_genes), "\n")
cat("ONLY XICOR significant genes:", length(setdiff(sig_xi, sig_ird)), "\n")
cat("ONLY Simple IRDC significant genes:", length(setdiff(sig_ird, sig_xi)), "\n")

## -----------------------------------------------------------------------------
irdc_detected_only <- setdiff(sig_ird, sig_xi)
irdc_only_fdr <- ird_fdr[match(irdc_detected_only, gene_names)]
top6_idx <- order(irdc_only_fdr)[1:6]
smallest_p_irdc_do <- irdc_detected_only[top6_idx]

irdc_do_genes <- yeast_genes_data[, which(gene_names %in% smallest_p_irdc_do) + 1]
irdc_do_genes <- cbind(time_points, irdc_do_genes)

## ----fig.height=4, fig.width=6, results='asis'--------------------------------
for (i in 1:6) {
  gene_to_plot <- colnames(irdc_do_genes)[i + 1]
  idx <- match(gene_to_plot, gene_names)

  p <- ggplot(irdc_do_genes, aes(x = time_points, y = .data[[gene_to_plot]])) +
    geom_point() +
    theme_bw() +
    labs(
      title = paste0("Only Detected by nu: xi FDR = ", round(xi_fdr[idx], 4),
                     ", nu FDR = ", round(ird_fdr[idx], 4)),
      x = "Time Points",
      y = gene_to_plot
    )
  print(p)
}

## -----------------------------------------------------------------------------
xi_irdc_only_fdr <- xi_fdr[match(irdc_detected_only, gene_names)]
top6_diff <- order(-(xi_irdc_only_fdr - irdc_only_fdr))[1:6]
largest_p_dif_irdc_do <- irdc_detected_only[top6_diff]

irdc_do_large_diff_genes <- yeast_genes_data[, which(gene_names %in% largest_p_dif_irdc_do) + 1]
irdc_do_large_diff_genes <- cbind(time_points, irdc_do_large_diff_genes)

## ----fig.height=4, fig.width=6, results='asis'--------------------------------
for (i in 1:6) {
  gene_to_plot <- colnames(irdc_do_large_diff_genes)[i + 1]
  idx <- match(gene_to_plot, gene_names)

  p <- ggplot(irdc_do_large_diff_genes, aes(x = time_points, y = .data[[gene_to_plot]])) +
    geom_point() +
    theme_bw() +
    labs(
      title = paste0("Only Detected by nu: xi FDR = ", round(xi_fdr[idx], 4),
                     ", nu FDR = ", round(ird_fdr[idx], 4)),
      x = "Time Points",
      y = gene_to_plot
    )
  print(p)
}

