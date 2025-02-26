#' Generate 4-panel figure using simulated fisheries data in ggplot
#'
#' @param timeseries the timeseries to plot, an example simulated data can be generated using the script fake-data.R
#' @param language French or English
#'
#' @return A graphics device with the four-panel of figures
#' @export
#'
#' @examples
#' fsar_plot_ggplot(sim_fsar_data(format="wide"))
#' @importFrom ggplot2 ggplot geom_line aes scale_y_continuous labs geom_ribbon
#'   expansion
fsar_plot_ggplot <- function(df, language = c("English","French")) {

  language <- match.arg(language)

  # Catch
  g1 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `Catch-MT`)) +
    geom_line(aes(y = `TAC-MT`), linetype = "dashed", colour = "grey45") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "Catch (t)") +
    csasdown::theme_csas()

  # SSB = Spawning stock biomass
  g2 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `SSB-MT`)) +
    geom_ribbon(aes(ymin = `SSBlow-MT`, ymax = `SSBhigh-MT`), alpha = 0.3) +
    geom_line(aes(y = `SSBlrp-MT`), linetype = "dashed", colour = "red") +
    geom_line(aes(y = `SSBusr-MT`), linetype = "dashed", colour = "grey45") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "SSB (t)") +
    csasdown::theme_csas()

  # Instantaneous fishing mortality
  g3 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `F-1/yr`)) +
    geom_ribbon(aes(ymin = `Flow-1/yr`, ymax = `Fhigh-1/yr`), alpha = 0.3) +
    geom_line(aes(y = `Flim-1/yr`), linetype = "dashed", colour = "grey45") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "Fishing mortality (yr<sup>-1</sup>)") +
    csasdown::theme_csas() +
    theme(axis.title.y = ggtext::element_markdown())

  g4 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `R-E06`)) +
    geom_ribbon(aes(ymin = `Rlow-E06`, ymax = `Rhigh-E06`), alpha = 0.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "Recruitment (10<sup>6</sup>)") +
    csasdown::theme_csas() +
    theme(axis.title.y = ggtext::element_markdown())

  cowplot::plot_grid(g1, g2, g3, g4, ncol = 2, labels = "AUTO", align = "hv")

}


#' Generate 4-panel figure using simulated fisheries data in base R
#'
#' @param timeseries the timeseries to plot
#' @param language French or English
#'
#' @return A graphics device with the four-panel of figures
#' @export
#'
#' @examples
#' fsar_plot_base(sim_fsar_data(format="long"))
#'
#' @importFrom grDevices grey
#' @importFrom graphics axis box layout legend lines mtext par
#' @importFrom stats arima.sim rnorm
fsar_plot_base <- function(in.df, language = c("English","French")) {

  language <- match.arg(language)

  mm <- matrix(c(rep(0, 5), 0, 1, 0, 2, 0, rep(0, 5), 0, 3, 0, 4, 0, rep(0, 5)), nc = 5, byrow = TRUE)
  ll <- layout(mm, widths = c(0.06, 0.43, 0.06, 0.43, 0.02), heights = c(c(0.02, 0.45, 0.04, 0.45, 0.04))) # layout.show(ll)
  par(mar = c(2, 2, 0, 0))

  # top-left panel - Catch and TAC
  idx <- which(in.df$panel.category == "Catch")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.4)
  tl.df <- in.df[idx, ]

  if (language == "English") {
    x.lab <- "Year"
    y.lab <- "Catch"
    legend.text <- c("Catch-MT", "TAC-MT")
  }
  if (language == "French") {
    x.lab <- "Année"
    y.lab <- "Capture"
    legend.text <- c("Capture-tonne", "TAC-tonne")
  }

  plot(ts.value ~ year, data = tl.df, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl)
  ## Catch
  lines(ts.value ~ year, data = tl.df[which(tl.df$ts.name == "Catch-MT"), ], type = "l", lwd = 2)
  ## TAC
  lines(ts.value ~ year, data = tl.df[which(tl.df$ts.name == "TAC-MT"), ], type = "l", lty = 2)

  legend("topright","(A)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, 2),
         lwd = c(2, 1),
         pch = c(-1, -1),
         box.lwd = 0.5
  )


  axis(side = 1, padj = -0.5)
  axis(side = 2, las = 1, hadj = 0.9)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)
  box()


  # top-right panel
  idx <- which(in.df$panel.category == "SSB")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.4)
  tr.df <- in.df[idx, ]

  if (language == "English") {
    y.lab <- "Biomass"
    legend.text <- c("SSB-MT", "95% confidence", "USR-MT", "LRP-MT")
  }
  if (language == "French") {
    y.lab <- "Biomasse"
    legend.text <- c("BSR-tonne", "confiance à 95%", "NRS-tonne", "NRL-tonne")
  }

  plot(ts.value ~ year, data = tr.df, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl)
  ## SSB
  ## lower and upper
  tr.df.low <- tr.df[which(tr.df$ts.name == "SSBlow-MT"), ]
  tr.df.high <- tr.df[which(tr.df$ts.name == "SSBhigh-MT"), ]
  ## use grey shading polygons instead of lines
  polygon(c(tr.df.low$year, rev(tr.df.high$year)), c(tr.df.low$ts.value, rev(tr.df.high$ts.value)), col=grey(0.8), border=grey(0.8))

  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "SSB-MT"), ], type = "l", lwd = 2)
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "SSBlow-MT"), ], type = "l", lty = 2)
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "SSBhigh-MT"), ], type = "l", lty = 2)

  ## LRP and USR
  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "SSBlrp-MT"), ], type = "l", lty = 3, lwd = 2, col = "red")
  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "SSBusr-MT"), ], type = "l", lty = 3, lwd = 2, col = "forestgreen")

  legend("topright","(B)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, -1, 3, 3),
         lwd = c(2, 0, 2, 2),
         pch = c(-1,15,-1,-1),
         col = c("black", grey(0.8), "forestgreen", "red"),
         box.lwd = 0.5
  )

  axis(side = 1, padj = -0.5)
  axis(side = 2, las = 1, hadj = 0.9)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)
  box()

  ## bottom-left panel
  idx <- which(in.df$panel.category == "Fishing")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.4)
  bl.df <- in.df[idx, ]

  if (language == "English") {
    y.lab <- "Mortality"
    legend.text <- c("F-1/yr", "95% confidence", "RR-1/yr", "M-1/yr")
  }
  if (language == "French") {
    y.lab <- "Mortalité"
    legend.text <- c("F-1/yr", "confiance à 95%", "RP-1/yr", "M-1/yr")
  }

  idx <- which(in.df$panel.category == "Fishing" & in.df$ts.name == "F-1/yr")
  plot(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "F-1/yr"), ], type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl)

  bl.df.low <- bl.df[which(bl.df$ts.name == "Flow-1/yr"), ]
  bl.df.high <- bl.df[which(bl.df$ts.name == "Fhigh-1/yr"), ]
  ## use grey shading polygons instead of lines
  polygon(c(bl.df.low$year, rev(bl.df.high$year)), c(bl.df.low$ts.value, rev(bl.df.high$ts.value)), col=grey(0.8), border=grey(0.8))
  lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "F-1/yr"), ], lwd = 2)
  # lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Flow-1/yr"), ], type = "l", lty = 2)
  # lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Fhigh-1/yr"), ], type = "l", lty = 2)
  lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Flim-1/yr"), ], lty = 3, lwd = 2, col = "red")

  lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "M-1/yr"), ], lty = 1, lwd = 2, col = grey(0.5))

  ## natural mortality

  legend("topright","(C)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, -1, 3, 1),
         lwd = c(2, 0, 2, 2),
         pch = c(-1,15,-1,-1),
         col = c("black", grey(0.8), "red", grey(0.5)),
         box.lwd = 0.5
  )

  axis(side = 1, padj = -0.5)
  axis(side = 2, las = 1, hadj = 0.9)
  # axis(side = 4, las = 1, hadj = 0.9)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)

  box()


  ## bottom-right panel
  idx <- which(in.df$panel.category == "Recruitment")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.2)
  br.df <- in.df[idx, ]

  if (language == "English") {
    y.lab <- "Recruitment"
    legend.text <- c("R-E06", "95% confidence")
  }
  if (language == "French") {
    y.lab <- "Recrutement"
    legend.text <- c("R-E06", "confiance à 95%")
  }
  plot(ts.value ~ year, data = br.df[which(br.df$ts.name == "R-E06"), ], type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl)
  # lines(ts.value ~ year, data = br.df[which(br.df$ts.name == "Rlow-E06"), ], type = "l", lty = 2)
  # lines(ts.value ~ year, data = br.df[which(br.df$ts.name == "Rhigh-E06"), ], type = "l", lty = 2)

  br.df.low <- br.df[which(br.df$ts.name == "Rlow-E06"), ]
  br.df.high <- br.df[which(br.df$ts.name == "Rhigh-E06"), ]
  ## use grey shading polygons instead of lines
  polygon(c(br.df.low$year, rev(br.df.high$year)), c(br.df.low$ts.value, rev(br.df.high$ts.value)), col=grey(0.8), border=grey(0.8))
  lines(ts.value ~ year, data = br.df[which(br.df$ts.name == "R-E06"), ], lwd = 2)


  legend("topright","(D)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, -1),
         lwd = c(2, 0),
         pch = c(-1,15),
         col = c("black", grey(0.8)),
         box.lwd = 0.5
  )

  axis(side = 1, padj = -0.5)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  axis(side = 2, las = 1, hadj = 0.9)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)
  box()
} # end function definition

