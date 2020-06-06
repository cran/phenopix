plotBP <- function(ts, breaks, bp.y, ...) {
  plot(ts, ...)
  ylons <- mean(ts, na.rm = TRUE)
  if (missing(bp.y))  bp.y <- quantile(ts, .005)
    colors <- palette()[1:length(breaks)]
  abline(v = breaks[2, ], col = colors, lty=2)
  text(breaks[2, ], y = bp.y, labels = names(breaks), 
    col = colors)
  for (a in 1:length(breaks)) arrows(breaks[1, a], ylons, 
    breaks[3, a], ylons, code = 3, angle = 90, length = 0.1, 
    lwd = 2, col = colors[a])
} 