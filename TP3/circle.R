circle <- function(c, radius, nv = 100, border = NULL, col = NA, 
                    lty = 1, lwd = 1) 
{
    xylim <- par("usr")
    plotdim <- par("pin")
    ymult <- (xylim[4] - xylim[3]) / (xylim[2] - xylim[1]) * 
            plotdim[1] / plotdim[2]
    angle.inc <- 2 * pi / nv
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    xv <- cos(angles) * radius + c[1]
    yv <- sin(angles) * radius * ymult + c[2]
    polygon(xv, yv, border = border, col = col, lty = lty, lwd = lwd)
    invisible(list(x = xv, y = yv))
}
