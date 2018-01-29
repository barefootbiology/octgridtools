#' Given an rows, columns, and radius, determine the grid of hexagon centers
#'
#' Code from:
#' http://hacksociety.net/Thread-Tutorial-Creating-a-hexagonal-grid-for-games-C
.calc_y = function(i, j, s, y0=0) {
    a = sqrt(3)*(s/2)
    return(y0 + (j%%2)*a + 2*i*a)
}

.calc_x = function(i, j, s, x0=0) {
    return(x0 + j*((3*s)/2))
}
