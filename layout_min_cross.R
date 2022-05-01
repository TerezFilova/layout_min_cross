library(igraph)
library(qgraph)
library(progress)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# functions for segment-crossing verification
# source:
# https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
#
# given three COLLINEAR points p, q, r, the function checks if
# point q lies on line segment 'pr'
onSegment <- function(p, q, r) {
  return(q$x <= max(p$x, r$x) && q$x >= min(p$x, r$x) &&
           q$y <= max(p$y, r$y) && q$y >= min(p$y, r$y));
}
# To find orientation of ordered triplet (p, q, r).
# The function returns following values
# 0 --> p, q and r are collinear
# 1 --> clockwise
# 2 --> counterclockwise
# for more explanation, see the source
orientation <- function(p, q, r) {
  # See https://www.geeksforgeeks.org/orientation-3-ordered-points/
  # for details of below formula.
  val <- (q$y - p$y) * (r$x - q$x) - (q$x - p$x) * (r$y - q$y);
  #
  if (val == 0) return(0); # collinear
  if (val > 0) return(1);  # clockwise
  return(2)                # counterclock wise
}
doIntersect <- function(p1, q1, p2, q2) {
  #
  # cases when the crossing point is just the common vertex 
  if (p1$x == q2$x & p1$y == q2$y) return(F)
  if (q1$x == p2$x & q1$y == p2$y) return(F)
  if (q1$x == q2$x & q1$y == q2$y) return(F)
  #
  o1 <- orientation(p1, q1, p2);
  o2 <- orientation(p1, q1, q2);
  o3 <- orientation(p2, q2, p1);
  o4 <- orientation(p2, q2, q1);
  #
  # General case
  if (o1 != o2 && o3 != o4) return(T);
  # Special cases
  # p1, q1 and p2 are collinear and p2 lies on segment p1q1
  if (o1 == 0 && onSegment(p1, p2, q1)) return(T);
  #  
  # p1, q1 and q2 are collinear and q2 lies on segment p1q1
  if (o2 == 0 && onSegment(p1, q2, q1)) return(T);
  #
  # p2, q2 and p1 are collinear and p1 lies on segment p2q2
  if (o3 == 0 && onSegment(p2, p1, q2)) return(T);
  #
  # p2, q2 and q1 are collinear and q1 lies on segment p2q2
  if (o4 == 0 && onSegment(p2, q1, q2)) return(T);
  #
  return(F); # Doesn't fall in any of the above cases
}
  # # Driver code
  # p1 <- data.frame(x = 1, y = 0);
  # q1 <- data.frame(x = 3, y = 0);
  # p2 <- data.frame(x = 0, y = 2);
  # q2 <- data.frame(x = 0, y = 5);
  # doIntersect(p1, q1, p2, q2) # FALSE
  # 
  # p1 <- data.frame(x = 1, y = 0);
  # q1 <- data.frame(x = 3, y = 0);
  # p2 <- data.frame(x = 2, y = -2);
  # q2 <- data.frame(x = 2, y = 5);
  # doIntersect(p1, q1, p2, q2) # TRUE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# my idea: fruchterman-reingold layout with such a minimal repulse.rad, that has
# the lowest no. of edge-crossings
# function might be usefull for small or poor grapghs
#
layout_min_cross <- function(g, max.rad, min.rad = 1) {
  if ((min.rad - max.rad) > 3.5) warning("This will take a while...")
  iter <- seq(min.rad, max.rad, by = 0.1)
  n.v <- igraph::vcount(g)
  list.e <- igraph::get.edgelist(g, names = FALSE)
  if (nrow(list.e) > 100) warning("This will take a while, bc G(V, E) looks complicated.")
  #
  pb <- progress::progress_bar$new(total = length(iter) + 1)
  #
  layouts <- matrix(ncol = 2*length(iter), nrow = n.v)
  total.cross <- c()
  for (i in seq_along(iter)) {
    pb$tick()
    # matrix that remembers all layouts
    layouts[, c(2*i - 1, 2*i)] <- qgraph::qgraph.layout.fruchtermanreingold(
                               list.e, vcount = n.v, repulse.rad = n.v^iter[i])
    n.cross <- 0 
    # matrix that remembers edge coordinates
    # column i  = as.vector(x, y the first vertex of edge i; x, y the second vertex)
    xy.e <- rbind(t(layouts[list.e[, 1], c(2*i - 1, 2*i)]), t(layouts[list.e[, 2], c(2*i - 1, 2*i)]))
      #
      # edge construction
      for (j in 1:nrow(list.e)) {
      line1.a <- data.frame(x = xy.e[1, j], y = xy.e[2, j])
      line1.b <- data.frame(x = xy.e[3, j], y = xy.e[4, j])
      #
      # verification of edge crossings
      for (k in 1:nrow(list.e)) {
        if (j == k) next
        if (all(list.e[j, ] == rev(list.e[k, ]))) next
        line2.a <- data.frame(x = xy.e[1, k], y = xy.e[2, k])
        line2.b <- data.frame(x = xy.e[3, k], y = xy.e[4, k])
        quest.cross <- doIntersect(line1.a, line1.b, line2.a, line2.b)
        n.cross <- n.cross + quest.cross
      }
    }
    total.cross[i] <- n.cross
  }
  pb$tick()
  lower <- which.min(total.cross)
  val.lower <- iter[lower]
  # output of the function
  plot(g, layout = layouts[, c(2*lower - 1, 2*lower)],
       edge.color = "darkgrey", edge.arrow.size = 0.2, vertex.size = 15)
  print(paste("repulse.rad = vcount(G)^", val.lower))
}
# - - - - - - - - - - - - - - - - - - END - - - - - - - - - - - - - - - - - - -