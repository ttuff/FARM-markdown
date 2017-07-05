plot.myworld <- function(b) {
  require(maps)
  map()
  c <- b[, 6]
  c[c == 1] <-  "blue"
  c[c == 2] <-  "red"
  c[is.na(c)] <-  "gray"
  points(b[, 2:3], pch = 20, col = c)
}

