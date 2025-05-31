#' Distribution Class
#'
#' @description
#' Create a distribution class:
#'
#' OOP.
#' @param name A name.
#' @param type "continuous" or "discrete".
#' @param ddist density function.
#' @param support support.
#' @param pdist cdf.
#' @param hdist hazard function.
#' @return A dist class.
#' @export
#' @examples
#' normal_density <- function(x, par1 = 0, par2 = 1) { dnorm(x, mean = par1, sd = par2) }
#' normal_cdf <- function(x, par1 = 0, par2 = 1) { pnorm(x, mean = par1, sd = par2) }
#' normal_hazard <- function(x, par1 = 0, par2 = 1) { normal_density(x, par1, par2) / ( 1 - normal_cdf(x, par1, par2) ) }
#' x <- new_dist(name = "my.normal",  type = "continuous", ddist = normal_density, supp = c(-Inf,Inf), pdist = normal_cdf, hdist = normal_hazard)
#' print(x)
#'
#' y <- new_dist(ddist=dexp, type="continuous", name="exponential", supp=c(0,Inf))
#' plot(y,type="b",pch=16)
#'
#' z <- new_dist()
#' plot(y+z,type="l",col=2)
#' save(x,y,z, file="data/dist.rda")
new_dist <- function(name=NULL, type=c("continuous","discrete"), ddist=NULL, supp=NA, pdist=NULL, qdist=NULL, hdist=NULL, ..., class = character()) {
  if (is.null(ddist)) {ddist <- dnorm; supp <- c(-Inf, Inf); type="continuous"; if (is.null(name)) name <- "Z-dist"}
  if (is.null(pdist) && type=="continuous") {pdist <- Vectorize(function(x, ...) integrate(ddist, supp[1], x, ...)[[1]])}
  if (is.null(qdist) && type=="continuous") {qdist <- Vectorize(inverse(function(x){pdist(x, ...)}))}
  if (is.null(hdist) && type=="continuous") {hdist <- function(x, ...) ddist(x, ...)/(1-pdist(x, ...))}
  structure(
    list(name=name, ddist=ddist, pdist=pdist, qdist=qdist, hdist=hdist, type=type, supp=supp, ...),
    class = c(class, "dist")
  )
}

#' @export
print.dist <- function(x) {
  print(paste0(x$name," is a ", x$type, " distribution defined from ", x$supp[1]," to ",x$supp[2],"."))
  invisible(x)
}

#' @export
plot.dist <- function(x, ...) {
  g <- seq(x$qdist(.001),x$qdist(.999),len=100)
  #g <- (max(x$supp[1],-30):min(x$supp[2],30))/10;
  qg <- seq(.0001,.9999,len=100)
  par(mfrow=c(2,2))
  try(plot(g,x$ddist(g), main=paste("density of", x$name), ...))
  try(plot(g,x$pdist(g), main=paste("cdf of", x$name), ...))
  try(plot(qg,x$qdist(qg), main=paste("quantile of", x$name), ...))
  try(plot(g,round(x$hdist(g),10), main=paste("hazard of", x$name), ...))
}

#' @export
'+.dist' <- function(x1,x2) {
  supp <- c(x1$supp[1]+x2$supp[1],x1$supp[2]+x2$supp[2])
  sdist <- Vectorize(function(y) {
    f <- function(x) x1$ddist(x)*x2$ddist(y-x)
    integrate(f,supp[1],supp[2])[[1]]
  })
  return(new_dist(name="sum",ddist=sdist, type="continuous", supp=supp))
}


#' @export
'*.dist' <- function(x1, x2) {
  n <- 512  # number of grid points (adjustable)
  qg <- c(.0001,.9999);
  from <- x1$qdist(qg[1]) + x2$qdist(qg[1])
  to   <- x1$qdist(qg[2]) + x2$qdist(qg[2])
  grid <- seq(from, to, length.out = n)

  dx <- (x1$qdist(qg[2]) - x1$qdist(qg[1])) / (n - 1)
  xvals <- seq(x1$qdist(qg[1]), x1$qdist(qg[2]), length.out = n)
  yvals <- seq(x2$qdist(qg[1]), x2$qdist(qg[2]), length.out = n)

  f1 <- x1$ddist(xvals)
  f2 <- x2$ddist(yvals)

  conv_y <- convolve_density(f1, f2) * dx  # scale by dx to approximate integral

  # result grid is wider than original
  conv_grid <- seq(from, to, length.out = length(conv_y))

  # Define new density function as interpolated version
  new_ddist <- approxfun(conv_grid, conv_y, yleft = 0, yright = 0)

  return(new_dist(
    name = paste0("(", x1$name, " + ", x2$name, ")"),
    ddist = new_ddist,
    type = "continuous",
    supp = x1$supp+x2$supp
  ))
}

inverse = function (f, lower = -100, upper = 100) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[[1]]
}
