#
# keith p jolley
# Wed Feb 22 15:13:58 PST 2017
# squalor heights, ca, usa
#
# R script to create the iconic "Unknown Pleasures" album cover, aka cp-1919
#
# https://blogs.scientificamerican.com/sa-visual/pop-culture-pulsar-origin-story-of-joy-division-s-unknown-pleasures-album-cover-video/
#
# I had a shirt with this printed on it in college but it's since left us. Bummer.
# I passed on the Disney version when it was available. Bummer.
#

library('ggplot2')

lines <- 80          # aka rows
pointsperrow <- 90   # aka columns
segments     <- 15   # to set ratio of "pulse" to "non-pulse" areas
alpha <- 15          # max elevation of peaks
beta  <- alpha/5     # noise within pulse
gamma <- alpha/10    # outer noise

df <- data.frame(x=integer, y=double, line=integer)
b <- "black"
w <- "white"

# Processing has this noise function built in. 
# This function has it's own story, including an Academy Award.
# http://mrl.nyu.edu/~perlin/doc/oscar.html
# this code yanked from:
# https://stackoverflow.com/questions/15387328/realistic-simulated-elevation-data-in-r-perlin-noise
perlin_noise <- function(
  n = 5,   m = 7,    # Size of the grid for the vector field
  N = 100, M = 100   # Dimension of the image
) {
  # For each point on this n*m grid, choose a unit 1 vector
  vector_field <- apply(
    array( rnorm( 2 * n * m ), dim = c(2,n,m) ),
    2:3,
    function(u) u / sqrt(sum(u^2))
  )
  f <- function(x,y) {
    # Find the grid cell in which the point (x,y) is
    i <- floor(x)
    j <- floor(y)
    stopifnot( i >= 1 || j >= 1 || i < n || j < m )
    # The 4 vectors, from the vector field, at the vertices of the square
    v1 <- vector_field[,i,j]
    v2 <- vector_field[,i+1,j]
    v3 <- vector_field[,i,j+1]
    v4 <- vector_field[,i+1,j+1]
    # Vectors from the point to the vertices
    u1 <- c(x,y) - c(i,j)
    u2 <- c(x,y) - c(i+1,j)
    u3 <- c(x,y) - c(i,j+1)
    u4 <- c(x,y) - c(i+1,j+1)
    # Scalar products
    a1 <- sum( v1 * u1 )
    a2 <- sum( v2 * u2 )
    a3 <- sum( v3 * u3 )
    a4 <- sum( v4 * u4 )
    # Weighted average of the scalar products
    s <- function(p) 3 * p^2 - 2 * p^3
    p <- s( x - i )
    q <- s( y - j )
    b1 <- (1-p)*a1 + p*a2
    b2 <- (1-p)*a3 + p*a4
    (1-q) * b1 + q * b2
  }
  xs <- seq(from = 1, to = n, length = N+1)[-(N+1)]
  ys <- seq(from = 1, to = m, length = M+1)[-(M+1)]
  outer( xs, ys, Vectorize(f) )
}

# mostly blacks everything out
unknown_theme <- function(base_size=12, base_family="Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
  theme(
        line = element_blank(),
        text = element_blank(),
        panel.background = element_rect(fill=b, colour=b),
        plot.background  = element_rect(fill=w, colour=w)
  )
}

# in my mind I have this done without a for loop but in practice I started with a loop and 
# i'm cool with using a few more milli-seconds of cpu time versus a few more hours(?) of my time.
# anyone want to pay me to undo this loop let me know - i'd like that.
for(i in 1:lines) {
  # this sets the start point and length of the impulse segments
  p1 <- round((4*pointsperrow/segments*1.1)-runif(1)/5)    # where pulse starts
  p3 <- round((4*pointsperrow/segments*1.1)-runif(1)/5)    # where pulse ends
  p2 <- pointsperrow-p1-p3                                 # width of pulse

  # create a front loaded peak with a slope to/from origins/peak
  lump <- abs(perlin_noise(n=25,m=2,N=1,M=round(p2/2)))
  lmax <- median(lump)    # seems like a good a height as any
  wmax <- which.max(lump) # where the peak is in the "lump" array
  impulse <- c(lump, rep(0,p2-length(lump)))  # copy the lump into the start of the larger impulse array
  # this choses the "taller" of either the peaks or the ramp signal
  impulse <- pmax(impulse, c(seq(0,lmax,length.out=wmax), seq(lmax,0,length.out=p2-wmax)))

  # add finer grained noise to the impulse
  impulse <- impulse*alpha + abs(perlin_noise(m=7,N=1,M=p2))*beta

  # insert the impulse into a larger "row" array
  row <- c(rep(0,p1), impulse, rep(0,p3))
  # add some very fine grained noise to the row. this is smoother around the edges than center. perfect.
  row <- row + abs(as.vector(perlin_noise(m=pointsperrow,N=1,M=pointsperrow)))*gamma
 
  # add this row to the dataframe and do this bookkeeping to draw in the right order.
  # this is important because geom_area() fills in from its "y" value down to the axis, overwriting
  # anything below it, so draw from the top down. 
  df <- rbind(df, data.frame(
                    x=1:length(row),
                    y=row+i,
                    line=-rep(i,length(row))
             ))
}

# this is all pretty straight forward, i think.
# there were a couple lines that i couldn't figure out where they were
# coming from. didn't see an answer online and my ggplot2 book is at 
# home so rather than kill myself trying to remove the correct way i just
# overwrite them with black lines.
p <- ggplot(df, aes(x, y, group=line))
p <- p + geom_area(fill=b, colour=w, position="identity", size=0.5)
p <- p + unknown_theme() + coord_fixed(ratio=1)
p <- p + geom_vline(xintercept=min(df$x), colour=b, size=1)
p <- p + geom_vline(xintercept=max(df$x), colour=b, size=1)
p <- p + geom_hline(yintercept=0, colour=b, size=1)

print(p)
