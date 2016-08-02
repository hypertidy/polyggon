#A.poly
#http://www.netlib.org/voronoi/triangle.zip
#https://www.cs.cmu.edu/~quake/triangle.poly.html

#First line: <# of vertices> <dimension (must be 2)> <# of attributes> <# of boundary markers (0 or 1)>
#  Following lines: <vertex #> <x> <y> [attributes] [boundary marker]
#One line: <# of segments> <# of boundary markers (0 or 1)>
#  Following lines: <segment #> <endpoint> <endpoint> [boundary marker]
#One line: <# of holes>
#  Following lines: <hole #> <x> <y>
#Optional line: <# of regional attributes and/or area constraints>
#  Optional following lines: <region #> <x> <y> <attribute> <maximum area>

x <- readLines("data-raw/A.poly")
l1 <- read.table(text = x[1])
nv <- l1[[1]]
v <- read.table(text = x[seq(2, nv + 1L)])
l2 <- read.table(text = x[nv + 2L])
s <- read.table(text = x[seq(nv + 2 + 1, length = l2[[1]])])
plot(v[,1:2])
apply(s, 1, function(x) lines(v[x, 1:2]))
