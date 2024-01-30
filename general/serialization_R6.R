library(pryr)

x = rnorm(1000000)
y = x
lx = list(x)
lxy = list(x, y)
object_size(lx)
object_size(lxy)

# serialization problem => R6 problem
object_size(serialize(lx, NULL))
object_size(serialize(lxy, NULL)) # it should be ~8MB, not duplicated (16MB)
