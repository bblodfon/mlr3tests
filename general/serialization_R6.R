library(lobstr)

x = rnorm(1000000)
y = x
lx = list(x)
lxy = list(x, y)
obj_size(lx)
obj_size(lxy)

# serialization problem => R6 problem
obj_size(serialize(lx, NULL))
obj_size(serialize(lxy, NULL)) # it should be ~8MB, not duplicated (16MB)
