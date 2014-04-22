require(MASS)

x = .testNorm()

# Przesylanie argumentow do depth
as.numeric(depth(x,x,method="LP",p=5)) == as.numeric(depth(x,x,method="LP",p=4))
