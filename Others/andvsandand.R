# &
z = 1:6; z[(z>2) & (z<5)]
# &&
z[(z > 2) && (z < 5)]

# && gives something we don't want. compare the following to see why
(z>2) & (z<5) #&
(z > 2) && (z < 5) #&&
