# Grade API H40 Pipe Data
import math
k=0.45
s=80  # Maximum Yield Strength 80 ksi
u=60  # Maximum ultimate Strength 60 ksi
d=2.375 # OD
t=0.1     # Wall Thickness Assumed
x1=(d/d-2*t)

pb=k*(s+u)*math.log(x1)
