from pycatenary.cable import MooringLine

# define properties of cable
length = 80  # length of line
w = 1.036  # submerged weight
EA = 560e3  # axial stiffness
floor = False  # if True, contact is possible at the level of the anchor
anchor = [0., 0., 0.]
fairlead = [65.766, 10.2, 0]

# create cable instance
l1 = MooringLine(L=length,
                       w=w,
                       EA=EA,
                       anchor=anchor,
                       fairlead=fairlead,
                       floor=floor)

# compute calculations
l1.computeSolution()

# get tension along line (between 0. and total line length)
s = 5.
T = l1.getTension(s)

# get xyz coordinates along line
xyz = l1.s2xyz(s)

# plot cable cable.MooringLine instance l1
l1.plot2D()

# plot cable cable.MooringLine instance l1
l1.plot3D()

