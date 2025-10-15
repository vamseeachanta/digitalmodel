import h5py
import math
import matplotlib.pyplot as pyplot

f = h5py.File("scratch/serialization_advanced.hdf5", "r")
count = len(f)
ncols = int(math.sqrt(count))
nrows = math.ceil(count / ncols)
i = 1
for varName, dataset in f.items():
    pyplot.subplot(nrows, ncols, i)
    pyplot.plot(dataset[...,0], dataset[...,1])
    pyplot.xlabel(dataset.attrs["x_axis_title"])
    pyplot.ylabel(dataset.attrs["y_axis_title"])
    i += 1

f.close()

pyplot.tight_layout() # avoid label overlapping
pyplot.show()
