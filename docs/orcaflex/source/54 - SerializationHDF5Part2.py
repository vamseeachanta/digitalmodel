import h5py
import matplotlib.pyplot as pyplot

f = h5py.File("scratch/serialization.hdf5", "r")
dataset = f["values"]
pyplot.plot(dataset[...,0], dataset[...,1])
pyplot.title(dataset.attrs["title"])
pyplot.xlabel(dataset.attrs["x_axis_title"])
pyplot.ylabel(dataset.attrs["y_axis_title"])
f.close()
pyplot.show()
