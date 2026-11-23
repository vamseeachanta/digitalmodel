import yaml
import matplotlib.pyplot as pyplot

with open("scratch/serialization.yaml", "r") as f:
    data = yaml.safe_load(f)

pyplot.plot(data["x_axis_values"], data["y_axis_values"])
pyplot.title(data["title"])
pyplot.xlabel(data["x_axis_title"])
pyplot.ylabel(data["y_axis_title"])
pyplot.show()
