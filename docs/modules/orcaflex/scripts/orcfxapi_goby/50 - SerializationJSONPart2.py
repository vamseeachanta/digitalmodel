import json
import matplotlib.pyplot as pyplot

with open("scratch/serialization.json", "r") as f:
    data = json.load(f)

pyplot.plot(data["x_axis_values"], data["y_axis_values"])
pyplot.title(data["title"])
pyplot.xlabel(data["x_axis_title"])
pyplot.ylabel(data["y_axis_title"])
pyplot.show()
