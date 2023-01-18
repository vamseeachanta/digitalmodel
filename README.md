# Introduction

Data science typically involves data, analysis, tests and outputs. A simple way to review, verify or present results is via Visualizations.

Plotly Dash is selected to help prepare and present visualizations in a single dashboard. The key is to help present results in a organized manner to avoid 

# Summary

A library is under development to standardize this. See [dashhtmlgrid](https://github.com/vamseeachanta/dashhtmlgrid)

**#TODO**
- A decidated plotly visualization library is required to help develop faster custom plotting
    - The data provided to charts should be dedicated dfs with simple configuration files


# Usage

- Prepare data
- Define html div matrix using app layout (based on expected outputs). Some examples are:
    - 
- Utilize standard elements to plug into each matrix divs charts, tables 

The library currently supports:
- 1 dropdown menu box (with only 1 input field)
- multiple tables
- multiple plots

<img src="docs/visualization_architecture.svg" width=auto, height=auto/>


## Example codes

- Dropdown, multiple option selection. Example is given below:

Code file: src\dashhtmlgrid\tests\test_dropdown_multiple_options.py

<img src="docs\test_dropdown_multiple_options.png" width=600, height=auto/>


- Dropdown - Single selection. Example is given below:
    - https://github.com/vamseeachanta/energy/blob/ea3d37d9c65175cb6718d3052948d1871b3e54e2/py/utilities/plotly_dash_dropdown_graph.py


## Drag value - Circular Callback

https://community.plotly.com/t/dash-1-19-0-circular-callbacks-drag-value-in-dcc-slider-close-button-in-dev-tools-dcc-graph-bug-fixes/49500


## CSS templates

https://towardsdatascience.com/how-to-embed-bootstrap-css-js-in-your-python-dash-app-8d95fc9e599e

## References

https://pypi.org/project/dash/
https://www.statworx.com/en/content-hub/blog/how-to-build-a-dashboard-in-python-plotly-dash-step-by-step-tutorial/
https://chart-studio.plotly.com/dashboard/PythonPlotBot:540/present#/

20 line code dashboard tutorial
- https://pythoninoffice.com/python-dash-web-app-tutorial/
- https://youtu.be/fhJ_6EbawUA

Unexpected errors
https://pyquestions.com/getting-the-error-message-r-is-null-whenever-running-a-dash-application#:~:text=The%20r%20is%20null%20also%20error%20occurs%20for,are%20no%20traces%20that%20meet%20the%20user-specified%20criteria.


Flask and Plolty Apps
https://towardsdatascience.com/an-interactive-web-dashboard-with-plotly-and-flask-c365cdec5e3f

