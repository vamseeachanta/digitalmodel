import pandas as pd
from bokeh.io import export_png, output_file, show
from bokeh.models import (BasicTicker, ColorBar, ColumnDataSource,
                          LinearColorMapper, PrintfTickFormatter)
from bokeh.plotting import figure
from bokeh.sampledata.unemployment1948 import data
from bokeh.transform import transform


def plotHeatMap(TableDF, data):
    dataDF = TableDF.copy()
# Data Transformation for the plot
    if not isinstance(dataDF.index[0],str):
        dataDF.index=dataDF.index.astype(str)
    if not isinstance(dataDF.columns[0],str):
        dataDF.columns = dataDF.columns.astype(str)

    x_range=list(dataDF.index)
    y_range=list(dataDF.columns)

    output_file(data["FileName"] + '_HeatMap.html')
    dataDF.columns.name = data['Columns_Name']
    dataDF.index.name = data['Index_Name']
    x = data['Columns_Name']
    y = data['Index_Name']
    title = data["Title"]
    # reshape to 1D array or rates with a month and year for each row.
    df = pd.DataFrame(dataDF.stack(), columns=['data']).reset_index()

    source = ColumnDataSource(df)
    # this is the colormap from the original NYTimes plot
    colors = ["#75968f", "#a5bab7", "#c9d9d3", "#e2e2e2", "#dfccce", "#ddb7b1", "#cc7878", "#933b41", "#550b1d"]
    mapper = LinearColorMapper(palette=colors, low=df.data.min(), high=df.data.max())

    p = figure(plot_width=800, plot_height=300, title=title,
            x_range=x_range, y_range=y_range,
            toolbar_location=None, tools="", x_axis_location="above")

    p.rect(x=x, y=y, width=1, height=1, source=source,
        line_color=None, fill_color=transform('data', mapper))

    color_bar = ColorBar(color_mapper=mapper, location=(0, 0),
                        ticker=BasicTicker(desired_num_ticks=len(colors)))

    p.add_layout(color_bar, 'right')

    p.axis.axis_line_color = None
    p.axis.major_tick_line_color = None
    p.axis.major_label_text_font_size = "10pt"
    p.axis.major_label_standoff = 0
    p.xaxis.major_label_orientation = 1.0

    # export_png(p, filename = data["FileName"] + '.png')
    # show(p)