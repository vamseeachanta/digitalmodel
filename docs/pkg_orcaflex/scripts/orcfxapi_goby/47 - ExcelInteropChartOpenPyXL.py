import OrcFxAPI
import openpyxl

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.TargetSegmentLength = 1.0,
model.CalculateStatics()
Te = line.RangeGraph("Effective tension")

book = openpyxl.Workbook()
sheet = book.active
sheet.append(["Arclength", "Te"])
for data in zip(Te.X, Te.Mean):
    sheet.append(data)

chart = openpyxl.chart.ScatterChart()
chart.title = "Effective tension in static state"
chart.x_axis.title = "Arclength (m)"
chart.y_axis.title = "Te (kN)"

N = len(Te.X)
x = openpyxl.chart.Reference(sheet, min_col=1, min_row=2, max_row=N+1)
y = openpyxl.chart.Reference(sheet, min_col=2, min_row=2, max_row=N+1)
series = openpyxl.chart.Series(y, x)
chart.series.append(series)
sheet.add_chart(chart, "D2")

book.save("scratch/chart_embedded.xlsx")
