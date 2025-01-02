import OrcFxAPI
import xlsxwriter

model = OrcFxAPI.Model()
line = model.CreateObject(OrcFxAPI.ObjectType.Line)
line.TargetSegmentLength = 1.0,
model.CalculateStatics()
Te = line.RangeGraph("Effective tension")

book = xlsxwriter.Workbook("scratch/chart_sheet.xlsx")
sheet = book.add_worksheet("ChartData")
sheet.write_row("A1", ["Arclength", "Te"])
sheet.write_column("A2", Te.X)
sheet.write_column("B2", Te.Mean)

chart = book.add_chart({"type": "scatter",
                        "subtype": "straight"})
chart.add_series({
    "name": "Effective tension",
    "categories": f"=ChartData!$A$2:$A${len(Te.X) + 1}",
    "values": f"=ChartData!$B$2:$B${len(Te.X) + 1}",
})
chart.set_title({"name": "Effective tension in static state"})
chart.set_x_axis({"name": "Arclength (m)",
                  "min": Te.X[0],
                  "max": Te.X[-1]})
chart.set_y_axis({"name": "Te (kn)"})
chart.set_legend({"none": True})
chartsheet = book.add_chartsheet("Effective tension chart")
chartsheet.set_chart(chart)

book.close()
