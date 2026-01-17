# Openpyxl Excel Automation Skill

> Quick Reference Guide for Excel Workbook Automation

## Overview

Create and manipulate Excel workbooks with formulas, charts, conditional formatting, and efficient handling of large datasets.

**Version**: 1.0.0
**Category**: office-docs

## Quick Start

```bash
# Install
pip install openpyxl

# With pandas integration
pip install openpyxl pandas
```

## Basic Usage

```python
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment
from openpyxl.chart import BarChart, Reference

# Create workbook
wb = Workbook()
ws = wb.active
ws.title = "Report"

# Add data with styling
ws['A1'] = 'Product'
ws['A1'].font = Font(bold=True)
ws['B1'] = 'Sales'

# Add formula
ws['B5'] = '=SUM(B2:B4)'

# Create chart
chart = BarChart()
data = Reference(ws, min_col=2, min_row=1, max_row=4)
chart.add_data(data, titles_from_data=True)
ws.add_chart(chart, 'D2')

# Save
wb.save('report.xlsx')
```

## Key Features

| Feature | Description |
|---------|-------------|
| Cells | Values, formulas, formatting |
| Charts | Bar, Line, Pie, Scatter, Area |
| Formatting | Fonts, fills, borders, alignment |
| Conditional | Color scales, data bars, icons |
| Large Files | Streaming read/write modes |

## When to Use

**Use for**:
- Excel report generation with formulas
- Financial spreadsheet automation
- Data visualization in Excel format
- Batch spreadsheet processing

**Avoid for**:
- Simple pandas export (use pandas directly)
- Real-time Excel (use xlwings)
- .xls format (use xlrd/xlwt)

## Common Patterns

### Styled Header Row
```python
header_fill = PatternFill(start_color="4472C4", fill_type="solid")
for cell in ws[1]:
    cell.fill = header_fill
    cell.font = Font(bold=True, color="FFFFFF")
```

### Large File Streaming
```python
# Write
wb = Workbook(write_only=True)
ws = wb.create_sheet()
ws.append(row)

# Read
wb = load_workbook('file.xlsx', read_only=True)
for row in ws.iter_rows(values_only=True):
    process(row)
```

## Files

```
openpyxl/
  SKILL.md    # Full documentation
  README.md   # This file
```

## Related Skills

- **pandas-data-processing** - Data analysis
- **python-docx** - Word automation
- **plotly** - Interactive charts

## Resources

- [Documentation](https://openpyxl.readthedocs.io/)
- [GitHub](https://github.com/theorchard/openpyxl)

---

See `SKILL.md` for complete examples and advanced patterns.
