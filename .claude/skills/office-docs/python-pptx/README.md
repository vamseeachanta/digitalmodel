# Python-pptx PowerPoint Automation Skill

> Quick Reference Guide for Presentation Automation

## Overview

Create and manipulate PowerPoint presentations with slides, shapes, charts, tables, and template-based generation.

**Version**: 1.0.0
**Category**: office-docs

## Quick Start

```bash
# Install
pip install python-pptx

# With image support
pip install python-pptx Pillow
```

## Basic Usage

```python
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.chart.data import CategoryChartData
from pptx.enum.chart import XL_CHART_TYPE

# Create presentation
prs = Presentation()

# Title slide
slide = prs.slides.add_slide(prs.slide_layouts[0])
slide.shapes.title.text = "Quarterly Report"
slide.placeholders[1].text = "Q1 2026"

# Content slide with bullets
slide = prs.slides.add_slide(prs.slide_layouts[1])
slide.shapes.title.text = "Key Points"
body = slide.placeholders[1]
tf = body.text_frame
tf.text = "First point"
p = tf.add_paragraph()
p.text = "Second point"

# Add chart
chart_data = CategoryChartData()
chart_data.categories = ['Q1', 'Q2', 'Q3']
chart_data.add_series('Sales', (100, 150, 200))
slide.shapes.add_chart(XL_CHART_TYPE.COLUMN_CLUSTERED,
    Inches(1), Inches(2), Inches(8), Inches(4), chart_data)

# Save
prs.save('report.pptx')
```

## Key Features

| Feature | Description |
|---------|-------------|
| Slides | Multiple layouts (title, content, blank) |
| Shapes | Rectangles, ovals, arrows, callouts |
| Charts | Column, line, pie, bar, doughnut |
| Tables | Styled headers, formatted cells |
| Images | PNG, JPG with sizing |

## When to Use

**Use for**:
- Automated report presentations
- Data-driven slide decks
- Template-based generation
- Batch presentation creation

**Avoid for**:
- Complex animations
- Video embedding
- Real-time editing

## Common Patterns

### Add Chart
```python
chart_data = CategoryChartData()
chart_data.categories = ['A', 'B', 'C']
chart_data.add_series('Data', (10, 20, 30))
slide.shapes.add_chart(
    XL_CHART_TYPE.PIE,
    Inches(2), Inches(2), Inches(6), Inches(4),
    chart_data
)
```

### Template Replacement
```python
for shape in slide.shapes:
    if shape.has_text_frame:
        for para in shape.text_frame.paragraphs:
            for run in para.runs:
                if '{{name}}' in run.text:
                    run.text = run.text.replace('{{name}}', 'Value')
```

## Files

```
python-pptx/
  SKILL.md    # Full documentation
  README.md   # This file
```

## Related Skills

- **python-docx** - Word automation
- **openpyxl** - Excel automation
- **plotly** - Interactive charts

## Resources

- [Documentation](https://python-pptx.readthedocs.io/)
- [GitHub](https://github.com/scanny/python-pptx)

---

See `SKILL.md` for complete examples and advanced patterns.
