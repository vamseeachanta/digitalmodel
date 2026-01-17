# Python-docx Word Document Skill

> Quick Reference Guide for Word Document Automation

## Overview

Create and manipulate Microsoft Word documents programmatically with full control over formatting, tables, images, and styles.

**Version**: 1.0.0
**Category**: office-docs

## Quick Start

```bash
# Install
pip install python-docx

# With image support
pip install python-docx Pillow
```

## Basic Usage

```python
from docx import Document
from docx.shared import Inches, Pt
from docx.enum.text import WD_ALIGN_PARAGRAPH

# Create document
doc = Document()

# Add title
doc.add_heading('Report Title', level=0)

# Add paragraph
para = doc.add_paragraph()
para.add_run('Bold text ').bold = True
para.add_run('and normal text.')

# Add table
table = doc.add_table(rows=3, cols=3)
table.style = 'Table Grid'
table.rows[0].cells[0].text = 'Header'

# Add image
doc.add_picture('chart.png', width=Inches(4))

# Save
doc.save('output.docx')
```

## Key Features

| Feature | Description |
|---------|-------------|
| Paragraphs | Text with fonts, alignment, spacing |
| Tables | Rows, columns, merged cells, styles |
| Images | PNG, JPG, sizing, positioning |
| Styles | Custom and built-in styles |
| Headers/Footers | Page numbers, logos, text |

## When to Use

**Use for**:
- Report generation from data
- Contract and invoice automation
- Template-based document creation
- Batch document processing

**Avoid for**:
- Simple template filling (use docx-templates)
- PDF-only output (use pypdf)
- Complex macros/VBA

## Common Patterns

### Table with Header Styling
```python
table = doc.add_table(rows=4, cols=3)
for i, header in enumerate(['ID', 'Name', 'Value']):
    cell = table.rows[0].cells[i]
    cell.text = header
    cell.paragraphs[0].runs[0].bold = True
```

### Page Setup
```python
section = doc.sections[0]
section.page_width = Inches(8.5)
section.page_height = Inches(11)
section.left_margin = Inches(1)
```

## Files

```
python-docx/
  SKILL.md    # Full documentation
  README.md   # This file
```

## Related Skills

- **docx-templates** - Jinja2-style templates
- **openpyxl** - Excel automation
- **pypdf** - PDF manipulation

## Resources

- [Documentation](https://python-docx.readthedocs.io/)
- [GitHub](https://github.com/python-openxml/python-docx)

---

See `SKILL.md` for complete examples and advanced patterns.
