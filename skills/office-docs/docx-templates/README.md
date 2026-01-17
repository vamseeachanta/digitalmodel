# Docx-Templates

> Generate Word documents from templates using Jinja2 syntax. Create professional reports, contracts, and documents with dynamic content, loops, conditionals, and tables.

## Overview

Docx-templates (docxtpl) enables template-based document generation using Jinja2 syntax within Word documents. It preserves formatting while allowing dynamic content insertion, loops, conditionals, and tables.

## Quick Start

```bash
# Install
pip install docxtpl

# With image support
pip install docxtpl Pillow
```

```python
from docxtpl import DocxTemplate

# Load template
template = DocxTemplate("invoice_template.docx")

# Prepare data
context = {
    "customer_name": "John Smith",
    "invoice_number": "INV-001",
    "items": [
        {"name": "Widget", "qty": 10, "price": "$50.00"},
        {"name": "Gadget", "qty": 5, "price": "$100.00"}
    ],
    "total": "$1,000.00"
}

# Render and save
template.render(context)
template.save("invoice_output.docx")
```

## Key Features

| Feature | Description |
|---------|-------------|
| Jinja2 Syntax | Use familiar template syntax in Word |
| Loops | Iterate over lists for tables and content |
| Conditionals | Include/exclude content based on data |
| Tables | Generate dynamic tables from data |
| Images | Insert images with sizing control |
| Mail Merge | Batch generate from data sources |

## When to Use

**USE when:**
- Generating documents from templates with dynamic data
- Creating mail merge documents from CSV/Excel/database
- Building reports with loops and conditional sections
- Templates need professional formatting preserved
- Non-technical users maintain template design

**DON'T USE when:**
- Building documents programmatically from scratch (use python-docx)
- Need complex document manipulation beyond template filling
- PDF output is required directly

## Template Syntax

### Variables
```
Hello {{ customer_name }},
Your order #{{ order_number }} is confirmed.
```

### Loops (Table Rows)
```
{%tr for item in items %}
| {{ item.name }} | {{ item.qty }} | {{ item.price }} |
{%tr endfor %}
```

### Conditionals
```
{% if is_premium_customer %}
Thank you for being a premium customer!
{% else %}
Consider upgrading to premium for benefits.
{% endif %}
```

### Loops (Paragraphs)
```
{%p for point in bullet_points %}
- {{ point }}
{%p endfor %}
```

## Common Patterns

### Invoice Generation
```python
template = DocxTemplate("invoice.docx")
template.render({
    "invoice_number": "INV-001",
    "items": items_list,
    "subtotal": "$900.00",
    "tax": "$72.00",
    "total": "$972.00"
})
template.save("output.docx")
```

### Mail Merge from CSV
```python
import csv
from docxtpl import DocxTemplate

with open("customers.csv") as f:
    for record in csv.DictReader(f):
        template = DocxTemplate("letter.docx")
        template.render(record)
        template.save(f"letters/{record['id']}.docx")
```

### Adding Images
```python
from docxtpl import DocxTemplate, InlineImage
from docx.shared import Mm

template = DocxTemplate("report.docx")
image = InlineImage(template, "logo.png", width=Mm(50))
template.render({"logo": image, "title": "Report"})
template.save("output.docx")
```

## Related Skills

- [python-docx](../python-docx/SKILL.md) - Programmatic document building
- [openpyxl](../openpyxl/SKILL.md) - Excel file automation
- [pypdf](../pypdf/SKILL.md) - PDF manipulation

## Resources

- [docxtpl Docs](https://docxtpl.readthedocs.io/)
- [GitHub Repository](https://github.com/elapouya/python-docx-template)
- [Jinja2 Template Syntax](https://jinja.palletsprojects.com/)

---

**Version**: 1.0.0
**Category**: office-docs
