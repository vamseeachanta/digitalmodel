# Office Docs Skills Library

> Office document automation, template generation, and document processing patterns
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 5 production-ready skills for automating Microsoft Office and PDF document workflows. Each skill covers a specific document type with patterns for generation, manipulation, and template-based automation. Skills follow the Anthropic Skills format with practical examples from real-world document processing pipelines.

## Quick Start

```bash
# Browse available skills
ls skills/office-docs/

# Read a skill
cat skills/office-docs/python-docx/SKILL.md

# Skills are documentation - implement patterns in your document workflows
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [python-docx](./python-docx/SKILL.md) | Word document creation and manipulation | Paragraphs, tables, images, styles |
| [openpyxl](./openpyxl/SKILL.md) | Excel workbook automation | Cells, formulas, charts, formatting |
| [python-pptx](./python-pptx/SKILL.md) | PowerPoint presentation generation | Slides, shapes, layouts, animations |
| [pypdf](./pypdf/SKILL.md) | PDF manipulation and extraction | Merge, split, extract, watermark |
| [docx-templates](./docx-templates/SKILL.md) | Template-based document generation | Jinja2 syntax, loops, conditionals |

## Skill Categories

### Document Creation
- **python-docx** - Create and modify Word documents programmatically
- **python-pptx** - Generate PowerPoint presentations from data

### Spreadsheet Automation
- **openpyxl** - Full Excel workbook manipulation with formulas

### PDF Processing
- **pypdf** - Read, merge, split, and modify PDF files

### Template Engines
- **docx-templates** - Fill Word templates with dynamic data

## Skill Selection Guide

### Choose python-docx when:
- Creating reports, contracts, or documentation from scratch
- Modifying existing Word documents programmatically
- Need full control over document structure and styling
- Generating documents with complex formatting requirements

### Choose openpyxl when:
- Automating Excel report generation
- Reading and processing spreadsheet data
- Creating workbooks with formulas and charts
- Batch processing multiple Excel files

### Choose python-pptx when:
- Generating presentations from data automatically
- Creating slide decks with consistent branding
- Building presentation templates with dynamic content
- Automating reporting dashboards as slides

### Choose pypdf when:
- Merging multiple PDFs into one document
- Extracting text or pages from PDF files
- Adding watermarks or page numbers
- Splitting large PDFs into smaller files

### Choose docx-templates when:
- Filling template documents with database records
- Generating bulk documents (letters, invoices, contracts)
- Non-developers need to maintain document templates
- Jinja2-style syntax is preferred for templates

## Quick Examples

### Python-docx Report Generation
```python
from docx import Document
from docx.shared import Inches, Pt
from docx.enum.text import WD_ALIGN_PARAGRAPH

# Create document
doc = Document()

# Add title
title = doc.add_heading('Monthly Report', level=0)
title.alignment = WD_ALIGN_PARAGRAPH.CENTER

# Add paragraph with formatting
para = doc.add_paragraph()
run = para.add_run('Executive Summary: ')
run.bold = True
para.add_run('This report covers key metrics for January 2026.')

# Add table
table = doc.add_table(rows=4, cols=3)
table.style = 'Table Grid'

# Header row
headers = ['Metric', 'Value', 'Change']
for i, header in enumerate(headers):
    cell = table.rows[0].cells[i]
    cell.text = header
    cell.paragraphs[0].runs[0].bold = True

# Data rows
data = [
    ['Revenue', '$1.2M', '+15%'],
    ['Users', '45,000', '+8%'],
    ['Retention', '92%', '+3%']
]
for row_idx, row_data in enumerate(data, start=1):
    for col_idx, value in enumerate(row_data):
        table.rows[row_idx].cells[col_idx].text = value

# Add image
doc.add_picture('chart.png', width=Inches(5))

# Save
doc.save('monthly_report.docx')
```

### Openpyxl Excel Automation
```python
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment
from openpyxl.chart import BarChart, Reference

# Create workbook
wb = Workbook()
ws = wb.active
ws.title = "Sales Data"

# Headers with styling
headers = ['Product', 'Q1', 'Q2', 'Q3', 'Q4', 'Total']
header_fill = PatternFill(start_color="4472C4", end_color="4472C4", fill_type="solid")
header_font = Font(color="FFFFFF", bold=True)

for col, header in enumerate(headers, start=1):
    cell = ws.cell(row=1, column=col, value=header)
    cell.fill = header_fill
    cell.font = header_font
    cell.alignment = Alignment(horizontal='center')

# Data with formulas
data = [
    ['Widget A', 100, 150, 200, 180],
    ['Widget B', 80, 120, 140, 160],
    ['Widget C', 200, 220, 250, 280]
]

for row_idx, row_data in enumerate(data, start=2):
    for col_idx, value in enumerate(row_data, start=1):
        ws.cell(row=row_idx, column=col_idx, value=value)
    # Total formula
    ws.cell(row=row_idx, column=6, value=f'=SUM(B{row_idx}:E{row_idx})')

# Create chart
chart = BarChart()
chart.title = "Quarterly Sales"
chart.x_axis.title = "Product"
chart.y_axis.title = "Units"

data_ref = Reference(ws, min_col=2, max_col=5, min_row=1, max_row=4)
cats_ref = Reference(ws, min_col=1, min_row=2, max_row=4)
chart.add_data(data_ref, titles_from_data=True)
chart.set_categories(cats_ref)

ws.add_chart(chart, "H2")

wb.save('sales_report.xlsx')
```

### Python-pptx Presentation
```python
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.enum.text import PP_ALIGN
from pptx.dml.color import RgbColor

# Create presentation
prs = Presentation()

# Title slide
title_layout = prs.slide_layouts[0]
slide = prs.slides.add_slide(title_layout)
slide.shapes.title.text = "Q1 2026 Results"
slide.placeholders[1].text = "Company Performance Review"

# Content slide with bullet points
bullet_layout = prs.slide_layouts[1]
slide = prs.slides.add_slide(bullet_layout)
slide.shapes.title.text = "Key Highlights"

body = slide.placeholders[1]
tf = body.text_frame
tf.text = "Revenue up 15% YoY"

for point in ["Customer base grew 8%", "New product launch successful", "Expanded to 3 new markets"]:
    p = tf.add_paragraph()
    p.text = point
    p.level = 1

# Slide with chart placeholder
chart_layout = prs.slide_layouts[5]
slide = prs.slides.add_slide(chart_layout)
slide.shapes.title.text = "Sales Breakdown"

# Add image
slide.shapes.add_picture(
    'sales_chart.png',
    Inches(1), Inches(2),
    width=Inches(8)
)

prs.save('quarterly_results.pptx')
```

### PyPDF Manipulation
```python
from pypdf import PdfReader, PdfWriter, PdfMerger

# Merge PDFs
merger = PdfMerger()
merger.append('report_part1.pdf')
merger.append('report_part2.pdf')
merger.append('appendix.pdf')
merger.write('complete_report.pdf')
merger.close()

# Split PDF
reader = PdfReader('large_document.pdf')
for i, page in enumerate(reader.pages):
    writer = PdfWriter()
    writer.add_page(page)
    writer.write(f'page_{i+1}.pdf')

# Extract text
reader = PdfReader('document.pdf')
text = ""
for page in reader.pages:
    text += page.extract_text()

# Add watermark
reader = PdfReader('document.pdf')
watermark = PdfReader('watermark.pdf')
writer = PdfWriter()

for page in reader.pages:
    page.merge_page(watermark.pages[0])
    writer.add_page(page)

writer.write('watermarked_document.pdf')
```

### Docx-Templates Bulk Generation
```python
from docxtpl import DocxTemplate

# Load template
doc = DocxTemplate("contract_template.docx")

# Context for template
context = {
    'client_name': 'Acme Corporation',
    'contract_date': '2026-01-17',
    'contract_value': '$50,000',
    'terms': '12 months',
    'services': [
        {'name': 'Consulting', 'hours': 100, 'rate': '$200'},
        {'name': 'Development', 'hours': 200, 'rate': '$150'},
        {'name': 'Support', 'hours': 50, 'rate': '$100'}
    ],
    'include_sla': True,
    'sla_response_time': '4 hours'
}

# Render and save
doc.render(context)
doc.save('acme_contract.docx')

# Bulk generation
clients = load_clients_from_database()
for client in clients:
    doc = DocxTemplate("invoice_template.docx")
    doc.render(client.to_dict())
    doc.save(f'invoices/{client.id}_invoice.docx')
```

## Integration Patterns

### Document Generation Pipeline
```
Data Source --> Template Selection --> Rendering --> Post-processing --> Output
     |                 |                   |              |               |
     +-- Database     +-- By type         +-- Variables +-- Convert     +-- Save
     +-- API          +-- By language     +-- Loops     +-- Combine     +-- Email
     +-- Form         +-- By recipient    +-- Images    +-- Sign        +-- Archive
```

### Batch Processing Pattern
```
Input Files --> Validation --> Processing --> Quality Check --> Output
     |              |              |               |              |
     +-- Queue     +-- Format    +-- Transform   +-- Verify     +-- Organize
     +-- Watch     +-- Content   +-- Extract     +-- Log        +-- Notify
```

### Template Management
```
Template Repo --> Version Control --> Environment --> Runtime
      |               |                  |              |
      +-- Design     +-- Review         +-- Dev/Prod   +-- Hot reload
      +-- Test       +-- Approve        +-- Variables  +-- Fallbacks
```

## Common Patterns Across Skills

### Error Handling
```python
from pathlib import Path

def safe_document_generation(template_path, context, output_path):
    """Generate document with comprehensive error handling."""
    try:
        if not Path(template_path).exists():
            raise FileNotFoundError(f"Template not found: {template_path}")

        doc = DocxTemplate(template_path)
        doc.render(context)

        # Ensure output directory exists
        Path(output_path).parent.mkdir(parents=True, exist_ok=True)
        doc.save(output_path)

        return {"success": True, "path": output_path}

    except Exception as e:
        logger.error(f"Document generation failed: {e}")
        return {"success": False, "error": str(e)}
```

### Temporary File Handling
```python
import tempfile
from contextlib import contextmanager

@contextmanager
def temp_document():
    """Create temporary document that auto-cleans."""
    with tempfile.NamedTemporaryFile(suffix='.docx', delete=False) as f:
        temp_path = f.name
    try:
        yield temp_path
    finally:
        Path(temp_path).unlink(missing_ok=True)
```

### Streaming Large Files
```python
def process_large_excel(file_path, chunk_size=1000):
    """Process large Excel files in chunks."""
    from openpyxl import load_workbook

    wb = load_workbook(file_path, read_only=True)
    ws = wb.active

    chunk = []
    for row in ws.iter_rows(values_only=True):
        chunk.append(row)
        if len(chunk) >= chunk_size:
            yield chunk
            chunk = []

    if chunk:
        yield chunk
```

## Integration with Workspace-Hub

These skills power document automation across the workspace-hub ecosystem:

```
workspace-hub/
├── documents/
│   ├── templates/           # Uses: docx-templates
│   │   ├── contracts/
│   │   ├── invoices/
│   │   └── reports/
│   ├── generators/          # Uses: python-docx, openpyxl, python-pptx
│   │   ├── report_builder.py
│   │   ├── spreadsheet_gen.py
│   │   └── presentation_gen.py
│   └── processors/          # Uses: pypdf
│       ├── pdf_merger.py
│       └── text_extractor.py
├── output/
│   ├── generated/
│   └── archive/
└── config/
    └── document_config.yaml
```

## Best Practices

### 1. Template Versioning
```
templates/
├── v1/
│   └── contract_template.docx
├── v2/
│   └── contract_template.docx  # Updated branding
└── current -> v2/              # Symlink to current version
```

### 2. Validation Before Generation
```python
def validate_context(context, required_fields):
    """Validate context before document generation."""
    missing = [f for f in required_fields if f not in context or not context[f]]
    if missing:
        raise ValueError(f"Missing required fields: {missing}")
    return True
```

### 3. Output Organization
```python
from datetime import datetime

def get_output_path(doc_type, client_id, extension='docx'):
    """Generate organized output path."""
    date_str = datetime.now().strftime('%Y/%m/%d')
    return f"output/{doc_type}/{date_str}/{client_id}.{extension}"
```

### 4. Logging and Audit Trail
```python
def generate_with_audit(template, context, output_path):
    """Generate document with audit logging."""
    start_time = time.time()

    result = generate_document(template, context, output_path)

    audit_log.info({
        'action': 'document_generated',
        'template': template,
        'output': output_path,
        'duration': time.time() - start_time,
        'context_keys': list(context.keys())
    })

    return result
```

## Testing Document Generation

```python
import pytest
from docx import Document

def test_report_generation():
    """Test report document structure."""
    generate_report(sample_data, 'test_output.docx')

    doc = Document('test_output.docx')

    # Verify structure
    assert len(doc.paragraphs) > 0
    assert doc.paragraphs[0].text == 'Monthly Report'

    # Verify tables
    assert len(doc.tables) == 1
    assert len(doc.tables[0].rows) == 4

def test_template_rendering():
    """Test template variable substitution."""
    context = {'name': 'Test Corp', 'amount': '$1000'}

    doc = DocxTemplate('template.docx')
    doc.render(context)
    doc.save('output.docx')

    result = Document('output.docx')
    full_text = '\n'.join([p.text for p in result.paragraphs])

    assert 'Test Corp' in full_text
    assert '$1000' in full_text

def test_pdf_merge():
    """Test PDF merging preserves pages."""
    merge_pdfs(['doc1.pdf', 'doc2.pdf'], 'merged.pdf')

    reader = PdfReader('merged.pdf')
    assert len(reader.pages) == 4  # 2 + 2 pages
```

## Related Resources

- [python-docx Documentation](https://python-docx.readthedocs.io/)
- [openpyxl Documentation](https://openpyxl.readthedocs.io/)
- [python-pptx Documentation](https://python-pptx.readthedocs.io/)
- [pypdf Documentation](https://pypdf.readthedocs.io/)
- [docxtpl Documentation](https://docxtpl.readthedocs.io/)

## Version History

- **1.0.0** (2026-01-17): Initial release with 5 office document skills

---

*These skills represent patterns refined across document automation systems generating thousands of documents daily in production environments.*
