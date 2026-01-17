# PyPDF PDF Manipulation Skill

> Quick Reference Guide for PDF Document Operations

## Overview

Manipulate PDF documents: merge, split, rotate, watermark, extract text, handle forms, and manage encryption.

**Version**: 1.0.0
**Category**: office-docs

## Quick Start

```bash
# Install
pip install pypdf

# With encryption support
pip install pypdf[crypto]

# With watermark creation
pip install pypdf reportlab
```

## Basic Usage

```python
from pypdf import PdfReader, PdfWriter, PdfMerger

# Merge PDFs
merger = PdfMerger()
merger.append('doc1.pdf')
merger.append('doc2.pdf')
merger.write('merged.pdf')
merger.close()

# Split PDF
reader = PdfReader('document.pdf')
for i, page in enumerate(reader.pages):
    writer = PdfWriter()
    writer.add_page(page)
    writer.write(f'page_{i+1}.pdf')

# Extract text
reader = PdfReader('document.pdf')
for page in reader.pages:
    print(page.extract_text())

# Rotate pages
reader = PdfReader('document.pdf')
writer = PdfWriter()
for page in reader.pages:
    page.rotate(90)
    writer.add_page(page)
writer.write('rotated.pdf')
```

## Key Features

| Feature | Description |
|---------|-------------|
| Merge | Combine multiple PDFs |
| Split | Extract pages to separate files |
| Rotate | Rotate pages 90/180/270 degrees |
| Watermark | Add text/image overlays |
| Extract | Get text content from pages |
| Encrypt | Password-protect PDFs |

## When to Use

**Use for**:
- Merging report sections
- Splitting large documents
- Adding watermarks/stamps
- Extracting text for analysis
- Form filling automation

**Avoid for**:
- Creating PDFs from scratch (use reportlab)
- OCR of scanned documents (use pytesseract)
- Complex text editing

## Common Patterns

### Add Watermark
```python
reader = PdfReader('document.pdf')
watermark = PdfReader('watermark.pdf').pages[0]
writer = PdfWriter()
for page in reader.pages:
    page.merge_page(watermark)
    writer.add_page(page)
writer.write('watermarked.pdf')
```

### Encrypt PDF
```python
writer = PdfWriter()
for page in PdfReader('document.pdf').pages:
    writer.add_page(page)
writer.encrypt('password')
writer.write('encrypted.pdf')
```

## Files

```
pypdf/
  SKILL.md    # Full documentation
  README.md   # This file
```

## Related Skills

- **reportlab** - Create PDFs from scratch
- **python-docx** - Word document handling
- **pillow** - Image processing

## Resources

- [Documentation](https://pypdf.readthedocs.io/)
- [GitHub](https://github.com/py-pdf/pypdf)

---

See `SKILL.md` for complete examples and advanced patterns.
