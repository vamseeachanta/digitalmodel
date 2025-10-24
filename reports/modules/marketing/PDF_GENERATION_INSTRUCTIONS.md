# PDF Generation Instructions

## Status

All marketing brochures are ready for PDF generation. The markdown files contain:
- ✅ **5 complete brochures** (40+ pages of content)
- ✅ **28 visual elements** (mermaid diagrams, ASCII art, images, tables)
- ✅ **3 real plot images** embedded in brochures
- ✅ **Professional formatting** ready for PDF conversion

## Files Ready for Conversion

| File | Size | Status | Visual Elements |
|------|------|--------|----------------|
| `marketing_brochure_fatigue_analysis.md` | 16 KB | ✅ Enhanced | 9 elements |
| `marketing_brochure_marine_analysis.md` | 17 KB | ✅ Enhanced | 10 elements |
| `marketing_brochure_stress_analysis.md` | 9.9 KB | ✅ Complete | Standard |
| `marketing_brochure_pipeline_analysis.md` | 9.7 KB | ✅ Complete | Standard |
| `marketing_brochure_orcaflex_integration.md` | 11 KB | ✅ Complete | Standard |

## Required Software

### Pandoc (Required)

Install pandoc for professional PDF generation:

**macOS:**
```bash
brew install pandoc
```

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install pandoc texlive-latex-base texlive-fonts-recommended texlive-latex-extra
```

**Windows:**
Download from: https://pandoc.org/installing.html

### Optional: Enhanced Quality (wkhtmltopdf)

For even better PDF quality:

**macOS:**
```bash
brew install wkhtmltopdf
```

**Ubuntu/Debian:**
```bash
sudo apt-get install wkhtmltopdf
```

## Generate PDFs

Once pandoc is installed:

```bash
cd /mnt/github/workspace-hub/digitalmodel
./scripts/generate_marketing_pdfs.sh
```

**Expected Output:**
- 5 professional PDFs in `reports/modules/marketing/`
- Filenames: `marketing_brochure_<module_name>.pdf`
- Professional styling: 1-inch margins, 11pt font, table of contents
- Embedded images and formatted tables
- Total size: ~2-3 MB per PDF with images

## PDF Generation Features

The script (`scripts/generate_marketing_pdfs.sh`) provides:
- ✅ Batch processing of all brochures
- ✅ Professional pandoc styling
- ✅ Automatic image embedding
- ✅ Table of contents generation (2-level depth)
- ✅ Monospace font preservation for ASCII art
- ✅ Color-coded progress output
- ✅ Error handling and summary

## Verify Installation

Check if pandoc is installed:

```bash
pandoc --version
```

Should show version 2.x or higher.

## Expected Results

After running the script, you'll have:

```
reports/modules/marketing/
├── marketing_brochure_fatigue_analysis.md
├── marketing_brochure_fatigue_analysis.pdf      ← Generated
├── marketing_brochure_marine_analysis.md
├── marketing_brochure_marine_analysis.pdf       ← Generated
├── marketing_brochure_stress_analysis.md
├── marketing_brochure_stress_analysis.pdf       ← Generated
├── marketing_brochure_pipeline_analysis.md
├── marketing_brochure_pipeline_analysis.pdf     ← Generated
├── marketing_brochure_orcaflex_integration.md
├── marketing_brochure_orcaflex_integration.pdf  ← Generated
└── images/
    ├── frequency_response_curves.png
    ├── hydrodynamic_coefficients.png
    └── polar_diagrams.png
```

## Troubleshooting

### Issue: "pandoc: command not found"
**Solution:** Install pandoc using instructions above

### Issue: Images not showing in PDF
**Solution:** Ensure `images/` directory is in the same location as markdown files

### Issue: ASCII art not aligned in PDF
**Solution:** Install full texlive package for better monospace font support:
```bash
sudo apt-get install texlive-full  # Linux
brew install basictex              # macOS
```

### Issue: Mermaid diagrams not rendering
**Solution:** Mermaid diagrams are displayed as code blocks in PDFs. To convert to images:
```bash
npm install -g @mermaid-js/mermaid-cli
mmdc -i diagram.mmd -o diagram.png
```

## Alternative: Online Conversion

If pandoc installation is not possible, you can:

1. **Use online converters:**
   - https://cloudconvert.com/md-to-pdf
   - https://www.markdowntopdf.com/

2. **Use markdown viewers:**
   - VS Code with Markdown Preview Enhanced extension
   - GitHub (renders markdown natively)
   - Typora (markdown editor with PDF export)

3. **Manual conversion:**
   - Open markdown file in any text editor
   - Copy content
   - Paste into Google Docs or Word
   - Export as PDF

## Next Steps

1. Install pandoc (see instructions above)
2. Run the PDF generation script
3. Review generated PDFs for quality
4. Distribute to engineering managers and marketing team
5. Post on company website or include in proposals

---

*Last Updated: 2025-10-23*
*Marketing Materials System v1.2*
