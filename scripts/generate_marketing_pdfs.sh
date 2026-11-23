#!/usr/bin/env bash
# Generate professional PDF brochures from markdown files

set -e  # Exit on error

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Paths
INPUT_DIR="$PROJECT_ROOT/reports/modules/marketing"
OUTPUT_DIR="$INPUT_DIR"
TEMPLATE_DIR="$PROJECT_ROOT/docs/templates"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}=================================${NC}"
echo -e "${BLUE}Marketing PDF Generator${NC}"
echo -e "${BLUE}=================================${NC}"
echo ""

# Check for pandoc
if ! command -v pandoc &> /dev/null; then
    echo -e "${RED}Error: pandoc is not installed${NC}"
    echo "Install with:"
    echo "  macOS: brew install pandoc"
    echo "  Linux: sudo apt-get install pandoc"
    echo "  or visit: https://pandoc.org/installing.html"
    exit 1
fi

# Check for wkhtmltopdf (better PDF generation)
if command -v wkhtmltopdf &> /dev/null; then
    PDF_ENGINE="--pdf-engine=wkhtmltopdf"
    echo -e "${GREEN}Using wkhtmltopdf for PDF generation${NC}"
else
    PDF_ENGINE=""
    echo -e "${BLUE}Using default pandoc PDF engine${NC}"
    echo "For better PDF quality, install wkhtmltopdf:"
    echo "  macOS: brew install wkhtmltopdf"
    echo "  Linux: sudo apt-get install wkhtmltopdf"
fi

echo ""

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Count markdown files
md_count=$(find "$INPUT_DIR" -name "marketing_brochure_*.md" | wc -l | tr -d ' ')
echo -e "Found ${GREEN}$md_count${NC} marketing brochures to convert"
echo ""

# Process each markdown file
success_count=0
fail_count=0

for md_file in "$INPUT_DIR"/marketing_brochure_*.md; do
    if [ -f "$md_file" ]; then
        # Get base filename
        base_name=$(basename "$md_file" .md)
        pdf_file="$OUTPUT_DIR/${base_name}.pdf"

        echo -e "${BLUE}Processing:${NC} $base_name"

        # Generate PDF with professional styling
        if pandoc "$md_file" \
            -o "$pdf_file" \
            --from markdown \
            --to pdf \
            $PDF_ENGINE \
            --variable geometry:margin=1in \
            --variable fontsize=11pt \
            --variable documentclass=article \
            --variable papersize=letter \
            --variable mainfont="Arial" \
            --variable colorlinks=true \
            --variable linkcolor=blue \
            --variable urlcolor=blue \
            --variable toccolor=black \
            --toc \
            --toc-depth=2 \
            --highlight-style=tango \
            2>&1 | grep -v "warning"; then

            echo -e "  ${GREEN}✓${NC} Generated: $pdf_file"
            ((success_count++))
        else
            echo -e "  ${RED}✗${NC} Failed to generate PDF"
            ((fail_count++))
        fi
        echo ""
    fi
done

# Summary
echo -e "${BLUE}=================================${NC}"
echo -e "${BLUE}Summary${NC}"
echo -e "${BLUE}=================================${NC}"
echo -e "Successfully generated: ${GREEN}$success_count${NC} PDFs"
if [ $fail_count -gt 0 ]; then
    echo -e "Failed: ${RED}$fail_count${NC} PDFs"
fi
echo -e "Output directory: ${BLUE}$OUTPUT_DIR${NC}"
echo ""

# List generated PDFs
echo -e "${BLUE}Generated PDF Files:${NC}"
ls -lh "$OUTPUT_DIR"/*.pdf 2>/dev/null || echo "No PDF files found"

echo ""
echo -e "${GREEN}PDF generation complete!${NC}"
