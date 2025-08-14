#!/usr/bin/env python3
"""
Test simple PDF generation without plotly images
"""

from reportlab.lib import colors
from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Paragraph, Spacer
from reportlab.lib.styles import getSampleStyleSheet
from reportlab.lib.units import inch
import tempfile
import os

def test_simple_pdf():
    """Test basic PDF generation"""
    print("Testing simple PDF generation...")
    
    # Create temporary file
    with tempfile.NamedTemporaryFile(delete=False, suffix='.pdf') as tmp:
        pdf_path = tmp.name
    
    # Create PDF
    doc = SimpleDocTemplate(pdf_path, pagesize=letter)
    styles = getSampleStyleSheet()
    story = []
    
    # Add title
    story.append(Paragraph("Test OrcaFlex Report", styles['Title']))
    story.append(Spacer(1, 0.5*inch))
    
    # Add table
    data = [
        ['Parameter', 'Value'],
        ['Max Tension', '8265.55 kN'],
        ['Critical Strut', 'Strut7'],
        ['Case ID', 'fsts_l015_hwl_ncl_240deg']
    ]
    
    table = Table(data)
    table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.grey),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 14),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black)
    ]))
    
    story.append(table)
    
    # Build PDF
    try:
        doc.build(story)
        print(f"[SUCCESS] PDF generated successfully: {pdf_path}")
        print(f"   File size: {os.path.getsize(pdf_path)} bytes")
        return True
    except Exception as e:
        print(f"[ERROR] Error generating PDF: {e}")
        return False

if __name__ == "__main__":
    test_simple_pdf()