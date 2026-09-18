"""Static, complete PDF edition of the vessel HTML report.

Optional runtime: reportlab, svglib and lxml. All details are expanded.
Usage: python example_vessel_pdf.py source.html output.pdf
"""
import io
import os
import re
import sys
from html import escape
from pathlib import Path

from lxml import etree, html
from reportlab.lib import colors
from reportlab.lib.enums import TA_LEFT
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.units import mm
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.platypus import (BaseDocTemplate, Frame, Image, KeepTogether,
    PageBreak, PageTemplate, Paragraph, Spacer, Table, TableStyle)
from reportlab.platypus.tableofcontents import TableOfContents
from svglib.svglib import svg2rlg

NAVY = colors.HexColor('#102f48')
WIDTH = A4[0] - 36 * mm


def styles():
    font_dir = Path(os.environ.get('WINDIR', '/usr/share')) / 'Fonts'
    for name, file in [('Report', 'arial.ttf'), ('Report-Bold', 'arialbd.ttf'),
                       ('Report-Italic', 'ariali.ttf'), ('Report-BoldItalic', 'arialbi.ttf')]:
        pdfmetrics.registerFont(TTFont(name, str(font_dir / file)))
    pdfmetrics.registerFontFamily('Report', normal='Report', bold='Report-Bold',
                                  italic='Report-Italic', boldItalic='Report-BoldItalic')
    base = getSampleStyleSheet()
    result = {}
    for name, size, lead in [('body', 9, 13), ('small', 7.5, 10), ('cell', 7.1, 9.7),
                             ('h1', 28, 34), ('h2', 17, 22), ('h3', 12, 16), ('h4', 10, 14)]:
        result[name] = ParagraphStyle(name, parent=base['Normal'], fontName='Report',
            fontSize=size, leading=lead, textColor=NAVY, spaceAfter=7,
            alignment=TA_LEFT, splitLongWords=True)
        if name.startswith('h'):
            result[name].fontName = 'Report-Bold'
            result[name].keepWithNext = True
            result[name].spaceBefore = 10
    return result


def inline(node):
    text = escape(node.text or '')
    for child in node:
        tag = etree.QName(child).localname if isinstance(child.tag, str) else ''
        value = inline(child)
        if tag in ('strong', 'b'):
            value = '<b>' + value + '</b>'
        elif tag in ('em', 'i'):
            value = '<i>' + value + '</i>'
        elif tag in ('sub', 'sup'):
            value = '<' + tag + '>' + value + '</' + tag + '>'
        elif tag == 'br':
            value = '<br/>'
        elif tag == 'a' and child.get('href') and not child.get('href').startswith('#'):
            value = '<link href="' + escape(child.get('href'), quote=True) + '">' + value + '</link>'
        text += value + escape(child.tail or '')
    return re.sub(r'[\u2010-\u2014]', '-', text)


class ReportDoc(BaseDocTemplate):
    def __init__(self, path):
        super().__init__(str(path), pagesize=A4, leftMargin=18*mm, rightMargin=18*mm,
                         topMargin=20*mm, bottomMargin=18*mm, title='Pressure-vessel FFS assessment',
                         author='AceEngineer')
        frame = Frame(self.leftMargin, self.bottomMargin, self.width, self.height,
                      leftPadding=0, rightPadding=0, topPadding=0, bottomPadding=0)
        self.addPageTemplates(PageTemplate(id='report', frames=frame, onPage=self.furniture))

    def furniture(self, canvas, doc):
        canvas.saveState()
        canvas.setFillColor(NAVY)
        canvas.setFont('Report', 8)
        canvas.drawString(18*mm, A4[1]-12*mm, 'ACEENGINEER  |  Pressure-vessel fitness-for-service assessment')
        canvas.setStrokeColor(colors.HexColor('#cbd9e3'))
        canvas.line(18*mm, 14*mm, A4[0]-18*mm, 14*mm)
        canvas.setFont('Report', 7)
        canvas.drawString(18*mm, 10*mm, 'Simulated inputs | Internal technical review | Revision 02')
        canvas.drawRightString(A4[0]-18*mm, 10*mm, str(doc.page))
        canvas.restoreState()

    def afterFlowable(self, flowable):
        if hasattr(flowable, 'bookmark'):
            key, title = flowable.bookmark
            self.canv.bookmarkPage(key)
            self.canv.addOutlineEntry(title, key, 0, False)
            self.notify('TOCEntry', (0, title, self.page, key))


class Converter:
    def __init__(self, source):
        self.source = source
        self.styles = styles()

    def paragraph(self, node, style='body'):
        return Paragraph(inline(node), self.styles[style])

    def table(self, node):
        rows = node.xpath('./tr | ./thead/tr | ./tbody/tr | ./tfoot/tr')
        cells = [[self.paragraph(cell, 'cell') for cell in row if cell.tag in ('td', 'th')]
                 for row in rows]
        if not cells:
            return []
        count = max(map(len, cells))
        widths = [WIDTH/count] * count
        if count == 2:
            widths = [WIDTH*.30, WIDTH*.70]
        elif count >= 8:
            widths = [WIDTH*.76/(count-1)] * (count-1) + [WIDTH*.24]
        table = Table(cells, colWidths=widths, repeatRows=1, hAlign='LEFT')
        table.setStyle(TableStyle([('VALIGN', (0,0), (-1,-1), 'TOP'),
            ('BACKGROUND', (0,0), (-1,0), colors.HexColor('#e6eef4')),
            ('ROWBACKGROUNDS', (0,1), (-1,-1), [colors.white, colors.HexColor('#f6f8fa')]),
            ('LINEBELOW', (0,0), (-1,-1), .3, colors.HexColor('#d8e2eb')),
            ('LEFTPADDING', (0,0), (-1,-1), 5), ('RIGHTPADDING', (0,0), (-1,-1), 5),
            ('TOPPADDING', (0,0), (-1,-1), 6), ('BOTTOMPADDING', (0,0), (-1,-1), 6)]))
        return [table, Spacer(1, 5)]

    def graphic(self, node):
        if node.tag == 'img':
            path = (self.source.parent / node.get('src')).resolve(strict=True)
            drawing = svg2rlg(str(path)) if path.suffix == '.svg' else Image(str(path))
        else:
            copy = etree.fromstring(etree.tostring(node))
            copy.set('xmlns', 'http://www.w3.org/2000/svg')
            view_box = copy.get('viewBox', copy.get('viewbox'))
            if view_box:
                copy.set('viewBox', view_box)
                bounds = view_box.split()
                copy.set('width', bounds[2])
                copy.set('height', bounds[3])
            for elem in copy.iter():
                if elem.tag == 'text':
                    elem.set('font-family', 'Arial')
                    elem.set('font-size', elem.get('font-size', '12'))
                    elem.set('fill', '#183044')
                if node.get('class') != 'grid' and elem.tag == 'rect':
                    elem.set('fill', elem.get('fill', '#e7f0f5'))
                    elem.set('stroke', elem.get('stroke', '#315c76'))
                if elem.tag == 'path':
                    elem.set('fill', elem.get('fill', 'none'))
                    elem.set('stroke', elem.get('stroke', '#315c76'))
            drawing = svg2rlg(io.BytesIO(etree.tostring(copy)))
        if drawing is None:
            raise ValueError('Unsupported or unreadable graphic')
        width = drawing.imageWidth if isinstance(drawing, Image) else drawing.width
        height = drawing.imageHeight if isinstance(drawing, Image) else drawing.height
        factor = min(WIDTH/width, 165*mm/height)
        if isinstance(drawing, Image):
            drawing.drawWidth, drawing.drawHeight = width*factor, height*factor
        else:
            drawing.scale(factor, factor)
            drawing.width, drawing.height = width*factor, height*factor
        return [drawing, Spacer(1, 5)]

    def text_block(self, node):
        tag = node.tag
        style = tag if tag.startswith('h') else 'small' if tag == 'figcaption' else 'body'
        if node.get('class') == 'caption':
            style = 'small'
        para = self.paragraph(node, style)
        if tag == 'h2':
            title = node.text_content().strip()
            para.bookmark = (node.getparent().get('id', re.sub(r'\W+', '-', title)), title)
        if tag == 'h3' and node.text_content().startswith('6.1 Model'):
            return [PageBreak(), para]
        return [para]

    def walk(self, node):
        tag = node.tag
        if tag in ('script', 'style', 'footer'):
            return []
        if tag == 'nav':
            toc = TableOfContents()
            toc.levelStyles = [self.styles['body']]
            return [Paragraph('Contents', self.styles['h2']), toc]
        if tag == 'table':
            return self.table(node)
        if tag == 'div' and (node.text or '').strip():
            return [self.paragraph(node)]
        if tag in ('img', 'svg'):
            return self.graphic(node)
        if tag in ('h1', 'h2', 'h3', 'h4', 'p', 'figcaption', 'summary', 'li'):
            return self.text_block(node)
        result = []
        if tag == 'section' or 'frontmatter' in node.get('class', ''):
            result.append(PageBreak())
        for child in node:
            converted = self.walk(child)
            if child.get('class') == 'caption' and len(result) >= 2:
                previous = result[-2]
                small_table = isinstance(previous, Table) and len(previous._cellvalues) <= 6
                graphic = hasattr(previous, 'renderScale') or isinstance(previous, Image)
                if isinstance(result[-1], Spacer) and (small_table or graphic):
                    start = -2
                    if len(result) >= 3 and isinstance(result[-3], Paragraph):
                        if result[-3].style.name in ('h3', 'h4'):
                            start = -3
                    result[start:] = [KeepTogether(result[start:] + converted)]
                    continue
            result.extend(converted)
        if 'cover' == node.get('class') and 'No field measurements were supplied' not in node.text_content():
            result.append(Paragraph('Illustrative measurement-style thickness grids; all values are assumed. '
                'No field measurements were supplied.', self.styles['body']))
        if tag == 'figure':
            return [KeepTogether(result)]
        if node.get('class') == 'case-block':
            return [PageBreak(), KeepTogether(result)]
        return result


def export_pdf(source, output):
    source, output = Path(source), Path(output)
    root = html.fromstring(source.read_text(encoding='utf-8'))
    main = root if root.tag == 'main' else root.xpath('//main')[0]
    evidence = main.xpath('.//section[@id="appendix-b"]')
    if evidence:
        note = etree.Element('p')
        note.text = ('Evidence links refer to private local files relative to this report. '
                     'The accompanying repository folders and authorized access are required.')
        evidence[0].insert(1, note)
    story = Converter(source).walk(main)
    story = [item for i, item in enumerate(story) if not (
        isinstance(item, PageBreak) and i and isinstance(story[i-1], PageBreak))]
    while story and isinstance(story[0], PageBreak):
        story.pop(0)
    output.parent.mkdir(parents=True, exist_ok=True)
    ReportDoc(output).multiBuild(story)


if __name__ == '__main__':
    export_pdf(*sys.argv[1:])
