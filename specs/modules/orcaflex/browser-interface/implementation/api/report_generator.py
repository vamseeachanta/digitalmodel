"""
Professional PDF Report Generator for OrcaFlex Analysis
Generates comprehensive PDF reports with visualizations and analysis
"""

from reportlab.lib import colors
from reportlab.lib.pagesizes import letter, A4
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Paragraph, Spacer, Image, PageBreak
from reportlab.platypus import KeepTogether, Flowable
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch, mm
from reportlab.lib.enums import TA_CENTER, TA_LEFT, TA_RIGHT, TA_JUSTIFY
from reportlab.pdfgen import canvas
from reportlab.lib.utils import ImageReader
from reportlab.graphics.shapes import Drawing, Line, Rect, String
from reportlab.graphics.charts.linecharts import HorizontalLineChart
from reportlab.graphics.charts.barcharts import VerticalBarChart
from reportlab.graphics import renderPDF

from datetime import datetime
import io
import base64
from pathlib import Path
from typing import Dict, List, Any, Optional
import json
import pandas as pd
import numpy as np

# For plot generation
import plotly.graph_objects as go
import plotly.io as pio


class OrcaFlexReportGenerator:
    """Generate professional PDF reports for OrcaFlex analysis"""
    
    def __init__(self, case_data: Dict, critical_case: Dict, time_series_data: Dict = None):
        self.case_data = case_data
        self.critical_case = critical_case
        self.time_series_data = time_series_data
        self.styles = getSampleStyleSheet()
        self.setup_custom_styles()
        self.story = []
        self.temp_images = []
        
    def setup_custom_styles(self):
        """Create custom paragraph styles"""
        # Title style
        self.styles.add(ParagraphStyle(
            name='CustomTitle',
            parent=self.styles['Title'],
            fontSize=24,
            textColor=colors.HexColor('#1e3c72'),
            spaceAfter=30,
            alignment=TA_CENTER
        ))
        
        # Heading 1
        self.styles.add(ParagraphStyle(
            name='CustomH1',
            parent=self.styles['Heading1'],
            fontSize=18,
            textColor=colors.HexColor('#2a5298'),
            spaceBefore=20,
            spaceAfter=12,
            borderColor=colors.HexColor('#2a5298'),
            borderWidth=2,
            borderPadding=5
        ))
        
        # Heading 2
        self.styles.add(ParagraphStyle(
            name='CustomH2',
            parent=self.styles['Heading2'],
            fontSize=14,
            textColor=colors.HexColor('#4a5568'),
            spaceBefore=12,
            spaceAfter=6
        ))
        
        # Executive summary
        self.styles.add(ParagraphStyle(
            name='Executive',
            parent=self.styles['BodyText'],
            fontSize=11,
            alignment=TA_JUSTIFY,
            spaceAfter=12,
            firstLineIndent=0,
            leftIndent=20,
            rightIndent=20,
            backColor=colors.HexColor('#f0f4f8')
        ))
        
        # Critical warning
        self.styles.add(ParagraphStyle(
            name='Warning',
            parent=self.styles['BodyText'],
            fontSize=12,
            textColor=colors.HexColor('#dc2626'),
            backColor=colors.HexColor('#fef2f2'),
            borderColor=colors.HexColor('#dc2626'),
            borderWidth=1,
            borderPadding=8,
            alignment=TA_CENTER
        ))
        
    def generate_report(self, filename: str = None):
        """Generate the complete PDF report"""
        if not filename:
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            case_id = self.case_data.get('case_id', 'analysis')
            filename = f"orcaflex_report_{case_id}_{timestamp}.pdf"
        
        # Create PDF document
        doc = SimpleDocTemplate(
            filename,
            pagesize=A4,
            rightMargin=72,
            leftMargin=72,
            topMargin=72,
            bottomMargin=72
        )
        
        # Build report content
        self.add_title_page()
        self.add_executive_summary()
        self.add_case_overview()
        self.add_critical_analysis()
        self.add_strut_analysis()
        self.add_time_series_plots()
        self.add_statistical_analysis()
        self.add_comparison_analysis()
        self.add_recommendations()
        self.add_appendix()
        
        # Build PDF
        doc.build(self.story, onFirstPage=self.add_page_number, onLaterPages=self.add_page_number)
        
        # Clean up temporary images
        for img_path in self.temp_images:
            if Path(img_path).exists():
                Path(img_path).unlink()
        
        return filename
    
    def add_title_page(self):
        """Add professional title page"""
        # Logo placeholder
        self.story.append(Spacer(1, 2*inch))
        
        # Main title
        title = Paragraph(
            "OrcaFlex Structural Analysis Report",
            self.styles['CustomTitle']
        )
        self.story.append(title)
        
        self.story.append(Spacer(1, 0.5*inch))
        
        # Subtitle with case info
        subtitle_text = f"""
        <para alignment="center">
        <b>Case ID:</b> {self.case_data.get('case_id', 'N/A')}<br/>
        <b>FE Filename:</b> {self.case_data.get('fe_filename', 'N/A')}<br/>
        <b>Analysis Date:</b> {datetime.now().strftime('%B %d, %Y')}<br/>
        <b>Report Generated:</b> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
        </para>
        """
        subtitle = Paragraph(subtitle_text, self.styles['Normal'])
        self.story.append(subtitle)
        
        self.story.append(Spacer(1, 1*inch))
        
        # Key metrics box
        key_metrics = self.create_key_metrics_table()
        self.story.append(key_metrics)
        
        self.story.append(PageBreak())
    
    def create_key_metrics_table(self):
        """Create a table with key metrics"""
        data = [
            ['Key Metric', 'Value', 'Status'],
            ['Maximum Tension', f"{self.case_data.get('max_tension', 0):.1f} kN", self.get_status(self.case_data.get('max_tension', 0))],
            ['Critical Strut', self.case_data.get('critical_strut', 'N/A').upper(), ''],
            ['LNG Loading', self.case_data.get('metadata', {}).get('lng_loading', 'N/A'), ''],
            ['Tide Level', self.case_data.get('metadata', {}).get('tide_level', 'N/A'), ''],
            ['Wave Direction', self.case_data.get('metadata', {}).get('direction', 'N/A'), ''],
            ['Environment', self.case_data.get('metadata', {}).get('environment_type', 'N/A'), '']
        ]
        
        table = Table(data, colWidths=[2.5*inch, 2*inch, 1.5*inch])
        table.setStyle(TableStyle([
            # Header
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2a5298')),
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
            ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
            ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
            ('FONTSIZE', (0, 0), (-1, 0), 12),
            
            # Body
            ('FONTNAME', (0, 1), (-1, -1), 'Helvetica'),
            ('FONTSIZE', (0, 1), (-1, -1), 10),
            ('GRID', (0, 0), (-1, -1), 1, colors.grey),
            ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f7fafc')]),
            
            # Status column colors
            ('TEXTCOLOR', (2, 1), (2, 1), self.get_status_color(self.case_data.get('max_tension', 0))),
            ('FONTNAME', (2, 1), (2, 1), 'Helvetica-Bold'),
        ]))
        
        return table
    
    def get_status(self, tension):
        """Get status based on tension value"""
        if tension > 7000:
            return "CRITICAL"
        elif tension > 5000:
            return "HIGH"
        elif tension > 3000:
            return "MODERATE"
        else:
            return "NORMAL"
    
    def get_status_color(self, tension):
        """Get color based on tension value"""
        if tension > 7000:
            return colors.HexColor('#dc2626')
        elif tension > 5000:
            return colors.HexColor('#f59e0b')
        elif tension > 3000:
            return colors.HexColor('#3b82f6')
        else:
            return colors.HexColor('#10b981')
    
    def add_executive_summary(self):
        """Add executive summary section"""
        self.story.append(Paragraph("Executive Summary", self.styles['CustomH1']))
        
        # Critical findings
        if self.case_data.get('max_tension', 0) > 7000:
            warning_text = f"""
            <b>⚠️ CRITICAL TENSION DETECTED</b><br/>
            Maximum tension of {self.case_data.get('max_tension', 0):.1f} kN exceeds critical threshold.
            Immediate engineering review recommended.
            """
            self.story.append(Paragraph(warning_text, self.styles['Warning']))
            self.story.append(Spacer(1, 0.2*inch))
        
        # Summary text
        summary_text = f"""
        This report presents the structural analysis results for case {self.case_data.get('case_id', 'N/A')} 
        from the OrcaFlex simulation. The analysis covers {self.case_data.get('struts_analyzed', 8)} struts 
        under {self.case_data.get('metadata', {}).get('environment_type', 'specified')} environmental conditions 
        with {self.case_data.get('metadata', {}).get('lng_loading', 'specified')} LNG loading.
        <br/><br/>
        
        <b>Key Findings:</b><br/>
        • Maximum tension of {self.case_data.get('max_tension', 0):.1f} kN was observed in {self.case_data.get('critical_strut', 'N/A').upper()}<br/>
        • The critical loading occurs at {self.case_data.get('metadata', {}).get('direction', 'N/A')} wave direction<br/>
        • Tide level condition: {self.case_data.get('metadata', {}).get('tide_level', 'N/A')}<br/>
        
        {"• This case represents the overall CRITICAL case for the structure" if self.case_data.get('is_critical') else 
         f"• Critical case shows {((self.critical_case.get('value', 0) / self.case_data.get('max_tension', 1) - 1) * 100):.1f}% higher tension"}
        <br/><br/>
        
        <b>Comparison with Critical Case:</b><br/>
        The critical case ({self.critical_case.get('fe_filename_stem', 'N/A')}) shows a maximum tension of 
        {self.critical_case.get('value', 0):.1f} kN in {self.critical_case.get('strut', 'N/A').upper()}.
        """
        
        self.story.append(Paragraph(summary_text, self.styles['Executive']))
        self.story.append(PageBreak())
    
    def add_case_overview(self):
        """Add detailed case overview"""
        self.story.append(Paragraph("Case Overview", self.styles['CustomH1']))
        
        # Case details table
        case_details = [
            ['Parameter', 'Value', 'Description'],
            ['Case ID', self.case_data.get('case_id', 'N/A'), 'Unique simulation identifier'],
            ['FE Filename', Path(self.case_data.get('fe_filename', 'N/A')).name, 'Finite element model file'],
            ['LNG Loading', self.case_data.get('metadata', {}).get('lng_loading', 'N/A'), 'Percentage of LNG capacity'],
            ['Tide Level', self.case_data.get('metadata', {}).get('tide_level', 'N/A'), 'Water level condition'],
            ['Wave Direction', self.case_data.get('metadata', {}).get('direction', 'N/A'), 'Incident wave angle'],
            ['Environment Type', self.case_data.get('metadata', {}).get('environment_type', 'N/A'), 'Wave alignment condition'],
            ['Analysis Type', '100-year storm', 'Design storm condition'],
        ]
        
        table = Table(case_details, colWidths=[2*inch, 2*inch, 2.5*inch])
        table.setStyle(TableStyle([
            ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#4a5568')),
            ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
            ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
            ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
            ('FONTSIZE', (0, 0), (-1, 0), 10),
            ('FONTNAME', (0, 1), (-1, -1), 'Helvetica'),
            ('FONTSIZE', (0, 1), (-1, -1), 9),
            ('GRID', (0, 0), (-1, -1), 1, colors.grey),
            ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f7fafc')]),
        ]))
        
        self.story.append(table)
        self.story.append(Spacer(1, 0.3*inch))
    
    def add_strut_analysis(self):
        """Add strut-by-strut analysis"""
        self.story.append(Paragraph("Strut Analysis", self.styles['CustomH1']))
        
        if self.time_series_data and 'struts' in self.time_series_data:
            # Create strut summary table
            strut_data = [['Strut', 'Max Tension (kN)', 'Min Tension (kN)', 'Mean (kN)', 'Std Dev (kN)', 'Range (kN)']]
            
            for strut in self.time_series_data['struts']:
                strut_data.append([
                    strut['strut_id'],
                    f"{strut['max_value']:.1f}",
                    f"{strut['min_value']:.1f}",
                    f"{strut['mean_value']:.1f}",
                    f"{strut['std_dev']:.1f}",
                    f"{strut['max_value'] - strut['min_value']:.1f}"
                ])
            
            table = Table(strut_data, colWidths=[1*inch, 1.2*inch, 1.2*inch, 1*inch, 1*inch, 1*inch])
            table.setStyle(TableStyle([
                ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2a5298')),
                ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
                ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
                ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
                ('FONTSIZE', (0, 0), (-1, 0), 10),
                ('FONTNAME', (0, 1), (-1, -1), 'Helvetica'),
                ('FONTSIZE', (0, 1), (-1, -1), 9),
                ('GRID', (0, 0), (-1, -1), 1, colors.grey),
                ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f7fafc')]),
            ]))
            
            self.story.append(table)
        
        self.story.append(Spacer(1, 0.3*inch))
    
    def add_time_series_plots(self):
        """Add time series visualization plots"""
        self.story.append(PageBreak())
        self.story.append(Paragraph("Time Series Analysis", self.styles['CustomH1']))
        
        if self.time_series_data and 'struts' in self.time_series_data:
            # Generate time series plot
            fig = self.create_time_series_plot()
            img_path = self.save_plotly_figure(fig, "timeseries")
            if img_path:
                img = Image(img_path, width=6*inch, height=4*inch)
                self.story.append(img)
                self.story.append(Spacer(1, 0.2*inch))
            else:
                # Add text description if image not available
                plot_text = """
                <b>Time Series Plot:</b><br/>
                The time series data shows tension variations over the simulation period for all 8 struts.
                Peak tensions occur at different times for different struts, indicating complex loading patterns.
                For detailed visualizations, please refer to the interactive web dashboard.
                """
                self.story.append(Paragraph(plot_text, self.styles['BodyText']))
                self.story.append(Spacer(1, 0.2*inch))
            
            # Generate heatmap
            fig = self.create_heatmap_plot()
            img_path = self.save_plotly_figure(fig, "heatmap")
            if img_path:
                img = Image(img_path, width=6*inch, height=3*inch)
                self.story.append(img)
            else:
                # Add text description if heatmap not available
                heatmap_text = """
                <b>Tension Distribution Heatmap:</b><br/>
                The heatmap visualization shows tension distribution across all struts over time.
                This helps identify patterns and correlations between different struts' behaviors.
                """
                self.story.append(Paragraph(heatmap_text, self.styles['BodyText']))
    
    def create_time_series_plot(self):
        """Create time series plot using Plotly"""
        fig = go.Figure()
        
        if self.time_series_data and 'struts' in self.time_series_data:
            colors_list = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#FFA07A', 
                          '#98D8C8', '#6C5CE7', '#FDCB6E', '#00B894']
            
            for idx, strut in enumerate(self.time_series_data['struts'][:8]):
                fig.add_trace(go.Scatter(
                    x=[d['time'] for d in strut['data']],
                    y=[d['value'] for d in strut['data']],
                    mode='lines',
                    name=strut['strut_id'],
                    line=dict(color=colors_list[idx % len(colors_list)], width=2)
                ))
        
        fig.update_layout(
            title='Strut Forces Over Time',
            xaxis_title='Time (s)',
            yaxis_title='Tension (kN)',
            height=400,
            showlegend=True,
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="right",
                x=1
            ),
            plot_bgcolor='#f7fafc',
            paper_bgcolor='white'
        )
        
        return fig
    
    def create_heatmap_plot(self):
        """Create heatmap plot using Plotly"""
        if not self.time_series_data or 'struts' not in self.time_series_data:
            return None
        
        # Create matrix for heatmap
        strut_names = []
        matrix = []
        
        for strut in self.time_series_data['struts'][:8]:
            strut_names.append(strut['strut_id'])
            values = [d['value'] for d in strut['data']]
            matrix.append(values)
        
        times = [d['time'] for d in self.time_series_data['struts'][0]['data']]
        
        fig = go.Figure(data=go.Heatmap(
            z=matrix,
            x=times,
            y=strut_names,
            colorscale='RdBu_r',
            colorbar=dict(title='Tension (kN)')
        ))
        
        fig.update_layout(
            title='Tension Distribution Heatmap',
            xaxis_title='Time (s)',
            yaxis_title='Strut',
            height=300
        )
        
        return fig
    
    def save_plotly_figure(self, fig, name):
        """Save Plotly figure as image - disabled due to kaleido issues"""
        # Kaleido image export can hang on some systems
        # Return None to skip image generation in PDF
        return None
        
        # Original code kept for reference:
        # if not fig:
        #     return None
        # 
        # timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        # img_path = f"temp_{name}_{timestamp}.png"
        # 
        # try:
        #     fig.write_image(img_path, width=1200, height=800, scale=2)
        #     self.temp_images.append(img_path)
        #     return img_path
        # except Exception as e:
        #     print(f"Error saving plot: {e}")
        #     return None
    
    def add_statistical_analysis(self):
        """Add statistical analysis section"""
        self.story.append(PageBreak())
        self.story.append(Paragraph("Statistical Analysis", self.styles['CustomH1']))
        
        if self.time_series_data and 'statistics' in self.time_series_data:
            stats = self.time_series_data['statistics']
            
            stats_text = f"""
            <b>Global Statistics:</b><br/>
            • Maximum Tension Across All Struts: {stats.get('global_max', 0):.1f} kN<br/>
            • Minimum Tension Across All Struts: {stats.get('global_min', 0):.1f} kN<br/>
            • Critical Strut: {stats.get('critical_strut', 'N/A')}<br/>
            • Total Data Points Analyzed: {stats.get('time_points', 0)}<br/>
            <br/>
            
            <b>Time Domain Analysis:</b><br/>
            • Simulation Duration: {self.time_series_data.get('time_range', {}).get('end', 0):.1f} seconds<br/>
            • Sample Rate: {self.time_series_data.get('sample_rate', 0.1):.3f} seconds<br/>
            """
            
            self.story.append(Paragraph(stats_text, self.styles['BodyText']))
    
    def add_comparison_analysis(self):
        """Add comparison with critical case"""
        self.story.append(Paragraph("Comparison Analysis", self.styles['CustomH1']))
        
        if self.case_data.get('case_id') != self.critical_case.get('fe_filename_stem'):
            # Create comparison table
            comparison_data = [
                ['Parameter', 'Current Case', 'Critical Case', 'Difference'],
                ['Case ID', self.case_data.get('case_id', 'N/A'), 
                 self.critical_case.get('fe_filename_stem', 'N/A'), '-'],
                ['Max Tension (kN)', f"{self.case_data.get('max_tension', 0):.1f}",
                 f"{self.critical_case.get('value', 0):.1f}",
                 f"{self.case_data.get('max_tension', 0) - self.critical_case.get('value', 0):.1f}"],
                ['Critical Strut', self.case_data.get('critical_strut', 'N/A').upper(),
                 self.critical_case.get('strut', 'N/A').upper(), '-'],
                ['LNG Loading', self.case_data.get('metadata', {}).get('lng_loading', 'N/A'),
                 self.critical_case.get('metadata', {}).get('lng_loading', 'N/A'), '-'],
                ['Wave Direction', self.case_data.get('metadata', {}).get('direction', 'N/A'),
                 self.critical_case.get('metadata', {}).get('direction', 'N/A'), '-'],
            ]
            
            table = Table(comparison_data, colWidths=[1.5*inch, 1.8*inch, 1.8*inch, 1.2*inch])
            table.setStyle(TableStyle([
                ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#4a5568')),
                ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
                ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
                ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
                ('FONTSIZE', (0, 0), (-1, 0), 10),
                ('FONTNAME', (0, 1), (-1, -1), 'Helvetica'),
                ('FONTSIZE', (0, 1), (-1, -1), 9),
                ('GRID', (0, 0), (-1, -1), 1, colors.grey),
                ('ROWBACKGROUNDS', (0, 1), (-1, -1), [colors.white, colors.HexColor('#f7fafc')]),
            ]))
            
            self.story.append(table)
    
    def add_recommendations(self):
        """Add engineering recommendations"""
        self.story.append(PageBreak())
        self.story.append(Paragraph("Engineering Recommendations", self.styles['CustomH1']))
        
        recommendations = []
        max_tension = self.case_data.get('max_tension', 0)
        
        if max_tension > 7000:
            recommendations.append("• CRITICAL: Immediate structural review required for excessive tensions")
            recommendations.append("• Consider design modifications to reduce loading on critical struts")
            recommendations.append("• Perform detailed fatigue analysis for high-cycle loading")
        elif max_tension > 5000:
            recommendations.append("• Monitor strut tensions during operation")
            recommendations.append("• Schedule regular inspection intervals")
            recommendations.append("• Consider reinforcement options for critical members")
        else:
            recommendations.append("• Structure operating within acceptable limits")
            recommendations.append("• Maintain regular inspection schedule")
            recommendations.append("• Document loading conditions for future reference")
        
        # Additional recommendations based on metadata
        if self.case_data.get('metadata', {}).get('environment_type') == 'Non-colinear':
            recommendations.append("• Non-colinear loading identified - verify multi-directional load combinations")
        
        if self.case_data.get('metadata', {}).get('lng_loading') == '15% LNG':
            recommendations.append("• Low LNG loading produces higher tensions - verify ballast conditions")
        
        rec_text = "<br/>".join(recommendations)
        self.story.append(Paragraph(rec_text, self.styles['BodyText']))
        
        self.story.append(Spacer(1, 0.3*inch))
        
        # Add signature block
        self.story.append(Paragraph("Report Certification", self.styles['CustomH2']))
        
        cert_data = [
            ['Prepared By:', '_____________________', 'Date:', '_____________________'],
            ['Reviewed By:', '_____________________', 'Date:', '_____________________'],
            ['Approved By:', '_____________________', 'Date:', '_____________________'],
        ]
        
        table = Table(cert_data, colWidths=[1.5*inch, 2*inch, 1*inch, 2*inch])
        table.setStyle(TableStyle([
            ('FONTNAME', (0, 0), (-1, -1), 'Helvetica'),
            ('FONTSIZE', (0, 0), (-1, -1), 10),
            ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
            ('LINEBELOW', (1, 0), (1, -1), 1, colors.black),
            ('LINEBELOW', (3, 0), (3, -1), 1, colors.black),
        ]))
        
        self.story.append(table)
    
    def add_appendix(self):
        """Add technical appendix"""
        self.story.append(PageBreak())
        self.story.append(Paragraph("Appendix A: Technical Details", self.styles['CustomH1']))
        
        appendix_text = """
        <b>Analysis Methodology:</b><br/>
        The analysis was performed using OrcaFlex dynamic simulation software with the following parameters:<br/>
        • Time domain simulation with 0.1 second time step<br/>
        • 100-year storm conditions per API RP 2A-WSD<br/>
        • Non-linear wave kinematics using Wheeler stretching<br/>
        • Morison equation for hydrodynamic loading<br/>
        <br/>
        
        <b>Load Cases:</b><br/>
        • Environmental loads based on site-specific metocean data<br/>
        • LNG loading conditions: 15% and 95% capacity<br/>
        • Tide levels: HHWL, MWL, LLWL<br/>
        • Wave directions: 0° to 345° at 15° intervals (non-colinear)<br/>
        <br/>
        
        <b>Acceptance Criteria:</b><br/>
        • Maximum allowable tension: 8000 kN (design limit)<br/>
        • Safety factor: 1.5 for extreme conditions<br/>
        • Fatigue life: Minimum 25 years<br/>
        <br/>
        
        <b>Quality Assurance:</b><br/>
        This analysis has been performed in accordance with:<br/>
        • API RP 2A-WSD: Planning, Designing and Constructing Fixed Offshore Platforms<br/>
        • DNV-OS-C101: Design of Offshore Steel Structures<br/>
        • ISO 19902: Petroleum and natural gas industries — Fixed steel offshore structures<br/>
        """
        
        self.story.append(Paragraph(appendix_text, self.styles['BodyText']))
    
    def add_page_number(self, canvas, doc):
        """Add page numbers to each page"""
        canvas.saveState()
        canvas.setFont('Helvetica', 9)
        page_num = canvas.getPageNumber()
        text = f"Page {page_num}"
        canvas.drawRightString(doc.pagesize[0]-72, 30, text)
        
        # Add header
        canvas.setFont('Helvetica-Bold', 10)
        canvas.setFillColor(colors.HexColor('#4a5568'))
        canvas.drawString(72, doc.pagesize[1]-30, "OrcaFlex Analysis Report")
        canvas.drawRightString(doc.pagesize[0]-72, doc.pagesize[1]-30, 
                               self.case_data.get('case_id', 'Analysis'))
        
        # Add header line
        canvas.setStrokeColor(colors.HexColor('#e2e8f0'))
        canvas.setLineWidth(1)
        canvas.line(72, doc.pagesize[1]-40, doc.pagesize[0]-72, doc.pagesize[1]-40)
        
        canvas.restoreState()


# FastAPI endpoint for report generation
from fastapi import APIRouter, HTTPException
from fastapi.responses import FileResponse
import tempfile
import os

router = APIRouter(prefix="/api/reports", tags=["Reports"])

@router.post("/generate/{case_id}")
async def generate_pdf_report(case_id: str):
    """Generate PDF report for a specific case"""
    
    try:
        # Import necessary modules
        import sys
        sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
        from case_selection import get_case_details
        
        # Get case data
        case_details = await get_case_details(case_id)
        
        # Get critical case for comparison
        import requests
        critical_response = requests.get('http://localhost:8001/api/critical')
        critical_case = critical_response.json() if critical_response.ok else {}
        
        # Get time series data
        try:
            ts_response = requests.get(
                f'http://localhost:8001/api/v2/timeseries/struts/{case_id}.sim/all?downsample=100'
            )
            time_series_data = ts_response.json() if ts_response.ok else None
        except:
            time_series_data = None
        
        # Prepare case data
        case_data = {
            'case_id': case_details.case_id,
            'fe_filename': case_details.fe_filename,
            'metadata': case_details.metadata,
            'max_tension': case_details.summary.get('max_tension', 0),
            'critical_strut': case_details.summary.get('critical_strut', 'N/A'),
            'struts_analyzed': len(case_details.strut_analysis),
            'is_critical': case_id == critical_case.get('fe_filename_stem')
        }
        
        # Generate report
        generator = OrcaFlexReportGenerator(case_data, critical_case, time_series_data)
        
        # Create temporary file
        with tempfile.NamedTemporaryFile(delete=False, suffix='.pdf') as tmp_file:
            report_path = tmp_file.name
        
        # Generate PDF
        generator.generate_report(report_path)
        
        # Return file
        return FileResponse(
            report_path,
            media_type='application/pdf',
            filename=f'orcaflex_report_{case_id}.pdf'
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))