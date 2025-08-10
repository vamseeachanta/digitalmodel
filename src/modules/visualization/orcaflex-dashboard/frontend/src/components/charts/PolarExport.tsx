/**
 * Polar Plot Export Component
 * 
 * Provides comprehensive export options for polar plots
 * Supports PNG, SVG, PDF formats with customizable options
 */

import React, { useState, useCallback, useRef } from 'react';
import {
  Box,
  Button,
  Menu,
  MenuItem,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  FormControl,
  InputLabel,
  Select,
  Slider,
  Typography,
  Switch,
  FormControlLabel,
  Chip,
  Alert,
  CircularProgress,
  useTheme,
  SelectChangeEvent
} from '@mui/material';
import {
  FileDownload,
  GetApp,
  Image,
  PictureAsPdf,
  Code,
  Settings,
  Share
} from '@mui/icons-material';
import jsPDF from 'jspdf';
import html2canvas from 'html2canvas';
import { saveAs } from 'file-saver';
import { PolarDataSet, exportPolarData } from './PolarDataProcessor';

interface PolarExportProps {
  plotRef: React.RefObject<HTMLElement>;
  datasets: PolarDataSet[];
  plotTitle: string;
  onExportStart?: () => void;
  onExportComplete?: (success: boolean, error?: string) => void;
  disabled?: boolean;
}

interface ExportOptions {
  format: 'png' | 'svg' | 'pdf' | 'csv' | 'json';
  quality: number;
  width: number;
  height: number;
  dpi: number;
  includeTitle: boolean;
  includeLegend: boolean;
  includeMetadata: boolean;
  backgroundColor: string;
  filename: string;
  compression: boolean;
}

const DEFAULT_EXPORT_OPTIONS: ExportOptions = {
  format: 'png',
  quality: 1.0,
  width: 800,
  height: 800,
  dpi: 300,
  includeTitle: true,
  includeLegend: true,
  includeMetadata: true,
  backgroundColor: '#ffffff',
  filename: 'polar_plot',
  compression: false
};

const FORMAT_OPTIONS = {
  png: {
    label: 'PNG Image',
    icon: Image,
    description: 'High-quality raster image with transparency support',
    extensions: ['.png'],
    maxSize: { width: 4096, height: 4096 }
  },
  svg: {
    label: 'SVG Vector',
    icon: Code,
    description: 'Scalable vector graphics with interactive elements',
    extensions: ['.svg'],
    maxSize: { width: 8192, height: 8192 }
  },
  pdf: {
    label: 'PDF Document',
    icon: PictureAsPdf,
    description: 'Professional document format with metadata',
    extensions: ['.pdf'],
    maxSize: { width: 2048, height: 2048 }
  },
  csv: {
    label: 'CSV Data',
    icon: GetApp,
    description: 'Raw data export for external analysis',
    extensions: ['.csv'],
    maxSize: { width: 0, height: 0 }
  },
  json: {
    label: 'JSON Data',
    icon: Code,
    description: 'Structured data with complete metadata',
    extensions: ['.json'],
    maxSize: { width: 0, height: 0 }
  }
};

const PRESET_SIZES = {
  'web-small': { width: 400, height: 400, dpi: 72 },
  'web-medium': { width: 800, height: 800, dpi: 72 },
  'web-large': { width: 1200, height: 1200, dpi: 72 },
  'print-small': { width: 600, height: 600, dpi: 300 },
  'print-medium': { width: 1200, height: 1200, dpi: 300 },
  'print-large': { width: 2400, height: 2400, dpi: 300 },
  'presentation': { width: 1920, height: 1080, dpi: 150 },
  'poster': { width: 3000, height: 3000, dpi: 300 }
};

export const PolarExport: React.FC<PolarExportProps> = ({
  plotRef,
  datasets,
  plotTitle,
  onExportStart,
  onExportComplete,
  disabled = false
}) => {
  const theme = useTheme();
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [exporting, setExporting] = useState(false);
  const [exportOptions, setExportOptions] = useState<ExportOptions>(DEFAULT_EXPORT_OPTIONS);
  const [previewUrl, setPreviewUrl] = useState<string | null>(null);

  const canvasRef = useRef<HTMLCanvasElement>(null);

  // Menu handlers
  const handleMenuClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleMenuClose = () => {
    setAnchorEl(null);
  };

  const handleQuickExport = useCallback(async (format: ExportOptions['format']) => {
    const quickOptions = { ...DEFAULT_EXPORT_OPTIONS, format };
    await performExport(quickOptions);
    handleMenuClose();
  }, []);

  const handleAdvancedExport = () => {
    setDialogOpen(true);
    handleMenuClose();
  };

  // Export option handlers
  const handleOptionChange = <K extends keyof ExportOptions>(
    field: K,
    value: ExportOptions[K]
  ) => {
    setExportOptions(prev => ({
      ...prev,
      [field]: value
    }));
  };

  const handleSelectChange = (field: keyof ExportOptions) => 
    (event: SelectChangeEvent<any>) => {
      handleOptionChange(field, event.target.value);
    };

  const handlePresetSize = (preset: keyof typeof PRESET_SIZES) => {
    const size = PRESET_SIZES[preset];
    setExportOptions(prev => ({
      ...prev,
      width: size.width,
      height: size.height,
      dpi: size.dpi
    }));
  };

  // Export functions
  const performExport = useCallback(async (options: ExportOptions) => {
    if (!plotRef.current) {
      onExportComplete?.(false, 'Plot element not found');
      return;
    }

    setExporting(true);
    onExportStart?.();

    try {
      const timestamp = new Date().toISOString().split('T')[0];
      const filename = `${options.filename}_${timestamp}`;

      switch (options.format) {
        case 'png':
          await exportAsPNG(options, filename);
          break;
        case 'svg':
          await exportAsSVG(options, filename);
          break;
        case 'pdf':
          await exportAsPDF(options, filename);
          break;
        case 'csv':
          await exportAsCSV(filename);
          break;
        case 'json':
          await exportAsJSON(filename);
          break;
        default:
          throw new Error(`Unsupported format: ${options.format}`);
      }

      onExportComplete?.(true);
    } catch (error) {
      console.error('Export failed:', error);
      onExportComplete?.(false, error instanceof Error ? error.message : 'Export failed');
    } finally {
      setExporting(false);
    }
  }, [plotRef, datasets, onExportStart, onExportComplete]);

  const exportAsPNG = async (options: ExportOptions, filename: string) => {
    if (!plotRef.current) throw new Error('Plot reference not available');

    const canvas = await html2canvas(plotRef.current, {
      width: options.width,
      height: options.height,
      scale: options.dpi / 96, // Convert DPI to scale factor
      backgroundColor: options.includeTitle ? options.backgroundColor : null,
      useCORS: true,
      logging: false
    });

    canvas.toBlob((blob) => {
      if (blob) {
        saveAs(blob, `${filename}.png`);
      }
    }, 'image/png', options.quality);
  };

  const exportAsSVG = async (options: ExportOptions, filename: string) => {
    // For SVG export, we need to extract the Plotly SVG
    const plotlyDiv = plotRef.current?.querySelector('.plotly-graph-div') as any;
    
    if (!plotlyDiv?._fullLayout) {
      throw new Error('Plotly graph not found');
    }

    // Use Plotly's built-in SVG export
    const svgString = await (window as any).Plotly.toImage(plotlyDiv, {
      format: 'svg',
      width: options.width,
      height: options.height
    });

    // Create blob and download
    const blob = new Blob([svgString], { type: 'image/svg+xml' });
    saveAs(blob, `${filename}.svg`);
  };

  const exportAsPDF = async (options: ExportOptions, filename: string) => {
    if (!plotRef.current) throw new Error('Plot reference not available');

    const canvas = await html2canvas(plotRef.current, {
      width: options.width,
      height: options.height,
      scale: options.dpi / 96,
      backgroundColor: options.backgroundColor,
      useCORS: true
    });

    const imgData = canvas.toDataURL('image/jpeg', options.quality);
    const pdf = new jsPDF({
      orientation: options.width > options.height ? 'landscape' : 'portrait',
      unit: 'pt',
      format: [options.width, options.height + (options.includeTitle ? 50 : 0)]
    });

    // Add metadata
    if (options.includeMetadata) {
      pdf.setProperties({
        title: plotTitle,
        subject: 'Polar Plot Export',
        author: 'OrcaFlex Dashboard',
        creator: 'Digital Model Platform',
        producer: 'React Plotly.js',
        keywords: 'marine engineering, polar plot, orcaflex'
      });
    }

    // Add title
    if (options.includeTitle) {
      pdf.setFontSize(16);
      pdf.text(plotTitle, options.width / 2, 30, { align: 'center' });
    }

    // Add image
    const yOffset = options.includeTitle ? 50 : 0;
    pdf.addImage(imgData, 'JPEG', 0, yOffset, options.width, options.height);

    // Add metadata page if requested
    if (options.includeMetadata && datasets.length > 0) {
      pdf.addPage();
      pdf.setFontSize(14);
      pdf.text('Dataset Information', 20, 30);
      
      let yPos = 60;
      datasets.forEach((dataset, index) => {
        if (yPos > pdf.internal.pageSize.height - 60) {
          pdf.addPage();
          yPos = 30;
        }
        
        pdf.setFontSize(12);
        pdf.text(`${index + 1}. ${dataset.label}`, 20, yPos);
        pdf.setFontSize(10);
        pdf.text(`   Type: ${dataset.responseType}`, 30, yPos + 15);
        pdf.text(`   Units: ${dataset.units}`, 30, yPos + 30);
        pdf.text(`   Data Points: ${dataset.data.length}`, 30, yPos + 45);
        
        yPos += 70;
      });
    }

    pdf.save(`${filename}.pdf`);
  };

  const exportAsCSV = async (filename: string) => {
    const csvData = exportPolarData(datasets, 'csv');
    const blob = new Blob([csvData], { type: 'text/csv' });
    saveAs(blob, `${filename}.csv`);
  };

  const exportAsJSON = async (filename: string) => {
    const jsonData = exportPolarData(datasets, 'json');
    const blob = new Blob([jsonData], { type: 'application/json' });
    saveAs(blob, `${filename}.json`);
  };

  // Generate preview
  const generatePreview = useCallback(async () => {
    if (!plotRef.current) return;

    try {
      const canvas = await html2canvas(plotRef.current, {
        width: 200,
        height: 200,
        scale: 0.25
      });
      
      setPreviewUrl(canvas.toDataURL());
    } catch (error) {
      console.error('Preview generation failed:', error);
    }
  }, [plotRef]);

  const formatInfo = FORMAT_OPTIONS[exportOptions.format];

  return (
    <>
      {/* Quick export button */}
      <Button
        startIcon={<FileDownload />}
        onClick={handleMenuClick}
        disabled={disabled || exporting}
        variant="outlined"
      >
        Export
      </Button>

      {/* Quick export menu */}
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleMenuClose}
        PaperProps={{
          sx: { minWidth: 200 }
        }}
      >
        <MenuItem onClick={() => handleQuickExport('png')}>
          <Image sx={{ mr: 1 }} />
          Export as PNG
        </MenuItem>
        <MenuItem onClick={() => handleQuickExport('svg')}>
          <Code sx={{ mr: 1 }} />
          Export as SVG
        </MenuItem>
        <MenuItem onClick={() => handleQuickExport('pdf')}>
          <PictureAsPdf sx={{ mr: 1 }} />
          Export as PDF
        </MenuItem>
        <MenuItem onClick={() => handleQuickExport('csv')}>
          <GetApp sx={{ mr: 1 }} />
          Export Data (CSV)
        </MenuItem>
        <MenuItem divider onClick={handleAdvancedExport}>
          <Settings sx={{ mr: 1 }} />
          Advanced Options...
        </MenuItem>
      </Menu>

      {/* Advanced export dialog */}
      <Dialog
        open={dialogOpen}
        onClose={() => setDialogOpen(false)}
        maxWidth="md"
        fullWidth
        PaperProps={{ sx: { minHeight: 600 } }}
      >
        <DialogTitle>
          Advanced Export Options
        </DialogTitle>
        
        <DialogContent>
          <Box sx={{ display: 'flex', gap: 3, mt: 1 }}>
            {/* Left panel - Options */}
            <Box sx={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 3 }}>
              {/* Format selection */}
              <Box>
                <FormControl fullWidth>
                  <InputLabel>Export Format</InputLabel>
                  <Select
                    value={exportOptions.format}
                    onChange={handleSelectChange('format')}
                    label="Export Format"
                  >
                    {Object.entries(FORMAT_OPTIONS).map(([value, info]) => {
                      const IconComponent = info.icon;
                      return (
                        <MenuItem key={value} value={value}>
                          <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                            <IconComponent fontSize="small" />
                            {info.label}
                          </Box>
                        </MenuItem>
                      );
                    })}
                  </Select>
                </FormControl>
                
                <Typography variant="caption" color="text.secondary" sx={{ mt: 0.5, display: 'block' }}>
                  {formatInfo.description}
                </Typography>
              </Box>

              {/* Filename */}
              <TextField
                fullWidth
                label="Filename"
                value={exportOptions.filename}
                onChange={(e) => handleOptionChange('filename', e.target.value)}
                helperText={`Will be saved as: ${exportOptions.filename}_${new Date().toISOString().split('T')[0]}${formatInfo.extensions[0]}`}
              />

              {/* Size presets */}
              {['png', 'svg', 'pdf'].includes(exportOptions.format) && (
                <Box>
                  <Typography variant="subtitle2" gutterBottom>
                    Size Presets
                  </Typography>
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1 }}>
                    {Object.entries(PRESET_SIZES).map(([key, preset]) => (
                      <Chip
                        key={key}
                        label={`${key.replace('-', ' ')} (${preset.width}×${preset.height})`}
                        onClick={() => handlePresetSize(key as keyof typeof PRESET_SIZES)}
                        variant="outlined"
                        size="small"
                        clickable
                      />
                    ))}
                  </Box>
                </Box>
              )}

              {/* Custom dimensions */}
              {['png', 'svg', 'pdf'].includes(exportOptions.format) && (
                <Box sx={{ display: 'flex', gap: 2 }}>
                  <TextField
                    label="Width (px)"
                    type="number"
                    value={exportOptions.width}
                    onChange={(e) => handleOptionChange('width', Number(e.target.value))}
                    inputProps={{ min: 100, max: formatInfo.maxSize.width }}
                  />
                  <TextField
                    label="Height (px)"
                    type="number"
                    value={exportOptions.height}
                    onChange={(e) => handleOptionChange('height', Number(e.target.value))}
                    inputProps={{ min: 100, max: formatInfo.maxSize.height }}
                  />
                </Box>
              )}

              {/* Quality settings */}
              {['png', 'pdf'].includes(exportOptions.format) && (
                <>
                  <Box>
                    <Typography variant="subtitle2" gutterBottom>
                      DPI: {exportOptions.dpi}
                    </Typography>
                    <Slider
                      value={exportOptions.dpi}
                      onChange={(_, value) => handleOptionChange('dpi', value as number)}
                      min={72}
                      max={300}
                      step={1}
                      marks={[
                        { value: 72, label: '72' },
                        { value: 150, label: '150' },
                        { value: 300, label: '300' }
                      ]}
                    />
                  </Box>

                  <Box>
                    <Typography variant="subtitle2" gutterBottom>
                      Quality: {(exportOptions.quality * 100).toFixed(0)}%
                    </Typography>
                    <Slider
                      value={exportOptions.quality}
                      onChange={(_, value) => handleOptionChange('quality', value as number)}
                      min={0.1}
                      max={1}
                      step={0.1}
                    />
                  </Box>
                </>
              )}

              {/* Options checkboxes */}
              <Box>
                <Typography variant="subtitle2" gutterBottom>
                  Include
                </Typography>
                <Box sx={{ display: 'flex', flexDirection: 'column' }}>
                  <FormControlLabel
                    control={
                      <Switch
                        checked={exportOptions.includeTitle}
                        onChange={(e) => handleOptionChange('includeTitle', e.target.checked)}
                      />
                    }
                    label="Plot Title"
                  />
                  <FormControlLabel
                    control={
                      <Switch
                        checked={exportOptions.includeLegend}
                        onChange={(e) => handleOptionChange('includeLegend', e.target.checked)}
                      />
                    }
                    label="Legend"
                  />
                  {exportOptions.format === 'pdf' && (
                    <FormControlLabel
                      control={
                        <Switch
                          checked={exportOptions.includeMetadata}
                          onChange={(e) => handleOptionChange('includeMetadata', e.target.checked)}
                        />
                      }
                      label="Dataset Metadata"
                    />
                  )}
                </Box>
              </Box>
            </Box>

            {/* Right panel - Preview */}
            <Box sx={{ width: 250, display: 'flex', flexDirection: 'column', gap: 2 }}>
              <Typography variant="subtitle2">Preview</Typography>
              
              <Box
                sx={{
                  border: `2px dashed ${theme.palette.divider}`,
                  borderRadius: 2,
                  p: 2,
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  minHeight: 200,
                  backgroundColor: exportOptions.backgroundColor
                }}
              >
                {previewUrl ? (
                  <img
                    src={previewUrl}
                    alt="Export Preview"
                    style={{
                      maxWidth: '100%',
                      maxHeight: '100%',
                      objectFit: 'contain'
                    }}
                  />
                ) : (
                  <Button
                    onClick={generatePreview}
                    variant="outlined"
                    size="small"
                  >
                    Generate Preview
                  </Button>
                )}
              </Box>

              {/* Export info */}
              <Alert severity="info" sx={{ fontSize: '0.75rem' }}>
                <Typography variant="caption">
                  Format: {formatInfo.label}<br />
                  Size: {exportOptions.width}×{exportOptions.height}px<br />
                  {['png', 'pdf'].includes(exportOptions.format) && (
                    <>DPI: {exportOptions.dpi}<br /></>
                  )}
                  Estimated file size: {estimateFileSize(exportOptions)}
                </Typography>
              </Alert>
            </Box>
          </Box>
        </DialogContent>

        <DialogActions>
          <Button onClick={() => setDialogOpen(false)}>
            Cancel
          </Button>
          <Button
            onClick={() => {
              performExport(exportOptions);
              setDialogOpen(false);
            }}
            variant="contained"
            disabled={exporting}
            startIcon={exporting ? <CircularProgress size={16} /> : <FileDownload />}
          >
            {exporting ? 'Exporting...' : 'Export'}
          </Button>
        </DialogActions>
      </Dialog>
    </>
  );
};

function estimateFileSize(options: ExportOptions): string {
  const { format, width, height, quality } = options;
  
  // Rough estimates based on format and dimensions
  const pixels = width * height;
  
  switch (format) {
    case 'png':
      return `${Math.round(pixels * 4 * quality / 1024 / 1024 * 10) / 10} MB`;
    case 'svg':
      return `${Math.round(pixels / 1000)} KB`;
    case 'pdf':
      return `${Math.round(pixels * 3 * quality / 1024 / 1024 * 10) / 10} MB`;
    case 'csv':
      return '< 1 MB';
    case 'json':
      return '< 1 MB';
    default:
      return 'Unknown';
  }
}

export default PolarExport;