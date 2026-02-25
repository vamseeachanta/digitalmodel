/**
 * Polar Plot Controls Component
 * 
 * Provides comprehensive controls for customizing polar plot display
 * Including data filtering, visualization options, and analysis settings
 */

import React, { useState, useCallback } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  FormControlLabel,
  Switch,
  Slider,
  Select,
  MenuItem,
  FormControl,
  InputLabel,
  Chip,
  Button,
  ButtonGroup,
  Divider,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  TextField,
  IconButton,
  Tooltip,
  useTheme,
  SelectChangeEvent
} from '@mui/material';
import {
  ExpandMore,
  Refresh,
  Settings,
  Visibility,
  VisibilityOff,
  FilterList,
  TuneOutlined,
  PaletteOutlined,
  GridOn,
  GridOff,
  ZoomIn,
  ZoomOut,
  RestartAlt
} from '@mui/icons-material';
import { PolarDataSet, ProcessingOptions } from './PolarDataProcessor';

interface PolarPlotControlsProps {
  datasets: PolarDataSet[];
  onDatasetToggle: (datasetId: string) => void;
  onProcessingOptionsChange: (options: Partial<ProcessingOptions>) => void;
  onVisualizationChange: (options: VisualizationOptions) => void;
  onResetView?: () => void;
  onRefreshData?: () => void;
  processingOptions: ProcessingOptions;
  visualizationOptions: VisualizationOptions;
  disabled?: boolean;
  compact?: boolean;
}

export interface VisualizationOptions {
  showGrid: boolean;
  showMarkers: boolean;
  showLimits: boolean;
  showStatistical: boolean;
  enableZoom: boolean;
  enablePan: boolean;
  enableSelection: boolean;
  plotSize: 'small' | 'medium' | 'large';
  colorScheme: 'default' | 'colorblind' | 'high-contrast' | 'monochrome';
  lineWidth: number;
  markerSize: number;
  gridOpacity: number;
  theme: 'auto' | 'light' | 'dark';
}

interface FilterOptions {
  responseRange: [number, number];
  headingRange: [number, number];
  loadCases: string[];
  severityLevels: string[];
  responseTypes: string[];
}

const DEFAULT_VISUALIZATION_OPTIONS: VisualizationOptions = {
  showGrid: true,
  showMarkers: true,
  showLimits: true,
  showStatistical: false,
  enableZoom: true,
  enablePan: true,
  enableSelection: false,
  plotSize: 'medium',
  colorScheme: 'default',
  lineWidth: 2,
  markerSize: 8,
  gridOpacity: 0.3,
  theme: 'auto'
};

const PLOT_SIZES = {
  small: { width: 400, height: 400 },
  medium: { width: 600, height: 600 },
  large: { width: 800, height: 800 }
};

const COLOR_SCHEMES = {
  default: 'Default',
  colorblind: 'Colorblind Safe',
  'high-contrast': 'High Contrast',
  monochrome: 'Monochrome'
};

export const PolarPlotControls: React.FC<PolarPlotControlsProps> = ({
  datasets,
  onDatasetToggle,
  onProcessingOptionsChange,
  onVisualizationChange,
  onResetView,
  onRefreshData,
  processingOptions,
  visualizationOptions = DEFAULT_VISUALIZATION_OPTIONS,
  disabled = false,
  compact = false
}) => {
  const theme = useTheme();
  const [filterOptions, setFilterOptions] = useState<FilterOptions>({
    responseRange: [0, 100],
    headingRange: [0, 360],
    loadCases: [],
    severityLevels: ['safe', 'caution', 'critical'],
    responseTypes: []
  });
  
  const [expandedPanel, setExpandedPanel] = useState<string>('datasets');

  // Extract unique values for filtering
  const availableLoadCases = Array.from(
    new Set(datasets.flatMap(ds => ds.data.map(d => d.loadCase).filter(Boolean)))
  );
  
  const availableResponseTypes = Array.from(
    new Set(datasets.map(ds => ds.responseType))
  );

  const visibleDatasetCount = datasets.filter(ds => ds.visible !== false).length;

  // Event handlers
  const handlePanelChange = (panel: string) => (
    event: React.SyntheticEvent,
    isExpanded: boolean
  ) => {
    setExpandedPanel(isExpanded ? panel : '');
  };

  const handleProcessingChange = useCallback((
    field: keyof ProcessingOptions,
    value: any
  ) => {
    onProcessingOptionsChange({ [field]: value });
  }, [onProcessingOptionsChange]);

  const handleVisualizationChange = useCallback((
    field: keyof VisualizationOptions,
    value: any
  ) => {
    const newOptions = { ...visualizationOptions, [field]: value };
    onVisualizationChange(newOptions);
  }, [visualizationOptions, onVisualizationChange]);

  const handleFilterChange = useCallback((
    field: keyof FilterOptions,
    value: any
  ) => {
    setFilterOptions(prev => ({ ...prev, [field]: value }));
  }, []);

  const handleSelectChange = (field: keyof VisualizationOptions) => 
    (event: SelectChangeEvent<any>) => {
      handleVisualizationChange(field, event.target.value);
    };

  const handleToggleAll = useCallback((visible: boolean) => {
    datasets.forEach(dataset => {
      if ((dataset.visible !== false) !== visible) {
        onDatasetToggle(dataset.id);
      }
    });
  }, [datasets, onDatasetToggle]);

  const DatasetControls = () => (
    <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
      {/* Dataset visibility toggles */}
      <Box>
        <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 1 }}>
          <Typography variant="subtitle2" color="text.primary">
            Datasets ({visibleDatasetCount}/{datasets.length} visible)
          </Typography>
          <Box>
            <Button
              size="small"
              onClick={() => handleToggleAll(true)}
              disabled={disabled || visibleDatasetCount === datasets.length}
            >
              Show All
            </Button>
            <Button
              size="small"
              onClick={() => handleToggleAll(false)}
              disabled={disabled || visibleDatasetCount === 0}
            >
              Hide All
            </Button>
          </Box>
        </Box>
        
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
          {datasets.map(dataset => (
            <Box
              key={dataset.id}
              sx={{
                display: 'flex',
                alignItems: 'center',
                p: 1,
                border: `1px solid ${theme.palette.divider}`,
                borderRadius: 1,
                backgroundColor: dataset.visible !== false 
                  ? 'transparent' 
                  : theme.palette.action.disabled
              }}
            >
              <FormControlLabel
                control={
                  <Switch
                    checked={dataset.visible !== false}
                    onChange={() => onDatasetToggle(dataset.id)}
                    disabled={disabled}
                    size="small"
                  />
                }
                label={
                  <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                    <Box
                      sx={{
                        width: 12,
                        height: 12,
                        borderRadius: '50%',
                        backgroundColor: dataset.color
                      }}
                    />
                    <Typography variant="body2">{dataset.label}</Typography>
                  </Box>
                }
              />
              <Box sx={{ ml: 'auto', display: 'flex', gap: 0.5 }}>
                <Chip
                  label={dataset.type}
                  size="small"
                  color="primary"
                  variant="outlined"
                />
                <Chip
                  label={`${dataset.data.length} pts`}
                  size="small"
                  variant="outlined"
                />
              </Box>
            </Box>
          ))}
        </Box>
      </Box>
    </Box>
  );

  const ProcessingControls = () => (
    <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
      {/* Heading increment */}
      <Box>
        <Typography variant="subtitle2" gutterBottom>
          Heading Increment
        </Typography>
        <FormControl size="small" fullWidth>
          <Select
            value={processingOptions.headingIncrement}
            onChange={(e) => handleProcessingChange('headingIncrement', Number(e.target.value))}
            disabled={disabled}
          >
            <MenuItem value={15}>15° (24 points)</MenuItem>
            <MenuItem value={30}>30° (12 points)</MenuItem>
            <MenuItem value={45}>45° (8 points)</MenuItem>
          </Select>
        </FormControl>
      </Box>

      {/* Processing options */}
      <Box>
        <Typography variant="subtitle2" gutterBottom>
          Data Processing
        </Typography>
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
          <FormControlLabel
            control={
              <Switch
                checked={processingOptions.interpolateGaps}
                onChange={(e) => handleProcessingChange('interpolateGaps', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Interpolate Missing Points"
          />
          <FormControlLabel
            control={
              <Switch
                checked={processingOptions.smoothing}
                onChange={(e) => handleProcessingChange('smoothing', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Apply Smoothing Filter"
          />
          <FormControlLabel
            control={
              <Switch
                checked={processingOptions.filterOutliers}
                onChange={(e) => handleProcessingChange('filterOutliers', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Filter Outliers"
          />
          <FormControlLabel
            control={
              <Switch
                checked={processingOptions.normalizeAmplitudes}
                onChange={(e) => handleProcessingChange('normalizeAmplitudes', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Normalize Amplitudes"
          />
        </Box>
      </Box>
    </Box>
  );

  const VisualizationControls = () => (
    <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
      {/* Display options */}
      <Box>
        <Typography variant="subtitle2" gutterBottom>
          Display Options
        </Typography>
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
          <FormControlLabel
            control={
              <Switch
                checked={visualizationOptions.showGrid}
                onChange={(e) => handleVisualizationChange('showGrid', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Show Grid"
          />
          <FormControlLabel
            control={
              <Switch
                checked={visualizationOptions.showMarkers}
                onChange={(e) => handleVisualizationChange('showMarkers', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Show Markers"
          />
          <FormControlLabel
            control={
              <Switch
                checked={visualizationOptions.showLimits}
                onChange={(e) => handleVisualizationChange('showLimits', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Show Limits"
          />
          <FormControlLabel
            control={
              <Switch
                checked={visualizationOptions.showStatistical}
                onChange={(e) => handleVisualizationChange('showStatistical', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Statistical Overlay"
          />
        </Box>
      </Box>

      {/* Interaction options */}
      <Box>
        <Typography variant="subtitle2" gutterBottom>
          Interaction
        </Typography>
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
          <FormControlLabel
            control={
              <Switch
                checked={visualizationOptions.enableZoom}
                onChange={(e) => handleVisualizationChange('enableZoom', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Enable Zoom"
          />
          <FormControlLabel
            control={
              <Switch
                checked={visualizationOptions.enablePan}
                onChange={(e) => handleVisualizationChange('enablePan', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Enable Pan"
          />
          <FormControlLabel
            control={
              <Switch
                checked={visualizationOptions.enableSelection}
                onChange={(e) => handleVisualizationChange('enableSelection', e.target.checked)}
                disabled={disabled}
              />
            }
            label="Enable Selection"
          />
        </Box>
      </Box>

      {/* Styling options */}
      <Box>
        <Typography variant="subtitle2" gutterBottom>
          Styling
        </Typography>
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
          {/* Plot size */}
          <FormControl size="small" fullWidth>
            <InputLabel>Plot Size</InputLabel>
            <Select
              value={visualizationOptions.plotSize}
              onChange={handleSelectChange('plotSize')}
              disabled={disabled}
              label="Plot Size"
            >
              <MenuItem value="small">Small (400×400)</MenuItem>
              <MenuItem value="medium">Medium (600×600)</MenuItem>
              <MenuItem value="large">Large (800×800)</MenuItem>
            </Select>
          </FormControl>

          {/* Color scheme */}
          <FormControl size="small" fullWidth>
            <InputLabel>Color Scheme</InputLabel>
            <Select
              value={visualizationOptions.colorScheme}
              onChange={handleSelectChange('colorScheme')}
              disabled={disabled}
              label="Color Scheme"
            >
              {Object.entries(COLOR_SCHEMES).map(([value, label]) => (
                <MenuItem key={value} value={value}>{label}</MenuItem>
              ))}
            </Select>
          </FormControl>

          {/* Line width */}
          <Box>
            <Typography variant="caption" color="text.secondary">
              Line Width: {visualizationOptions.lineWidth}px
            </Typography>
            <Slider
              value={visualizationOptions.lineWidth}
              onChange={(_, value) => handleVisualizationChange('lineWidth', value)}
              min={1}
              max={5}
              step={0.5}
              disabled={disabled}
              size="small"
            />
          </Box>

          {/* Marker size */}
          <Box>
            <Typography variant="caption" color="text.secondary">
              Marker Size: {visualizationOptions.markerSize}px
            </Typography>
            <Slider
              value={visualizationOptions.markerSize}
              onChange={(_, value) => handleVisualizationChange('markerSize', value)}
              min={4}
              max={16}
              step={1}
              disabled={disabled}
              size="small"
            />
          </Box>

          {/* Grid opacity */}
          <Box>
            <Typography variant="caption" color="text.secondary">
              Grid Opacity: {(visualizationOptions.gridOpacity * 100).toFixed(0)}%
            </Typography>
            <Slider
              value={visualizationOptions.gridOpacity}
              onChange={(_, value) => handleVisualizationChange('gridOpacity', value)}
              min={0.1}
              max={1}
              step={0.1}
              disabled={disabled}
              size="small"
            />
          </Box>
        </Box>
      </Box>
    </Box>
  );

  const FilterControls = () => (
    <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
      {/* Response range filter */}
      <Box>
        <Typography variant="subtitle2" gutterBottom>
          Response Range Filter
        </Typography>
        <Box sx={{ px: 1 }}>
          <Slider
            value={filterOptions.responseRange}
            onChange={(_, value) => handleFilterChange('responseRange', value as [number, number])}
            valueLabelDisplay="auto"
            min={0}
            max={Math.max(...datasets.flatMap(ds => ds.data.map(d => d.response))) * 1.1}
            step={0.1}
            disabled={disabled}
            size="small"
          />
        </Box>
      </Box>

      {/* Load case filter */}
      {availableLoadCases.length > 0 && (
        <FormControl size="small" fullWidth>
          <InputLabel>Load Cases</InputLabel>
          <Select
            multiple
            value={filterOptions.loadCases}
            onChange={(e) => handleFilterChange('loadCases', e.target.value)}
            disabled={disabled}
            renderValue={(selected) => (
              <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                {(selected as string[]).map((value) => (
                  <Chip key={value} label={value} size="small" />
                ))}
              </Box>
            )}
          >
            {availableLoadCases.map((loadCase) => (
              <MenuItem key={loadCase} value={loadCase}>
                {loadCase}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      )}

      {/* Response type filter */}
      <FormControl size="small" fullWidth>
        <InputLabel>Response Types</InputLabel>
        <Select
          multiple
          value={filterOptions.responseTypes}
          onChange={(e) => handleFilterChange('responseTypes', e.target.value)}
          disabled={disabled}
          renderValue={(selected) => (
            <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
              {(selected as string[]).map((value) => (
                <Chip key={value} label={value} size="small" />
              ))}
            </Box>
          )}
        >
          {availableResponseTypes.map((responseType) => (
            <MenuItem key={responseType} value={responseType}>
              {responseType}
            </MenuItem>
          ))}
        </Select>
      </FormControl>
    </Box>
  );

  return (
    <Card sx={{ width: '100%', maxWidth: compact ? 300 : 400 }}>
      <CardContent sx={{ p: compact ? 1 : 2 }}>
        {/* Header with action buttons */}
        <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 2 }}>
          <Typography variant="h6" component="h2">
            Plot Controls
          </Typography>
          <Box>
            {onRefreshData && (
              <Tooltip title="Refresh Data">
                <IconButton
                  size="small"
                  onClick={onRefreshData}
                  disabled={disabled}
                >
                  <Refresh />
                </IconButton>
              </Tooltip>
            )}
            {onResetView && (
              <Tooltip title="Reset View">
                <IconButton
                  size="small"
                  onClick={onResetView}
                  disabled={disabled}
                >
                  <RestartAlt />
                </IconButton>
              </Tooltip>
            )}
          </Box>
        </Box>

        {/* Accordion panels */}
        <Box>
          <Accordion
            expanded={expandedPanel === 'datasets'}
            onChange={handlePanelChange('datasets')}
            disabled={disabled}
          >
            <AccordionSummary expandIcon={<ExpandMore />}>
              <Typography variant="subtitle1">Datasets</Typography>
            </AccordionSummary>
            <AccordionDetails>
              <DatasetControls />
            </AccordionDetails>
          </Accordion>

          <Accordion
            expanded={expandedPanel === 'processing'}
            onChange={handlePanelChange('processing')}
            disabled={disabled}
          >
            <AccordionSummary expandIcon={<ExpandMore />}>
              <Typography variant="subtitle1">Processing</Typography>
            </AccordionSummary>
            <AccordionDetails>
              <ProcessingControls />
            </AccordionDetails>
          </Accordion>

          <Accordion
            expanded={expandedPanel === 'visualization'}
            onChange={handlePanelChange('visualization')}
            disabled={disabled}
          >
            <AccordionSummary expandIcon={<ExpandMore />}>
              <Typography variant="subtitle1">Visualization</Typography>
            </AccordionSummary>
            <AccordionDetails>
              <VisualizationControls />
            </AccordionDetails>
          </Accordion>

          <Accordion
            expanded={expandedPanel === 'filters'}
            onChange={handlePanelChange('filters')}
            disabled={disabled}
          >
            <AccordionSummary expandIcon={<ExpandMore />}>
              <Typography variant="subtitle1">Filters</Typography>
            </AccordionSummary>
            <AccordionDetails>
              <FilterControls />
            </AccordionDetails>
          </Accordion>
        </Box>
      </CardContent>
    </Card>
  );
};

export default PolarPlotControls;