/**
 * Multi-Polar Grid Component
 * 
 * Grid layout for displaying multiple polar plots simultaneously
 * Supports synchronized views, comparative analysis, and batch operations
 */

import React, { useState, useCallback, useMemo, useRef, useEffect } from 'react';
import {
  Box,
  Grid,
  Paper,
  Typography,
  IconButton,
  Toolbar,
  FormControl,
  Select,
  MenuItem,
  Chip,
  Button,
  Menu,
  Switch,
  FormControlLabel,
  Divider,
  Alert,
  useTheme,
  alpha
} from '@mui/material';
import {
  GridView,
  ViewModule,
  ViewQuilt,
  Sync,
  SyncDisabled,
  Fullscreen,
  FullscreenExit,
  MoreVert,
  Settings,
  CompareArrows,
  Download
} from '@mui/icons-material';
import PolarPlot from './PolarPlot';
import PolarExport from './PolarExport';
import {
  PolarDataSet,
  PolarLimits,
  StatisticalOverlay,
  groupByLoadCase
} from './PolarDataProcessor';
import { VisualizationOptions } from './PolarPlotControls';

interface MultiPolarGridProps {
  datasets: PolarDataSet[];
  limits?: PolarLimits;
  statisticalOverlays?: Record<string, StatisticalOverlay>;
  visualizationOptions: VisualizationOptions;
  gridLayout: GridLayoutType;
  onLayoutChange: (layout: GridLayoutType) => void;
  synchronized?: boolean;
  onSyncChange?: (synchronized: boolean) => void;
  enableComparison?: boolean;
  maxPlots?: number;
  loading?: boolean;
  error?: string;
}

interface PlotInstance {
  id: string;
  title: string;
  datasets: PolarDataSet[];
  limits?: PolarLimits;
  statisticalOverlay?: StatisticalOverlay;
  position: { row: number; col: number };
  size: { width: number; height: number };
  fullscreen?: boolean;
}

type GridLayoutType = '1x1' | '2x1' | '1x2' | '2x2' | '3x2' | '2x3' | '3x3' | 'auto';

interface SyncedViewState {
  zoom: any;
  pan: any;
  selection: any;
}

const GRID_LAYOUTS = {
  '1x1': { rows: 1, cols: 1, maxPlots: 1 },
  '2x1': { rows: 2, cols: 1, maxPlots: 2 },
  '1x2': { rows: 1, cols: 2, maxPlots: 2 },
  '2x2': { rows: 2, cols: 2, maxPlots: 4 },
  '3x2': { rows: 3, cols: 2, maxPlots: 6 },
  '2x3': { rows: 2, cols: 3, maxPlots: 6 },
  '3x3': { rows: 3, cols: 3, maxPlots: 9 },
  'auto': { rows: 0, cols: 0, maxPlots: Infinity }
};

const DEFAULT_PLOT_HEIGHT = 400;
const TOOLBAR_HEIGHT = 64;

export const MultiPolarGrid: React.FC<MultiPolarGridProps> = ({
  datasets,
  limits,
  statisticalOverlays = {},
  visualizationOptions,
  gridLayout,
  onLayoutChange,
  synchronized = false,
  onSyncChange,
  enableComparison = true,
  maxPlots = 9,
  loading = false,
  error
}) => {
  const theme = useTheme();
  const gridContainerRef = useRef<HTMLDivElement>(null);
  const plotRefs = useRef<Record<string, HTMLElement | null>>({});

  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const [fullscreenPlot, setFullscreenPlot] = useState<string | null>(null);
  const [syncedViewState, setSyncedViewState] = useState<SyncedViewState>({
    zoom: null,
    pan: null,
    selection: null
  });
  const [compareMode, setCompareMode] = useState(false);
  const [selectedPlots, setSelectedPlots] = useState<string[]>([]);

  // Generate plot instances based on datasets and layout
  const plotInstances: PlotInstance[] = useMemo(() => {
    const instances: PlotInstance[] = [];
    
    if (datasets.length === 0) return instances;

    const layout = GRID_LAYOUTS[gridLayout];
    let plotCount = 0;

    if (gridLayout === 'auto') {
      // Auto layout: group by load case or response type
      const groupedData = groupByLoadCase(datasets);
      const groups = Object.entries(groupedData);
      
      // Calculate optimal grid size
      const totalGroups = groups.length;
      const cols = Math.ceil(Math.sqrt(totalGroups));
      const rows = Math.ceil(totalGroups / cols);
      
      groups.forEach(([loadCase, groupDatasets], index) => {
        if (plotCount >= maxPlots) return;
        
        const row = Math.floor(index / cols);
        const col = index % cols;
        
        instances.push({
          id: `plot_${loadCase}`,
          title: `Load Case: ${loadCase}`,
          datasets: groupDatasets,
          limits,
          statisticalOverlay: statisticalOverlays[loadCase],
          position: { row, col },
          size: { 
            width: Math.floor(12 / cols), 
            height: DEFAULT_PLOT_HEIGHT 
          }
        });
        
        plotCount++;
      });
    } else {
      // Fixed layout: distribute datasets evenly
      const { rows, cols, maxPlots: layoutMaxPlots } = layout;
      const effectiveMaxPlots = Math.min(maxPlots, layoutMaxPlots);
      
      // Group datasets for distribution
      const datasetsPerPlot = Math.ceil(datasets.length / effectiveMaxPlots);
      
      for (let i = 0; i < effectiveMaxPlots && plotCount * datasetsPerPlot < datasets.length; i++) {
        const startIdx = i * datasetsPerPlot;
        const endIdx = Math.min(startIdx + datasetsPerPlot, datasets.length);
        const plotDatasets = datasets.slice(startIdx, endIdx);
        
        if (plotDatasets.length === 0) break;
        
        const row = Math.floor(i / cols);
        const col = i % cols;
        
        const plotTitle = plotDatasets.length === 1 
          ? plotDatasets[0].responseType
          : `Combined View ${i + 1}`;
        
        instances.push({
          id: `plot_${i}`,
          title: plotTitle,
          datasets: plotDatasets,
          limits,
          statisticalOverlay: statisticalOverlays[plotDatasets[0]?.loadCase || 'default'],
          position: { row, col },
          size: { 
            width: Math.floor(12 / cols), 
            height: DEFAULT_PLOT_HEIGHT 
          }
        });
        
        plotCount++;
      }
    }
    
    return instances;
  }, [datasets, gridLayout, maxPlots, limits, statisticalOverlays]);

  // Synchronized event handlers
  const handlePlotZoom = useCallback((plotId: string, zoomData: any) => {
    if (synchronized) {
      setSyncedViewState(prev => ({ ...prev, zoom: zoomData }));
    }
  }, [synchronized]);

  const handlePlotPan = useCallback((plotId: string, panData: any) => {
    if (synchronized) {
      setSyncedViewState(prev => ({ ...prev, pan: panData }));
    }
  }, [synchronized]);

  const handlePlotSelection = useCallback((plotId: string, selectionData: any) => {
    if (synchronized) {
      setSyncedViewState(prev => ({ ...prev, selection: selectionData }));
    }
  }, [synchronized]);

  // Layout and view handlers
  const handleMenuClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleMenuClose = () => {
    setAnchorEl(null);
  };

  const handleLayoutChange = (newLayout: GridLayoutType) => {
    onLayoutChange(newLayout);
    handleMenuClose();
  };

  const toggleFullscreen = (plotId: string) => {
    setFullscreenPlot(current => current === plotId ? null : plotId);
  };

  const toggleSync = () => {
    const newSync = !synchronized;
    onSyncChange?.(newSync);
    if (!newSync) {
      // Clear synced state when disabling sync
      setSyncedViewState({ zoom: null, pan: null, selection: null });
    }
  };

  const toggleCompareMode = () => {
    setCompareMode(!compareMode);
    if (!compareMode) {
      setSelectedPlots([]);
    }
  };

  const handlePlotSelect = (plotId: string) => {
    if (!compareMode) return;
    
    setSelectedPlots(prev => 
      prev.includes(plotId) 
        ? prev.filter(id => id !== plotId)
        : [...prev, plotId].slice(0, 4) // Max 4 plots for comparison
    );
  };

  // Export all plots
  const handleExportAll = useCallback(async () => {
    const exportPromises = plotInstances.map(async (instance) => {
      const plotElement = plotRefs.current[instance.id];
      if (plotElement) {
        // Implementation would depend on export requirements
        console.log(`Exporting plot: ${instance.title}`);
      }
    });
    
    await Promise.all(exportPromises);
  }, [plotInstances]);

  // Render individual plot
  const renderPlot = (instance: PlotInstance) => {
    const isFullscreen = fullscreenPlot === instance.id;
    const isSelected = selectedPlots.includes(instance.id);
    const isComparing = compareMode && isSelected;

    return (
      <Grid
        item
        xs={isFullscreen ? 12 : instance.size.width}
        key={instance.id}
        sx={{
          height: isFullscreen ? 'calc(100vh - 120px)' : instance.size.height,
          position: isFullscreen ? 'fixed' : 'relative',
          top: isFullscreen ? TOOLBAR_HEIGHT : 'auto',
          left: isFullscreen ? 0 : 'auto',
          right: isFullscreen ? 0 : 'auto',
          zIndex: isFullscreen ? theme.zIndex.modal : 'auto',
          backgroundColor: isFullscreen ? theme.palette.background.paper : 'transparent'
        }}
      >
        <Paper
          sx={{
            height: '100%',
            display: 'flex',
            flexDirection: 'column',
            border: isComparing ? `3px solid ${theme.palette.primary.main}` : undefined,
            cursor: compareMode ? 'pointer' : 'default'
          }}
          onClick={() => handlePlotSelect(instance.id)}
        >
          {/* Plot header */}
          <Box
            sx={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'space-between',
              p: 1,
              backgroundColor: alpha(theme.palette.primary.main, 0.1),
              borderBottom: `1px solid ${theme.palette.divider}`
            }}
          >
            <Typography variant="subtitle2" fontWeight={600}>
              {instance.title}
            </Typography>
            
            <Box sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
              {/* Dataset count */}
              <Chip
                label={`${instance.datasets.length} datasets`}
                size="small"
                variant="outlined"
              />
              
              {/* Sync indicator */}
              {synchronized && (
                <Chip
                  icon={<Sync />}
                  label="Synced"
                  size="small"
                  color="primary"
                  variant="outlined"
                />
              )}
              
              {/* Compare selection */}
              {compareMode && (
                <Chip
                  label={isSelected ? 'Selected' : 'Click to select'}
                  size="small"
                  color={isSelected ? 'primary' : 'default'}
                  variant={isSelected ? 'filled' : 'outlined'}
                />
              )}
              
              {/* Fullscreen toggle */}
              <IconButton
                size="small"
                onClick={(e) => {
                  e.stopPropagation();
                  toggleFullscreen(instance.id);
                }}
              >
                {isFullscreen ? <FullscreenExit /> : <Fullscreen />}
              </IconButton>
            </Box>
          </Box>

          {/* Plot content */}
          <Box
            sx={{ flex: 1, overflow: 'hidden' }}
            ref={(el) => { plotRefs.current[instance.id] = el; }}
          >
            <PolarPlot
              datasets={instance.datasets}
              limits={instance.limits}
              statisticalOverlay={instance.statisticalOverlay}
              title=""
              width="100%"
              height="100%"
              showGrid={visualizationOptions.showGrid}
              showMarkers={visualizationOptions.showMarkers}
              enableZoom={visualizationOptions.enableZoom}
              enablePan={visualizationOptions.enablePan}
              enableSelection={visualizationOptions.enableSelection}
              onSelectionChange={(data) => handlePlotSelection(instance.id, data)}
              responsive
            />
          </Box>
        </Paper>
      </Grid>
    );
  };

  if (error) {
    return (
      <Alert severity="error" sx={{ m: 2 }}>
        <Typography variant="h6">Error Loading Multi-Polar Grid</Typography>
        <Typography variant="body2">{error}</Typography>
      </Alert>
    );
  }

  if (plotInstances.length === 0) {
    return (
      <Box
        sx={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          height: 400,
          backgroundColor: alpha(theme.palette.grey[500], 0.1),
          border: `2px dashed ${theme.palette.divider}`,
          borderRadius: 2
        }}
      >
        <Typography variant="h6" color="text.secondary">
          No data available for polar plots
        </Typography>
      </Box>
    );
  }

  return (
    <Box ref={gridContainerRef} sx={{ width: '100%' }}>
      {/* Toolbar */}
      <Toolbar
        sx={{
          backgroundColor: theme.palette.background.paper,
          border: `1px solid ${theme.palette.divider}`,
          borderRadius: '8px 8px 0 0',
          minHeight: `${TOOLBAR_HEIGHT}px !important`
        }}
      >
        {/* Layout selector */}
        <FormControl size="small" sx={{ minWidth: 120 }}>
          <Select
            value={gridLayout}
            onChange={(e) => handleLayoutChange(e.target.value as GridLayoutType)}
          >
            <MenuItem value="1x1">1×1 Grid</MenuItem>
            <MenuItem value="2x1">2×1 Grid</MenuItem>
            <MenuItem value="1x2">1×2 Grid</MenuItem>
            <MenuItem value="2x2">2×2 Grid</MenuItem>
            <MenuItem value="3x2">3×2 Grid</MenuItem>
            <MenuItem value="2x3">2×3 Grid</MenuItem>
            <MenuItem value="3x3">3×3 Grid</MenuItem>
            <MenuItem value="auto">Auto Layout</MenuItem>
          </Select>
        </FormControl>

        <Divider orientation="vertical" sx={{ mx: 2 }} />

        {/* Sync toggle */}
        <FormControlLabel
          control={
            <Switch
              checked={synchronized}
              onChange={toggleSync}
              icon={<SyncDisabled />}
              checkedIcon={<Sync />}
            />
          }
          label="Sync Views"
        />

        <Divider orientation="vertical" sx={{ mx: 2 }} />

        {/* Compare mode toggle */}
        {enableComparison && (
          <FormControlLabel
            control={
              <Switch
                checked={compareMode}
                onChange={toggleCompareMode}
                icon={<CompareArrows />}
                checkedIcon={<CompareArrows />}
              />
            }
            label="Compare Mode"
          />
        )}

        <Box sx={{ flexGrow: 1 }} />

        {/* Plot count indicator */}
        <Chip
          label={`${plotInstances.length} plots`}
          size="small"
          variant="outlined"
          sx={{ mr: 2 }}
        />

        {/* Export all button */}
        <Button
          startIcon={<Download />}
          onClick={handleExportAll}
          size="small"
          disabled={loading}
        >
          Export All
        </Button>

        {/* More options menu */}
        <IconButton onClick={handleMenuClick}>
          <MoreVert />
        </IconButton>
      </Toolbar>

      {/* Grid content */}
      <Box
        sx={{
          border: `1px solid ${theme.palette.divider}`,
          borderTop: 'none',
          borderRadius: '0 0 8px 8px',
          overflow: 'hidden'
        }}
      >
        <Grid container spacing={1} sx={{ p: 1 }}>
          {plotInstances.map(renderPlot)}
        </Grid>
      </Box>

      {/* Compare mode info */}
      {compareMode && (
        <Alert severity="info" sx={{ mt: 2 }}>
          <Typography variant="body2">
            Compare mode active. Click on plots to select them for comparison (max 4).
            {selectedPlots.length > 0 && ` Selected: ${selectedPlots.length} plots.`}
          </Typography>
        </Alert>
      )}

      {/* Options menu */}
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleMenuClose}
      >
        <MenuItem onClick={() => handleLayoutChange('auto')}>
          <GridView sx={{ mr: 1 }} />
          Auto Layout
        </MenuItem>
        <MenuItem onClick={() => handleLayoutChange('2x2')}>
          <ViewModule sx={{ mr: 1 }} />
          2×2 Grid
        </MenuItem>
        <MenuItem onClick={() => handleLayoutChange('3x3')}>
          <ViewQuilt sx={{ mr: 1 }} />
          3×3 Grid
        </MenuItem>
        <Divider />
        <MenuItem onClick={() => setFullscreenPlot(plotInstances[0]?.id || null)}>
          <Fullscreen sx={{ mr: 1 }} />
          View First Plot Fullscreen
        </MenuItem>
        <MenuItem onClick={handleExportAll}>
          <Download sx={{ mr: 1 }} />
          Export All Plots
        </MenuItem>
      </Menu>
    </Box>
  );
};

export default MultiPolarGrid;