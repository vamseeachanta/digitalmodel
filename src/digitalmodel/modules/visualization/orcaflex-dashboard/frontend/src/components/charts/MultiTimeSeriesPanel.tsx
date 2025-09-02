import React, { useState, useCallback, useMemo, useEffect } from 'react';
import {
  Box,
  Grid,
  Paper,
  Typography,
  IconButton,
  Tooltip,
  Fab,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Chip,
} from '@mui/material';
import {
  Add as AddIcon,
  Remove as RemoveIcon,
  Fullscreen as FullscreenIcon,
  GridView as GridViewIcon,
  ViewColumn as ViewColumnIcon,
  Sync as SyncIcon,
  SyncDisabled as SyncDisabledIcon,
} from '@mui/icons-material';
import { useTheme } from '@mui/material/styles';
import TimeSeries from './TimeSeries';
import TimeRangeSelector from './TimeRangeSelector';
import ComponentGroupSelector from './ComponentGroupSelector';
import {
  TimeSeriesData,
  ComponentGroup,
  TimeRange,
  MultiTimeSeriesConfig,
  ChartInstance,
} from './types';

interface MultiTimeSeriesPanelProps {
  data: TimeSeriesData[];
  componentGroups: ComponentGroup[];
  availableComponents: string[];
  initialConfig?: Partial<MultiTimeSeriesConfig>;
  onConfigChange?: (config: MultiTimeSeriesConfig) => void;
  loading?: boolean;
}

const MultiTimeSeriesPanel: React.FC<MultiTimeSeriesPanelProps> = ({
  data,
  componentGroups,
  availableComponents,
  initialConfig,
  onConfigChange,
  loading = false,
}) => {
  const theme = useTheme();
  
  // Panel state
  const [config, setConfig] = useState<MultiTimeSeriesConfig>({
    charts: [
      {
        id: 'chart-1',
        title: 'Primary Analysis',
        selectedComponents: [],
        yAxisLabel: 'Response (m)',
        showStatistics: true,
        statisticalTypes: ['mean', 'rms'],
      },
    ],
    layout: 'grid',
    synchronized: true,
    globalTimeRange: null,
    showMasterControls: true,
    decimationFactor: 1,
    ...initialConfig,
  });

  const [selectedChart, setSelectedChart] = useState<string | null>(null);
  const [addChartDialog, setAddChartDialog] = useState(false);
  const [fullscreenChart, setFullscreenChart] = useState<string | null>(null);

  // Global time range state
  const [globalTimeRange, setGlobalTimeRange] = useState<TimeRange | null>(
    config.globalTimeRange
  );

  // Update parent when config changes
  useEffect(() => {
    onConfigChange?.(config);
  }, [config, onConfigChange]);

  // Handle time range selection from any chart
  const handleTimeRangeSelect = useCallback((range: TimeRange, chartId?: string) => {
    if (config.synchronized) {
      setGlobalTimeRange(range);
      setConfig(prev => ({
        ...prev,
        globalTimeRange: range,
      }));
    } else if (chartId) {
      setConfig(prev => ({
        ...prev,
        charts: prev.charts.map(chart =>
          chart.id === chartId ? { ...chart, timeRange: range } : chart
        ),
      }));
    }
  }, [config.synchronized]);

  // Add new chart
  const addChart = useCallback((template?: Partial<ChartInstance>) => {
    const newChart: ChartInstance = {
      id: `chart-${Date.now()}`,
      title: `Chart ${config.charts.length + 1}`,
      selectedComponents: [],
      yAxisLabel: 'Response',
      showStatistics: false,
      statisticalTypes: ['mean'],
      ...template,
    };

    setConfig(prev => ({
      ...prev,
      charts: [...prev.charts, newChart],
    }));
    
    setAddChartDialog(false);
  }, [config.charts.length]);

  // Remove chart
  const removeChart = useCallback((chartId: string) => {
    if (config.charts.length <= 1) return; // Keep at least one chart
    
    setConfig(prev => ({
      ...prev,
      charts: prev.charts.filter(chart => chart.id !== chartId),
    }));
  }, [config.charts.length]);

  // Update chart configuration
  const updateChart = useCallback((chartId: string, updates: Partial<ChartInstance>) => {
    setConfig(prev => ({
      ...prev,
      charts: prev.charts.map(chart =>
        chart.id === chartId ? { ...chart, ...updates } : chart
      ),
    }));
  }, []);

  // Toggle synchronization
  const toggleSync = useCallback(() => {
    setConfig(prev => ({
      ...prev,
      synchronized: !prev.synchronized,
      globalTimeRange: !prev.synchronized ? globalTimeRange : null,
    }));
  }, [globalTimeRange]);

  // Toggle layout
  const toggleLayout = useCallback(() => {
    setConfig(prev => ({
      ...prev,
      layout: prev.layout === 'grid' ? 'column' : 'grid',
    }));
  }, []);

  // Calculate grid layout
  const gridConfig = useMemo(() => {
    const chartCount = config.charts.length;
    
    if (config.layout === 'column') {
      return { xs: 12 };
    }
    
    // Smart grid layout based on chart count
    if (chartCount === 1) return { xs: 12 };
    if (chartCount === 2) return { xs: 12, md: 6 };
    if (chartCount <= 4) return { xs: 12, md: 6, lg: 6 };
    return { xs: 12, md: 6, lg: 4 };
  }, [config.charts.length, config.layout]);

  // Chart presets for quick addition
  const chartPresets = [
    {
      title: 'Strut Analysis',
      selectedComponents: componentGroups
        .find(g => g.type === 'struts')?.componentIds || [],
      yAxisLabel: 'Displacement (m)',
      showStatistics: true,
      statisticalTypes: ['mean', 'rms', 'p95'],
    },
    {
      title: 'Jacket Response',
      selectedComponents: componentGroups
        .find(g => g.type === 'jackets')?.componentIds || [],
      yAxisLabel: 'Force (kN)',
      showStatistics: true,
      statisticalTypes: ['mean', 'stdDev', 'max'],
    },
    {
      title: 'Foundation Loads',
      selectedComponents: componentGroups
        .find(g => g.type === 'foundations')?.componentIds || [],
      yAxisLabel: 'Load (kN)',
      showStatistics: true,
      statisticalTypes: ['rms', 'p99'],
    },
  ];

  return (
    <Box sx={{ width: '100%', height: '100%' }}>
      {/* Master Controls */}
      {config.showMasterControls && (
        <Paper sx={{ p: 2, mb: 2 }}>
          <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 2 }}>
            <Typography variant="h6">
              Multi Time Series Analysis
            </Typography>
            
            <Box sx={{ display: 'flex', gap: 1 }}>
              <Tooltip title={config.synchronized ? 'Disable Sync' : 'Enable Sync'}>
                <IconButton onClick={toggleSync} color={config.synchronized ? 'primary' : 'default'}>
                  {config.synchronized ? <SyncIcon /> : <SyncDisabledIcon />}
                </IconButton>
              </Tooltip>
              
              <Tooltip title={`Switch to ${config.layout === 'grid' ? 'Column' : 'Grid'} Layout`}>
                <IconButton onClick={toggleLayout}>
                  {config.layout === 'grid' ? <ViewColumnIcon /> : <GridViewIcon />}
                </IconButton>
              </Tooltip>
            </Box>
          </Box>

          {/* Global Time Range Selector */}
          {config.synchronized && (
            <Box sx={{ mb: 2 }}>
              <TimeRangeSelector
                data={data}
                selectedRange={globalTimeRange}
                onRangeChange={setGlobalTimeRange}
                label="Global Time Range"
              />
            </Box>
          )}

          {/* Global Component Group Selector */}
          <ComponentGroupSelector
            componentGroups={componentGroups}
            availableComponents={availableComponents}
            onSelectionChange={(components) => {
              // Apply to all charts or selected chart
              if (selectedChart) {
                updateChart(selectedChart, { selectedComponents: components });
              } else {
                setConfig(prev => ({
                  ...prev,
                  charts: prev.charts.map(chart => ({
                    ...chart,
                    selectedComponents: [...new Set([...chart.selectedComponents, ...components])],
                  })),
                }));
              }
            }}
            selectedComponents={selectedChart 
              ? config.charts.find(c => c.id === selectedChart)?.selectedComponents || []
              : []
            }
          />
        </Paper>
      )}

      {/* Charts Grid */}
      <Grid container spacing={2}>
        {config.charts.map((chart) => (
          <Grid key={chart.id} item {...gridConfig}>
            <Box sx={{ position: 'relative' }}>
              {/* Chart Controls */}
              <Box sx={{ 
                position: 'absolute', 
                top: 8, 
                right: 8, 
                zIndex: 1,
                display: 'flex',
                gap: 0.5,
              }}>
                <Tooltip title="Fullscreen">
                  <IconButton
                    size="small"
                    onClick={() => setFullscreenChart(chart.id)}
                    sx={{ 
                      backgroundColor: theme.palette.background.paper,
                      opacity: 0.8,
                      '&:hover': { opacity: 1 },
                    }}
                  >
                    <FullscreenIcon fontSize="small" />
                  </IconButton>
                </Tooltip>
                
                {config.charts.length > 1 && (
                  <Tooltip title="Remove Chart">
                    <IconButton
                      size="small"
                      onClick={() => removeChart(chart.id)}
                      sx={{ 
                        backgroundColor: theme.palette.background.paper,
                        opacity: 0.8,
                        '&:hover': { opacity: 1 },
                      }}
                    >
                      <RemoveIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                )}
              </Box>

              <TimeSeries
                data={data}
                title={chart.title}
                yAxisLabel={chart.yAxisLabel}
                componentGroups={componentGroups}
                selectedComponents={chart.selectedComponents}
                showStatistics={chart.showStatistics}
                timeRange={config.synchronized ? globalTimeRange : chart.timeRange}
                onTimeRangeSelect={(range) => handleTimeRangeSelect(range, chart.id)}
                height={400}
                loading={loading}
                syncId={config.synchronized ? 'global-sync' : undefined}
                decimationFactor={config.decimationFactor}
              />
            </Box>
          </Grid>
        ))}
      </Grid>

      {/* Add Chart FAB */}
      <Fab
        color="primary"
        onClick={() => setAddChartDialog(true)}
        sx={{
          position: 'fixed',
          bottom: 24,
          right: 24,
        }}
      >
        <AddIcon />
      </Fab>

      {/* Add Chart Dialog */}
      <Dialog
        open={addChartDialog}
        onClose={() => setAddChartDialog(false)}
        maxWidth="sm"
        fullWidth
      >
        <DialogTitle>Add New Chart</DialogTitle>
        <DialogContent>
          <Typography variant="body2" sx={{ mb: 2 }}>
            Choose a preset or create a blank chart:
          </Typography>
          
          <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
            {chartPresets.map((preset, index) => (
              <Chip
                key={index}
                label={preset.title}
                variant="outlined"
                clickable
                onClick={() => addChart(preset)}
                sx={{ justifyContent: 'flex-start' }}
              />
            ))}
          </Box>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setAddChartDialog(false)}>Cancel</Button>
          <Button onClick={() => addChart()} variant="contained">
            Blank Chart
          </Button>
        </DialogActions>
      </Dialog>

      {/* Fullscreen Chart Dialog */}
      <Dialog
        open={!!fullscreenChart}
        onClose={() => setFullscreenChart(null)}
        maxWidth="xl"
        fullWidth
        fullScreen
      >
        {fullscreenChart && (
          <>
            <DialogTitle sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
              <Typography variant="h6">
                {config.charts.find(c => c.id === fullscreenChart)?.title || 'Chart'}
              </Typography>
              <IconButton onClick={() => setFullscreenChart(null)}>
                <RemoveIcon />
              </IconButton>
            </DialogTitle>
            <DialogContent>
              <TimeSeries
                data={data}
                {...(config.charts.find(c => c.id === fullscreenChart) || {})}
                componentGroups={componentGroups}
                timeRange={config.synchronized ? globalTimeRange : 
                  config.charts.find(c => c.id === fullscreenChart)?.timeRange}
                onTimeRangeSelect={(range) => handleTimeRangeSelect(range, fullscreenChart)}
                height={window.innerHeight - 200}
                syncId={config.synchronized ? 'global-sync' : undefined}
                decimationFactor={config.decimationFactor}
              />
            </DialogContent>
          </>
        )}
      </Dialog>
    </Box>
  );
};

export default MultiTimeSeriesPanel;