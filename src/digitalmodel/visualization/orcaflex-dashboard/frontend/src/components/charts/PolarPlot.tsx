/**
 * Interactive Polar Plot Component for OrcaFlex Results Dashboard
 * 
 * Displays marine engineering response quantities vs. environmental heading
 * Mathematical implementation: R(θ) vs. θ ∈ [0°, 345°]
 * Features: Interactive plots, severity zones, RAO display, hover tooltips
 */

import React, { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import {
  Box,
  Paper,
  Typography,
  useTheme,
  alpha,
  CircularProgress,
  Alert
} from '@mui/material';
import Plot from 'react-plotly.js';
import { Config, Data, Layout, PlotlyHTMLElement } from 'plotly.js';
import {
  PolarDataSet,
  PolarLimits,
  StatisticalOverlay,
  degreesToRadians
} from './PolarDataProcessor';

interface PolarPlotProps {
  datasets: PolarDataSet[];
  limits?: PolarLimits;
  statisticalOverlay?: StatisticalOverlay;
  title?: string;
  width?: number | string;
  height?: number | string;
  showGrid?: boolean;
  showMarkers?: boolean;
  enableZoom?: boolean;
  enablePan?: boolean;
  enableSelection?: boolean;
  onSelectionChange?: (selectedData: any) => void;
  onHover?: (data: any) => void;
  loading?: boolean;
  error?: string;
  className?: string;
  responsive?: boolean;
}

const DEFAULT_COLORS = [
  '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
  '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'
];

const SEVERITY_COLORS = {
  safe: '#4caf50',
  caution: '#ff9800',
  critical: '#f44336'
};

export const PolarPlot: React.FC<PolarPlotProps> = ({
  datasets,
  limits,
  statisticalOverlay,
  title = 'Polar Response Plot',
  width = '100%',
  height = 600,
  showGrid = true,
  showMarkers = true,
  enableZoom = true,
  enablePan = true,
  enableSelection = false,
  onSelectionChange,
  onHover,
  loading = false,
  error,
  className,
  responsive = true
}) => {
  const theme = useTheme();
  const plotRef = useRef<PlotlyHTMLElement>(null);
  const [plotReady, setPlotReady] = useState(false);

  // Generate plot data for Plotly polar charts
  const plotData: Data[] = useMemo(() => {
    const traces: Data[] = [];

    // Add limit zones as filled areas
    if (limits && showGrid) {
      // Survival limit zone (critical)
      const survivalTheta = Array.from({ length: 25 }, (_, i) => i * 15); // 0° to 360°
      const survivalR = survivalTheta.map(() => limits.survival);
      
      traces.push({
        type: 'scatterpolar',
        r: survivalR,
        theta: survivalTheta,
        mode: 'lines',
        line: {
          color: SEVERITY_COLORS.critical,
          width: 2,
          dash: 'dash'
        },
        fill: 'toself',
        fillcolor: alpha(SEVERITY_COLORS.critical, 0.1),
        name: `Survival Limit (${limits.survival.toFixed(1)} ${limits.units})`,
        hovertemplate: `<b>Survival Limit</b><br>` +
                      `θ: %{theta}°<br>` +
                      `R: ${limits.survival.toFixed(2)} ${limits.units}<br>` +
                      `<extra></extra>`,
        showlegend: false
      });

      // Operational limit zone (caution)
      const operationalR = survivalTheta.map(() => limits.operational);
      
      traces.push({
        type: 'scatterpolar',
        r: operationalR,
        theta: survivalTheta,
        mode: 'lines',
        line: {
          color: SEVERITY_COLORS.caution,
          width: 2,
          dash: 'dot'
        },
        fill: 'toself',
        fillcolor: alpha(SEVERITY_COLORS.caution, 0.1),
        name: `Operational Limit (${limits.operational.toFixed(1)} ${limits.units})`,
        hovertemplate: `<b>Operational Limit</b><br>` +
                      `θ: %{theta}°<br>` +
                      `R: ${limits.operational.toFixed(2)} ${limits.units}<br>` +
                      `<extra></extra>`,
        showlegend: false
      });
    }

    // Add statistical overlay
    if (statisticalOverlay) {
      // Mean line
      traces.push({
        type: 'scatterpolar',
        r: statisticalOverlay.mean.map(d => d.response),
        theta: statisticalOverlay.mean.map(d => d.heading),
        mode: 'lines',
        line: {
          color: theme.palette.primary.main,
          width: 3,
          dash: 'solid'
        },
        name: 'Statistical Mean',
        hovertemplate: `<b>Mean Response</b><br>` +
                      `θ: %{theta}°<br>` +
                      `R: %{r:.3f}<br>` +
                      `<extra></extra>`,
        showlegend: false
      });

      // Confidence bands
      const upperBand = statisticalOverlay.stdBands.upper.map(d => d.response);
      const lowerBand = statisticalOverlay.stdBands.lower.map(d => d.response);
      const thetaValues = statisticalOverlay.stdBands.upper.map(d => d.heading);

      traces.push({
        type: 'scatterpolar',
        r: [...upperBand, ...lowerBand.reverse()],
        theta: [...thetaValues, ...thetaValues.reverse()],
        mode: 'lines',
        line: { color: 'transparent' },
        fill: 'toself',
        fillcolor: alpha(theme.palette.primary.main, 0.2),
        name: `${(statisticalOverlay.confidence * 100)}% Confidence Band`,
        hoverinfo: 'skip',
        showlegend: false
      });
    }

    // Add dataset traces
    datasets
      .filter(dataset => dataset.visible !== false)
      .forEach((dataset, index) => {
        const colorIndex = index % DEFAULT_COLORS.length;
        const baseColor = dataset.color || DEFAULT_COLORS[colorIndex];
        
        // Sort data by heading for proper polar display
        const sortedData = [...dataset.data].sort((a, b) => a.heading - b.heading);
        
        const rValues = sortedData.map(d => d.response);
        const thetaValues = sortedData.map(d => d.heading);
        
        // Generate severity-based colors for markers
        const markerColors = sortedData.map(point => {
          if (point.severity) {
            return SEVERITY_COLORS[point.severity];
          }
          return baseColor;
        });

        // Main data trace
        traces.push({
          type: 'scatterpolar',
          r: rValues,
          theta: thetaValues,
          mode: showMarkers ? 'lines+markers' : 'lines',
          line: {
            color: baseColor,
            width: dataset.lineWidth || 2
          },
          marker: showMarkers ? {
            color: markerColors,
            size: 8,
            line: {
              color: theme.palette.background.paper,
              width: 1
            }
          } : undefined,
          name: dataset.label,
          hovertemplate: `<b>${dataset.label}</b><br>` +
                        `θ: %{theta}°<br>` +
                        `R: %{r:.3f} ${dataset.units}<br>` +
                        `Type: ${dataset.responseType}<br>` +
                        `Load Case: ${sortedData[0]?.loadCase || 'N/A'}<br>` +
                        `<extra></extra>`,
          showlegend: false // We use custom legend component
        });

        // Add phase information as secondary trace if available
        const phaseData = sortedData.filter(d => d.phase !== undefined);
        if (phaseData.length > 0 && dataset.type === 'rao') {
          traces.push({
            type: 'scatterpolar',
            r: phaseData.map(d => d.response),
            theta: phaseData.map(d => d.heading),
            mode: 'markers',
            marker: {
              symbol: 'diamond',
              color: alpha(baseColor, 0.6),
              size: 6,
              line: {
                color: baseColor,
                width: 1
              }
            },
            name: `${dataset.label} (Phase)`,
            hovertemplate: `<b>${dataset.label} - Phase</b><br>` +
                          `θ: %{theta}°<br>` +
                          `R: %{r:.3f} ${dataset.units}<br>` +
                          `Phase: ${phaseData.map(d => d.phase?.toFixed(1)).join(', ')}°<br>` +
                          `<extra></extra>`,
            showlegend: false
          });
        }
      });

    return traces;
  }, [datasets, limits, statisticalOverlay, showGrid, showMarkers, theme]);

  // Plot layout configuration
  const layout: Partial<Layout> = useMemo(() => ({
    title: {
      text: title,
      font: {
        size: 16,
        color: theme.palette.text.primary,
        family: theme.typography.fontFamily
      },
      x: 0.5,
      xanchor: 'center'
    },
    polar: {
      bgcolor: alpha(theme.palette.background.default, 0.5),
      radialaxis: {
        visible: true,
        range: [0, Math.max(
          ...datasets.flatMap(ds => ds.data.map(d => d.response)),
          limits?.survival || 0
        ) * 1.1],
        showline: true,
        linecolor: theme.palette.divider,
        gridcolor: alpha(theme.palette.divider, 0.3),
        tickfont: {
          color: theme.palette.text.secondary,
          size: 10
        },
        title: {
          text: datasets[0]?.units || 'Response',
          font: {
            color: theme.palette.text.primary,
            size: 12
          }
        }
      },
      angularaxis: {
        visible: true,
        direction: 'clockwise',
        rotation: 90, // 0° at top (North)
        showline: true,
        linecolor: theme.palette.divider,
        gridcolor: alpha(theme.palette.divider, 0.3),
        tickmode: 'array',
        tickvals: [0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330],
        ticktext: ['0°', '30°', '60°', '90°', '120°', '150°', '180°', '210°', '240°', '270°', '300°', '330°'],
        tickfont: {
          color: theme.palette.text.secondary,
          size: 10
        }
      }
    },
    paper_bgcolor: 'transparent',
    plot_bgcolor: 'transparent',
    font: {
      family: theme.typography.fontFamily,
      color: theme.palette.text.primary
    },
    margin: { t: 60, r: 60, b: 60, l: 60 },
    showlegend: false, // Using custom legend
    hovermode: 'closest',
    autosize: responsive
  }), [title, theme, datasets, limits, responsive]);

  // Plot configuration
  const config: Partial<Config> = useMemo(() => ({
    displayModeBar: true,
    displaylogo: false,
    modeBarButtonsToRemove: [
      'pan2d',
      'select2d',
      'lasso2d',
      'resetScale2d',
      'toggleHover'
    ],
    modeBarButtonsToAdd: [
      {
        name: 'Download plot as PNG',
        icon: {
          width: 857.1,
          height: 1000,
          path: 'M214.3 714.3h428.6v71.4H214.3z'
        },
        click: function(gd) {
          const plotElement = gd as PlotlyHTMLElement;
          // This will be handled by the export component
        }
      }
    ],
    toImageButtonOptions: {
      format: 'png',
      filename: `polar_plot_${new Date().toISOString().split('T')[0]}`,
      height: 800,
      width: 800,
      scale: 2
    },
    scrollZoom: enableZoom,
    doubleClick: 'reset+autosize',
    responsive
  }), [enableZoom, responsive]);

  // Event handlers
  const handlePlotlyClick = useCallback((data: any) => {
    if (enableSelection && onSelectionChange) {
      onSelectionChange(data);
    }
  }, [enableSelection, onSelectionChange]);

  const handlePlotlyHover = useCallback((data: any) => {
    if (onHover) {
      onHover(data);
    }
  }, [onHover]);

  const handlePlotlyReady = useCallback(() => {
    setPlotReady(true);
  }, []);

  // Error handling
  if (error) {
    return (
      <Paper sx={{ p: 2 }}>
        <Alert severity="error">
          <Typography variant="h6">Error Loading Polar Plot</Typography>
          <Typography variant="body2">{error}</Typography>
        </Alert>
      </Paper>
    );
  }

  // Loading state
  if (loading) {
    return (
      <Paper 
        sx={{ 
          p: 4, 
          display: 'flex', 
          alignItems: 'center', 
          justifyContent: 'center',
          height: typeof height === 'number' ? height : 400
        }}
      >
        <Box sx={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: 2 }}>
          <CircularProgress />
          <Typography variant="body2" color="text.secondary">
            Loading polar plot data...
          </Typography>
        </Box>
      </Paper>
    );
  }

  return (
    <Box className={className} sx={{ position: 'relative' }}>
      <Plot
        ref={plotRef}
        data={plotData}
        layout={layout}
        config={config}
        style={{ width, height }}
        onClick={handlePlotlyClick}
        onHover={handlePlotlyHover}
        onInitialized={handlePlotlyReady}
        useResizeHandler={responsive}
      />
      
      {/* Loading overlay for plot updates */}
      {!plotReady && (
        <Box
          sx={{
            position: 'absolute',
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            backgroundColor: alpha(theme.palette.background.default, 0.7),
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            zIndex: 1
          }}
        >
          <CircularProgress size={24} />
        </Box>
      )}

      {/* Data quality indicator */}
      {datasets.length > 0 && (
        <Box
          sx={{
            position: 'absolute',
            top: 8,
            right: 8,
            backgroundColor: alpha(theme.palette.background.paper, 0.9),
            p: 0.5,
            borderRadius: 1,
            fontSize: '0.7rem'
          }}
        >
          <Typography variant="caption" color="text.secondary">
            {datasets.reduce((sum, ds) => sum + ds.data.length, 0)} points
          </Typography>
        </Box>
      )}
    </Box>
  );
};

export default PolarPlot;