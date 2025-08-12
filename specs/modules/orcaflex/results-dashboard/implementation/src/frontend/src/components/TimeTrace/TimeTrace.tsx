import React, { useMemo } from 'react';
import Plot from 'react-plotly.js';
import { Box, Paper, Typography } from '@mui/material';

interface TimeTraceProps {
  data: {
    timestamps: number[];
    values: number[];
    unit: string;
    title?: string;
    statistics?: {
      max: number;
      min: number;
      mean: number;
      std: number;
    };
  };
  width?: number;
  height?: number;
  showStatistics?: boolean;
}

const TimeTrace: React.FC<TimeTraceProps> = ({ 
  data, 
  width = 800, 
  height = 400,
  showStatistics = true 
}) => {
  const plotData = useMemo(() => {
    const traces: any[] = [
      {
        x: data.timestamps,
        y: data.values,
        type: 'scatter',
        mode: 'lines',
        name: 'Time Trace',
        line: { color: '#2196F3', width: 2 }
      }
    ];

    if (showStatistics && data.statistics) {
      // Add mean line
      traces.push({
        x: data.timestamps,
        y: new Array(data.timestamps.length).fill(data.statistics.mean),
        type: 'scatter',
        mode: 'lines',
        name: 'Mean',
        line: { color: '#4CAF50', width: 2, dash: 'dash' }
      });

      // Add std dev bands
      traces.push({
        x: data.timestamps,
        y: new Array(data.timestamps.length).fill(data.statistics.mean + data.statistics.std),
        type: 'scatter',
        mode: 'lines',
        name: '+1σ',
        line: { color: '#FFC107', width: 1, dash: 'dot' },
        showlegend: false
      });

      traces.push({
        x: data.timestamps,
        y: new Array(data.timestamps.length).fill(data.statistics.mean - data.statistics.std),
        type: 'scatter',
        mode: 'lines',
        name: '-1σ',
        line: { color: '#FFC107', width: 1, dash: 'dot' },
        fill: 'tonexty',
        fillcolor: 'rgba(255, 193, 7, 0.1)',
        showlegend: false
      });

      // Add max/min lines
      traces.push({
        x: data.timestamps,
        y: new Array(data.timestamps.length).fill(data.statistics.max),
        type: 'scatter',
        mode: 'lines',
        name: 'Max',
        line: { color: '#F44336', width: 1, dash: 'dashdot' }
      });

      traces.push({
        x: data.timestamps,
        y: new Array(data.timestamps.length).fill(data.statistics.min),
        type: 'scatter',
        mode: 'lines',
        name: 'Min',
        line: { color: '#F44336', width: 1, dash: 'dashdot' }
      });
    }

    return traces;
  }, [data, showStatistics]);

  const layout = {
    title: data.title || 'Time Trace Analysis',
    xaxis: {
      title: 'Time (s)',
      showgrid: true,
      gridcolor: '#e0e0e0',
      zeroline: true,
      zerolinecolor: '#969696'
    },
    yaxis: {
      title: `Value (${data.unit})`,
      showgrid: true,
      gridcolor: '#e0e0e0',
      zeroline: true,
      zerolinecolor: '#969696'
    },
    hovermode: 'x unified',
    showlegend: true,
    legend: {
      x: 1,
      xanchor: 'right',
      y: 1
    },
    margin: { t: 50, r: 50, b: 50, l: 70 },
    plot_bgcolor: '#fafafa',
    paper_bgcolor: 'white'
  };

  const config = {
    displayModeBar: true,
    modeBarButtonsToRemove: ['select2d', 'lasso2d'],
    toImageButtonOptions: {
      format: 'png',
      filename: 'time_trace',
      height: 600,
      width: 1200,
      scale: 2
    }
  };

  return (
    <Paper elevation={2} sx={{ p: 2 }}>
      <Plot
        data={plotData}
        layout={layout}
        config={config}
        style={{ width: '100%', height: `${height}px` }}
      />
      {showStatistics && data.statistics && (
        <Box sx={{ mt: 2, p: 2, bgcolor: '#f5f5f5', borderRadius: 1 }}>
          <Typography variant="subtitle2" gutterBottom>
            Statistical Summary
          </Typography>
          <Box display="flex" gap={3}>
            <Box>
              <Typography variant="caption" color="textSecondary">Maximum</Typography>
              <Typography variant="body2">{data.statistics.max.toFixed(2)} {data.unit}</Typography>
            </Box>
            <Box>
              <Typography variant="caption" color="textSecondary">Minimum</Typography>
              <Typography variant="body2">{data.statistics.min.toFixed(2)} {data.unit}</Typography>
            </Box>
            <Box>
              <Typography variant="caption" color="textSecondary">Mean</Typography>
              <Typography variant="body2">{data.statistics.mean.toFixed(2)} {data.unit}</Typography>
            </Box>
            <Box>
              <Typography variant="caption" color="textSecondary">Std Dev</Typography>
              <Typography variant="body2">{data.statistics.std.toFixed(2)} {data.unit}</Typography>
            </Box>
          </Box>
        </Box>
      )}
    </Paper>
  );
};

export default TimeTrace;