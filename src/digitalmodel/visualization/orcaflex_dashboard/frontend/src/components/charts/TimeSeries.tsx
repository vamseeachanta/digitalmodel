import React, { useMemo, useCallback, useRef, useEffect } from 'react';
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
  ReferenceArea,
  ReferenceLine,
} from 'recharts';
import { Box, Paper, Typography, Skeleton } from '@mui/material';
import { useTheme, alpha } from '@mui/material/styles';
import { TimeSeriesData, ComponentGroup, StatisticalOverlay, TimeRange } from './types';

interface TimeSeriesProps {
  data: TimeSeriesData[];
  title?: string;
  yAxisLabel?: string;
  xAxisLabel?: string;
  componentGroups?: ComponentGroup[];
  selectedComponents?: string[];
  showStatistics?: boolean;
  statisticalOverlays?: StatisticalOverlay[];
  timeRange?: TimeRange;
  onTimeRangeSelect?: (range: TimeRange) => void;
  height?: number;
  loading?: boolean;
  syncId?: string;
  showGrid?: boolean;
  enableZoom?: boolean;
  decimationFactor?: number;
}

const TimeSeries: React.FC<TimeSeriesProps> = ({
  data,
  title,
  yAxisLabel = 'Value',
  xAxisLabel = 'Time (s)',
  componentGroups = [],
  selectedComponents = [],
  showStatistics = false,
  statisticalOverlays = [],
  timeRange,
  onTimeRangeSelect,
  height = 400,
  loading = false,
  syncId,
  showGrid = true,
  enableZoom = true,
  decimationFactor = 1,
}) => {
  const theme = useTheme();
  const chartRef = useRef<any>(null);
  const [zoomDomain, setZoomDomain] = React.useState<TimeRange | null>(null);
  const [isSelecting, setIsSelecting] = React.useState(false);

  // Color palette for different component groups
  const groupColors = useMemo(() => ({
    struts: theme.palette.primary.main,
    jackets: theme.palette.secondary.main,
    foundations: theme.palette.error.main,
    default: theme.palette.grey[600],
  }), [theme]);

  // Data decimation for performance with large datasets
  const decimatedData = useMemo(() => {
    if (!data.length || decimationFactor <= 1) return data;
    
    return data.filter((_, index) => index % decimationFactor === 0);
  }, [data, decimationFactor]);

  // Filter data based on time range
  const filteredData = useMemo(() => {
    if (!timeRange) return decimatedData;
    
    return decimatedData.filter(point => 
      point.time >= timeRange.start && point.time <= timeRange.end
    );
  }, [decimatedData, timeRange]);

  // Group data by component type
  const groupedData = useMemo(() => {
    const groups: { [key: string]: TimeSeriesData[] } = {};
    
    componentGroups.forEach(group => {
      groups[group.name] = filteredData.filter(point => 
        group.componentIds.some(id => point.hasOwnProperty(id))
      );
    });
    
    return groups;
  }, [filteredData, componentGroups]);

  // Generate line components for each selected component
  const renderLines = useMemo(() => {
    if (!filteredData.length) return [];

    const lines: JSX.Element[] = [];
    
    selectedComponents.forEach((componentId, index) => {
      // Find which group this component belongs to
      const group = componentGroups.find(g => 
        g.componentIds.includes(componentId)
      );
      
      const color = group ? groupColors[group.type] || groupColors.default : groupColors.default;
      const strokeWidth = group?.type === 'struts' ? 2 : 1.5;
      const strokeDasharray = group?.type === 'jackets' ? '5,5' : undefined;
      
      lines.push(
        <Line
          key={componentId}
          type="monotone"
          dataKey={componentId}
          stroke={color}
          strokeWidth={strokeWidth}
          strokeDasharray={strokeDasharray}
          dot={false}
          connectNulls={false}
          animationDuration={300}
        />
      );
    });

    return lines;
  }, [filteredData, selectedComponents, componentGroups, groupColors]);

  // Render statistical overlays
  const renderStatistics = useMemo(() => {
    if (!showStatistics || !statisticalOverlays.length) return null;

    return statisticalOverlays.map((overlay, index) => (
      <ReferenceLine
        key={`stat-${index}`}
        y={overlay.value}
        stroke={alpha(theme.palette.warning.main, 0.7)}
        strokeDasharray="8,8"
        strokeWidth={1}
        label={{
          value: `${overlay.label}: ${overlay.value.toFixed(3)}`,
          position: 'topLeft',
          style: { fontSize: 12, fill: theme.palette.text.secondary },
        }}
      />
    ));
  }, [showStatistics, statisticalOverlays, theme]);

  // Handle mouse events for time range selection
  const handleMouseDown = useCallback((e: any) => {
    if (!enableZoom || !e) return;
    setIsSelecting(true);
  }, [enableZoom]);

  const handleMouseMove = useCallback((e: any) => {
    if (!isSelecting || !e) return;
    
    const { activeLabel } = e;
    if (activeLabel && zoomDomain?.start) {
      setZoomDomain({
        start: zoomDomain.start,
        end: parseFloat(activeLabel),
      });
    }
  }, [isSelecting, zoomDomain]);

  const handleMouseUp = useCallback(() => {
    if (!isSelecting || !zoomDomain) {
      setIsSelecting(false);
      return;
    }

    setIsSelecting(false);
    
    if (zoomDomain.end > zoomDomain.start && onTimeRangeSelect) {
      onTimeRangeSelect(zoomDomain);
    }
    
    setZoomDomain(null);
  }, [isSelecting, zoomDomain, onTimeRangeSelect]);

  // Custom tooltip with component grouping information
  const CustomTooltip = ({ active, payload, label }: any) => {
    if (!active || !payload?.length) return null;

    return (
      <Paper
        elevation={8}
        sx={{
          p: 2,
          backgroundColor: alpha(theme.palette.background.paper, 0.95),
          border: `1px solid ${theme.palette.divider}`,
        }}
      >
        <Typography variant="subtitle2" sx={{ mb: 1 }}>
          Time: {parseFloat(label).toFixed(3)}s
        </Typography>
        {payload.map((entry: any, index: number) => {
          const group = componentGroups.find(g => 
            g.componentIds.includes(entry.dataKey)
          );
          
          return (
            <Box key={index} sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
              <Box
                sx={{
                  width: 12,
                  height: 12,
                  backgroundColor: entry.color,
                  borderRadius: group?.type === 'struts' ? '50%' : '20%',
                }}
              />
              <Typography variant="body2">
                {entry.dataKey}: {entry.value?.toFixed(4)}
                {group && (
                  <Typography
                    component="span"
                    variant="caption"
                    sx={{ ml: 1, color: theme.palette.text.secondary }}
                  >
                    ({group.type})
                  </Typography>
                )}
              </Typography>
            </Box>
          );
        })}
      </Paper>
    );
  };

  if (loading) {
    return (
      <Box sx={{ width: '100%', height }}>
        <Skeleton variant="rectangular" width="100%" height="100%" />
      </Box>
    );
  }

  return (
    <Paper elevation={2} sx={{ p: 2, height: height + 100 }}>
      {title && (
        <Typography variant="h6" sx={{ mb: 2 }}>
          {title}
        </Typography>
      )}
      
      <ResponsiveContainer width="100%" height={height}>
        <LineChart
          ref={chartRef}
          data={filteredData}
          syncId={syncId}
          onMouseDown={handleMouseDown}
          onMouseMove={handleMouseMove}
          onMouseUp={handleMouseUp}
        >
          {showGrid && (
            <CartesianGrid
              strokeDasharray="3,3"
              stroke={alpha(theme.palette.text.disabled, 0.2)}
            />
          )}
          
          <XAxis
            dataKey="time"
            type="number"
            scale="linear"
            domain={timeRange ? [timeRange.start, timeRange.end] : ['dataMin', 'dataMax']}
            tickFormatter={(value) => value.toFixed(2)}
            label={{
              value: xAxisLabel,
              position: 'insideBottom',
              offset: -10,
            }}
            stroke={theme.palette.text.secondary}
          />
          
          <YAxis
            label={{
              value: yAxisLabel,
              angle: -90,
              position: 'insideLeft',
            }}
            tickFormatter={(value) => value.toExponential(2)}
            stroke={theme.palette.text.secondary}
          />
          
          <Tooltip content={<CustomTooltip />} />
          
          <Legend
            wrapperStyle={{
              paddingTop: '20px',
              fontSize: '12px',
            }}
            iconType="line"
          />
          
          {renderLines}
          {renderStatistics}
          
          {/* Time range selection area */}
          {isSelecting && zoomDomain && (
            <ReferenceArea
              x1={zoomDomain.start}
              x2={zoomDomain.end}
              fill={alpha(theme.palette.primary.main, 0.2)}
              stroke={theme.palette.primary.main}
            />
          )}
        </LineChart>
      </ResponsiveContainer>
    </Paper>
  );
};

export default TimeSeries;