import React, { useState, useMemo, useCallback, useEffect } from 'react';
import {
  Box,
  Card,
  Typography,
  Slider,
  TextField,
  Button,
  ButtonGroup,
  IconButton,
  Tooltip,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Chip,
  Paper,
} from '@mui/material';
import {
  ZoomIn as ZoomInIcon,
  ZoomOut as ZoomOutIcon,
  ZoomOutMap as ZoomOutMapIcon,
  PlayArrow as PlayArrowIcon,
  Pause as PauseIcon,
  SkipPrevious as SkipPreviousIcon,
  SkipNext as SkipNextIcon,
} from '@mui/icons-material';
import { useTheme } from '@mui/material/styles';
import { TimeSeriesData, TimeRange } from './types';

interface TimeRangeSelectorProps {
  data: TimeSeriesData[];
  selectedRange?: TimeRange | null;
  onRangeChange?: (range: TimeRange | null) => void;
  label?: string;
  enableAnimation?: boolean;
  presetRanges?: { label: string; range: TimeRange }[];
  showStatistics?: boolean;
  disabled?: boolean;
}

const TimeRangeSelector: React.FC<TimeRangeSelectorProps> = ({
  data,
  selectedRange,
  onRangeChange,
  label = 'Time Range',
  enableAnimation = false,
  presetRanges = [],
  showStatistics = true,
  disabled = false,
}) => {
  const theme = useTheme();
  
  // Animation state
  const [isAnimating, setIsAnimating] = useState(false);
  const [animationSpeed, setAnimationSpeed] = useState(1); // seconds per real second
  const [currentAnimationTime, setCurrentAnimationTime] = useState(0);

  // Calculate data bounds
  const dataBounds = useMemo(() => {
    if (!data.length) return { min: 0, max: 100 };
    
    const times = data.map(d => d.time).filter(t => t !== undefined);
    return {
      min: Math.min(...times),
      max: Math.max(...times),
    };
  }, [data]);

  // Current range or data bounds
  const currentRange = selectedRange || dataBounds;
  const duration = currentRange.end - currentRange.start;

  // Default preset ranges
  const defaultPresets = useMemo(() => [
    {
      label: 'Full Range',
      range: dataBounds,
    },
    {
      label: 'First 25%',
      range: {
        start: dataBounds.min,
        end: dataBounds.min + (dataBounds.max - dataBounds.min) * 0.25,
      },
    },
    {
      label: 'Middle 50%',
      range: {
        start: dataBounds.min + (dataBounds.max - dataBounds.min) * 0.25,
        end: dataBounds.min + (dataBounds.max - dataBounds.min) * 0.75,
      },
    },
    {
      label: 'Last 25%',
      range: {
        start: dataBounds.min + (dataBounds.max - dataBounds.min) * 0.75,
        end: dataBounds.max,
      },
    },
    {
      label: 'Last 10s',
      range: {
        start: Math.max(dataBounds.min, dataBounds.max - 10),
        end: dataBounds.max,
      },
    },
    {
      label: 'Last 60s',
      range: {
        start: Math.max(dataBounds.min, dataBounds.max - 60),
        end: dataBounds.max,
      },
    },
  ], [dataBounds]);

  const allPresets = [...defaultPresets, ...presetRanges];

  // Handle range change
  const handleRangeChange = useCallback((newRange: [number, number]) => {
    const range: TimeRange = {
      start: newRange[0],
      end: newRange[1],
    };
    onRangeChange?.(range);
  }, [onRangeChange]);

  // Handle text input changes
  const handleStartChange = useCallback((value: string) => {
    const start = parseFloat(value);
    if (isNaN(start)) return;
    
    handleRangeChange([start, currentRange.end]);
  }, [currentRange.end, handleRangeChange]);

  const handleEndChange = useCallback((value: string) => {
    const end = parseFloat(value);
    if (isNaN(end)) return;
    
    handleRangeChange([currentRange.start, end]);
  }, [currentRange.start, handleRangeChange]);

  // Zoom functions
  const zoomIn = useCallback(() => {
    const center = (currentRange.start + currentRange.end) / 2;
    const newDuration = duration * 0.5;
    const newRange: TimeRange = {
      start: Math.max(dataBounds.min, center - newDuration / 2),
      end: Math.min(dataBounds.max, center + newDuration / 2),
    };
    onRangeChange?.(newRange);
  }, [currentRange, duration, dataBounds, onRangeChange]);

  const zoomOut = useCallback(() => {
    const center = (currentRange.start + currentRange.end) / 2;
    const newDuration = Math.min(duration * 2, dataBounds.max - dataBounds.min);
    const newRange: TimeRange = {
      start: Math.max(dataBounds.min, center - newDuration / 2),
      end: Math.min(dataBounds.max, center + newDuration / 2),
    };
    onRangeChange?.(newRange);
  }, [currentRange, duration, dataBounds, onRangeChange]);

  const resetZoom = useCallback(() => {
    onRangeChange?.(null);
  }, [onRangeChange]);

  // Pan functions
  const panLeft = useCallback(() => {
    const panAmount = duration * 0.1;
    const newStart = Math.max(dataBounds.min, currentRange.start - panAmount);
    const newEnd = newStart + duration;
    
    if (newEnd <= dataBounds.max) {
      handleRangeChange([newStart, newEnd]);
    }
  }, [currentRange, duration, dataBounds, handleRangeChange]);

  const panRight = useCallback(() => {
    const panAmount = duration * 0.1;
    const newEnd = Math.min(dataBounds.max, currentRange.end + panAmount);
    const newStart = newEnd - duration;
    
    if (newStart >= dataBounds.min) {
      handleRangeChange([newStart, newEnd]);
    }
  }, [currentRange, duration, dataBounds, handleRangeChange]);

  // Animation functions
  const startAnimation = useCallback(() => {
    setIsAnimating(true);
    setCurrentAnimationTime(currentRange.start);
  }, [currentRange.start]);

  const stopAnimation = useCallback(() => {
    setIsAnimating(false);
  }, []);

  // Animation loop
  useEffect(() => {
    if (!isAnimating) return;

    const interval = setInterval(() => {
      setCurrentAnimationTime(prev => {
        const next = prev + (0.1 * animationSpeed); // 100ms intervals
        
        if (next >= currentRange.end) {
          setIsAnimating(false);
          return currentRange.start; // Loop back to start
        }
        
        // Update visible window during animation
        const windowSize = Math.min(10, duration * 0.1); // Show 10s or 10% of range
        const windowStart = Math.max(currentRange.start, next - windowSize / 2);
        const windowEnd = Math.min(currentRange.end, next + windowSize / 2);
        
        onRangeChange?.({ start: windowStart, end: windowEnd });
        
        return next;
      });
    }, 100);

    return () => clearInterval(interval);
  }, [isAnimating, animationSpeed, currentRange, duration, onRangeChange]);

  // Statistics
  const rangeStatistics = useMemo(() => {
    if (!showStatistics) return null;
    
    const pointsInRange = data.filter(d => 
      d.time >= currentRange.start && d.time <= currentRange.end
    ).length;
    
    const totalPoints = data.length;
    const coverage = totalPoints > 0 ? (pointsInRange / totalPoints) * 100 : 0;
    
    return {
      pointsInRange,
      totalPoints,
      coverage,
      duration,
      samplingRate: pointsInRange > 0 ? pointsInRange / duration : 0,
    };
  }, [data, currentRange, duration, showStatistics]);

  return (
    <Card sx={{ p: 2 }}>
      <Typography variant="subtitle1" sx={{ mb: 2 }}>
        {label}
      </Typography>

      {/* Main Range Slider */}
      <Box sx={{ px: 2, mb: 3 }}>
        <Slider
          value={[currentRange.start, currentRange.end]}
          onChange={(_, value) => handleRangeChange(value as [number, number])}
          min={dataBounds.min}
          max={dataBounds.max}
          step={(dataBounds.max - dataBounds.min) / 1000}
          valueLabelDisplay="auto"
          valueLabelFormat={(value) => `${value.toFixed(2)}s`}
          disabled={disabled}
          marks={[
            { value: dataBounds.min, label: `${dataBounds.min.toFixed(1)}s` },
            { value: dataBounds.max, label: `${dataBounds.max.toFixed(1)}s` },
          ]}
        />
      </Box>

      {/* Precise Input Controls */}
      <Box sx={{ display: 'flex', gap: 2, mb: 2, alignItems: 'center' }}>
        <TextField
          label="Start Time"
          value={currentRange.start.toFixed(3)}
          onChange={(e) => handleStartChange(e.target.value)}
          size="small"
          disabled={disabled}
          InputProps={{ endAdornment: 's' }}
          sx={{ flex: 1 }}
        />
        
        <TextField
          label="End Time"
          value={currentRange.end.toFixed(3)}
          onChange={(e) => handleEndChange(e.target.value)}
          size="small"
          disabled={disabled}
          InputProps={{ endAdornment: 's' }}
          sx={{ flex: 1 }}
        />

        <TextField
          label="Duration"
          value={duration.toFixed(3)}
          size="small"
          disabled
          InputProps={{ endAdornment: 's' }}
          sx={{ flex: 1 }}
        />
      </Box>

      {/* Control Buttons */}
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 2 }}>
        {/* Zoom Controls */}
        <ButtonGroup size="small" disabled={disabled}>
          <Tooltip title="Zoom In">
            <IconButton onClick={zoomIn}>
              <ZoomInIcon />
            </IconButton>
          </Tooltip>
          <Tooltip title="Zoom Out">
            <IconButton onClick={zoomOut}>
              <ZoomOutIcon />
            </IconButton>
          </Tooltip>
          <Tooltip title="Reset Zoom">
            <IconButton onClick={resetZoom}>
              <ZoomOutMapIcon />
            </IconButton>
          </Tooltip>
        </ButtonGroup>

        {/* Pan Controls */}
        <ButtonGroup size="small" disabled={disabled}>
          <Tooltip title="Pan Left">
            <IconButton onClick={panLeft}>
              <SkipPreviousIcon />
            </IconButton>
          </Tooltip>
          <Tooltip title="Pan Right">
            <IconButton onClick={panRight}>
              <SkipNextIcon />
            </IconButton>
          </Tooltip>
        </ButtonGroup>

        {/* Animation Controls */}
        {enableAnimation && (
          <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
            <FormControl size="small" sx={{ minWidth: 80 }}>
              <Select
                value={animationSpeed}
                onChange={(e) => setAnimationSpeed(e.target.value as number)}
                disabled={disabled || isAnimating}
              >
                <MenuItem value={0.5}>0.5x</MenuItem>
                <MenuItem value={1}>1x</MenuItem>
                <MenuItem value={2}>2x</MenuItem>
                <MenuItem value={5}>5x</MenuItem>
                <MenuItem value={10}>10x</MenuItem>
              </Select>
            </FormControl>
            
            <IconButton
              onClick={isAnimating ? stopAnimation : startAnimation}
              disabled={disabled}
              color={isAnimating ? 'secondary' : 'primary'}
            >
              {isAnimating ? <PauseIcon /> : <PlayArrowIcon />}
            </IconButton>
          </Box>
        )}
      </Box>

      {/* Preset Ranges */}
      <Box sx={{ mb: 2 }}>
        <Typography variant="body2" sx={{ mb: 1 }}>
          Preset Ranges:
        </Typography>
        <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 1 }}>
          {allPresets.map((preset, index) => (
            <Chip
              key={index}
              label={preset.label}
              size="small"
              clickable
              disabled={disabled}
              onClick={() => onRangeChange?.(preset.range)}
              variant="outlined"
            />
          ))}
        </Box>
      </Box>

      {/* Statistics */}
      {rangeStatistics && (
        <Paper sx={{ p: 1, backgroundColor: theme.palette.grey[50] }}>
          <Box sx={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(120px, 1fr))', gap: 1 }}>
            <Box sx={{ textAlign: 'center' }}>
              <Typography variant="caption" color="text.secondary">
                Points in Range
              </Typography>
              <Typography variant="body2" fontWeight="bold">
                {rangeStatistics.pointsInRange.toLocaleString()}
              </Typography>
            </Box>
            
            <Box sx={{ textAlign: 'center' }}>
              <Typography variant="caption" color="text.secondary">
                Coverage
              </Typography>
              <Typography variant="body2" fontWeight="bold">
                {rangeStatistics.coverage.toFixed(1)}%
              </Typography>
            </Box>
            
            <Box sx={{ textAlign: 'center' }}>
              <Typography variant="caption" color="text.secondary">
                Sampling Rate
              </Typography>
              <Typography variant="body2" fontWeight="bold">
                {rangeStatistics.samplingRate.toFixed(1)} Hz
              </Typography>
            </Box>
          </Box>
        </Paper>
      )}

      {/* Animation Progress */}
      {isAnimating && (
        <Box sx={{ mt: 2 }}>
          <Typography variant="caption" color="text.secondary">
            Animation Progress: {((currentAnimationTime - currentRange.start) / duration * 100).toFixed(1)}%
          </Typography>
        </Box>
      )}
    </Card>
  );
};

export default TimeRangeSelector;