import React, { useState, useEffect, useMemo } from 'react';
import {
  Box,
  Chip,
  FormControl,
  InputLabel,
  MenuItem,
  OutlinedInput,
  Select,
  SelectChangeEvent,
  TextField,
  Typography,
  Checkbox,
  ListItemText,
  Slider,
  Grid,
  Paper,
  ToggleButtonGroup,
  ToggleButton,
  IconButton,
  Tooltip,
  Alert,
} from '@mui/material';
import {
  Clear as ClearIcon,
  Explore as CompassIcon,
  PlayArrow as ArrowIcon,
  Add as AddIcon,
  Remove as RemoveIcon,
} from '@mui/icons-material';
import { useFilterContext } from './FilterContext';
import { useSelector } from 'react-redux';
import { RootState } from '../../store';

interface HeadingFilterProps {
  onSelectionChange?: (selectedHeadings: number[]) => void;
  disabled?: boolean;
  minHeading?: number;
  maxHeading?: number;
  step?: number;
}

const COMMON_HEADINGS = [0, 45, 90, 135, 180, 225, 270, 315];
const CARDINAL_DIRECTIONS = [
  { heading: 0, label: 'N', fullLabel: 'North' },
  { heading: 45, label: 'NE', fullLabel: 'Northeast' },
  { heading: 90, label: 'E', fullLabel: 'East' },
  { heading: 135, label: 'SE', fullLabel: 'Southeast' },
  { heading: 180, label: 'S', fullLabel: 'South' },
  { heading: 225, label: 'SW', fullLabel: 'Southwest' },
  { heading: 270, label: 'W', fullLabel: 'West' },
  { heading: 315, label: 'NW', fullLabel: 'Northwest' },
];

export const HeadingFilter: React.FC<HeadingFilterProps> = ({
  onSelectionChange,
  disabled = false,
  minHeading = 0,
  maxHeading = 345,
  step = 15,
}) => {
  const { state, actions } = useFilterContext();
  const [filterMode, setFilterMode] = useState<'range' | 'individual' | 'visual'>('individual');
  const [rangeValue, setRangeValue] = useState<number[]>([0, 345]);
  const [customHeading, setCustomHeading] = useState<string>('');
  
  // Get available headings based on selected analysis cases and loading conditions
  const analysisData = useSelector((state: RootState) => state.dashboard?.data);
  const selectedAnalysisCases = state.analysisCase;
  const selectedLoadingConditions = state.loadingCondition;

  const availableHeadings = useMemo(() => {
    if (selectedAnalysisCases.length === 0 || !analysisData) {
      // Return all valid headings
      const headings: number[] = [];
      for (let h = minHeading; h <= maxHeading; h += step) {
        headings.push(h);
      }
      return headings;
    }

    const headings = new Set<number>();
    
    selectedAnalysisCases.forEach(caseId => {
      const caseData = analysisData[caseId];
      if (caseData?.headings) {
        // If loading conditions are selected, filter headings by those conditions
        if (selectedLoadingConditions.length > 0) {
          selectedLoadingConditions.forEach(condition => {
            const conditionData = caseData.headings[`${condition.waterLevel}-${condition.volume}-${condition.side}`];
            if (conditionData) {
              conditionData.forEach((h: number) => headings.add(h));
            }
          });
        } else {
          // Add all headings for the case
          Object.values(caseData.headings).forEach((conditionHeadings: any) => {
            if (Array.isArray(conditionHeadings)) {
              conditionHeadings.forEach((h: number) => headings.add(h));
            }
          });
        }
      }
    });

    return Array.from(headings).sort((a, b) => a - b);
  }, [selectedAnalysisCases, selectedLoadingConditions, analysisData, minHeading, maxHeading, step]);

  const selectedHeadings = state.heading;

  // Validate heading value
  const isValidHeading = (heading: number): boolean => {
    return heading >= minHeading && heading <= maxHeading && heading % step === 0;
  };

  // Handle range mode
  const handleRangeChange = (event: Event, newValue: number | number[]) => {
    const range = newValue as number[];
    setRangeValue(range);
    
    const headings = availableHeadings.filter(h => h >= range[0] && h <= range[1]);
    actions.setHeading(headings);
    onSelectionChange?.(headings);
  };

  // Handle individual selection
  const handleIndividualChange = (event: SelectChangeEvent<string[]>) => {
    const value = event.target.value;
    const headingStrings = typeof value === 'string' ? value.split(',') : value;
    const headings = headingStrings.map(Number).filter(h => !isNaN(h));
    
    actions.setHeading(headings);
    onSelectionChange?.(headings);
  };

  // Handle cardinal direction toggle
  const handleCardinalToggle = (heading: number) => {
    const newSelection = selectedHeadings.includes(heading)
      ? selectedHeadings.filter(h => h !== heading)
      : [...selectedHeadings, heading].sort((a, b) => a - b);
    
    actions.setHeading(newSelection);
    onSelectionChange?.(newSelection);
  };

  // Handle custom heading addition
  const handleAddCustomHeading = () => {
    const heading = parseFloat(customHeading);
    if (!isNaN(heading) && isValidHeading(heading) && !selectedHeadings.includes(heading)) {
      const newSelection = [...selectedHeadings, heading].sort((a, b) => a - b);
      actions.setHeading(newSelection);
      onSelectionChange?.(newSelection);
      setCustomHeading('');
    }
  };

  // Handle quick selection
  const handleSelectCommon = () => {
    const commonAvailable = COMMON_HEADINGS.filter(h => availableHeadings.includes(h));
    actions.setHeading(commonAvailable);
    onSelectionChange?.(commonAvailable);
  };

  const handleSelectAll = () => {
    actions.setHeading(availableHeadings);
    onSelectionChange?.(availableHeadings);
  };

  const handleClearAll = () => {
    actions.setHeading([]);
    setRangeValue([minHeading, maxHeading]);
    onSelectionChange?.([]);
  };

  // Format heading for display with cardinal direction
  const formatHeading = (heading: number): string => {
    const cardinal = CARDINAL_DIRECTIONS.find(dir => dir.heading === heading);
    return cardinal ? `${heading}° (${cardinal.label})` : `${heading}°`;
  };

  // Visual compass component
  const CompassVisual: React.FC<{ size?: number }> = ({ size = 200 }) => {
    const center = size / 2;
    const radius = center - 20;
    
    return (
      <Box sx={{ display: 'flex', justifyContent: 'center', mb: 2 }}>
        <svg width={size} height={size}>
          {/* Compass circle */}
          <circle
            cx={center}
            cy={center}
            r={radius}
            fill="none"
            stroke="#ccc"
            strokeWidth="2"
          />
          
          {/* Cardinal directions */}
          {CARDINAL_DIRECTIONS.map((dir) => {
            const angle = (dir.heading - 90) * (Math.PI / 180); // -90 to start from top
            const x1 = center + (radius - 15) * Math.cos(angle);
            const y1 = center + (radius - 15) * Math.sin(angle);
            const x2 = center + radius * Math.cos(angle);
            const y2 = center + radius * Math.sin(angle);
            const textX = center + (radius + 12) * Math.cos(angle);
            const textY = center + (radius + 12) * Math.sin(angle);
            
            const isSelected = selectedHeadings.includes(dir.heading);
            const isAvailable = availableHeadings.includes(dir.heading);
            
            return (
              <g key={dir.heading}>
                {/* Tick mark */}
                <line
                  x1={x1}
                  y1={y1}
                  x2={x2}
                  y2={y2}
                  stroke={isSelected ? '#1976d2' : isAvailable ? '#666' : '#ccc'}
                  strokeWidth={isSelected ? 3 : 2}
                />
                
                {/* Label */}
                <text
                  x={textX}
                  y={textY}
                  textAnchor="middle"
                  dominantBaseline="middle"
                  fontSize="12"
                  fill={isSelected ? '#1976d2' : isAvailable ? '#666' : '#ccc'}
                  style={{ cursor: isAvailable ? 'pointer' : 'default' }}
                  onClick={() => isAvailable && handleCardinalToggle(dir.heading)}
                >
                  {dir.label}
                </text>
                
                {/* Selected indicator */}
                {isSelected && (
                  <circle
                    cx={textX}
                    cy={textY}
                    r="10"
                    fill="rgba(25, 118, 210, 0.2)"
                    stroke="#1976d2"
                    strokeWidth="1"
                  />
                )}
              </g>
            );
          })}
          
          {/* Center point */}
          <circle cx={center} cy={center} r="3" fill="#666" />
          
          {/* North indicator */}
          <text
            x={center}
            y={15}
            textAnchor="middle"
            fontSize="10"
            fontWeight="bold"
            fill="#666"
          >
            N
          </text>
        </svg>
      </Box>
    );
  };

  const isDisabled = disabled || selectedAnalysisCases.length === 0;

  return (
    <Box sx={{ minWidth: 350 }}>
      {/* Mode toggle */}
      <Box sx={{ mb: 2 }}>
        <ToggleButtonGroup
          value={filterMode}
          exclusive
          onChange={(e, value) => value && setFilterMode(value)}
          size="small"
        >
          <ToggleButton value="individual">Individual</ToggleButton>
          <ToggleButton value="range">Range</ToggleButton>
          <ToggleButton value="visual">Visual</ToggleButton>
        </ToggleButtonGroup>
      </Box>

      {/* Warning when no analysis cases selected */}
      {selectedAnalysisCases.length === 0 && (
        <Alert severity="info" sx={{ mb: 2 }}>
          Select analysis cases first to see available headings
        </Alert>
      )}

      {filterMode === 'range' && (
        <Box>
          <Typography variant="subtitle2" gutterBottom>
            Heading Range (degrees)
          </Typography>
          <Slider
            value={rangeValue}
            onChange={handleRangeChange}
            valueLabelDisplay="on"
            min={minHeading}
            max={maxHeading}
            step={step}
            marks={CARDINAL_DIRECTIONS.map(dir => ({
              value: dir.heading,
              label: dir.label,
            }))}
            disabled={isDisabled}
            sx={{ mx: 2 }}
          />
          <Typography variant="caption" color="text.secondary">
            Selected range: {rangeValue[0]}° - {rangeValue[1]}°
          </Typography>
        </Box>
      )}

      {filterMode === 'individual' && (
        <FormControl fullWidth size="small">
          <InputLabel id="heading-select-label">Headings</InputLabel>
          <Select
            labelId="heading-select-label"
            multiple
            value={selectedHeadings.map(String)}
            onChange={handleIndividualChange}
            input={<OutlinedInput label="Headings" />}
            renderValue={(selected) => (
              <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5, maxHeight: 100, overflow: 'auto' }}>
                {selected.map((headingStr) => {
                  const heading = Number(headingStr);
                  return (
                    <Chip
                      key={heading}
                      label={formatHeading(heading)}
                      size="small"
                      onDelete={(e) => {
                        e.stopPropagation();
                        const newSelected = selectedHeadings.filter(h => h !== heading);
                        actions.setHeading(newSelected);
                        onSelectionChange?.(newSelected);
                      }}
                      deleteIcon={<ClearIcon />}
                    />
                  );
                })}
              </Box>
            )}
            disabled={isDisabled}
          >
            {availableHeadings.map((heading) => (
              <MenuItem key={heading} value={String(heading)}>
                <Checkbox
                  checked={selectedHeadings.includes(heading)}
                  size="small"
                />
                <ListItemText primary={formatHeading(heading)} />
              </MenuItem>
            ))}
            
            {availableHeadings.length === 0 && !isDisabled && (
              <MenuItem disabled>
                <Typography variant="body2" color="text.secondary">
                  No headings available for current selection
                </Typography>
              </MenuItem>
            )}
          </Select>
        </FormControl>
      )}

      {filterMode === 'visual' && (
        <Box>
          <CompassVisual />
          <Typography variant="caption" color="text.secondary" align="center" display="block">
            Click on compass directions to select/deselect headings
          </Typography>
        </Box>
      )}

      {/* Custom heading input */}
      <Box sx={{ mt: 2, display: 'flex', gap: 1, alignItems: 'center' }}>
        <TextField
          size="small"
          label="Custom Heading"
          value={customHeading}
          onChange={(e) => setCustomHeading(e.target.value)}
          placeholder="Enter heading (0-345°)"
          sx={{ flexGrow: 1 }}
          disabled={isDisabled}
          helperText={`Must be between ${minHeading}° and ${maxHeading}° in ${step}° increments`}
        />
        <IconButton
          onClick={handleAddCustomHeading}
          disabled={
            isDisabled ||
            !customHeading ||
            !isValidHeading(parseFloat(customHeading)) ||
            selectedHeadings.includes(parseFloat(customHeading))
          }
        >
          <AddIcon />
        </IconButton>
      </Box>

      {/* Quick selection buttons */}
      <Box sx={{ mt: 2, display: 'flex', gap: 1, flexWrap: 'wrap' }}>
        <Chip
          label="Select Common"
          size="small"
          variant="outlined"
          onClick={handleSelectCommon}
          disabled={isDisabled}
        />
        <Chip
          label="Select All Available"
          size="small"
          variant="outlined"
          onClick={handleSelectAll}
          disabled={isDisabled || availableHeadings.length === 0}
        />
        <Chip
          label="Clear All"
          size="small"
          variant="outlined"
          onClick={handleClearAll}
          disabled={selectedHeadings.length === 0}
        />
        <Typography variant="caption" sx={{ alignSelf: 'center', ml: 'auto' }}>
          {selectedHeadings.length} of {availableHeadings.length} selected
        </Typography>
      </Box>

      {/* Current selections display */}
      {selectedHeadings.length > 0 && (
        <Box sx={{ mt: 2 }}>
          <Typography variant="caption" color="text.secondary" gutterBottom>
            Selected Headings:
          </Typography>
          <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
            {selectedHeadings.sort((a, b) => a - b).map((heading) => (
              <Chip
                key={heading}
                label={formatHeading(heading)}
                size="small"
                variant="outlined"
                onDelete={() => {
                  const newSelected = selectedHeadings.filter(h => h !== heading);
                  actions.setHeading(newSelected);
                  onSelectionChange?.(newSelected);
                }}
                icon={<CompassIcon />}
              />
            ))}
          </Box>
        </Box>
      )}

      {/* Error display */}
      {state.error && state.error.includes('heading') && (
        <Typography variant="caption" color="error" sx={{ mt: 0.5 }}>
          {state.error}
        </Typography>
      )}

      {/* Statistics */}
      {!isDisabled && availableHeadings.length > 0 && (
        <Box sx={{ mt: 2, p: 1, bgcolor: 'background.paper', borderRadius: 1 }}>
          <Typography variant="caption" color="text.secondary">
            Available range: {Math.min(...availableHeadings)}° - {Math.max(...availableHeadings)}°
            {' '}({availableHeadings.length} headings available)
          </Typography>
        </Box>
      )}
    </Box>
  );
};

export default HeadingFilter;