import React from 'react';
import {
  Box,
  Card,
  FormControl,
  FormControlLabel,
  FormGroup,
  InputLabel,
  MenuItem,
  Select,
  Switch,
  TextField,
  Slider,
  Typography,
  Divider,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  Chip,
  IconButton,
  Tooltip,
} from '@mui/material';
import {
  ExpandMore as ExpandMoreIcon,
  Download as DownloadIcon,
  Refresh as RefreshIcon,
  Settings as SettingsIcon,
} from '@mui/icons-material';
import { TimeSeriesControlsState, ComponentGroup } from './types';

interface TimeSeriesControlsProps {
  state: TimeSeriesControlsState;
  onChange: (newState: Partial<TimeSeriesControlsState>) => void;
  componentGroups: ComponentGroup[];
  availableComponents: string[];
  onExport?: () => void;
  onRefresh?: () => void;
  onReset?: () => void;
}

const TimeSeriesControls: React.FC<TimeSeriesControlsProps> = ({
  state,
  onChange,
  componentGroups,
  availableComponents,
  onExport,
  onRefresh,
  onReset,
}) => {
  const handleComponentToggle = (componentId: string) => {
    const newSelected = state.selectedComponents.includes(componentId)
      ? state.selectedComponents.filter(id => id !== componentId)
      : [...state.selectedComponents, componentId];
    
    onChange({ selectedComponents: newSelected });
  };

  const handleGroupToggle = (groupId: string) => {
    const group = componentGroups.find(g => g.id === groupId);
    if (!group) return;

    const groupComponents = group.componentIds;
    const allSelected = groupComponents.every(id => 
      state.selectedComponents.includes(id)
    );

    const newSelected = allSelected
      ? state.selectedComponents.filter(id => !groupComponents.includes(id))
      : [...new Set([...state.selectedComponents, ...groupComponents])];

    onChange({ selectedComponents: newSelected });
  };

  const getGroupSelectionState = (groupId: string) => {
    const group = componentGroups.find(g => g.id === groupId);
    if (!group) return 'none';

    const groupComponents = group.componentIds;
    const selectedCount = groupComponents.filter(id => 
      state.selectedComponents.includes(id)
    ).length;

    if (selectedCount === 0) return 'none';
    if (selectedCount === groupComponents.length) return 'all';
    return 'partial';
  };

  return (
    <Card sx={{ p: 2, mb: 2 }}>
      <Box sx={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', mb: 2 }}>
        <Typography variant="h6">
          Time Series Controls
        </Typography>
        
        <Box sx={{ display: 'flex', gap: 1 }}>
          {onRefresh && (
            <Tooltip title="Refresh Data">
              <IconButton onClick={onRefresh} size="small">
                <RefreshIcon />
              </IconButton>
            </Tooltip>
          )}
          {onExport && (
            <Tooltip title="Export Chart">
              <IconButton onClick={onExport} size="small">
                <DownloadIcon />
              </IconButton>
            </Tooltip>
          )}
        </Box>
      </Box>

      <Accordion defaultExpanded>
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <Typography variant="subtitle1">Component Selection</Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Box sx={{ mb: 2 }}>
            <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
              Selected: {state.selectedComponents.length} of {availableComponents.length}
            </Typography>
            
            {/* Group selection */}
            {componentGroups.map((group) => {
              const selectionState = getGroupSelectionState(group.id);
              
              return (
                <Box key={group.id} sx={{ mb: 2 }}>
                  <FormControlLabel
                    control={
                      <Switch
                        checked={selectionState === 'all'}
                        indeterminate={selectionState === 'partial'}
                        onChange={() => handleGroupToggle(group.id)}
                        color="primary"
                      />
                    }
                    label={
                      <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                        <Typography variant="subtitle2">
                          {group.name}
                        </Typography>
                        <Chip
                          label={group.type}
                          size="small"
                          variant="outlined"
                          sx={{ textTransform: 'capitalize' }}
                        />
                      </Box>
                    }
                  />
                  
                  <Box sx={{ ml: 4, display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                    {group.componentIds.map((componentId) => (
                      <Chip
                        key={componentId}
                        label={componentId}
                        size="small"
                        clickable
                        variant={state.selectedComponents.includes(componentId) ? 'filled' : 'outlined'}
                        onClick={() => handleComponentToggle(componentId)}
                        color={state.selectedComponents.includes(componentId) ? 'primary' : 'default'}
                      />
                    ))}
                  </Box>
                </Box>
              );
            })}
          </Box>
        </AccordionDetails>
      </Accordion>

      <Accordion>
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <Typography variant="subtitle1">Display Options</Typography>
        </AccordionSummary>
        <AccordionDetails>
          <FormGroup>
            <FormControlLabel
              control={
                <Switch
                  checked={state.showStatistics}
                  onChange={(e) => onChange({ showStatistics: e.target.checked })}
                />
              }
              label="Show Statistical Overlays"
            />
            
            <FormControlLabel
              control={
                <Switch
                  checked={state.showGrid}
                  onChange={(e) => onChange({ showGrid: e.target.checked })}
                />
              }
              label="Show Grid"
            />
            
            <FormControlLabel
              control={
                <Switch
                  checked={state.enableZoom}
                  onChange={(e) => onChange({ enableZoom: e.target.checked })}
                />
              }
              label="Enable Zoom Selection"
            />
            
            <FormControlLabel
              control={
                <Switch
                  checked={state.syncCharts}
                  onChange={(e) => onChange({ syncCharts: e.target.checked })}
                />
              }
              label="Synchronize Multiple Charts"
            />
          </FormGroup>
        </AccordionDetails>
      </Accordion>

      <Accordion>
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <Typography variant="subtitle1">Performance</Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Box sx={{ mb: 2 }}>
            <Typography gutterBottom>
              Data Decimation Factor: {state.decimationFactor}x
            </Typography>
            <Slider
              value={state.decimationFactor}
              onChange={(_, value) => onChange({ decimationFactor: value as number })}
              min={1}
              max={100}
              step={1}
              marks={[
                { value: 1, label: '1x' },
                { value: 10, label: '10x' },
                { value: 50, label: '50x' },
                { value: 100, label: '100x' },
              ]}
              valueLabelDisplay="auto"
              sx={{ width: '100%' }}
            />
            <Typography variant="caption" color="text.secondary">
              Higher values improve performance but reduce data resolution
            </Typography>
          </Box>
        </AccordionDetails>
      </Accordion>

      <Accordion>
        <AccordionSummary expandIcon={<ExpandMoreIcon />}>
          <Typography variant="subtitle1">Chart Settings</Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
            <TextField
              label="Chart Height (px)"
              type="number"
              value={state.chartHeight}
              onChange={(e) => onChange({ chartHeight: parseInt(e.target.value) || 400 })}
              inputProps={{ min: 200, max: 1000, step: 50 }}
              size="small"
            />

            <FormControl size="small">
              <InputLabel>Statistical Overlays</InputLabel>
              <Select
                multiple
                value={state.statisticalTypes}
                onChange={(e) => onChange({ 
                  statisticalTypes: typeof e.target.value === 'string' 
                    ? e.target.value.split(',') 
                    : e.target.value 
                })}
                renderValue={(selected) => (
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                    {selected.map((value) => (
                      <Chip key={value} label={value} size="small" />
                    ))}
                  </Box>
                )}
              >
                <MenuItem value="mean">Mean</MenuItem>
                <MenuItem value="stdDev">Standard Deviation</MenuItem>
                <MenuItem value="rms">RMS</MenuItem>
                <MenuItem value="p95">95th Percentile</MenuItem>
                <MenuItem value="p99">99th Percentile</MenuItem>
                <MenuItem value="min">Minimum</MenuItem>
                <MenuItem value="max">Maximum</MenuItem>
              </Select>
            </FormControl>

            <TextField
              label="Y-Axis Label"
              value={state.yAxisLabel}
              onChange={(e) => onChange({ yAxisLabel: e.target.value })}
              size="small"
              fullWidth
            />

            <TextField
              label="X-Axis Label"
              value={state.xAxisLabel}
              onChange={(e) => onChange({ xAxisLabel: e.target.value })}
              size="small"
              fullWidth
            />
          </Box>
        </AccordionDetails>
      </Accordion>

      {/* Quick Actions */}
      <Divider sx={{ my: 2 }} />
      
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
        <Typography variant="caption" color="text.secondary">
          Data points: {availableComponents.length}
        </Typography>
        
        {onReset && (
          <Tooltip title="Reset to Default Settings">
            <IconButton onClick={onReset} size="small">
              <SettingsIcon />
            </IconButton>
          </Tooltip>
        )}
      </Box>
    </Card>
  );
};

export default TimeSeriesControls;