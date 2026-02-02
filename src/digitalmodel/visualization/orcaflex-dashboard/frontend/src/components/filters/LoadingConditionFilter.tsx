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
  Typography,
  Checkbox,
  ListItemText,
  Grid,
  Paper,
  ToggleButton,
  ToggleButtonGroup,
  Divider,
  Alert,
} from '@mui/material';
import {
  Clear as ClearIcon,
  Water as WaterIcon,
  LocalShipping as VolumeIcon,
  Navigation as SideIcon,
} from '@mui/icons-material';
import { useFilterContext, LoadingCondition } from './FilterContext';
import { useSelector } from 'react-redux';
import { RootState } from '../../store';

interface LoadingConditionFilterProps {
  onSelectionChange?: (selectedConditions: LoadingCondition[]) => void;
  disabled?: boolean;
  availableConditions?: LoadingCondition[];
}

const WATER_LEVEL_OPTIONS = [
  { value: 'hwl' as const, label: 'High Water Level (HWL)', icon: '🌊' },
  { value: 'lwl' as const, label: 'Low Water Level (LWL)', icon: '🏖️' },
];

const VOLUME_OPTIONS = [
  { value: '125km3' as const, label: '125 km³', icon: '📦' },
  { value: '180km3' as const, label: '180 km³', icon: '📦' },
];

const SIDE_OPTIONS = [
  { value: 'pb' as const, label: 'Port (PB)', icon: '⬅️' },
  { value: 'sb' as const, label: 'Starboard (SB)', icon: '➡️' },
];

export const LoadingConditionFilter: React.FC<LoadingConditionFilterProps> = ({
  onSelectionChange,
  disabled = false,
  availableConditions,
}) => {
  const { state, actions } = useFilterContext();
  const [filterMode, setFilterMode] = useState<'simple' | 'advanced'>('simple');
  const [waterLevels, setWaterLevels] = useState<('hwl' | 'lwl')[]>([]);
  const [volumes, setVolumes] = useState<('125km3' | '180km3')[]>([]);
  const [sides, setSides] = useState<('pb' | 'sb')[]>([]);

  // Get available loading conditions based on selected analysis cases
  const analysisData = useSelector((state: RootState) => state.dashboard?.data);
  const selectedAnalysisCases = state.analysisCase;

  const validCombinations = useMemo(() => {
    if (availableConditions) {
      return availableConditions;
    }

    // Generate valid combinations based on selected analysis cases
    if (selectedAnalysisCases.length === 0 || !analysisData) {
      return [];
    }

    const combinations: LoadingCondition[] = [];
    
    // Extract available combinations from analysis data
    selectedAnalysisCases.forEach(caseId => {
      const caseData = analysisData[caseId];
      if (caseData?.loadingConditions) {
        combinations.push(...caseData.loadingConditions);
      }
    });

    // Remove duplicates
    return combinations.filter((condition, index, self) =>
      index === self.findIndex(c =>
        c.waterLevel === condition.waterLevel &&
        c.volume === condition.volume &&
        c.side === condition.side
      )
    );
  }, [selectedAnalysisCases, analysisData, availableConditions]);

  const selectedConditions = state.loadingCondition;

  // Update individual selections when conditions change
  useEffect(() => {
    const uniqueWaterLevels = [...new Set(selectedConditions.map(c => c.waterLevel))];
    const uniqueVolumes = [...new Set(selectedConditions.map(c => c.volume))];
    const uniqueSides = [...new Set(selectedConditions.map(c => c.side))];
    
    setWaterLevels(uniqueWaterLevels);
    setVolumes(uniqueVolumes);
    setSides(uniqueSides);
  }, [selectedConditions]);

  // Handle simple mode selections
  const handleWaterLevelChange = (
    event: React.MouseEvent<HTMLElement>,
    newWaterLevels: ('hwl' | 'lwl')[]
  ) => {
    setWaterLevels(newWaterLevels);
    updateConditionsFromComponents(newWaterLevels, volumes, sides);
  };

  const handleVolumeChange = (
    event: React.MouseEvent<HTMLElement>,
    newVolumes: ('125km3' | '180km3')[]
  ) => {
    setVolumes(newVolumes);
    updateConditionsFromComponents(waterLevels, newVolumes, sides);
  };

  const handleSideChange = (
    event: React.MouseEvent<HTMLElement>,
    newSides: ('pb' | 'sb')[]
  ) => {
    setSides(newSides);
    updateConditionsFromComponents(waterLevels, volumes, newSides);
  };

  // Update conditions based on component selections
  const updateConditionsFromComponents = (
    wls: ('hwl' | 'lwl')[],
    vols: ('125km3' | '180km3')[],
    sds: ('pb' | 'sb')[]
  ) => {
    if (wls.length === 0 || vols.length === 0 || sds.length === 0) {
      actions.setLoadingCondition([]);
      onSelectionChange?.([]);
      return;
    }

    const combinations: LoadingCondition[] = [];
    wls.forEach(wl => {
      vols.forEach(vol => {
        sds.forEach(sd => {
          const condition = { waterLevel: wl, volume: vol, side: sd };
          // Only add if it's a valid combination
          if (validCombinations.some(vc => 
            vc.waterLevel === condition.waterLevel &&
            vc.volume === condition.volume &&
            vc.side === condition.side
          )) {
            combinations.push(condition);
          }
        });
      });
    });

    actions.setLoadingCondition(combinations);
    onSelectionChange?.(combinations);
  };

  // Handle advanced mode direct selection
  const handleDirectSelectionChange = (event: SelectChangeEvent<string[]>) => {
    const value = event.target.value;
    const selectedIds = typeof value === 'string' ? value.split(',') : value;
    
    const conditions = selectedIds.map(id => {
      const [waterLevel, volume, side] = id.split('-');
      return {
        waterLevel: waterLevel as 'hwl' | 'lwl',
        volume: volume as '125km3' | '180km3',
        side: side as 'pb' | 'sb',
      };
    });

    actions.setLoadingCondition(conditions);
    onSelectionChange?.(conditions);
  };

  // Handle clear all
  const handleClearAll = () => {
    actions.setLoadingCondition([]);
    setWaterLevels([]);
    setVolumes([]);
    setSides([]);
    onSelectionChange?.([]);
  };

  // Format condition for display
  const formatCondition = (condition: LoadingCondition): string => {
    const wlLabel = WATER_LEVEL_OPTIONS.find(opt => opt.value === condition.waterLevel)?.label || condition.waterLevel;
    const volLabel = VOLUME_OPTIONS.find(opt => opt.value === condition.volume)?.label || condition.volume;
    const sideLabel = SIDE_OPTIONS.find(opt => opt.value === condition.side)?.label || condition.side;
    
    return `${wlLabel}, ${volLabel}, ${sideLabel}`;
  };

  // Get condition ID for multi-select
  const getConditionId = (condition: LoadingCondition): string => {
    return `${condition.waterLevel}-${condition.volume}-${condition.side}`;
  };

  const isDisabled = disabled || selectedAnalysisCases.length === 0;

  return (
    <Box sx={{ minWidth: 400 }}>
      {/* Mode toggle */}
      <Box sx={{ mb: 2 }}>
        <ToggleButtonGroup
          value={filterMode}
          exclusive
          onChange={(e, value) => value && setFilterMode(value)}
          size="small"
        >
          <ToggleButton value="simple">Simple</ToggleButton>
          <ToggleButton value="advanced">Advanced</ToggleButton>
        </ToggleButtonGroup>
      </Box>

      {/* Warning when no analysis cases selected */}
      {selectedAnalysisCases.length === 0 && (
        <Alert severity="info" sx={{ mb: 2 }}>
          Select analysis cases first to see available loading conditions
        </Alert>
      )}

      {filterMode === 'simple' ? (
        // Simple mode with component toggles
        <Box>
          <Grid container spacing={2}>
            {/* Water Level */}
            <Grid item xs={12}>
              <Paper sx={{ p: 2 }}>
                <Box sx={{ display: 'flex', alignItems: 'center', mb: 1 }}>
                  <WaterIcon sx={{ mr: 1 }} />
                  <Typography variant="subtitle2">Water Level</Typography>
                </Box>
                <ToggleButtonGroup
                  value={waterLevels}
                  onChange={handleWaterLevelChange}
                  fullWidth
                  size="small"
                  disabled={isDisabled}
                >
                  {WATER_LEVEL_OPTIONS.map(option => (
                    <ToggleButton key={option.value} value={option.value}>
                      <Box sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
                        <span>{option.icon}</span>
                        {option.label}
                      </Box>
                    </ToggleButton>
                  ))}
                </ToggleButtonGroup>
              </Paper>
            </Grid>

            {/* Volume */}
            <Grid item xs={12}>
              <Paper sx={{ p: 2 }}>
                <Box sx={{ display: 'flex', alignItems: 'center', mb: 1 }}>
                  <VolumeIcon sx={{ mr: 1 }} />
                  <Typography variant="subtitle2">Volume</Typography>
                </Box>
                <ToggleButtonGroup
                  value={volumes}
                  onChange={handleVolumeChange}
                  fullWidth
                  size="small"
                  disabled={isDisabled}
                >
                  {VOLUME_OPTIONS.map(option => (
                    <ToggleButton key={option.value} value={option.value}>
                      <Box sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
                        <span>{option.icon}</span>
                        {option.label}
                      </Box>
                    </ToggleButton>
                  ))}
                </ToggleButtonGroup>
              </Paper>
            </Grid>

            {/* Side */}
            <Grid item xs={12}>
              <Paper sx={{ p: 2 }}>
                <Box sx={{ display: 'flex', alignItems: 'center', mb: 1 }}>
                  <SideIcon sx={{ mr: 1 }} />
                  <Typography variant="subtitle2">Side</Typography>
                </Box>
                <ToggleButtonGroup
                  value={sides}
                  onChange={handleSideChange}
                  fullWidth
                  size="small"
                  disabled={isDisabled}
                >
                  {SIDE_OPTIONS.map(option => (
                    <ToggleButton key={option.value} value={option.value}>
                      <Box sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
                        <span>{option.icon}</span>
                        {option.label}
                      </Box>
                    </ToggleButton>
                  ))}
                </ToggleButtonGroup>
              </Paper>
            </Grid>
          </Grid>
        </Box>
      ) : (
        // Advanced mode with direct selection
        <FormControl fullWidth size="small">
          <InputLabel id="loading-condition-select-label">Loading Conditions</InputLabel>
          <Select
            labelId="loading-condition-select-label"
            multiple
            value={selectedConditions.map(getConditionId)}
            onChange={handleDirectSelectionChange}
            input={<OutlinedInput label="Loading Conditions" />}
            renderValue={(selected) => (
              <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5, maxHeight: 100, overflow: 'auto' }}>
                {selected.map((id) => {
                  const [waterLevel, volume, side] = id.split('-');
                  const condition = { waterLevel, volume, side } as LoadingCondition;
                  return (
                    <Chip
                      key={id}
                      label={formatCondition(condition)}
                      size="small"
                      onDelete={(e) => {
                        e.stopPropagation();
                        const newSelected = selectedConditions.filter(c => getConditionId(c) !== id);
                        actions.setLoadingCondition(newSelected);
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
            {validCombinations.map((condition) => {
              const id = getConditionId(condition);
              return (
                <MenuItem key={id} value={id}>
                  <Checkbox
                    checked={selectedConditions.some(c => getConditionId(c) === id)}
                    size="small"
                  />
                  <ListItemText primary={formatCondition(condition)} />
                </MenuItem>
              );
            })}
            
            {validCombinations.length === 0 && !isDisabled && (
              <MenuItem disabled>
                <Typography variant="body2" color="text.secondary">
                  No loading conditions available for selected analysis cases
                </Typography>
              </MenuItem>
            )}
          </Select>
        </FormControl>
      )}

      {/* Action buttons */}
      <Box sx={{ mt: 2, display: 'flex', gap: 1 }}>
        <Chip
          label="Clear All"
          size="small"
          variant="outlined"
          onClick={handleClearAll}
          disabled={selectedConditions.length === 0}
        />
        <Typography variant="caption" sx={{ alignSelf: 'center', ml: 'auto' }}>
          {selectedConditions.length} condition{selectedConditions.length !== 1 ? 's' : ''} selected
        </Typography>
      </Box>

      {/* Current selections display */}
      {selectedConditions.length > 0 && (
        <Box sx={{ mt: 2 }}>
          <Typography variant="caption" color="text.secondary" gutterBottom>
            Selected Combinations:
          </Typography>
          <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
            {selectedConditions.map((condition, index) => (
              <Chip
                key={index}
                label={formatCondition(condition)}
                size="small"
                variant="outlined"
                onDelete={() => {
                  const newSelected = selectedConditions.filter((_, i) => i !== index);
                  actions.setLoadingCondition(newSelected);
                  onSelectionChange?.(newSelected);
                }}
              />
            ))}
          </Box>
        </Box>
      )}

      {/* Error display */}
      {state.error && state.error.includes('loading condition') && (
        <Typography variant="caption" color="error" sx={{ mt: 0.5 }}>
          {state.error}
        </Typography>
      )}
    </Box>
  );
};

export default LoadingConditionFilter;