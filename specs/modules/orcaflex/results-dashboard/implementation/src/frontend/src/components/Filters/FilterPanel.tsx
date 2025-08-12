import React, { useEffect, useState } from 'react';
import {
  Box,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Button,
  Chip,
  Stack,
  Typography,
  SelectChangeEvent,
  Autocomplete,
  TextField
} from '@mui/material';
import { FilterList, Clear } from '@mui/icons-material';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '@store/index';
import {
  setSelectedCase,
  setSelectedComponent,
  setSelectedLoadingCondition,
  setSelectedHeading,
  resetFilters
} from '@store/slices/filterSlice';
import { dataAPI } from '@services/api';

interface FilterPanelProps {
  onApplyFilters?: () => void;
}

const FilterPanel: React.FC<FilterPanelProps> = ({ onApplyFilters }) => {
  const dispatch = useDispatch();
  const filters = useSelector((state: RootState) => state.filter);
  const [cases, setCases] = useState<string[]>([]);
  const [components, setComponents] = useState<string[]>([]);
  const [loadingConditions] = useState<string[]>([
    'hwl_125km3_pb',
    'hwl_125km3_sb',
    'hwl_180km3_pb',
    'hwl_180km3_sb',
    'lwl_125km3_pb',
    'lwl_125km3_sb',
    'lwl_180km3_pb',
    'lwl_180km3_sb'
  ]);
  const [headings] = useState<number[]>(
    Array.from({ length: 24 }, (_, i) => i * 15)
  );

  useEffect(() => {
    loadFilterOptions();
  }, []);

  useEffect(() => {
    if (filters.selectedCase) {
      loadComponents(filters.selectedCase);
    }
  }, [filters.selectedCase]);

  const loadFilterOptions = async () => {
    try {
      const casesResponse = await dataAPI.getCases();
      setCases(casesResponse.data);
    } catch (error) {
      console.error('Failed to load filter options:', error);
    }
  };

  const loadComponents = async (caseId: string) => {
    try {
      const componentsResponse = await dataAPI.getComponents(caseId);
      setComponents(componentsResponse.data);
    } catch (error) {
      console.error('Failed to load components:', error);
    }
  };

  const handleCaseChange = (event: SelectChangeEvent) => {
    dispatch(setSelectedCase(event.target.value));
    dispatch(setSelectedComponent(null));
  };

  const handleComponentChange = (_: any, value: string | null) => {
    dispatch(setSelectedComponent(value));
  };

  const handleLoadingConditionChange = (event: SelectChangeEvent) => {
    dispatch(setSelectedLoadingCondition(event.target.value));
  };

  const handleHeadingChange = (event: SelectChangeEvent) => {
    dispatch(setSelectedHeading(Number(event.target.value)));
  };

  const handleApplyFilters = () => {
    if (onApplyFilters) {
      onApplyFilters();
    }
  };

  const handleResetFilters = () => {
    dispatch(resetFilters());
    setComponents([]);
  };

  const getActiveFilterCount = () => {
    let count = 0;
    if (filters.selectedCase) count++;
    if (filters.selectedComponent) count++;
    if (filters.selectedLoadingCondition) count++;
    if (filters.selectedHeading !== null) count++;
    return count;
  };

  return (
    <Box sx={{ p: 2, bgcolor: 'background.paper', borderRadius: 1 }}>
      <Box display="flex" alignItems="center" mb={2}>
        <FilterList sx={{ mr: 1 }} />
        <Typography variant="h6">Filters</Typography>
        {getActiveFilterCount() > 0 && (
          <Chip
            label={`${getActiveFilterCount()} active`}
            size="small"
            color="primary"
            sx={{ ml: 2 }}
          />
        )}
      </Box>

      <Stack spacing={2}>
        <FormControl fullWidth>
          <InputLabel>Analysis Case</InputLabel>
          <Select
            value={filters.selectedCase || ''}
            onChange={handleCaseChange}
            label="Analysis Case"
          >
            <MenuItem value="">
              <em>None</em>
            </MenuItem>
            {cases.map((caseItem) => (
              <MenuItem key={caseItem} value={caseItem}>
                {caseItem}
              </MenuItem>
            ))}
          </Select>
        </FormControl>

        <Autocomplete
          options={components}
          value={filters.selectedComponent}
          onChange={handleComponentChange}
          disabled={!filters.selectedCase}
          renderInput={(params) => (
            <TextField
              {...params}
              label="Component"
              placeholder="Search components..."
            />
          )}
        />

        <FormControl fullWidth>
          <InputLabel>Loading Condition</InputLabel>
          <Select
            value={filters.selectedLoadingCondition || ''}
            onChange={handleLoadingConditionChange}
            label="Loading Condition"
          >
            <MenuItem value="">
              <em>None</em>
            </MenuItem>
            {loadingConditions.map((condition) => (
              <MenuItem key={condition} value={condition}>
                {condition.toUpperCase().replace(/_/g, ' ')}
              </MenuItem>
            ))}
          </Select>
        </FormControl>

        <FormControl fullWidth>
          <InputLabel>Heading (degrees)</InputLabel>
          <Select
            value={filters.selectedHeading?.toString() || ''}
            onChange={handleHeadingChange}
            label="Heading (degrees)"
          >
            <MenuItem value="">
              <em>None</em>
            </MenuItem>
            {headings.map((heading) => (
              <MenuItem key={heading} value={heading.toString()}>
                {heading}°
              </MenuItem>
            ))}
          </Select>
        </FormControl>

        <Box display="flex" gap={1}>
          <Button
            variant="contained"
            fullWidth
            onClick={handleApplyFilters}
            disabled={!filters.selectedCase}
            startIcon={<FilterList />}
          >
            Apply Filters
          </Button>
          <Button
            variant="outlined"
            fullWidth
            onClick={handleResetFilters}
            startIcon={<Clear />}
          >
            Reset
          </Button>
        </Box>
      </Stack>

      {getActiveFilterCount() > 0 && (
        <Box mt={2}>
          <Typography variant="caption" color="textSecondary">
            Active Filters:
          </Typography>
          <Stack direction="row" spacing={1} flexWrap="wrap" mt={1}>
            {filters.selectedCase && (
              <Chip
                label={`Case: ${filters.selectedCase}`}
                size="small"
                onDelete={() => dispatch(setSelectedCase(null))}
              />
            )}
            {filters.selectedComponent && (
              <Chip
                label={`Component: ${filters.selectedComponent}`}
                size="small"
                onDelete={() => dispatch(setSelectedComponent(null))}
              />
            )}
            {filters.selectedLoadingCondition && (
              <Chip
                label={`Loading: ${filters.selectedLoadingCondition}`}
                size="small"
                onDelete={() => dispatch(setSelectedLoadingCondition(null))}
              />
            )}
            {filters.selectedHeading !== null && (
              <Chip
                label={`Heading: ${filters.selectedHeading}°`}
                size="small"
                onDelete={() => dispatch(setSelectedHeading(null))}
              />
            )}
          </Stack>
        </Box>
      )}
    </Box>
  );
};

export default FilterPanel;