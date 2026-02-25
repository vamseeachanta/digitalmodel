import React, { useState, useEffect, useCallback } from 'react';
import {
  Box,
  Paper,
  Typography,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  Button,
  IconButton,
  Chip,
  Alert,
  Divider,
  Grid,
  Collapse,
  LinearProgress,
  Tooltip,
  Switch,
  FormControlLabel,
} from '@mui/material';
import {
  ExpandMore as ExpandMoreIcon,
  FilterList as FilterIcon,
  Clear as ClearIcon,
  Refresh as RefreshIcon,
  Tune as TuneIcon,
  Speed as SpeedIcon,
  Visibility as VisibilityIcon,
  VisibilityOff as VisibilityOffIcon,
  Warning as WarningIcon,
  CheckCircle as CheckCircleIcon,
} from '@mui/icons-material';
import { useFilterContext } from './FilterContext';
import AnalysisCaseFilter from './AnalysisCaseFilter';
import LoadingConditionFilter from './LoadingConditionFilter';
import HeadingFilter from './HeadingFilter';
import FilterPresets from './FilterPresets';
import { useSelector, useDispatch } from 'react-redux';
import { RootState } from '../../store';
import { debounce } from 'lodash';

interface FilterPanelProps {
  onFilterChange?: () => void;
  autoApplyFilters?: boolean;
  showPresets?: boolean;
  compactMode?: boolean;
  className?: string;
}

const FilterPanel: React.FC<FilterPanelProps> = ({
  onFilterChange,
  autoApplyFilters = true,
  showPresets = true,
  compactMode = false,
  className,
}) => {
  const { state, actions } = useFilterContext();
  const dispatch = useDispatch();
  const [isVisible, setIsVisible] = useState(true);
  const [expandedPanels, setExpandedPanels] = useState<Set<string>>(
    new Set(['analysis-cases'])
  );
  const [lastUpdateTime, setLastUpdateTime] = useState<Date | null>(null);
  const [isUpdating, setIsUpdating] = useState(false);

  // Get dashboard state from Redux
  const dashboardState = useSelector((state: RootState) => state.dashboard);
  const isDataLoading = dashboardState?.loading || false;
  const hasData = Boolean(dashboardState?.data);

  // Debounced filter change handler
  const debouncedFilterChange = useCallback(
    debounce(() => {
      if (autoApplyFilters) {
        setIsUpdating(true);
        onFilterChange?.();
        setLastUpdateTime(new Date());
        
        // Simulate processing time
        setTimeout(() => setIsUpdating(false), 500);
      }
    }, 500),
    [onFilterChange, autoApplyFilters]
  );

  // Trigger filter change when filters update
  useEffect(() => {
    if (state.lastUpdated) {
      debouncedFilterChange();
    }
    return () => {
      debouncedFilterChange.cancel();
    };
  }, [state.lastUpdated, debouncedFilterChange]);

  // Handle panel expansion
  const handlePanelToggle = (panel: string) => {
    const newExpanded = new Set(expandedPanels);
    if (newExpanded.has(panel)) {
      newExpanded.delete(panel);
    } else {
      newExpanded.add(panel);
    }
    setExpandedPanels(newExpanded);
  };

  // Handle manual refresh
  const handleRefresh = () => {
    setIsUpdating(true);
    onFilterChange?.();
    setLastUpdateTime(new Date());
    setTimeout(() => setIsUpdating(false), 1000);
  };

  // Reset all filters
  const handleResetFilters = () => {
    actions.resetFilters();
    setLastUpdateTime(new Date());
  };

  // Get filter summary
  const getFilterSummary = () => {
    const parts = [];
    if (state.analysisCase.length > 0) {
      parts.push(`${state.analysisCase.length} case${state.analysisCase.length !== 1 ? 's' : ''}`);
    }
    if (state.loadingCondition.length > 0) {
      parts.push(`${state.loadingCondition.length} condition${state.loadingCondition.length !== 1 ? 's' : ''}`);
    }
    if (state.heading.length > 0) {
      parts.push(`${state.heading.length} heading${state.heading.length !== 1 ? 's' : ''}`);
    }
    return parts.length > 0 ? parts.join(', ') : 'No filters applied';
  };

  // Check if filters are valid for cascading
  const isValidCascade = () => {
    return state.analysisCase.length > 0;
  };

  // Get total filter count
  const totalFilterCount = state.analysisCase.length + state.loadingCondition.length + state.heading.length;

  if (!isVisible) {
    return (
      <Paper sx={{ p: 1 }}>
        <Box sx={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between' }}>
          <IconButton onClick={() => setIsVisible(true)} size="small">
            <VisibilityIcon />
          </IconButton>
          <Typography variant="caption" color="text.secondary">
            Filters hidden ({totalFilterCount} active)
          </Typography>
        </Box>
      </Paper>
    );
  }

  return (
    <Paper className={className} sx={{ width: '100%', maxWidth: compactMode ? 400 : 600 }}>
      {/* Header */}
      <Box sx={{ p: 2, borderBottom: '1px solid', borderColor: 'divider' }}>
        <Box sx={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between' }}>
          <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
            <FilterIcon />
            <Typography variant="h6">
              OrcaFlex Results Filters
            </Typography>
            {totalFilterCount > 0 && (
              <Chip label={totalFilterCount} size="small" color="primary" />
            )}
          </Box>
          
          <Box>
            <Tooltip title="Hide Filters">
              <IconButton onClick={() => setIsVisible(false)} size="small">
                <VisibilityOffIcon />
              </IconButton>
            </Tooltip>
            <Tooltip title="Refresh Data">
              <IconButton onClick={handleRefresh} size="small" disabled={isUpdating}>
                <RefreshIcon />
              </IconButton>
            </Tooltip>
            <Tooltip title="Reset All Filters">
              <IconButton 
                onClick={handleResetFilters} 
                size="small" 
                disabled={totalFilterCount === 0}
                color="error"
              >
                <ClearIcon />
              </IconButton>
            </Tooltip>
          </Box>
        </Box>

        {/* Filter summary */}
        <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
          {getFilterSummary()}
        </Typography>

        {/* Progress indicator */}
        {(isUpdating || isDataLoading) && (
          <LinearProgress sx={{ mt: 1, borderRadius: 1 }} />
        )}

        {/* Auto-apply toggle */}
        <Box sx={{ mt: 1 }}>
          <FormControlLabel
            control={
              <Switch
                checked={autoApplyFilters}
                size="small"
                icon={<SpeedIcon />}
                checkedIcon={<CheckCircleIcon />}
              />
            }
            label="Auto-apply filters"
          />
          {lastUpdateTime && (
            <Typography variant="caption" color="text.secondary" sx={{ ml: 2 }}>
              Last updated: {lastUpdateTime.toLocaleTimeString()}
            </Typography>
          )}
        </Box>
      </Box>

      {/* Error display */}
      {state.error && (
        <Alert severity="error" sx={{ m: 2 }} onClose={() => actions.setError(null)}>
          <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
            <WarningIcon />
            {state.error}
          </Box>
        </Alert>
      )}

      {/* Validation warning */}
      {!isValidCascade() && (totalFilterCount > 0) && (
        <Alert severity="warning" sx={{ m: 2 }}>
          Select analysis cases first to enable cascading filters
        </Alert>
      )}

      {/* No data warning */}
      {!hasData && !isDataLoading && (
        <Alert severity="info" sx={{ m: 2 }}>
          No data available. Load analysis results to enable filtering.
        </Alert>
      )}

      {/* Filter sections */}
      <Box sx={{ pb: 2 }}>
        {/* Analysis Cases Filter */}
        <Accordion
          expanded={expandedPanels.has('analysis-cases')}
          onChange={() => handlePanelToggle('analysis-cases')}
        >
          <AccordionSummary expandIcon={<ExpandMoreIcon />}>
            <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, width: '100%' }}>
              <Typography variant="subtitle1">Analysis Cases</Typography>
              {state.analysisCase.length > 0 && (
                <Chip label={state.analysisCase.length} size="small" color="primary" />
              )}
              <Box sx={{ ml: 'auto', mr: 2 }}>
                <Typography variant="caption" color="text.secondary">
                  Primary filter
                </Typography>
              </Box>
            </Box>
          </AccordionSummary>
          <AccordionDetails>
            <AnalysisCaseFilter
              disabled={isDataLoading}
              onSelectionChange={() => {
                // Clear dependent filters when analysis cases change
                if (state.loadingCondition.length > 0) {
                  actions.setLoadingCondition([]);
                }
                if (state.heading.length > 0) {
                  actions.setHeading([]);
                }
              }}
            />
          </AccordionDetails>
        </Accordion>

        {/* Loading Conditions Filter */}
        <Accordion
          expanded={expandedPanels.has('loading-conditions')}
          onChange={() => handlePanelToggle('loading-conditions')}
          disabled={state.analysisCase.length === 0}
        >
          <AccordionSummary expandIcon={<ExpandMoreIcon />}>
            <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, width: '100%' }}>
              <Typography 
                variant="subtitle1" 
                color={state.analysisCase.length === 0 ? 'text.disabled' : 'inherit'}
              >
                Loading Conditions
              </Typography>
              {state.loadingCondition.length > 0 && (
                <Chip label={state.loadingCondition.length} size="small" color="primary" />
              )}
              <Box sx={{ ml: 'auto', mr: 2 }}>
                <Typography variant="caption" color="text.secondary">
                  Depends on cases
                </Typography>
              </Box>
            </Box>
          </AccordionSummary>
          <AccordionDetails>
            <LoadingConditionFilter
              disabled={isDataLoading || state.analysisCase.length === 0}
              onSelectionChange={() => {
                // Clear headings when loading conditions change
                if (state.heading.length > 0) {
                  actions.setHeading([]);
                }
              }}
            />
          </AccordionDetails>
        </Accordion>

        {/* Environmental Headings Filter */}
        <Accordion
          expanded={expandedPanels.has('headings')}
          onChange={() => handlePanelToggle('headings')}
          disabled={state.analysisCase.length === 0}
        >
          <AccordionSummary expandIcon={<ExpandMoreIcon />}>
            <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, width: '100%' }}>
              <Typography 
                variant="subtitle1"
                color={state.analysisCase.length === 0 ? 'text.disabled' : 'inherit'}
              >
                Environmental Headings
              </Typography>
              {state.heading.length > 0 && (
                <Chip label={state.heading.length} size="small" color="primary" />
              )}
              <Box sx={{ ml: 'auto', mr: 2 }}>
                <Typography variant="caption" color="text.secondary">
                  0° - 345°
                </Typography>
              </Box>
            </Box>
          </AccordionSummary>
          <AccordionDetails>
            <HeadingFilter
              disabled={isDataLoading || state.analysisCase.length === 0}
            />
          </AccordionDetails>
        </Accordion>

        {/* Filter Presets */}
        {showPresets && (
          <Accordion
            expanded={expandedPanels.has('presets')}
            onChange={() => handlePanelToggle('presets')}
          >
            <AccordionSummary expandIcon={<ExpandMoreIcon />}>
              <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                <TuneIcon />
                <Typography variant="subtitle1">Filter Presets</Typography>
              </Box>
            </AccordionSummary>
            <AccordionDetails>
              <FilterPresets
                onPresetApply={() => setLastUpdateTime(new Date())}
              />
            </AccordionDetails>
          </Accordion>
        )}
      </Box>

      {/* Manual apply button (when auto-apply is disabled) */}
      {!autoApplyFilters && (
        <Box sx={{ p: 2, borderTop: '1px solid', borderColor: 'divider' }}>
          <Button
            variant="contained"
            fullWidth
            onClick={handleRefresh}
            disabled={isUpdating || totalFilterCount === 0}
            startIcon={<FilterIcon />}
          >
            Apply Filters {totalFilterCount > 0 && `(${totalFilterCount})`}
          </Button>
        </Box>
      )}

      {/* Debug info (development only) */}
      {process.env.NODE_ENV === 'development' && (
        <Box sx={{ p: 1, bgcolor: 'grey.50', fontSize: '0.7rem', fontFamily: 'monospace' }}>
          <details>
            <summary>Debug Info</summary>
            <pre style={{ margin: 0, fontSize: '0.6rem' }}>
              {JSON.stringify({
                analysisCase: state.analysisCase.length,
                loadingCondition: state.loadingCondition.length,
                heading: state.heading.length,
                isLoading: state.isLoading,
                error: state.error,
                lastUpdated: state.lastUpdated?.toISOString(),
                hasData,
                isDataLoading,
              }, null, 2)}
            </pre>
          </details>
        </Box>
      )}
    </Paper>
  );
};

export default FilterPanel;