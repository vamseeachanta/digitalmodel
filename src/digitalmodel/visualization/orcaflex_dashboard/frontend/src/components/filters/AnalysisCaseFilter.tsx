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
  Divider,
  IconButton,
  Collapse,
} from '@mui/material';
import {
  Clear as ClearIcon,
  ExpandMore as ExpandMoreIcon,
  ExpandLess as ExpandLessIcon,
  Search as SearchIcon,
} from '@mui/icons-material';
import { useFilterContext } from './FilterContext';
import { useSelector } from 'react-redux';
import { RootState } from '../../store';

interface AnalysisCase {
  id: string;
  name: string;
  description: string;
  category: string;
  tags: string[];
  isActive: boolean;
  createdDate: string;
}

interface AnalysisCaseFilterProps {
  onSelectionChange?: (selectedCases: string[]) => void;
  disabled?: boolean;
  maxHeight?: number;
}

const ITEM_HEIGHT = 48;
const ITEM_PADDING_TOP = 8;
const MenuProps = {
  PaperProps: {
    style: {
      maxHeight: ITEM_HEIGHT * 4.5 + ITEM_PADDING_TOP,
      width: 300,
    },
  },
};

export const AnalysisCaseFilter: React.FC<AnalysisCaseFilterProps> = ({
  onSelectionChange,
  disabled = false,
  maxHeight = 200,
}) => {
  const { state, actions } = useFilterContext();
  const [searchTerm, setSearchTerm] = useState('');
  const [expandedCategories, setExpandedCategories] = useState<Set<string>>(new Set());
  
  // Get analysis cases from Redux store
  const analysisCases = useSelector((state: RootState) => 
    state.dashboard?.analysisCases || []
  ) as AnalysisCase[];
  
  const isLoading = useSelector((state: RootState) => 
    state.dashboard?.loading || false
  );

  // Filter and group analysis cases
  const filteredAndGroupedCases = useMemo(() => {
    let filtered = analysisCases.filter((case_) => 
      case_.isActive && (
        case_.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
        case_.description.toLowerCase().includes(searchTerm.toLowerCase()) ||
        case_.tags.some(tag => tag.toLowerCase().includes(searchTerm.toLowerCase()))
      )
    );

    // Group by category
    const grouped = filtered.reduce((acc, case_) => {
      const category = case_.category || 'Uncategorized';
      if (!acc[category]) {
        acc[category] = [];
      }
      acc[category].push(case_);
      return acc;
    }, {} as Record<string, AnalysisCase[]>);

    // Sort categories and cases within each category
    const sortedGrouped: Record<string, AnalysisCase[]> = {};
    Object.keys(grouped)
      .sort()
      .forEach(category => {
        sortedGrouped[category] = grouped[category].sort((a, b) => 
          a.name.localeCompare(b.name)
        );
      });

    return sortedGrouped;
  }, [analysisCases, searchTerm]);

  const selectedCases = state.analysisCase;
  const allCaseIds = Object.values(filteredAndGroupedCases).flat().map(c => c.id);

  // Handle selection change
  const handleSelectionChange = (event: SelectChangeEvent<string[]>) => {
    const value = event.target.value;
    const selectedIds = typeof value === 'string' ? value.split(',') : value;
    
    actions.setAnalysisCase(selectedIds);
    onSelectionChange?.(selectedIds);
  };

  // Handle category expansion
  const toggleCategory = (category: string) => {
    const newExpanded = new Set(expandedCategories);
    if (newExpanded.has(category)) {
      newExpanded.delete(category);
    } else {
      newExpanded.add(category);
    }
    setExpandedCategories(newExpanded);
  };

  // Handle select all/none for a category
  const handleCategorySelection = (category: string, selectAll: boolean) => {
    const categoryIds = filteredAndGroupedCases[category].map(c => c.id);
    let newSelected: string[];
    
    if (selectAll) {
      newSelected = [...new Set([...selectedCases, ...categoryIds])];
    } else {
      newSelected = selectedCases.filter(id => !categoryIds.includes(id));
    }
    
    actions.setAnalysisCase(newSelected);
    onSelectionChange?.(newSelected);
  };

  // Handle clear all
  const handleClearAll = () => {
    actions.setAnalysisCase([]);
    onSelectionChange?.([]);
  };

  // Handle select all visible
  const handleSelectAllVisible = () => {
    actions.setAnalysisCase(allCaseIds);
    onSelectionChange?.(allCaseIds);
  };

  // Get display name for selected cases
  const getSelectedDisplayNames = (selected: string[]) => {
    return selected
      .map(id => analysisCases.find(c => c.id === id)?.name)
      .filter(Boolean)
      .join(', ');
  };

  // Expand all categories initially if there are few
  useEffect(() => {
    const categories = Object.keys(filteredAndGroupedCases);
    if (categories.length <= 3 && expandedCategories.size === 0) {
      setExpandedCategories(new Set(categories));
    }
  }, [filteredAndGroupedCases, expandedCategories.size]);

  return (
    <Box sx={{ minWidth: 300, maxWidth: 500 }}>
      <FormControl fullWidth size="small">
        <InputLabel id="analysis-case-select-label">Analysis Cases</InputLabel>
        <Select
          labelId="analysis-case-select-label"
          multiple
          value={selectedCases}
          onChange={handleSelectionChange}
          input={<OutlinedInput label="Analysis Cases" />}
          renderValue={(selected) => (
            <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5, maxHeight: 100, overflow: 'auto' }}>
              {selected.map((id) => {
                const case_ = analysisCases.find(c => c.id === id);
                return (
                  <Chip
                    key={id}
                    label={case_?.name || id}
                    size="small"
                    onDelete={(e) => {
                      e.stopPropagation();
                      const newSelected = selectedCases.filter(caseId => caseId !== id);
                      actions.setAnalysisCase(newSelected);
                      onSelectionChange?.(newSelected);
                    }}
                    deleteIcon={<ClearIcon />}
                  />
                );
              })}
            </Box>
          )}
          MenuProps={MenuProps}
          disabled={disabled || isLoading}
        >
          {/* Search and bulk actions */}
          <Box sx={{ p: 1, position: 'sticky', top: 0, bgcolor: 'background.paper', zIndex: 1 }}>
            <TextField
              size="small"
              placeholder="Search analysis cases..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              fullWidth
              InputProps={{
                startAdornment: <SearchIcon sx={{ mr: 1, color: 'text.secondary' }} />,
              }}
              sx={{ mb: 1 }}
            />
            
            <Box sx={{ display: 'flex', gap: 1, flexWrap: 'wrap' }}>
              <Chip
                label="Select All Visible"
                size="small"
                variant="outlined"
                onClick={handleSelectAllVisible}
                disabled={allCaseIds.length === 0}
              />
              <Chip
                label="Clear All"
                size="small"
                variant="outlined"
                onClick={handleClearAll}
                disabled={selectedCases.length === 0}
              />
              <Typography variant="caption" sx={{ alignSelf: 'center', ml: 'auto' }}>
                {selectedCases.length} of {analysisCases.length} selected
              </Typography>
            </Box>
          </Box>
          
          <Divider />
          
          {/* Categorized options */}
          {Object.entries(filteredAndGroupedCases).map(([category, cases]) => {
            const categoryIds = cases.map(c => c.id);
            const selectedInCategory = categoryIds.filter(id => selectedCases.includes(id));
            const isAllSelected = selectedInCategory.length === categoryIds.length;
            const isPartiallySelected = selectedInCategory.length > 0 && !isAllSelected;
            const isExpanded = expandedCategories.has(category);
            
            return (
              <React.Fragment key={category}>
                {/* Category header */}
                <MenuItem
                  sx={{
                    bgcolor: 'action.hover',
                    '&:hover': { bgcolor: 'action.selected' },
                    borderBottom: '1px solid',
                    borderColor: 'divider',
                  }}
                >
                  <Box sx={{ display: 'flex', alignItems: 'center', width: '100%' }}>
                    <IconButton
                      size="small"
                      onClick={(e) => {
                        e.stopPropagation();
                        toggleCategory(category);
                      }}
                    >
                      {isExpanded ? <ExpandLessIcon /> : <ExpandMoreIcon />}
                    </IconButton>
                    
                    <Checkbox
                      checked={isAllSelected}
                      indeterminate={isPartiallySelected}
                      onChange={(e) => {
                        e.stopPropagation();
                        handleCategorySelection(category, !isAllSelected);
                      }}
                      size="small"
                    />
                    
                    <ListItemText
                      primary={
                        <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                          <Typography variant="subtitle2" fontWeight="bold">
                            {category}
                          </Typography>
                          <Chip
                            label={`${selectedInCategory.length}/${categoryIds.length}`}
                            size="small"
                            variant="outlined"
                          />
                        </Box>
                      }
                    />
                  </Box>
                </MenuItem>
                
                {/* Category cases */}
                <Collapse in={isExpanded}>
                  {cases.map((case_) => (
                    <MenuItem key={case_.id} value={case_.id} sx={{ pl: 6 }}>
                      <Checkbox
                        checked={selectedCases.includes(case_.id)}
                        size="small"
                      />
                      <ListItemText
                        primary={case_.name}
                        secondary={
                          <Box>
                            <Typography variant="body2" color="text.secondary">
                              {case_.description}
                            </Typography>
                            {case_.tags.length > 0 && (
                              <Box sx={{ display: 'flex', gap: 0.5, mt: 0.5, flexWrap: 'wrap' }}>
                                {case_.tags.map((tag) => (
                                  <Chip
                                    key={tag}
                                    label={tag}
                                    size="small"
                                    variant="outlined"
                                    sx={{ height: 16, fontSize: '0.6rem' }}
                                  />
                                ))}
                              </Box>
                            )}
                          </Box>
                        }
                      />
                    </MenuItem>
                  ))}
                </Collapse>
              </React.Fragment>
            );
          })}
          
          {/* Empty state */}
          {Object.keys(filteredAndGroupedCases).length === 0 && (
            <MenuItem disabled>
              <Typography variant="body2" color="text.secondary">
                {searchTerm ? 'No analysis cases match your search' : 'No analysis cases available'}
              </Typography>
            </MenuItem>
          )}
        </Select>
        
        {/* Error display */}
        {state.error && state.error.includes('analysis case') && (
          <Typography variant="caption" color="error" sx={{ mt: 0.5 }}>
            {state.error}
          </Typography>
        )}
        
        {/* Selection summary */}
        {selectedCases.length > 0 && (
          <Typography variant="caption" color="text.secondary" sx={{ mt: 0.5 }}>
            Selected: {getSelectedDisplayNames(selectedCases)}
          </Typography>
        )}
      </FormControl>
    </Box>
  );
};

export default AnalysisCaseFilter;