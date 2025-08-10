import React, { useState, useMemo } from 'react';
import {
  Box,
  Card,
  Typography,
  FormControl,
  FormControlLabel,
  FormGroup,
  Checkbox,
  Switch,
  Chip,
  TextField,
  InputAdornment,
  IconButton,
  Collapse,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  ListItemSecondaryAction,
  Divider,
  Tooltip,
  Button,
  Menu,
  MenuItem,
} from '@mui/material';
import {
  Search as SearchIcon,
  Clear as ClearIcon,
  ExpandMore as ExpandMoreIcon,
  ExpandLess as ExpandLessIcon,
  FilterList as FilterListIcon,
  Visibility as VisibilityIcon,
  VisibilityOff as VisibilityOffIcon,
  AccountTree as AccountTreeIcon,
} from '@mui/icons-material';
import { useTheme } from '@mui/material/styles';
import { ComponentGroup } from './types';

interface ComponentGroupSelectorProps {
  componentGroups: ComponentGroup[];
  availableComponents: string[];
  selectedComponents: string[];
  onSelectionChange: (selectedComponents: string[]) => void;
  showGroupStatistics?: boolean;
  enableSearch?: boolean;
  enableFiltering?: boolean;
  compactView?: boolean;
  maxHeight?: number;
}

const ComponentGroupSelector: React.FC<ComponentGroupSelectorProps> = ({
  componentGroups,
  availableComponents,
  selectedComponents,
  onSelectionChange,
  showGroupStatistics = true,
  enableSearch = true,
  enableFiltering = true,
  compactView = false,
  maxHeight = 400,
}) => {
  const theme = useTheme();
  
  // Local state
  const [searchTerm, setSearchTerm] = useState('');
  const [expandedGroups, setExpandedGroups] = useState<string[]>(
    componentGroups.map(g => g.id)
  );
  const [filterMenuAnchor, setFilterMenuAnchor] = useState<null | HTMLElement>(null);
  const [activeFilters, setActiveFilters] = useState<string[]>([]);

  // Group colors
  const getGroupColor = (groupType: string) => {
    switch (groupType) {
      case 'struts': return theme.palette.primary.main;
      case 'jackets': return theme.palette.secondary.main;
      case 'foundations': return theme.palette.error.main;
      default: return theme.palette.grey[600];
    }
  };

  // Filter components based on search term and active filters
  const filteredComponents = useMemo(() => {
    let components = availableComponents;

    // Apply search filter
    if (searchTerm) {
      components = components.filter(component =>
        component.toLowerCase().includes(searchTerm.toLowerCase())
      );
    }

    // Apply group type filters
    if (activeFilters.length > 0) {
      const filteredGroupComponents = new Set<string>();
      
      componentGroups.forEach(group => {
        if (activeFilters.includes(group.type)) {
          group.componentIds.forEach(id => {
            if (components.includes(id)) {
              filteredGroupComponents.add(id);
            }
          });
        }
      });

      components = components.filter(id => filteredGroupComponents.has(id));
    }

    return components;
  }, [availableComponents, searchTerm, activeFilters, componentGroups]);

  // Group components with selection statistics
  const groupsWithStats = useMemo(() => {
    return componentGroups.map(group => {
      const groupComponents = group.componentIds.filter(id => 
        filteredComponents.includes(id)
      );
      const selectedInGroup = groupComponents.filter(id =>
        selectedComponents.includes(id)
      );

      return {
        ...group,
        visibleComponents: groupComponents,
        selectedCount: selectedInGroup.length,
        totalCount: groupComponents.length,
        selectionState: 
          selectedInGroup.length === 0 ? 'none' :
          selectedInGroup.length === groupComponents.length ? 'all' :
          'partial'
      };
    });
  }, [componentGroups, filteredComponents, selectedComponents]);

  // Handle individual component toggle
  const handleComponentToggle = (componentId: string) => {
    const newSelected = selectedComponents.includes(componentId)
      ? selectedComponents.filter(id => id !== componentId)
      : [...selectedComponents, componentId];
    
    onSelectionChange(newSelected);
  };

  // Handle group toggle
  const handleGroupToggle = (groupId: string) => {
    const group = groupsWithStats.find(g => g.id === groupId);
    if (!group) return;

    const newSelected = group.selectionState === 'all'
      ? selectedComponents.filter(id => !group.visibleComponents.includes(id))
      : [...new Set([...selectedComponents, ...group.visibleComponents])];
    
    onSelectionChange(newSelected);
  };

  // Handle group expansion
  const handleGroupExpansion = (groupId: string) => {
    setExpandedGroups(prev =>
      prev.includes(groupId)
        ? prev.filter(id => id !== groupId)
        : [...prev, groupId]
    );
  };

  // Handle select all/none
  const handleSelectAll = () => {
    onSelectionChange(filteredComponents);
  };

  const handleSelectNone = () => {
    const remainingSelected = selectedComponents.filter(id =>
      !filteredComponents.includes(id)
    );
    onSelectionChange(remainingSelected);
  };

  // Filter menu handlers
  const handleFilterMenuOpen = (event: React.MouseEvent<HTMLElement>) => {
    setFilterMenuAnchor(event.currentTarget);
  };

  const handleFilterMenuClose = () => {
    setFilterMenuAnchor(null);
  };

  const handleFilterToggle = (groupType: string) => {
    setActiveFilters(prev =>
      prev.includes(groupType)
        ? prev.filter(type => type !== groupType)
        : [...prev, groupType]
    );
  };

  // Get unique group types for filtering
  const groupTypes = useMemo(() => {
    return [...new Set(componentGroups.map(g => g.type))];
  }, [componentGroups]);

  const totalSelected = selectedComponents.filter(id => 
    filteredComponents.includes(id)
  ).length;

  return (
    <Card sx={{ p: 2, maxHeight, display: 'flex', flexDirection: 'column' }}>
      {/* Header */}
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 2 }}>
        <Typography variant="h6">
          Component Selection
        </Typography>
        
        <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
          <Chip
            label={`${totalSelected} / ${filteredComponents.length}`}
            size="small"
            color={totalSelected > 0 ? 'primary' : 'default'}
          />
          
          {enableFiltering && (
            <Tooltip title="Filter by Group Type">
              <IconButton size="small" onClick={handleFilterMenuOpen}>
                <FilterListIcon />
              </IconButton>
            </Tooltip>
          )}
        </Box>
      </Box>

      {/* Search */}
      {enableSearch && (
        <TextField
          fullWidth
          size="small"
          placeholder="Search components..."
          value={searchTerm}
          onChange={(e) => setSearchTerm(e.target.value)}
          InputProps={{
            startAdornment: (
              <InputAdornment position="start">
                <SearchIcon />
              </InputAdornment>
            ),
            endAdornment: searchTerm && (
              <InputAdornment position="end">
                <IconButton
                  size="small"
                  onClick={() => setSearchTerm('')}
                  edge="end"
                >
                  <ClearIcon />
                </IconButton>
              </InputAdornment>
            ),
          }}
          sx={{ mb: 2 }}
        />
      )}

      {/* Quick Actions */}
      <Box sx={{ display: 'flex', gap: 1, mb: 2 }}>
        <Button
          size="small"
          onClick={handleSelectAll}
          disabled={totalSelected === filteredComponents.length}
        >
          Select All
        </Button>
        <Button
          size="small"
          onClick={handleSelectNone}
          disabled={totalSelected === 0}
        >
          Select None
        </Button>
      </Box>

      {/* Groups List */}
      <Box sx={{ flex: 1, overflow: 'auto' }}>
        {groupsWithStats
          .filter(group => group.visibleComponents.length > 0)
          .map((group) => (
            <Box key={group.id} sx={{ mb: 1 }}>
              {/* Group Header */}
              <Box
                sx={{
                  display: 'flex',
                  alignItems: 'center',
                  p: 1,
                  borderRadius: 1,
                  backgroundColor: theme.palette.grey[50],
                  cursor: 'pointer',
                }}
                onClick={() => handleGroupExpansion(group.id)}
              >
                <FormControlLabel
                  control={
                    <Switch
                      checked={group.selectionState === 'all'}
                      indeterminate={group.selectionState === 'partial'}
                      onChange={(e) => {
                        e.stopPropagation();
                        handleGroupToggle(group.id);
                      }}
                      size="small"
                    />
                  }
                  label=""
                  sx={{ mr: 1, ml: 0 }}
                />

                <AccountTreeIcon
                  sx={{ 
                    mr: 1, 
                    color: getGroupColor(group.type),
                    fontSize: 20,
                  }}
                />

                <Box sx={{ flex: 1 }}>
                  <Typography variant="subtitle2" sx={{ fontWeight: 600 }}>
                    {group.name}
                  </Typography>
                  <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, mt: 0.5 }}>
                    <Chip
                      label={group.type}
                      size="small"
                      sx={{
                        backgroundColor: getGroupColor(group.type),
                        color: theme.palette.getContrastText(getGroupColor(group.type)),
                        fontSize: '0.7rem',
                        height: 20,
                      }}
                    />
                    {showGroupStatistics && (
                      <Typography variant="caption" color="text.secondary">
                        {group.selectedCount}/{group.totalCount} selected
                      </Typography>
                    )}
                  </Box>
                </Box>

                <IconButton size="small">
                  {expandedGroups.includes(group.id) ? 
                    <ExpandLessIcon /> : <ExpandMoreIcon />
                  }
                </IconButton>
              </Box>

              {/* Group Components */}
              <Collapse in={expandedGroups.includes(group.id)}>
                <List dense sx={{ pl: 2, py: 0 }}>
                  {group.visibleComponents.map((componentId) => (
                    <ListItem
                      key={componentId}
                      dense
                      divider
                      sx={{ 
                        py: 0.5,
                        ...(compactView && { minHeight: 32 })
                      }}
                    >
                      <ListItemIcon sx={{ minWidth: 36 }}>
                        <Checkbox
                          size="small"
                          checked={selectedComponents.includes(componentId)}
                          onChange={() => handleComponentToggle(componentId)}
                        />
                      </ListItemIcon>
                      
                      <ListItemText
                        primary={componentId}
                        primaryTypographyProps={{
                          variant: compactView ? 'body2' : 'body1',
                          fontFamily: 'monospace',
                        }}
                      />
                      
                      <ListItemSecondaryAction>
                        <IconButton
                          size="small"
                          onClick={() => handleComponentToggle(componentId)}
                        >
                          {selectedComponents.includes(componentId) ? 
                            <VisibilityIcon fontSize="small" /> : 
                            <VisibilityOffIcon fontSize="small" />
                          }
                        </IconButton>
                      </ListItemSecondaryAction>
                    </ListItem>
                  ))}
                </List>
              </Collapse>

              {group !== groupsWithStats[groupsWithStats.length - 1] && (
                <Divider sx={{ my: 1 }} />
              )}
            </Box>
          ))
        }
      </Box>

      {/* Filter Menu */}
      <Menu
        anchorEl={filterMenuAnchor}
        open={Boolean(filterMenuAnchor)}
        onClose={handleFilterMenuClose}
      >
        <MenuItem key="header" disabled>
          <Typography variant="subtitle2">
            Filter by Group Type
          </Typography>
        </MenuItem>
        <Divider />
        
        {groupTypes.map((groupType) => (
          <MenuItem key={groupType}>
            <FormControlLabel
              control={
                <Checkbox
                  size="small"
                  checked={activeFilters.includes(groupType)}
                  onChange={() => handleFilterToggle(groupType)}
                />
              }
              label={
                <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                  <Box
                    sx={{
                      width: 12,
                      height: 12,
                      borderRadius: '50%',
                      backgroundColor: getGroupColor(groupType),
                    }}
                  />
                  <Typography variant="body2" sx={{ textTransform: 'capitalize' }}>
                    {groupType}
                  </Typography>
                </Box>
              }
            />
          </MenuItem>
        ))}
        
        <Divider />
        <MenuItem
          onClick={() => {
            setActiveFilters([]);
            handleFilterMenuClose();
          }}
        >
          <Typography variant="body2">Clear Filters</Typography>
        </MenuItem>
      </Menu>

      {/* Footer Statistics */}
      {showGroupStatistics && (
        <>
          <Divider sx={{ my: 2 }} />
          <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
            <Typography variant="caption" color="text.secondary">
              {groupsWithStats.length} groups, {filteredComponents.length} components
            </Typography>
            
            {(searchTerm || activeFilters.length > 0) && (
              <Button
                size="small"
                startIcon={<ClearIcon />}
                onClick={() => {
                  setSearchTerm('');
                  setActiveFilters([]);
                }}
              >
                Clear Filters
              </Button>
            )}
          </Box>
        </>
      )}
    </Card>
  );
};

export default ComponentGroupSelector;