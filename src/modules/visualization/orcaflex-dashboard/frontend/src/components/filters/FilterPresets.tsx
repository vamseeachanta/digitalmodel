import React, { useState, useEffect } from 'react';
import {
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  ListItemSecondaryAction,
  Menu,
  MenuItem,
  TextField,
  Typography,
  Chip,
  Alert,
  Tooltip,
  Divider,
  Grid,
} from '@mui/material';
import {
  Add as AddIcon,
  Edit as EditIcon,
  Delete as DeleteIcon,
  Save as SaveIcon,
  Cancel as CancelIcon,
  MoreVert as MoreIcon,
  Bookmark as BookmarkIcon,
  BookmarkBorder as BookmarkBorderIcon,
  Share as ShareIcon,
  Download as DownloadIcon,
  Upload as UploadIcon,
  History as HistoryIcon,
} from '@mui/icons-material';
import { useFilterContext, FilterPreset, LoadingCondition } from './FilterContext';

interface FilterPresetsProps {
  onPresetApply?: (preset: FilterPreset) => void;
  onPresetSave?: (preset: FilterPreset) => void;
  onPresetDelete?: (id: string) => void;
}

const FilterPresets: React.FC<FilterPresetsProps> = ({
  onPresetApply,
  onPresetSave,
  onPresetDelete,
}) => {
  const { state, actions, presets } = useFilterContext();
  const [dialogOpen, setDialogOpen] = useState(false);
  const [editingPreset, setEditingPreset] = useState<FilterPreset | null>(null);
  const [presetName, setPresetName] = useState('');
  const [presetDescription, setPresetDescription] = useState('');
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const [selectedPreset, setSelectedPreset] = useState<FilterPreset | null>(null);
  const [importDialogOpen, setImportDialogOpen] = useState(false);
  const [importData, setImportData] = useState('');
  const [error, setError] = useState<string | null>(null);

  // Check if current filters match any preset
  const currentFiltersHash = JSON.stringify({
    analysisCase: state.analysisCase.sort(),
    loadingCondition: state.loadingCondition.sort(),
    heading: state.heading.sort(),
  });

  const matchingPreset = presets.find(preset => 
    JSON.stringify({
      analysisCase: preset.filters.analysisCase.sort(),
      loadingCondition: preset.filters.loadingCondition.sort(),
      heading: preset.filters.heading.sort(),
    }) === currentFiltersHash
  );

  // Handle save preset
  const handleSavePreset = () => {
    if (!presetName.trim()) {
      setError('Preset name is required');
      return;
    }

    if (presets.some(p => p.name === presetName.trim() && p.id !== editingPreset?.id)) {
      setError('A preset with this name already exists');
      return;
    }

    try {
      let savedPreset: FilterPreset;
      
      if (editingPreset) {
        // Update existing preset
        savedPreset = {
          ...editingPreset,
          name: presetName.trim(),
          description: presetDescription.trim(),
          filters: {
            analysisCase: state.analysisCase,
            loadingCondition: state.loadingCondition,
            heading: state.heading,
          },
        };
        
        // This would typically update via context
        // For now, we'll assume the context handles the update
      } else {
        // Create new preset
        savedPreset = actions.savePreset(presetName.trim(), presetDescription.trim());
      }

      onPresetSave?.(savedPreset);
      handleCloseDialog();
    } catch (err) {
      setError('Failed to save preset');
    }
  };

  // Handle apply preset
  const handleApplyPreset = (preset: FilterPreset) => {
    actions.applyPreset(preset);
    onPresetApply?.(preset);
    handleCloseMenu();
  };

  // Handle delete preset
  const handleDeletePreset = (preset: FilterPreset) => {
    actions.deletePreset(preset.id);
    onPresetDelete?.(preset.id);
    handleCloseMenu();
  };

  // Handle dialog open
  const handleOpenDialog = (preset?: FilterPreset) => {
    if (preset) {
      setEditingPreset(preset);
      setPresetName(preset.name);
      setPresetDescription(preset.description);
    } else {
      setEditingPreset(null);
      setPresetName('');
      setPresetDescription('');
    }
    setError(null);
    setDialogOpen(true);
  };

  // Handle dialog close
  const handleCloseDialog = () => {
    setDialogOpen(false);
    setEditingPreset(null);
    setPresetName('');
    setPresetDescription('');
    setError(null);
  };

  // Handle menu
  const handleOpenMenu = (event: React.MouseEvent<HTMLElement>, preset: FilterPreset) => {
    setAnchorEl(event.currentTarget);
    setSelectedPreset(preset);
  };

  const handleCloseMenu = () => {
    setAnchorEl(null);
    setSelectedPreset(null);
  };

  // Export presets
  const handleExportPresets = () => {
    const exportData = {
      version: '1.0.0',
      timestamp: new Date().toISOString(),
      presets: presets,
    };
    
    const blob = new Blob([JSON.stringify(exportData, null, 2)], {
      type: 'application/json',
    });
    
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `orcaflex-filter-presets-${new Date().toISOString().split('T')[0]}.json`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  };

  // Import presets
  const handleImportPresets = () => {
    try {
      const importedData = JSON.parse(importData);
      
      if (!importedData.presets || !Array.isArray(importedData.presets)) {
        throw new Error('Invalid preset file format');
      }

      // Validate preset structure
      for (const preset of importedData.presets) {
        if (!preset.id || !preset.name || !preset.filters) {
          throw new Error('Invalid preset structure');
        }
      }

      // Import presets (would need to be implemented in context)
      // For now, just close the dialog
      setImportDialogOpen(false);
      setImportData('');
      setError(null);
    } catch (err) {
      setError('Failed to import presets. Please check the file format.');
    }
  };

  // Format loading condition for display
  const formatLoadingCondition = (condition: LoadingCondition): string => {
    return `${condition.waterLevel.toUpperCase()}, ${condition.volume}, ${condition.side.toUpperCase()}`;
  };

  // Get preset summary
  const getPresetSummary = (preset: FilterPreset): string => {
    const parts = [];
    if (preset.filters.analysisCase.length) {
      parts.push(`${preset.filters.analysisCase.length} case${preset.filters.analysisCase.length !== 1 ? 's' : ''}`);
    }
    if (preset.filters.loadingCondition.length) {
      parts.push(`${preset.filters.loadingCondition.length} condition${preset.filters.loadingCondition.length !== 1 ? 's' : ''}`);
    }
    if (preset.filters.heading.length) {
      parts.push(`${preset.filters.heading.length} heading${preset.filters.heading.length !== 1 ? 's' : ''}`);
    }
    return parts.join(', ') || 'No filters';
  };

  return (
    <Box>
      {/* Header */}
      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', mb: 2 }}>
        <Typography variant="h6">Filter Presets</Typography>
        <Box>
          <Tooltip title="Import Presets">
            <IconButton onClick={() => setImportDialogOpen(true)} size="small">
              <UploadIcon />
            </IconButton>
          </Tooltip>
          <Tooltip title="Export Presets">
            <IconButton onClick={handleExportPresets} size="small" disabled={presets.length === 0}>
              <DownloadIcon />
            </IconButton>
          </Tooltip>
          <Button
            startIcon={<AddIcon />}
            variant="outlined"
            size="small"
            onClick={() => handleOpenDialog()}
            disabled={state.analysisCase.length === 0}
          >
            Save Current
          </Button>
        </Box>
      </Box>

      {/* Current filter indicator */}
      {matchingPreset && (
        <Alert severity="info" sx={{ mb: 2 }}>
          <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
            <BookmarkIcon />
            Current filters match preset: <strong>{matchingPreset.name}</strong>
          </Box>
        </Alert>
      )}

      {/* No filters warning */}
      {state.analysisCase.length === 0 && state.loadingCondition.length === 0 && state.heading.length === 0 && (
        <Alert severity="warning" sx={{ mb: 2 }}>
          No filters are currently applied. Apply some filters before saving a preset.
        </Alert>
      )}

      {/* Presets list */}
      {presets.length === 0 ? (
        <Box sx={{ textAlign: 'center', py: 4 }}>
          <BookmarkBorderIcon sx={{ fontSize: 48, color: 'text.secondary', mb: 1 }} />
          <Typography variant="body2" color="text.secondary">
            No saved presets yet. Save your current filter configuration to create your first preset.
          </Typography>
        </Box>
      ) : (
        <Grid container spacing={2}>
          {presets.map((preset) => (
            <Grid item xs={12} sm={6} md={4} key={preset.id}>
              <Card variant="outlined" sx={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
                <CardContent sx={{ flexGrow: 1 }}>
                  <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', mb: 1 }}>
                    <Typography variant="subtitle1" noWrap>
                      {preset.name}
                    </Typography>
                    <IconButton
                      size="small"
                      onClick={(e) => handleOpenMenu(e, preset)}
                    >
                      <MoreIcon />
                    </IconButton>
                  </Box>
                  
                  {preset.description && (
                    <Typography variant="body2" color="text.secondary" sx={{ mb: 1 }}>
                      {preset.description}
                    </Typography>
                  )}
                  
                  <Typography variant="caption" color="text.secondary" sx={{ mb: 1 }}>
                    {getPresetSummary(preset)}
                  </Typography>
                  
                  <Typography variant="caption" color="text.secondary">
                    Created: {preset.createdAt.toLocaleDateString()}
                  </Typography>
                  
                  {/* Filter preview */}
                  <Box sx={{ mt: 1 }}>
                    {preset.filters.analysisCase.length > 0 && (
                      <Chip
                        label={`${preset.filters.analysisCase.length} Cases`}
                        size="small"
                        sx={{ mr: 0.5, mb: 0.5 }}
                      />
                    )}
                    {preset.filters.loadingCondition.length > 0 && (
                      <Chip
                        label={`${preset.filters.loadingCondition.length} Conditions`}
                        size="small"
                        sx={{ mr: 0.5, mb: 0.5 }}
                      />
                    )}
                    {preset.filters.heading.length > 0 && (
                      <Chip
                        label={`${preset.filters.heading.length} Headings`}
                        size="small"
                        sx={{ mr: 0.5, mb: 0.5 }}
                      />
                    )}
                  </Box>
                </CardContent>
                
                <CardActions sx={{ justifyContent: 'space-between' }}>
                  <Button
                    size="small"
                    onClick={() => handleApplyPreset(preset)}
                    startIcon={<BookmarkIcon />}
                  >
                    Apply
                  </Button>
                  <Box>
                    <Tooltip title="Edit">
                      <IconButton
                        size="small"
                        onClick={() => handleOpenDialog(preset)}
                      >
                        <EditIcon />
                      </IconButton>
                    </Tooltip>
                    <Tooltip title="Delete">
                      <IconButton
                        size="small"
                        onClick={() => handleDeletePreset(preset)}
                        color="error"
                      >
                        <DeleteIcon />
                      </IconButton>
                    </Tooltip>
                  </Box>
                </CardActions>
              </Card>
            </Grid>
          ))}
        </Grid>
      )}

      {/* Context menu */}
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleCloseMenu}
      >
        <MenuItem onClick={() => selectedPreset && handleApplyPreset(selectedPreset)}>
          <ListItemIcon><BookmarkIcon /></ListItemIcon>
          Apply Preset
        </MenuItem>
        <MenuItem onClick={() => selectedPreset && handleOpenDialog(selectedPreset)}>
          <ListItemIcon><EditIcon /></ListItemIcon>
          Edit
        </MenuItem>
        <Divider />
        <MenuItem
          onClick={() => selectedPreset && handleDeletePreset(selectedPreset)}
          sx={{ color: 'error.main' }}
        >
          <ListItemIcon><DeleteIcon color="error" /></ListItemIcon>
          Delete
        </MenuItem>
      </Menu>

      {/* Save/Edit preset dialog */}
      <Dialog open={dialogOpen} onClose={handleCloseDialog} maxWidth="sm" fullWidth>
        <DialogTitle>
          {editingPreset ? 'Edit Preset' : 'Save Filter Preset'}
        </DialogTitle>
        <DialogContent>
          {error && (
            <Alert severity="error" sx={{ mb: 2 }}>
              {error}
            </Alert>
          )}
          
          <TextField
            autoFocus
            margin="dense"
            label="Preset Name"
            fullWidth
            variant="outlined"
            value={presetName}
            onChange={(e) => setPresetName(e.target.value)}
            error={!presetName.trim()}
            helperText={!presetName.trim() ? 'Name is required' : ''}
          />
          
          <TextField
            margin="dense"
            label="Description (optional)"
            fullWidth
            multiline
            rows={3}
            variant="outlined"
            value={presetDescription}
            onChange={(e) => setPresetDescription(e.target.value)}
          />
          
          {/* Current filters preview */}
          <Box sx={{ mt: 2 }}>
            <Typography variant="subtitle2" gutterBottom>
              Current Filters Preview:
            </Typography>
            <Box sx={{ p: 2, bgcolor: 'background.paper', borderRadius: 1, border: '1px solid', borderColor: 'divider' }}>
              <Typography variant="body2">
                <strong>Analysis Cases:</strong> {state.analysisCase.length || 'None'}
              </Typography>
              <Typography variant="body2">
                <strong>Loading Conditions:</strong> {state.loadingCondition.length || 'None'}
              </Typography>
              <Typography variant="body2">
                <strong>Headings:</strong> {state.heading.length || 'None'}
              </Typography>
            </Box>
          </Box>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} startIcon={<CancelIcon />}>
            Cancel
          </Button>
          <Button
            onClick={handleSavePreset}
            variant="contained"
            startIcon={<SaveIcon />}
            disabled={!presetName.trim()}
          >
            {editingPreset ? 'Update' : 'Save'} Preset
          </Button>
        </DialogActions>
      </Dialog>

      {/* Import dialog */}
      <Dialog open={importDialogOpen} onClose={() => setImportDialogOpen(false)} maxWidth="sm" fullWidth>
        <DialogTitle>Import Filter Presets</DialogTitle>
        <DialogContent>
          {error && (
            <Alert severity="error" sx={{ mb: 2 }}>
              {error}
            </Alert>
          )}
          
          <TextField
            margin="dense"
            label="Paste preset data (JSON)"
            fullWidth
            multiline
            rows={10}
            variant="outlined"
            value={importData}
            onChange={(e) => setImportData(e.target.value)}
            helperText="Paste the exported preset JSON data here"
          />
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setImportDialogOpen(false)}>
            Cancel
          </Button>
          <Button
            onClick={handleImportPresets}
            variant="contained"
            disabled={!importData.trim()}
          >
            Import
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
};

export default FilterPresets;