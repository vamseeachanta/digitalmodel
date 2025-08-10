import React, { useState, useEffect } from 'react';
import { 
  Box, 
  Grid, 
  Paper, 
  Typography, 
  Button, 
  CircularProgress,
  Alert 
} from '@mui/material';
import { Download, Refresh } from '@mui/icons-material';
import FilterPanel from '@components/Filters/FilterPanel';
import PolarPlot from '@components/PolarPlot/PolarPlot';
import ExportDialog from '@components/Export/ExportDialog';
import { useSelector } from 'react-redux';
import { RootState } from '@store/index';
import { dataAPI } from '@services/api';

const PolarAnalysis: React.FC = () => {
  const filters = useSelector((state: RootState) => state.filter);
  const [polarData, setPolarData] = useState<any>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [exportDialogOpen, setExportDialogOpen] = useState(false);

  useEffect(() => {
    if (filters.selectedCase && filters.selectedComponent) {
      loadPolarData();
    }
  }, []);

  const loadPolarData = async () => {
    if (!filters.selectedCase || !filters.selectedComponent) {
      setError('Please select a case and component');
      return;
    }

    setLoading(true);
    setError(null);

    try {
      const response = await dataAPI.getPolarData({
        case: filters.selectedCase,
        component: filters.selectedComponent,
        loading_condition: filters.selectedLoadingCondition,
        include_statistics: true
      });

      setPolarData({
        ...response.data,
        title: `${filters.selectedComponent} - ${filters.selectedCase}`
      });
    } catch (err: any) {
      setError(err.message || 'Failed to load polar data');
    } finally {
      setLoading(false);
    }
  };

  const handleApplyFilters = () => {
    loadPolarData();
  };

  const handleExport = () => {
    setExportDialogOpen(true);
  };

  return (
    <Box>
      <Box display="flex" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4">Polar Analysis</Typography>
        <Box display="flex" gap={1}>
          <Button
            variant="outlined"
            startIcon={<Refresh />}
            onClick={loadPolarData}
            disabled={loading || !filters.selectedCase}
          >
            Refresh
          </Button>
          <Button
            variant="contained"
            startIcon={<Download />}
            onClick={handleExport}
            disabled={!polarData}
          >
            Export
          </Button>
        </Box>
      </Box>

      <Grid container spacing={3}>
        <Grid item xs={12} md={3}>
          <FilterPanel onApplyFilters={handleApplyFilters} />
        </Grid>

        <Grid item xs={12} md={9}>
          {loading && (
            <Box display="flex" justifyContent="center" alignItems="center" height={400}>
              <CircularProgress />
            </Box>
          )}

          {error && (
            <Alert severity="error" sx={{ mb: 2 }}>
              {error}
            </Alert>
          )}

          {!loading && !error && !polarData && (
            <Paper sx={{ p: 4, textAlign: 'center' }}>
              <Typography variant="h6" color="textSecondary" gutterBottom>
                No Data Loaded
              </Typography>
              <Typography variant="body2" color="textSecondary">
                Select filters and click "Apply Filters" to load polar data
              </Typography>
            </Paper>
          )}

          {!loading && !error && polarData && (
            <>
              <PolarPlot data={polarData} width={700} height={700} />
              
              <Paper sx={{ mt: 2, p: 2 }}>
                <Typography variant="h6" gutterBottom>
                  Analysis Summary
                </Typography>
                <Grid container spacing={2}>
                  <Grid item xs={6} sm={3}>
                    <Typography variant="caption" color="textSecondary">
                      Maximum Response
                    </Typography>
                    <Typography variant="h6">
                      {polarData.max_value?.toFixed(2)} {polarData.unit}
                    </Typography>
                  </Grid>
                  <Grid item xs={6} sm={3}>
                    <Typography variant="caption" color="textSecondary">
                      Minimum Response
                    </Typography>
                    <Typography variant="h6">
                      {polarData.min_value?.toFixed(2)} {polarData.unit}
                    </Typography>
                  </Grid>
                  <Grid item xs={6} sm={3}>
                    <Typography variant="caption" color="textSecondary">
                      Mean Response
                    </Typography>
                    <Typography variant="h6">
                      {polarData.mean_value?.toFixed(2)} {polarData.unit}
                    </Typography>
                  </Grid>
                  <Grid item xs={6} sm={3}>
                    <Typography variant="caption" color="textSecondary">
                      Standard Deviation
                    </Typography>
                    <Typography variant="h6">
                      {polarData.std_dev?.toFixed(2)} {polarData.unit}
                    </Typography>
                  </Grid>
                </Grid>
              </Paper>
            </>
          )}
        </Grid>
      </Grid>

      <ExportDialog
        open={exportDialogOpen}
        onClose={() => setExportDialogOpen(false)}
        exportType="chart"
        data={polarData}
      />
    </Box>
  );
};

export default PolarAnalysis;