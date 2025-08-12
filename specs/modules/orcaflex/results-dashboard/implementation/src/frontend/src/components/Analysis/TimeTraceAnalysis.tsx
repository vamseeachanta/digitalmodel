import React, { useState, useEffect } from 'react';
import {
  Box,
  Grid,
  Paper,
  Typography,
  Button,
  CircularProgress,
  Alert,
  FormControlLabel,
  Switch
} from '@mui/material';
import { Download, Refresh, ShowChart } from '@mui/icons-material';
import FilterPanel from '@components/Filters/FilterPanel';
import TimeTrace from '@components/TimeTrace/TimeTrace';
import ExportDialog from '@components/Export/ExportDialog';
import { useSelector } from 'react-redux';
import { RootState } from '@store/index';
import { dataAPI } from '@services/api';

const TimeTraceAnalysis: React.FC = () => {
  const filters = useSelector((state: RootState) => state.filter);
  const [timeTraceData, setTimeTraceData] = useState<any>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [showStatistics, setShowStatistics] = useState(true);
  const [exportDialogOpen, setExportDialogOpen] = useState(false);

  useEffect(() => {
    if (filters.selectedCase && filters.selectedComponent && filters.selectedHeading !== null) {
      loadTimeTraceData();
    }
  }, []);

  const loadTimeTraceData = async () => {
    if (!filters.selectedCase || !filters.selectedComponent || filters.selectedHeading === null) {
      setError('Please select a case, component, and heading');
      return;
    }

    setLoading(true);
    setError(null);

    try {
      const response = await dataAPI.getTimeTrace({
        case: filters.selectedCase,
        component: filters.selectedComponent,
        heading: filters.selectedHeading,
        include_statistics: true
      });

      setTimeTraceData({
        ...response.data,
        title: `${filters.selectedComponent} @ ${filters.selectedHeading}° - ${filters.selectedCase}`
      });
    } catch (err: any) {
      setError(err.message || 'Failed to load time trace data');
    } finally {
      setLoading(false);
    }
  };

  const handleApplyFilters = () => {
    loadTimeTraceData();
  };

  const handleExport = () => {
    setExportDialogOpen(true);
  };

  return (
    <Box>
      <Box display="flex" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4">Time Trace Analysis</Typography>
        <Box display="flex" gap={1} alignItems="center">
          <FormControlLabel
            control={
              <Switch
                checked={showStatistics}
                onChange={(e) => setShowStatistics(e.target.checked)}
              />
            }
            label="Show Statistics"
          />
          <Button
            variant="outlined"
            startIcon={<Refresh />}
            onClick={loadTimeTraceData}
            disabled={loading || !filters.selectedCase}
          >
            Refresh
          </Button>
          <Button
            variant="contained"
            startIcon={<Download />}
            onClick={handleExport}
            disabled={!timeTraceData}
          >
            Export
          </Button>
        </Box>
      </Box>

      <Grid container spacing={3}>
        <Grid item xs={12} md={3}>
          <FilterPanel onApplyFilters={handleApplyFilters} />
          
          <Paper sx={{ mt: 2, p: 2 }}>
            <Typography variant="subtitle2" gutterBottom>
              <ShowChart sx={{ verticalAlign: 'middle', mr: 1 }} />
              Analysis Options
            </Typography>
            <Typography variant="body2" color="textSecondary" paragraph>
              Select a specific heading angle to view the time trace data for that direction.
            </Typography>
            <Typography variant="body2" color="textSecondary">
              Statistics overlay shows mean, standard deviation, and min/max values.
            </Typography>
          </Paper>
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

          {!loading && !error && !timeTraceData && (
            <Paper sx={{ p: 4, textAlign: 'center' }}>
              <Typography variant="h6" color="textSecondary" gutterBottom>
                No Data Loaded
              </Typography>
              <Typography variant="body2" color="textSecondary">
                Select filters including a heading angle and click "Apply Filters" to load time trace data
              </Typography>
            </Paper>
          )}

          {!loading && !error && timeTraceData && (
            <>
              <TimeTrace 
                data={timeTraceData} 
                height={500} 
                showStatistics={showStatistics} 
              />
              
              {timeTraceData.statistics && (
                <Grid container spacing={2} sx={{ mt: 2 }}>
                  <Grid item xs={12} md={6}>
                    <Paper sx={{ p: 2 }}>
                      <Typography variant="subtitle2" gutterBottom>
                        Frequency Analysis
                      </Typography>
                      <Typography variant="body2" color="textSecondary">
                        Dominant frequency analysis and spectral content will be displayed here
                      </Typography>
                    </Paper>
                  </Grid>
                  <Grid item xs={12} md={6}>
                    <Paper sx={{ p: 2 }}>
                      <Typography variant="subtitle2" gutterBottom>
                        Peak Detection
                      </Typography>
                      <Typography variant="body2" color="textSecondary">
                        Identified peaks and cycles will be displayed here
                      </Typography>
                    </Paper>
                  </Grid>
                </Grid>
              )}
            </>
          )}
        </Grid>
      </Grid>

      <ExportDialog
        open={exportDialogOpen}
        onClose={() => setExportDialogOpen(false)}
        exportType="data"
        data={timeTraceData}
      />
    </Box>
  );
};

export default TimeTraceAnalysis;