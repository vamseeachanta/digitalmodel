'use client';

import React, { useState, useEffect } from 'react';
import {
  Box,
  Typography,
  Button,
  Paper,
  Container,
  Tabs,
  Tab,
  Fab,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  MenuItem,
  Chip,
  Grid,
  Alert,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import { Add, FilterList, Search, PlayArrow, Pause, Stop } from '@mui/icons-material';

import LoadingSpinner from '../../components/common/LoadingSpinner';
import ErrorBoundary from '../../components/common/ErrorBoundary';
import DataTable, { DataTableColumn } from '../../components/common/DataTable';
import AnalysisCard, { AnalysisData, AnalysisStatus } from '../../components/dashboard/AnalysisCard';

const AnalysesContainer = styled(Container)(({ theme }) => ({
  paddingTop: theme.spacing(3),
  paddingBottom: theme.spacing(3),
}));

const HeaderSection = styled(Box)(({ theme }) => ({
  display: 'flex',
  justifyContent: 'space-between',
  alignItems: 'center',
  marginBottom: theme.spacing(3),
  flexWrap: 'wrap',
  gap: theme.spacing(2),
}));

const FilterSection = styled(Paper)(({ theme }) => ({
  padding: theme.spacing(2),
  marginBottom: theme.spacing(3),
}));

const FloatingActionButton = styled(Fab)(({ theme }) => ({
  position: 'fixed',
  bottom: theme.spacing(2),
  right: theme.spacing(2),
  zIndex: 1000,
}));

interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  value: number;
}

function TabPanel(props: TabPanelProps) {
  const { children, value, index, ...other } = props;

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`analyses-tabpanel-${index}`}
      aria-labelledby={`analyses-tab-${index}`}
      {...other}
    >
      {value === index && <Box>{children}</Box>}
    </div>
  );
}

// Mock data - expanded set
const mockAnalyses: AnalysisData[] = [
  {
    id: '1',
    name: 'Floating Platform Analysis',
    description: 'Dynamic analysis of semi-submersible platform in 100-year storm',
    status: 'running',
    progress: 67,
    startTime: new Date(Date.now() - 2 * 60 * 60 * 1000),
    model: 'Platform_Rev_C.dat',
    type: 'dynamic',
    tags: ['platform', 'storm', 'dynamic'],
  },
  {
    id: '2',
    name: 'Mooring Line Fatigue',
    description: 'Long-term fatigue analysis of polyester mooring system',
    status: 'completed',
    startTime: new Date(Date.now() - 4 * 60 * 60 * 1000),
    endTime: new Date(Date.now() - 1 * 60 * 60 * 1000),
    duration: 180 * 60,
    model: 'Mooring_System_v2.dat',
    type: 'fatigue',
    results: {
      maxTension: 2847,
      utilizationRatio: 0.73,
      criticalSection: 'Fairlead Connection',
    },
    tags: ['mooring', 'fatigue', 'polyester'],
  },
  {
    id: '3',
    name: 'Riser Installation',
    description: 'Static analysis of flexible riser during installation',
    status: 'failed',
    startTime: new Date(Date.now() - 30 * 60 * 1000),
    model: 'Riser_Install.dat',
    type: 'static',
    tags: ['riser', 'installation', 'static'],
  },
  {
    id: '4',
    name: 'Turret Static Analysis',
    description: 'Static analysis of turret bearing system under maximum load',
    status: 'pending',
    model: 'Turret_v1.dat',
    type: 'static',
    tags: ['turret', 'bearing', 'static'],
  },
  {
    id: '5',
    name: 'Pipeline Free Span',
    description: 'Dynamic analysis of subsea pipeline free span sections',
    status: 'completed',
    startTime: new Date(Date.now() - 24 * 60 * 60 * 1000),
    endTime: new Date(Date.now() - 22 * 60 * 60 * 1000),
    duration: 120 * 60,
    model: 'Pipeline_Span.dat',
    type: 'dynamic',
    results: {
      maxDisplacement: 2.3,
      utilizationRatio: 0.45,
      criticalSection: 'Mid-span',
    },
    tags: ['pipeline', 'free-span', 'subsea'],
  },
  {
    id: '6',
    name: 'Umbilical Dynamics',
    description: 'Dynamic analysis of steel tube umbilical in harsh environment',
    status: 'paused',
    startTime: new Date(Date.now() - 3 * 60 * 60 * 1000),
    model: 'Umbilical_STU.dat',
    type: 'dynamic',
    progress: 34,
    tags: ['umbilical', 'steel-tube', 'dynamic'],
  },
];

const analysisTableColumns: DataTableColumn[] = [
  {
    id: 'name',
    label: 'Analysis Name',
    minWidth: 200,
    sortable: true,
  },
  {
    id: 'type',
    label: 'Type',
    minWidth: 100,
    format: (value: string) => (
      <Chip
        label={value.charAt(0).toUpperCase() + value.slice(1)}
        size="small"
        color={
          value === 'dynamic' ? 'secondary' :
          value === 'static' ? 'primary' :
          value === 'fatigue' ? 'warning' : 'default'
        }
        variant="outlined"
      />
    ),
  },
  {
    id: 'status',
    label: 'Status',
    minWidth: 120,
    type: 'status',
  },
  {
    id: 'model',
    label: 'Model File',
    minWidth: 150,
  },
  {
    id: 'startTime',
    label: 'Started',
    minWidth: 150,
    format: (value: Date) => value?.toLocaleString() || '-',
  },
  {
    id: 'duration',
    label: 'Duration',
    minWidth: 100,
    format: (value: number) => {
      if (!value) return '-';
      const minutes = Math.floor(value / 60);
      return minutes > 60 ? `${Math.floor(minutes / 60)}h ${minutes % 60}m` : `${minutes}m`;
    },
  },
  {
    id: 'progress',
    label: 'Progress',
    minWidth: 100,
    format: (value: number) => value ? `${value}%` : '-',
  },
];

const AnalysesPage: React.FC = () => {
  const [loading, setLoading] = useState(true);
  const [analyses, setAnalyses] = useState<AnalysisData[]>([]);
  const [tabValue, setTabValue] = useState(0);
  const [viewMode, setViewMode] = useState<'cards' | 'table'>('cards');
  const [newAnalysisDialogOpen, setNewAnalysisDialogOpen] = useState(false);
  const [filterOpen, setFilterOpen] = useState(false);
  const [filters, setFilters] = useState({
    status: '',
    type: '',
    search: '',
  });

  useEffect(() => {
    const loadAnalyses = async () => {
      setLoading(true);
      try {
        await new Promise(resolve => setTimeout(resolve, 1000));
        setAnalyses(mockAnalyses);
      } finally {
        setLoading(false);
      }
    };

    loadAnalyses();
  }, []);

  const getFilteredAnalyses = (status?: AnalysisStatus) => {
    let filtered = analyses;

    if (status) {
      filtered = filtered.filter(analysis => analysis.status === status);
    }

    if (filters.search) {
      filtered = filtered.filter(analysis =>
        analysis.name.toLowerCase().includes(filters.search.toLowerCase()) ||
        analysis.description?.toLowerCase().includes(filters.search.toLowerCase())
      );
    }

    if (filters.status) {
      filtered = filtered.filter(analysis => analysis.status === filters.status);
    }

    if (filters.type) {
      filtered = filtered.filter(analysis => analysis.type === filters.type);
    }

    return filtered;
  };

  const handleTabChange = (_event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  const handleRunAnalysis = (analysis: AnalysisData) => {
    setAnalyses(prev =>
      prev.map(a =>
        a.id === analysis.id
          ? { ...a, status: 'running' as const, progress: 0, startTime: new Date() }
          : a
      )
    );
  };

  const handlePauseAnalysis = (analysis: AnalysisData) => {
    setAnalyses(prev =>
      prev.map(a =>
        a.id === analysis.id ? { ...a, status: 'paused' as const } : a
      )
    );
  };

  const handleStopAnalysis = (analysis: AnalysisData) => {
    setAnalyses(prev =>
      prev.map(a =>
        a.id === analysis.id ? { ...a, status: 'pending' as const, progress: undefined } : a
      )
    );
  };

  const handleDeleteAnalysis = (analysis: AnalysisData) => {
    if (confirm(`Are you sure you want to delete "${analysis.name}"?`)) {
      setAnalyses(prev => prev.filter(a => a.id !== analysis.id));
    }
  };

  const handleNewAnalysis = () => {
    setNewAnalysisDialogOpen(true);
  };

  if (loading) {
    return <LoadingSpinner fullScreen message="Loading analyses..." />;
  }

  const allAnalyses = getFilteredAnalyses();
  const runningAnalyses = getFilteredAnalyses('running');
  const completedAnalyses = getFilteredAnalyses('completed');
  const failedAnalyses = getFilteredAnalyses('failed');

  return (
    <ErrorBoundary>
      <AnalysesContainer maxWidth="xl">
        <HeaderSection>
          <Box>
            <Typography variant="h4" component="h1" gutterBottom color="primary">
              Analysis Management
            </Typography>
            <Typography variant="body1" color="text.secondary">
              Create, monitor, and manage your OrcaFlex analyses
            </Typography>
          </Box>

          <Box sx={{ display: 'flex', gap: 2, alignItems: 'center' }}>
            <Button
              variant="outlined"
              startIcon={<FilterList />}
              onClick={() => setFilterOpen(!filterOpen)}
            >
              Filters
            </Button>
            
            <Button
              variant="outlined"
              onClick={() => setViewMode(viewMode === 'cards' ? 'table' : 'cards')}
            >
              {viewMode === 'cards' ? 'Table View' : 'Card View'}
            </Button>

            <Button
              variant="contained"
              startIcon={<Add />}
              onClick={handleNewAnalysis}
            >
              New Analysis
            </Button>
          </Box>
        </HeaderSection>

        {filterOpen && (
          <FilterSection>
            <Grid container spacing={2} alignItems="center">
              <Grid item xs={12} sm={6} md={3}>
                <TextField
                  fullWidth
                  size="small"
                  label="Search"
                  value={filters.search}
                  onChange={(e) => setFilters(prev => ({ ...prev, search: e.target.value }))}
                  InputProps={{
                    startAdornment: <Search sx={{ mr: 1, color: 'text.secondary' }} />,
                  }}
                />
              </Grid>
              
              <Grid item xs={12} sm={6} md={3}>
                <TextField
                  fullWidth
                  size="small"
                  select
                  label="Status"
                  value={filters.status}
                  onChange={(e) => setFilters(prev => ({ ...prev, status: e.target.value }))}
                >
                  <MenuItem value="">All Statuses</MenuItem>
                  <MenuItem value="pending">Pending</MenuItem>
                  <MenuItem value="running">Running</MenuItem>
                  <MenuItem value="completed">Completed</MenuItem>
                  <MenuItem value="failed">Failed</MenuItem>
                  <MenuItem value="paused">Paused</MenuItem>
                </TextField>
              </Grid>
              
              <Grid item xs={12} sm={6} md={3}>
                <TextField
                  fullWidth
                  size="small"
                  select
                  label="Type"
                  value={filters.type}
                  onChange={(e) => setFilters(prev => ({ ...prev, type: e.target.value }))}
                >
                  <MenuItem value="">All Types</MenuItem>
                  <MenuItem value="static">Static</MenuItem>
                  <MenuItem value="dynamic">Dynamic</MenuItem>
                  <MenuItem value="frequency">Frequency</MenuItem>
                  <MenuItem value="fatigue">Fatigue</MenuItem>
                </TextField>
              </Grid>
              
              <Grid item xs={12} sm={6} md={3}>
                <Button
                  variant="outlined"
                  fullWidth
                  onClick={() => setFilters({ status: '', type: '', search: '' })}
                >
                  Clear Filters
                </Button>
              </Grid>
            </Grid>
          </FilterSection>
        )}

        <Paper sx={{ mb: 3 }}>
          <Tabs
            value={tabValue}
            onChange={handleTabChange}
            indicatorColor="primary"
            textColor="primary"
            variant="scrollable"
            scrollButtons="auto"
          >
            <Tab label={`All (${allAnalyses.length})`} />
            <Tab label={`Running (${runningAnalyses.length})`} />
            <Tab label={`Completed (${completedAnalyses.length})`} />
            <Tab label={`Failed (${failedAnalyses.length})`} />
          </Tabs>
        </Paper>

        <TabPanel value={tabValue} index={0}>
          {viewMode === 'cards' ? (
            <Grid container spacing={3}>
              {allAnalyses.map((analysis) => (
                <Grid item xs={12} md={6} lg={4} key={analysis.id}>
                  <AnalysisCard
                    analysis={analysis}
                    onRun={handleRunAnalysis}
                    onPause={handlePauseAnalysis}
                    onStop={handleStopAnalysis}
                    onDelete={handleDeleteAnalysis}
                    onView={(analysis) => console.log('View:', analysis.id)}
                    onDownload={(analysis) => console.log('Download:', analysis.id)}
                  />
                </Grid>
              ))}
            </Grid>
          ) : (
            <DataTable
              title="All Analyses"
              columns={analysisTableColumns}
              rows={allAnalyses}
              searchable
              exportable
              onRowClick={(row) => console.log('Row clicked:', row)}
              onExport={() => console.log('Export data')}
            />
          )}
          
          {allAnalyses.length === 0 && (
            <Alert severity="info" sx={{ textAlign: 'center' }}>
              No analyses found. Create your first analysis to get started.
            </Alert>
          )}
        </TabPanel>

        <TabPanel value={tabValue} index={1}>
          <Grid container spacing={3}>
            {runningAnalyses.map((analysis) => (
              <Grid item xs={12} md={6} lg={4} key={analysis.id}>
                <AnalysisCard
                  analysis={analysis}
                  onPause={handlePauseAnalysis}
                  onStop={handleStopAnalysis}
                  onView={(analysis) => console.log('View:', analysis.id)}
                />
              </Grid>
            ))}
          </Grid>
          
          {runningAnalyses.length === 0 && (
            <Alert severity="info">
              No analyses are currently running.
            </Alert>
          )}
        </TabPanel>

        <TabPanel value={tabValue} index={2}>
          <Grid container spacing={3}>
            {completedAnalyses.map((analysis) => (
              <Grid item xs={12} md={6} lg={4} key={analysis.id}>
                <AnalysisCard
                  analysis={analysis}
                  onView={(analysis) => console.log('View:', analysis.id)}
                  onDownload={(analysis) => console.log('Download:', analysis.id)}
                  onDelete={handleDeleteAnalysis}
                />
              </Grid>
            ))}
          </Grid>
          
          {completedAnalyses.length === 0 && (
            <Alert severity="info">
              No completed analyses found.
            </Alert>
          )}
        </TabPanel>

        <TabPanel value={tabValue} index={3}>
          <Grid container spacing={3}>
            {failedAnalyses.map((analysis) => (
              <Grid item xs={12} md={6} lg={4} key={analysis.id}>
                <AnalysisCard
                  analysis={analysis}
                  onRun={handleRunAnalysis}
                  onDelete={handleDeleteAnalysis}
                  onView={(analysis) => console.log('View logs:', analysis.id)}
                />
              </Grid>
            ))}
          </Grid>
          
          {failedAnalyses.length === 0 && (
            <Alert severity="success">
              No failed analyses - great job!
            </Alert>
          )}
        </TabPanel>

        <FloatingActionButton
          color="primary"
          aria-label="add analysis"
          onClick={handleNewAnalysis}
        >
          <Add />
        </FloatingActionButton>

        {/* New Analysis Dialog */}
        <Dialog
          open={newAnalysisDialogOpen}
          onClose={() => setNewAnalysisDialogOpen(false)}
          maxWidth="sm"
          fullWidth
        >
          <DialogTitle>Create New Analysis</DialogTitle>
          <DialogContent>
            <Typography color="text.secondary" gutterBottom>
              This feature will integrate with your OrcaFlex installation to create and configure new analyses.
            </Typography>
            <Alert severity="info" sx={{ mt: 2 }}>
              Full analysis creation interface coming soon. This will include model file selection, 
              analysis type configuration, and parameter setup.
            </Alert>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setNewAnalysisDialogOpen(false)}>
              Cancel
            </Button>
            <Button variant="contained" onClick={() => setNewAnalysisDialogOpen(false)}>
              Create Analysis
            </Button>
          </DialogActions>
        </Dialog>
      </AnalysesContainer>
    </ErrorBoundary>
  );
};

export default AnalysesPage;