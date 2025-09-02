'use client';

import React, { useState, useEffect } from 'react';
import {
  Box,
  Grid,
  Typography,
  Button,
  Paper,
  Container,
  Fade,
  Alert,
  AlertTitle,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import { Add, Refresh, TrendingUp } from '@mui/icons-material';

import LoadingSpinner from '../../components/common/LoadingSpinner';
import ErrorBoundary from '../../components/common/ErrorBoundary';
import StatsWidget, {
  createAnalysisStatsWidget,
  createPerformanceWidget,
  createStatusWidget,
} from '../../components/dashboard/StatsWidget';
import AnalysisCard, { AnalysisData } from '../../components/dashboard/AnalysisCard';
import RecentActivity, { ActivityItem, createActivityItem } from '../../components/dashboard/RecentActivity';

const DashboardContainer = styled(Container)(({ theme }) => ({
  paddingTop: theme.spacing(3),
  paddingBottom: theme.spacing(3),
}));

const HeaderSection = styled(Paper)(({ theme }) => ({
  padding: theme.spacing(3),
  marginBottom: theme.spacing(3),
  background: `linear-gradient(135deg, ${theme.palette.primary.main} 0%, ${theme.palette.primary.dark} 100%)`,
  color: theme.palette.primary.contrastText,
  borderRadius: theme.spacing(2),
}));

const SectionTitle = styled(Typography)(({ theme }) => ({
  color: theme.palette.primary.main,
  fontWeight: 600,
  marginBottom: theme.spacing(2),
  display: 'flex',
  alignItems: 'center',
  gap: theme.spacing(1),
}));

// Mock data - in a real application, this would come from your API
const mockAnalyses: AnalysisData[] = [
  {
    id: '1',
    name: 'Floating Platform Analysis',
    description: 'Dynamic analysis of semi-submersible platform in 100-year storm',
    status: 'running',
    progress: 67,
    startTime: new Date(Date.now() - 2 * 60 * 60 * 1000), // 2 hours ago
    model: 'Platform_Rev_C.dat',
    type: 'dynamic',
    tags: ['platform', 'storm', 'dynamic'],
  },
  {
    id: '2',
    name: 'Mooring Line Fatigue',
    description: 'Long-term fatigue analysis of polyester mooring system',
    status: 'completed',
    startTime: new Date(Date.now() - 4 * 60 * 60 * 1000), // 4 hours ago
    endTime: new Date(Date.now() - 1 * 60 * 60 * 1000), // 1 hour ago
    duration: 180 * 60, // 3 hours in seconds
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
    startTime: new Date(Date.now() - 30 * 60 * 1000), // 30 minutes ago
    model: 'Riser_Install.dat',
    type: 'static',
    tags: ['riser', 'installation', 'static'],
  },
];

const mockActivities: ActivityItem[] = [
  createActivityItem('completed', 'Mooring Line Fatigue', 'fatigue', 'John Smith'),
  createActivityItem('started', 'Floating Platform Analysis', 'dynamic', 'Sarah Johnson'),
  createActivityItem('failed', 'Riser Installation', 'static', 'Mike Chen'),
  createActivityItem('downloaded', 'Turret Analysis', 'static', 'Emma Wilson'),
  createActivityItem('viewed', 'Pipeline Dynamics', 'dynamic', 'David Brown'),
];

const DashboardPage: React.FC = () => {
  const [loading, setLoading] = useState(true);
  const [analyses, setAnalyses] = useState<AnalysisData[]>([]);
  const [activities, setActivities] = useState<ActivityItem[]>([]);
  const [refreshing, setRefreshing] = useState(false);

  // Simulate data loading
  useEffect(() => {
    const loadData = async () => {
      setLoading(true);
      try {
        // Simulate API call
        await new Promise(resolve => setTimeout(resolve, 1500));
        setAnalyses(mockAnalyses);
        setActivities(mockActivities);
      } catch (error) {
        console.error('Failed to load dashboard data:', error);
      } finally {
        setLoading(false);
      }
    };

    loadData();
  }, []);

  const handleRefresh = async () => {
    setRefreshing(true);
    try {
      // Simulate refresh
      await new Promise(resolve => setTimeout(resolve, 1000));
      // Update with fresh data
      setActivities([
        createActivityItem('completed', 'New Dynamic Analysis', 'dynamic', 'Current User'),
        ...activities,
      ]);
    } finally {
      setRefreshing(false);
    }
  };

  const handleNewAnalysis = () => {
    // Navigate to new analysis page
    console.log('Navigate to new analysis');
  };

  const handleViewAnalysis = (analysis: AnalysisData) => {
    console.log('View analysis:', analysis.id);
  };

  const handleRunAnalysis = (analysis: AnalysisData) => {
    console.log('Run analysis:', analysis.id);
    // Update analysis status
    setAnalyses(prev =>
      prev.map(a =>
        a.id === analysis.id
          ? { ...a, status: 'running' as const, progress: 0, startTime: new Date() }
          : a
      )
    );
  };

  if (loading) {
    return <LoadingSpinner fullScreen message="Loading OrcaFlex Dashboard..." />;
  }

  // Calculate statistics
  const totalAnalyses = analyses.length + 47; // Mock total including historical
  const runningAnalyses = analyses.filter(a => a.status === 'running').length;
  const completedToday = 3; // Mock completed today
  const avgDuration = 95; // Mock average duration in minutes
  const successRate = 87; // Mock success rate
  const failureCount = analyses.filter(a => a.status === 'failed').length + 2;

  const statusCounts = {
    completed: analyses.filter(a => a.status === 'completed').length + 12,
    running: runningAnalyses,
    pending: analyses.filter(a => a.status === 'pending').length + 5,
    failed: failureCount,
  };

  return (
    <ErrorBoundary>
      <DashboardContainer maxWidth="xl">
        <Fade in timeout={600}>
          <div>
            <HeaderSection elevation={0}>
              <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start' }}>
                <Box>
                  <Typography variant="h4" component="h1" gutterBottom fontWeight="bold">
                    OrcaFlex Marine Analysis Dashboard
                  </Typography>
                  <Typography variant="body1" sx={{ opacity: 0.9, mb: 2 }}>
                    Monitor and manage your offshore engineering analyses
                  </Typography>
                  <Typography variant="caption" sx={{ opacity: 0.7 }}>
                    Last updated: {new Date().toLocaleString()}
                  </Typography>
                </Box>
                
                <Box sx={{ display: 'flex', gap: 2 }}>
                  <Button
                    variant="contained"
                    color="secondary"
                    startIcon={<Refresh />}
                    onClick={handleRefresh}
                    disabled={refreshing}
                  >
                    {refreshing ? 'Refreshing...' : 'Refresh'}
                  </Button>
                  
                  <Button
                    variant="contained"
                    color="inherit"
                    startIcon={<Add />}
                    onClick={handleNewAnalysis}
                    sx={{ color: 'primary.main' }}
                  >
                    New Analysis
                  </Button>
                </Box>
              </Box>
            </HeaderSection>

            {/* Statistics Overview */}
            <Grid container spacing={3} sx={{ mb: 4 }}>
              <Grid item xs={12} sm={6} md={4}>
                <StatsWidget
                  data={createAnalysisStatsWidget(totalAnalyses, runningAnalyses, completedToday)}
                />
              </Grid>
              
              <Grid item xs={12} sm={6} md={4}>
                <StatsWidget
                  data={createPerformanceWidget(avgDuration, successRate, failureCount)}
                />
              </Grid>
              
              <Grid item xs={12} sm={6} md={4}>
                <StatsWidget
                  data={createStatusWidget(
                    statusCounts.completed,
                    statusCounts.failed,
                    statusCounts.running,
                    statusCounts.pending
                  )}
                />
              </Grid>
            </Grid>

            <Grid container spacing={3}>
              {/* Recent Analyses */}
              <Grid item xs={12} lg={8}>
                <SectionTitle variant="h6">
                  <TrendingUp />
                  Active Analyses
                </SectionTitle>
                
                {analyses.length === 0 ? (
                  <Alert severity="info" sx={{ mb: 3 }}>
                    <AlertTitle>No Active Analyses</AlertTitle>
                    Start your first OrcaFlex analysis to see real-time progress and results here.
                  </Alert>
                ) : (
                  <Grid container spacing={2}>
                    {analyses.map((analysis) => (
                      <Grid item xs={12} md={6} xl={4} key={analysis.id}>
                        <AnalysisCard
                          analysis={analysis}
                          onView={handleViewAnalysis}
                          onRun={handleRunAnalysis}
                          compact
                        />
                      </Grid>
                    ))}
                  </Grid>
                )}
              </Grid>

              {/* Recent Activity */}
              <Grid item xs={12} lg={4}>
                <RecentActivity
                  activities={activities}
                  maxItems={8}
                  onViewAll={() => console.log('View all activities')}
                  onActivityClick={(activity) => console.log('Activity clicked:', activity)}
                />
              </Grid>
            </Grid>

            {/* Quick Actions */}
            <Paper sx={{ p: 3, mt: 4 }} elevation={1}>
              <Typography variant="h6" color="primary" gutterBottom>
                Quick Actions
              </Typography>
              
              <Grid container spacing={2}>
                <Grid item xs={12} sm={6} md={3}>
                  <Button
                    variant="outlined"
                    fullWidth
                    sx={{ py: 2, flexDirection: 'column', gap: 1 }}
                    onClick={handleNewAnalysis}
                  >
                    <Add />
                    New Analysis
                  </Button>
                </Grid>
                
                <Grid item xs={12} sm={6} md={3}>
                  <Button
                    variant="outlined"
                    fullWidth
                    sx={{ py: 2, flexDirection: 'column', gap: 1 }}
                    onClick={() => console.log('View all analyses')}
                  >
                    <TrendingUp />
                    View All Analyses
                  </Button>
                </Grid>
                
                <Grid item xs={12} sm={6} md={3}>
                  <Button
                    variant="outlined"
                    fullWidth
                    sx={{ py: 2, flexDirection: 'column', gap: 1 }}
                    onClick={() => console.log('View results')}
                  >
                    Assessment
                    View Results
                  </Button>
                </Grid>
                
                <Grid item xs={12} sm={6} md={3}>
                  <Button
                    variant="outlined"
                    fullWidth
                    sx={{ py: 2, flexDirection: 'column', gap: 1 }}
                    onClick={handleRefresh}
                  >
                    <Refresh />
                    Refresh Status
                  </Button>
                </Grid>
              </Grid>
            </Paper>
          </div>
        </Fade>
      </DashboardContainer>
    </ErrorBoundary>
  );
};

export default DashboardPage;