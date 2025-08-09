'use client';

import React from 'react';
import { 
  Container, 
  Typography, 
  Grid, 
  Card, 
  CardContent, 
  Button, 
  Box,
  Chip,
  LinearProgress
} from '@mui/material';
import { 
  Add as AddIcon,
  Analytics as AnalyticsIcon,
  Assessment as AssessmentIcon,
  CloudUpload as CloudUploadIcon
} from '@mui/icons-material';

import { DashboardLayout } from '@/components/layout/DashboardLayout';
import { useAnalyses } from '@/hooks/useAnalyses';
import { useResults } from '@/hooks/useResults';
import { AnalysisStatus } from '@/types/analysis';

const Dashboard: React.FC = () => {
  const { data: analyses = [], isLoading: analysesLoading } = useAnalyses();
  const { data: results = [], isLoading: resultsLoading } = useResults();

  // Calculate statistics
  const runningAnalyses = analyses.filter(a => a.status === AnalysisStatus.RUNNING);
  const completedAnalyses = analyses.filter(a => a.status === AnalysisStatus.COMPLETED);
  const failedAnalyses = analyses.filter(a => a.status === AnalysisStatus.FAILED);

  const quickStats = [
    {
      title: 'Total Analyses',
      value: analyses.length,
      icon: <AnalyticsIcon />,
      color: 'primary',
    },
    {
      title: 'Running',
      value: runningAnalyses.length,
      icon: <LinearProgress />,
      color: 'warning',
    },
    {
      title: 'Completed',
      value: completedAnalyses.length,
      icon: <AssessmentIcon />,
      color: 'success',
    },
    {
      title: 'Results Available',
      value: results.length,
      icon: <CloudUploadIcon />,
      color: 'info',
    },
  ];

  const getStatusColor = (status: AnalysisStatus) => {
    switch (status) {
      case AnalysisStatus.RUNNING:
        return 'warning';
      case AnalysisStatus.COMPLETED:
        return 'success';
      case AnalysisStatus.FAILED:
        return 'error';
      case AnalysisStatus.CANCELLED:
        return 'default';
      default:
        return 'primary';
    }
  };

  return (
    <DashboardLayout>
      <Container maxWidth=\"xl\" sx={{ py: 4 }}>
        {/* Header */}
        <Box sx={{ mb: 4 }}>
          <Typography variant=\"h1\" gutterBottom>
            OrcaFlex Results Dashboard
          </Typography>
          <Typography variant=\"body1\" color=\"text.secondary\">
            Visualize and analyze your OrcaFlex simulation results
          </Typography>
        </Box>

        {/* Quick Stats */}
        <Grid container spacing={3} sx={{ mb: 4 }}>
          {quickStats.map((stat, index) => (
            <Grid item xs={12} sm={6} md={3} key={index}>
              <Card>
                <CardContent>
                  <Box sx={{ display: 'flex', alignItems: 'center', mb: 1 }}>
                    <Box sx={{ mr: 2, color: `${stat.color}.main` }}>
                      {stat.icon}
                    </Box>
                    <Typography variant=\"h3\" component=\"div\">
                      {stat.value}
                    </Typography>
                  </Box>
                  <Typography variant=\"body2\" color=\"text.secondary\">
                    {stat.title}
                  </Typography>
                </CardContent>
              </Card>
            </Grid>
          ))}
        </Grid>

        {/* Quick Actions */}
        <Grid container spacing={3} sx={{ mb: 4 }}>
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant=\"h3\" gutterBottom>
                  Quick Actions
                </Typography>
                <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap' }}>
                  <Button
                    variant=\"contained\"
                    startIcon={<AddIcon />}
                    href=\"/analyses/new\"
                  >
                    New Analysis
                  </Button>
                  <Button
                    variant=\"outlined\"
                    startIcon={<CloudUploadIcon />}
                    href=\"/upload\"
                  >
                    Upload Files
                  </Button>
                  <Button
                    variant=\"outlined\"
                    startIcon={<AssessmentIcon />}
                    href=\"/results\"
                  >
                    View Results
                  </Button>
                </Box>
              </CardContent>
            </Card>
          </Grid>
          
          <Grid item xs={12} md={6}>
            <Card>
              <CardContent>
                <Typography variant=\"h3\" gutterBottom>
                  System Status
                </Typography>
                <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
                  <Box sx={{ display: 'flex', justifyContent: 'space-between' }}>
                    <Typography variant=\"body2\">API Status</Typography>
                    <Chip label=\"Healthy\" color=\"success\" size=\"small\" />
                  </Box>
                  <Box sx={{ display: 'flex', justifyContent: 'space-between' }}>
                    <Typography variant=\"body2\">OrcaFlex License</Typography>
                    <Chip label=\"Available\" color=\"success\" size=\"small\" />
                  </Box>
                  <Box sx={{ display: 'flex', justifyContent: 'space-between' }}>
                    <Typography variant=\"body2\">Storage</Typography>
                    <Chip label=\"85% Free\" color=\"info\" size=\"small\" />
                  </Box>
                </Box>
              </CardContent>
            </Card>
          </Grid>
        </Grid>

        {/* Recent Analyses */}
        <Grid container spacing={3}>
          <Grid item xs={12} lg={8}>
            <Card>
              <CardContent>
                <Typography variant=\"h3\" gutterBottom>
                  Recent Analyses
                </Typography>
                {analysesLoading ? (
                  <LinearProgress />
                ) : (
                  <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
                    {analyses.slice(0, 5).map((analysis) => (
                      <Box
                        key={analysis.id}
                        sx={{
                          display: 'flex',
                          justifyContent: 'space-between',
                          alignItems: 'center',
                          p: 2,
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1,
                        }}
                      >
                        <Box>
                          <Typography variant=\"body1\" fontWeight=\"medium\">
                            {analysis.name}
                          </Typography>
                          <Typography variant=\"body2\" color=\"text.secondary\">
                            {analysis.analysis_type} • {new Date(analysis.created_at).toLocaleDateString()}
                          </Typography>
                        </Box>
                        <Chip
                          label={analysis.status}
                          color={getStatusColor(analysis.status)}
                          size=\"small\"
                        />
                      </Box>
                    ))}
                    {analyses.length === 0 && (
                      <Typography variant=\"body2\" color=\"text.secondary\" sx={{ textAlign: 'center', py: 4 }}>
                        No analyses found. Create your first analysis to get started.
                      </Typography>
                    )}
                  </Box>
                )}
              </CardContent>
            </Card>
          </Grid>

          <Grid item xs={12} lg={4}>
            <Card>
              <CardContent>
                <Typography variant=\"h3\" gutterBottom>
                  Recent Results
                </Typography>
                {resultsLoading ? (
                  <LinearProgress />
                ) : (
                  <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
                    {results.slice(0, 5).map((result) => (
                      <Box
                        key={result.result_id}
                        sx={{
                          p: 2,
                          border: '1px solid',
                          borderColor: 'divider',
                          borderRadius: 1,
                        }}
                      >
                        <Typography variant=\"body2\" fontWeight=\"medium\">
                          {result.name}
                        </Typography>
                        <Typography variant=\"caption\" color=\"text.secondary\">
                          {new Date(result.created_at).toLocaleDateString()}
                        </Typography>
                      </Box>
                    ))}
                    {results.length === 0 && (
                      <Typography variant=\"body2\" color=\"text.secondary\" sx={{ textAlign: 'center', py: 2 }}>
                        No results available
                      </Typography>
                    )}
                  </Box>
                )}
              </CardContent>
            </Card>
          </Grid>
        </Grid>
      </Container>
    </DashboardLayout>
  );
};

export default Dashboard;