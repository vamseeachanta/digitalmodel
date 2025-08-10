import React, { useEffect, useState } from 'react';
import { Grid, Card, CardContent, Typography, Paper, Box } from '@mui/material';
import { dataAPI } from '@services/api';

const Dashboard: React.FC = () => {
  const [stats, setStats] = useState({
    totalCases: 0,
    totalComponents: 0,
    lastUpdate: new Date().toLocaleString(),
  });

  useEffect(() => {
    loadDashboardData();
  }, []);

  const loadDashboardData = async () => {
    try {
      const casesResponse = await dataAPI.getCases();
      const componentsResponse = await dataAPI.getComponents();
      
      setStats({
        totalCases: casesResponse.data.length,
        totalComponents: componentsResponse.data.length,
        lastUpdate: new Date().toLocaleString(),
      });
    } catch (error) {
      console.error('Failed to load dashboard data:', error);
    }
  };

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        OrcaFlex Analysis Dashboard
      </Typography>
      
      <Grid container spacing={3}>
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="textSecondary" gutterBottom>
                Total Cases
              </Typography>
              <Typography variant="h3">
                {stats.totalCases}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="textSecondary" gutterBottom>
                Components
              </Typography>
              <Typography variant="h3">
                {stats.totalComponents}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="textSecondary" gutterBottom>
                Active Analyses
              </Typography>
              <Typography variant="h3">
                0
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Typography color="textSecondary" gutterBottom>
                Last Update
              </Typography>
              <Typography variant="body1">
                {stats.lastUpdate}
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>
      
      <Paper sx={{ mt: 3, p: 3 }}>
        <Typography variant="h5" gutterBottom>
          Quick Start
        </Typography>
        <Typography variant="body1">
          Welcome to the OrcaFlex Dashboard. Use the navigation menu to:
        </Typography>
        <ul>
          <li>View polar plots of structural responses</li>
          <li>Analyze time trace data</li>
          <li>Compare multiple analysis cases</li>
          <li>Generate professional reports</li>
        </ul>
      </Paper>
    </Box>
  );
};

export default Dashboard;