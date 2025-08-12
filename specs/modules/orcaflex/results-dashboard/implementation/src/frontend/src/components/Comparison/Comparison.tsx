import React, { useState } from 'react';
import {
  Box,
  Grid,
  Paper,
  Typography,
  Button,
  Card,
  CardContent,
  Chip,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  SelectChangeEvent,
  Alert
} from '@mui/material';
import { CompareArrows, Add, Remove } from '@mui/icons-material';
import PolarPlot from '@components/PolarPlot/PolarPlot';
import TimeTrace from '@components/TimeTrace/TimeTrace';
import { dataAPI } from '@services/api';

interface ComparisonCase {
  id: string;
  name: string;
  data: any;
  color: string;
}

const Comparison: React.FC = () => {
  const [cases, setCases] = useState<string[]>([]);
  const [selectedCases, setSelectedCases] = useState<ComparisonCase[]>([]);
  const [comparisonType, setComparisonType] = useState<'polar' | 'timeseries'>('polar');
  const [loading, setLoading] = useState(false);
  const [availableCases, setAvailableCases] = useState<string[]>([]);

  const colors = ['#2196F3', '#4CAF50', '#FF9800', '#F44336', '#9C27B0'];

  React.useEffect(() => {
    loadAvailableCases();
  }, []);

  const loadAvailableCases = async () => {
    try {
      const response = await dataAPI.getCases();
      setAvailableCases(response.data);
    } catch (error) {
      console.error('Failed to load cases:', error);
    }
  };

  const handleAddCase = async (caseName: string) => {
    if (selectedCases.length >= 4) {
      return;
    }

    setLoading(true);
    try {
      // Load data for the case
      const components = await dataAPI.getComponents(caseName);
      const component = components.data[0]; // Use first component for demo

      let data;
      if (comparisonType === 'polar') {
        const response = await dataAPI.getPolarData({
          case: caseName,
          component: component,
          include_statistics: true
        });
        data = response.data;
      } else {
        const response = await dataAPI.getTimeTrace({
          case: caseName,
          component: component,
          heading: 0,
          include_statistics: true
        });
        data = response.data;
      }

      const newCase: ComparisonCase = {
        id: caseName,
        name: caseName,
        data: data,
        color: colors[selectedCases.length]
      };

      setSelectedCases([...selectedCases, newCase]);
    } catch (error) {
      console.error('Failed to load case data:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleRemoveCase = (caseId: string) => {
    setSelectedCases(selectedCases.filter(c => c.id !== caseId));
  };

  const handleComparisonTypeChange = (event: SelectChangeEvent) => {
    setComparisonType(event.target.value as 'polar' | 'timeseries');
    setSelectedCases([]); // Clear cases when changing type
  };

  const calculateDifferences = () => {
    if (selectedCases.length < 2) return null;

    const baseline = selectedCases[0];
    const differences = selectedCases.slice(1).map(caseItem => {
      const maxDiff = ((caseItem.data.max_value || 0) - (baseline.data.max_value || 0)) / (baseline.data.max_value || 1) * 100;
      const meanDiff = ((caseItem.data.mean_value || 0) - (baseline.data.mean_value || 0)) / (baseline.data.mean_value || 1) * 100;
      
      return {
        name: caseItem.name,
        maxDiff: maxDiff.toFixed(1),
        meanDiff: meanDiff.toFixed(1)
      };
    });

    return { baseline: baseline.name, differences };
  };

  const diffAnalysis = calculateDifferences();

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        Multi-Case Comparison
      </Typography>

      <Paper sx={{ p: 2, mb: 3 }}>
        <Grid container spacing={2} alignItems="center">
          <Grid item xs={12} md={3}>
            <FormControl fullWidth>
              <InputLabel>Comparison Type</InputLabel>
              <Select
                value={comparisonType}
                onChange={handleComparisonTypeChange}
                label="Comparison Type"
              >
                <MenuItem value="polar">Polar Plots</MenuItem>
                <MenuItem value="timeseries">Time Series</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={6}>
            <FormControl fullWidth>
              <InputLabel>Add Case</InputLabel>
              <Select
                value=""
                onChange={(e) => handleAddCase(e.target.value)}
                label="Add Case"
                disabled={selectedCases.length >= 4 || loading}
              >
                {availableCases
                  .filter(c => !selectedCases.find(sc => sc.id === c))
                  .map(caseItem => (
                    <MenuItem key={caseItem} value={caseItem}>
                      {caseItem}
                    </MenuItem>
                  ))}
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={3}>
            <Typography variant="body2" color="textSecondary">
              {selectedCases.length}/4 cases selected
            </Typography>
          </Grid>
        </Grid>

        <Box sx={{ mt: 2 }}>
          {selectedCases.map((caseItem, index) => (
            <Chip
              key={caseItem.id}
              label={caseItem.name}
              onDelete={() => handleRemoveCase(caseItem.id)}
              sx={{ 
                m: 0.5, 
                bgcolor: caseItem.color,
                color: 'white'
              }}
              deleteIcon={<Remove style={{ color: 'white' }} />}
            />
          ))}
        </Box>
      </Paper>

      {selectedCases.length === 0 && (
        <Alert severity="info">
          Select up to 4 cases to compare. Choose a comparison type and add cases from the dropdown.
        </Alert>
      )}

      {selectedCases.length > 0 && (
        <>
          <Grid container spacing={2}>
            {comparisonType === 'polar' && selectedCases.map((caseItem, index) => (
              <Grid item xs={12} md={6} key={caseItem.id}>
                <Card>
                  <CardContent>
                    <Typography variant="h6" gutterBottom>
                      {caseItem.name}
                    </Typography>
                    <PolarPlot 
                      data={{
                        ...caseItem.data,
                        title: caseItem.name
                      }} 
                      width={400} 
                      height={400} 
                    />
                  </CardContent>
                </Card>
              </Grid>
            ))}

            {comparisonType === 'timeseries' && (
              <Grid item xs={12}>
                <Card>
                  <CardContent>
                    <Typography variant="h6" gutterBottom>
                      Time Series Comparison
                    </Typography>
                    {selectedCases.map((caseItem, index) => (
                      <Box key={caseItem.id} sx={{ mb: 2 }}>
                        <Typography variant="subtitle2" color={caseItem.color}>
                          {caseItem.name}
                        </Typography>
                        <TimeTrace 
                          data={{
                            ...caseItem.data,
                            title: ''
                          }} 
                          height={300} 
                          showStatistics={false}
                        />
                      </Box>
                    ))}
                  </CardContent>
                </Card>
              </Grid>
            )}
          </Grid>

          {diffAnalysis && (
            <Paper sx={{ mt: 3, p: 2 }}>
              <Typography variant="h6" gutterBottom>
                <CompareArrows sx={{ verticalAlign: 'middle', mr: 1 }} />
                Difference Analysis
              </Typography>
              <Typography variant="body2" color="textSecondary" gutterBottom>
                Baseline: {diffAnalysis.baseline}
              </Typography>
              <Grid container spacing={2} sx={{ mt: 1 }}>
                {diffAnalysis.differences.map((diff, index) => (
                  <Grid item xs={12} md={4} key={index}>
                    <Card variant="outlined">
                      <CardContent>
                        <Typography variant="subtitle2" gutterBottom>
                          {diff.name}
                        </Typography>
                        <Typography variant="body2">
                          Max Difference: {diff.maxDiff}%
                        </Typography>
                        <Typography variant="body2">
                          Mean Difference: {diff.meanDiff}%
                        </Typography>
                      </CardContent>
                    </Card>
                  </Grid>
                ))}
              </Grid>
            </Paper>
          )}
        </>
      )}
    </Box>
  );
};

export default Comparison;