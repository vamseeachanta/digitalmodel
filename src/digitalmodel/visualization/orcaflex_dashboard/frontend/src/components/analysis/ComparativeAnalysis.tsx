/**
 * Comparative Analysis Component
 * 
 * Provides side-by-side comparison capabilities including:
 * - Multi-case loading condition comparisons
 * - Statistical significance testing
 * - Environmental condition sensitivity
 * - Performance benchmarking
 * - Visual comparison charts
 */

import React, { useState, useEffect, useMemo } from 'react';
import {
  Box,
  Paper,
  Typography,
  Grid,
  Card,
  CardContent,
  CardHeader,
  Button,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Chip,
  Alert,
  CircularProgress,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  Tooltip,
  IconButton,
  FormControlLabel,
  Switch,
  Divider
} from '@mui/material';
import {
  Compare,
  TrendingUp,
  Assessment,
  ExpandMore,
  Download,
  Refresh,
  Info,
  CheckCircle,
  Warning,
  Error as ErrorIcon,
  Analytics
} from '@mui/icons-material';
import {
  ResponsiveContainer,
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip as RechartsTooltip,
  LineChart,
  Line,
  ScatterChart,
  Scatter,
  Cell,
  RadarChart,
  PolarGrid,
  PolarAngleAxis,
  PolarRadiusAxis,
  Radar
} from 'recharts';
import { ComparativeAnalysisService } from '../../services/comparativeAnalysisService';

// Types
interface ComparisonResult {
  case_1: string;
  case_2: string;
  metric: string;
  test_statistic: number;
  p_value: number;
  effect_size: number;
  is_significant: boolean;
  confidence_level: number;
  mean_difference: number;
  percent_difference: number;
  recommendation: string;
}

interface GroupComparisonResult {
  groups: string[];
  metric: string;
  test_statistic: number;
  p_value: number;
  is_significant: boolean;
  post_hoc_results: ComparisonResult[];
  effect_size: number;
  confidence_level: number;
}

interface ComparisonData {
  comparison_results?: {
    [metric: string]: {
      pairwise_comparisons: ComparisonResult[];
      group_comparison?: GroupComparisonResult;
      descriptive_statistics: {
        [condition: string]: {
          count: number;
          mean: number;
          median: number;
          std: number;
          min: number;
          max: number;
        };
      };
      recommendations: string[];
    };
  };
  summary?: {
    total_metrics: number;
    metrics_with_significant_differences: number;
    most_variable_metric?: string;
    least_variable_metric?: string;
  };
  metadata?: {
    conditions: string[];
    metrics: string[];
    confidence_level: number;
    test_type: string;
  };
  isLoading: boolean;
  error?: string;
}

interface Props {
  casesData: { [caseName: string]: any[] };
  selectedMetrics: string[];
  onComparisonComplete?: (results: any) => void;
}

const ComparativeAnalysis: React.FC<Props> = ({
  casesData,
  selectedMetrics,
  onComparisonComplete
}) => {
  const [comparisonData, setComparisonData] = useState<ComparisonData>({
    isLoading: false
  });
  const [selectedCases, setSelectedCases] = useState<string[]>([]);
  const [testType, setTestType] = useState<string>('auto');
  const [confidenceLevel, setConfidenceLevel] = useState<number>(0.95);
  const [selectedMetric, setSelectedMetric] = useState<string>('');
  const [showStatisticalDetails, setShowStatisticalDetails] = useState<boolean>(false);
  const [comparisonType, setComparisonType] = useState<string>('loading_conditions');

  const comparativeService = useMemo(() => new ComparativeAnalysisService(), []);

  const availableCases = useMemo(() => Object.keys(casesData), [casesData]);

  const numericMetrics = useMemo(() => {
    if (!casesData || Object.keys(casesData).length === 0) return [];
    
    const firstCase = Object.values(casesData)[0];
    if (!firstCase || firstCase.length === 0) return [];
    
    const firstRow = firstCase[0];
    return Object.keys(firstRow).filter(key => 
      typeof firstRow[key] === 'number' && selectedMetrics.includes(key)
    );
  }, [casesData, selectedMetrics]);

  useEffect(() => {
    if (availableCases.length > 0) {
      setSelectedCases(availableCases.slice(0, Math.min(4, availableCases.length)));
    }
  }, [availableCases]);

  useEffect(() => {
    if (numericMetrics.length > 0) {
      setSelectedMetric(numericMetrics[0]);
    }
  }, [numericMetrics]);

  const performComparativeAnalysis = async () => {
    if (selectedCases.length < 2 || numericMetrics.length === 0) {
      return;
    }

    setComparisonData({ isLoading: true });

    try {
      // Prepare results data for selected cases
      const resultsData: { [caseName: string]: any } = {};
      
      selectedCases.forEach(caseName => {
        if (casesData[caseName]) {
          resultsData[caseName] = casesData[caseName];
        }
      });

      let analysisResults;

      if (comparisonType === 'loading_conditions') {
        analysisResults = await comparativeService.compareLoadingConditions(
          resultsData,
          numericMetrics,
          confidenceLevel,
          testType
        );
      } else if (comparisonType === 'environmental') {
        // For environmental comparison, we need environmental data
        // This would typically come from the case metadata
        const environmentalData: { [caseName: string]: { [param: string]: number } } = {};
        
        selectedCases.forEach(caseName => {
          // Extract environmental parameters from case data or metadata
          // This is a simplified example - in practice, this would come from case configuration
          environmentalData[caseName] = {
            wave_height: Math.random() * 5 + 1,
            wave_period: Math.random() * 5 + 8,
            wind_speed: Math.random() * 20 + 5,
            current_speed: Math.random() * 2 + 0.5
          };
        });

        analysisResults = await comparativeService.compareEnvironmentalConditions(
          environmentalData,
          resultsData,
          numericMetrics
        );
      }

      setComparisonData({
        ...analysisResults,
        isLoading: false
      });

      if (onComparisonComplete) {
        onComparisonComplete(analysisResults);
      }

    } catch (error) {
      console.error('Comparative analysis error:', error);
      setComparisonData({
        isLoading: false,
        error: error instanceof Error ? error.message : 'Analysis failed'
      });
    }
  };

  const renderComparisonSummary = () => {
    if (!comparisonData.summary) return null;

    const { summary, metadata } = comparisonData;

    return (
      <Card sx={{ mb: 2 }}>
        <CardHeader title="Comparison Summary" />
        <CardContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={3}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h4" color="primary">
                  {summary.total_metrics}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Metrics Analyzed
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12} md={3}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h4" color="warning.main">
                  {summary.metrics_with_significant_differences}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Significant Differences
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12} md={3}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h6" color="success.main">
                  {summary.most_variable_metric || 'N/A'}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Most Variable Metric
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12} md={3}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h6" color="info.main">
                  {summary.least_variable_metric || 'N/A'}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Least Variable Metric
                </Typography>
              </Box>
            </Grid>
          </Grid>
          
          {metadata && (
            <Box sx={{ mt: 2 }}>
              <Typography variant="body2" color="text.secondary">
                Conditions: {metadata.conditions?.join(', ')} | 
                Test Type: {metadata.test_type} | 
                Confidence Level: {(metadata.confidence_level * 100).toFixed(0)}%
              </Typography>
            </Box>
          )}
        </CardContent>
      </Card>
    );
  };

  const renderMetricComparison = (metric: string, results: any) => {
    if (!results) return null;

    const { pairwise_comparisons, descriptive_statistics, recommendations } = results;

    // Prepare data for visualization
    const meanData = Object.entries(descriptive_statistics).map(([condition, stats]: [string, any]) => ({
      condition,
      mean: stats.mean,
      std: stats.std,
      median: stats.median,
      min: stats.min,
      max: stats.max
    }));

    const significantComparisons = pairwise_comparisons.filter(
      (comp: ComparisonResult) => comp.is_significant
    );

    return (
      <Accordion key={metric}>
        <AccordionSummary expandIcon={<ExpandMore />}>
          <Box sx={{ display: 'flex', alignItems: 'center', gap: 2, width: '100%' }}>
            <Typography variant="h6">{metric}</Typography>
            <Box sx={{ flexGrow: 1 }} />
            <Chip 
              label={`${significantComparisons.length} significant`}
              color={significantComparisons.length > 0 ? 'warning' : 'success'}
              size="small"
            />
          </Box>
        </AccordionSummary>
        
        <AccordionDetails>
          <Grid container spacing={2}>
            {/* Visual Comparison */}
            <Grid item xs={12} md={8}>
              <Typography variant="subtitle1" gutterBottom>Mean Comparison</Typography>
              <ResponsiveContainer width="100%" height={300}>
                <BarChart data={meanData}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="condition" />
                  <YAxis />
                  <RechartsTooltip 
                    formatter={(value: any, name: string) => [
                      typeof value === 'number' ? value.toFixed(3) : value, 
                      name
                    ]}
                  />
                  <Bar dataKey="mean" fill="#8884d8" />
                </BarChart>
              </ResponsiveContainer>
            </Grid>
            
            {/* Statistical Summary */}
            <Grid item xs={12} md={4}>
              <Typography variant="subtitle1" gutterBottom>Statistical Summary</Typography>
              
              <TableContainer component={Paper} sx={{ maxHeight: 300 }}>
                <Table size="small">
                  <TableHead>
                    <TableRow>
                      <TableCell>Condition</TableCell>
                      <TableCell align="right">Mean</TableCell>
                      <TableCell align="right">Std Dev</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {Object.entries(descriptive_statistics).map(([condition, stats]: [string, any]) => (
                      <TableRow key={condition}>
                        <TableCell component="th" scope="row">
                          {condition}
                        </TableCell>
                        <TableCell align="right">{stats.mean.toFixed(3)}</TableCell>
                        <TableCell align="right">{stats.std.toFixed(3)}</TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </TableContainer>
            </Grid>
            
            {/* Pairwise Comparisons */}
            {showStatisticalDetails && pairwise_comparisons.length > 0 && (
              <Grid item xs={12}>
                <Typography variant="subtitle1" gutterBottom>Pairwise Comparisons</Typography>
                
                <TableContainer component={Paper}>
                  <Table size="small">
                    <TableHead>
                      <TableRow>
                        <TableCell>Comparison</TableCell>
                        <TableCell align="right">Mean Difference</TableCell>
                        <TableCell align="right">% Difference</TableCell>
                        <TableCell align="right">P-value</TableCell>
                        <TableCell align="center">Significant</TableCell>
                        <TableCell>Effect Size</TableCell>
                      </TableRow>
                    </TableHead>
                    <TableBody>
                      {pairwise_comparisons.map((comparison: ComparisonResult, index: number) => (
                        <TableRow key={index}>
                          <TableCell component="th" scope="row">
                            {comparison.case_1} vs {comparison.case_2}
                          </TableCell>
                          <TableCell align="right">
                            {comparison.mean_difference.toFixed(3)}
                          </TableCell>
                          <TableCell align="right">
                            {comparison.percent_difference.toFixed(1)}%
                          </TableCell>
                          <TableCell align="right">
                            {comparison.p_value.toFixed(4)}
                          </TableCell>
                          <TableCell align="center">
                            {comparison.is_significant ? (
                              <CheckCircle color="success" fontSize="small" />
                            ) : (
                              <ErrorIcon color="disabled" fontSize="small" />
                            )}
                          </TableCell>
                          <TableCell>
                            <Chip 
                              label={comparison.effect_size.toFixed(3)}
                              size="small"
                              color={
                                comparison.effect_size > 0.8 ? 'error' :
                                comparison.effect_size > 0.5 ? 'warning' :
                                comparison.effect_size > 0.2 ? 'info' : 'default'
                              }
                            />
                          </TableCell>
                        </TableRow>
                      ))}
                    </TableBody>
                  </Table>
                </TableContainer>
              </Grid>
            )}
            
            {/* Recommendations */}
            {recommendations.length > 0 && (
              <Grid item xs={12}>
                <Typography variant="subtitle1" gutterBottom>Recommendations</Typography>
                {recommendations.map((recommendation: string, index: number) => (
                  <Alert key={index} severity="info" sx={{ mb: 1 }}>
                    {recommendation}
                  </Alert>
                ))}
              </Grid>
            )}
          </Grid>
        </AccordionDetails>
      </Accordion>
    );
  };

  const renderRadarComparison = () => {
    if (!comparisonData.comparison_results || selectedCases.length < 2) return null;

    // Prepare radar chart data
    const radarData = numericMetrics.map(metric => {
      const metricResults = comparisonData.comparison_results![metric];
      if (!metricResults) return null;

      const { descriptive_statistics } = metricResults;
      
      const dataPoint: any = { metric };
      
      selectedCases.forEach(caseName => {
        if (descriptive_statistics[caseName]) {
          // Normalize values for radar chart (0-100 scale)
          const allMeans = Object.values(descriptive_statistics).map((stats: any) => stats.mean);
          const minMean = Math.min(...allMeans);
          const maxMean = Math.max(...allMeans);
          const range = maxMean - minMean;
          
          if (range > 0) {
            dataPoint[caseName] = ((descriptive_statistics[caseName].mean - minMean) / range) * 100;
          } else {
            dataPoint[caseName] = 50; // Default middle value
          }
        }
      });
      
      return dataPoint;
    }).filter(Boolean);

    if (radarData.length === 0) return null;

    const colors = ['#8884d8', '#82ca9d', '#ffc658', '#ff7c7c', '#8dd1e1'];

    return (
      <Card sx={{ mb: 2 }}>
        <CardHeader title="Multi-Metric Radar Comparison" />
        <CardContent>
          <ResponsiveContainer width="100%" height={400}>
            <RadarChart data={radarData}>
              <PolarGrid />
              <PolarAngleAxis dataKey="metric" />
              <PolarRadiusAxis angle={90} domain={[0, 100]} />
              {selectedCases.map((caseName, index) => (
                <Radar
                  key={caseName}
                  name={caseName}
                  dataKey={caseName}
                  stroke={colors[index % colors.length]}
                  fill={colors[index % colors.length]}
                  fillOpacity={0.1}
                  strokeWidth={2}
                />
              ))}
            </RadarChart>
          </ResponsiveContainer>
          
          <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
            Values are normalized to 0-100 scale for comparison. Higher values indicate relatively better performance.
          </Typography>
        </CardContent>
      </Card>
    );
  };

  const exportResults = () => {
    if (!comparisonData.comparison_results) return;
    
    const results = {
      timestamp: new Date().toISOString(),
      comparison_type: comparisonType,
      selected_cases: selectedCases,
      analysis_results: comparisonData
    };
    
    const blob = new Blob([JSON.stringify(results, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `comparative_analysis_${new Date().toISOString().slice(0, 10)}.json`;
    a.click();
    URL.revokeObjectURL(url);
  };

  return (
    <Box sx={{ p: 2 }}>
      <Paper sx={{ p: 2, mb: 2 }}>
        <Typography variant="h4" gutterBottom>
          Comparative Analysis Dashboard
        </Typography>
        
        {/* Control Panel */}
        <Grid container spacing={2} sx={{ mb: 2 }}>
          <Grid item xs={12} md={3}>
            <FormControl fullWidth size="small">
              <InputLabel>Analysis Type</InputLabel>
              <Select
                value={comparisonType}
                onChange={(e) => setComparisonType(e.target.value)}
                label="Analysis Type"
              >
                <MenuItem value="loading_conditions">Loading Conditions</MenuItem>
                <MenuItem value="environmental">Environmental Sensitivity</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={2}>
            <FormControl fullWidth size="small">
              <InputLabel>Test Type</InputLabel>
              <Select
                value={testType}
                onChange={(e) => setTestType(e.target.value)}
                label="Test Type"
              >
                <MenuItem value="auto">Auto</MenuItem>
                <MenuItem value="parametric">Parametric</MenuItem>
                <MenuItem value="non_parametric">Non-parametric</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={2}>
            <FormControl fullWidth size="small">
              <InputLabel>Confidence Level</InputLabel>
              <Select
                value={confidenceLevel}
                onChange={(e) => setConfidenceLevel(e.target.value as number)}
                label="Confidence Level"
              >
                <MenuItem value={0.90}>90%</MenuItem>
                <MenuItem value={0.95}>95%</MenuItem>
                <MenuItem value={0.99}>99%</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={3}>
            <FormControl fullWidth size="small">
              <InputLabel>Cases to Compare</InputLabel>
              <Select
                multiple
                value={selectedCases}
                onChange={(e) => setSelectedCases(e.target.value as string[])}
                label="Cases to Compare"
                renderValue={(selected) => (
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                    {selected.map((value) => (
                      <Chip key={value} label={value} size="small" />
                    ))}
                  </Box>
                )}
              >
                {availableCases.map(caseName => (
                  <MenuItem key={caseName} value={caseName}>{caseName}</MenuItem>
                ))}
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={2}>
            <Button
              variant="contained"
              onClick={performComparativeAnalysis}
              disabled={comparisonData.isLoading || selectedCases.length < 2}
              startIcon={comparisonData.isLoading ? <CircularProgress size={16} /> : <Compare />}
              fullWidth
            >
              {comparisonData.isLoading ? 'Comparing...' : 'Compare'}
            </Button>
          </Grid>
        </Grid>
        
        {/* Options */}
        <Box sx={{ display: 'flex', gap: 2, mb: 2 }}>
          <FormControlLabel
            control={
              <Switch
                checked={showStatisticalDetails}
                onChange={(e) => setShowStatisticalDetails(e.target.checked)}
              />
            }
            label="Show Statistical Details"
          />
          
          <Button
            variant="outlined"
            startIcon={<Download />}
            onClick={exportResults}
            disabled={!comparisonData.comparison_results}
          >
            Export Results
          </Button>
        </Box>
      </Paper>

      {/* Error Display */}
      {comparisonData.error && (
        <Alert severity="error" sx={{ mb: 2 }}>
          {comparisonData.error}
        </Alert>
      )}

      {/* Results */}
      {comparisonData.comparison_results && (
        <>
          {renderComparisonSummary()}
          {renderRadarComparison()}
          
          {/* Individual Metric Comparisons */}
          <Card>
            <CardHeader title="Detailed Metric Comparisons" />
            <CardContent>
              {Object.entries(comparisonData.comparison_results).map(([metric, results]) =>
                renderMetricComparison(metric, results)
              )}
            </CardContent>
          </Card>
        </>
      )}
    </Box>
  );
};

export default ComparativeAnalysis;