/**
 * Statistical Analysis Dashboard Component
 * 
 * Provides comprehensive statistical analysis visualization including:
 * - Correlation matrices with heatmaps
 * - Regression analysis with confidence intervals
 * - Distribution analysis and normality tests
 * - Trend analysis with forecasting
 * - Statistical summary reports
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
  Divider,
  Tooltip,
  IconButton
} from '@mui/material';
import {
  TrendingUp,
  Assessment,
  ShowChart,
  Download,
  Refresh,
  Info,
  Warning,
  CheckCircle
} from '@mui/icons-material';
import {
  ResponsiveContainer,
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip as RechartsTooltip,
  ScatterChart,
  Scatter,
  Cell,
  Heatmap,
  BarChart,
  Bar,
  Area,
  AreaChart
} from 'recharts';
import { StatisticalAnalysisService } from '../../services/statisticalAnalysisService';

// Types
interface CorrelationMatrix {
  correlation_matrix: number[][];
  p_value_matrix: number[][];
  significance_matrix: boolean[][];
  column_names: string[];
  method: string;
  n_samples: number;
  confidence_level: number;
}

interface RegressionResult {
  slope: number;
  intercept: number;
  r_squared: number;
  p_value: number;
  std_error: number;
  confidence_intervals: {
    slope: [number, number];
    intercept: [number, number];
  };
  predicted_values: number[];
  residuals: number[];
}

interface DistributionAnalysis {
  basic_statistics: {
    mean: number;
    median: number;
    std_dev: number;
    variance: number;
    skewness: number;
    kurtosis: number;
    min: number;
    max: number;
    q25: number;
    q75: number;
    n_samples: number;
  };
  normality_tests: {
    [testName: string]: {
      statistic: number;
      p_value: number;
      is_normal: boolean;
    };
  };
  recommended_analysis: string;
}

interface TrendAnalysis {
  method: string;
  slope: number;
  intercept?: number;
  r_squared: number;
  p_value?: number;
  trend_strength?: number;
  trend_direction: string;
  predicted_values: number[];
  residuals?: number[];
}

interface StatisticalData {
  correlationMatrix?: CorrelationMatrix;
  regressionResults?: { [key: string]: RegressionResult };
  distributionAnalysis?: { [key: string]: DistributionAnalysis };
  trendAnalysis?: { [key: string]: TrendAnalysis };
  isLoading: boolean;
  error?: string;
}

interface Props {
  data: any[];
  selectedColumns: string[];
  onAnalysisComplete?: (results: any) => void;
}

const StatisticalDashboard: React.FC<Props> = ({
  data,
  selectedColumns,
  onAnalysisComplete
}) => {
  const [analysisData, setAnalysisData] = useState<StatisticalData>({
    isLoading: false
  });
  const [selectedMethod, setSelectedMethod] = useState<string>('pearson');
  const [confidenceLevel, setConfidenceLevel] = useState<number>(0.95);
  const [selectedVariable1, setSelectedVariable1] = useState<string>('');
  const [selectedVariable2, setSelectedVariable2] = useState<string>('');
  const [timeColumn, setTimeColumn] = useState<string>('');

  const statisticalService = useMemo(() => new StatisticalAnalysisService(), []);

  const numericColumns = useMemo(() => {
    if (!data || data.length === 0) return [];
    
    const firstRow = data[0];
    return Object.keys(firstRow).filter(key => 
      typeof firstRow[key] === 'number' && selectedColumns.includes(key)
    );
  }, [data, selectedColumns]);

  useEffect(() => {
    if (numericColumns.length > 0) {
      setSelectedVariable1(numericColumns[0]);
      setSelectedVariable2(numericColumns[1] || numericColumns[0]);
    }
  }, [numericColumns]);

  const performStatisticalAnalysis = async () => {
    if (!data || data.length === 0 || numericColumns.length === 0) {
      return;
    }

    setAnalysisData({ isLoading: true });

    try {
      // Convert data to DataFrame-like structure
      const dataFrame = data.map(row => {
        const filteredRow: any = {};
        numericColumns.forEach(col => {
          filteredRow[col] = row[col];
        });
        return filteredRow;
      });

      // Perform correlation analysis
      const correlationResult = await statisticalService.calculateCorrelationMatrix(
        dataFrame,
        selectedMethod,
        confidenceLevel
      );

      // Perform regression analysis for selected variables
      const regressionResults: { [key: string]: RegressionResult } = {};
      if (selectedVariable1 !== selectedVariable2) {
        const xData = data.map(row => row[selectedVariable1]).filter(val => !isNaN(val));
        const yData = data.map(row => row[selectedVariable2]).filter(val => !isNaN(val));
        
        if (xData.length === yData.length && xData.length > 2) {
          const regressionResult = await statisticalService.performLinearRegression(
            xData,
            yData,
            confidenceLevel
          );
          regressionResults[`${selectedVariable1}_vs_${selectedVariable2}`] = regressionResult;
        }
      }

      // Perform distribution analysis for each variable
      const distributionAnalysis: { [key: string]: DistributionAnalysis } = {};
      for (const column of numericColumns) {
        const columnData = data.map(row => row[column]).filter(val => !isNaN(val));
        if (columnData.length > 0) {
          const distResult = await statisticalService.analyzeDistribution(columnData);
          distributionAnalysis[column] = distResult;
        }
      }

      // Perform trend analysis if time column is selected
      let trendAnalysis: { [key: string]: TrendAnalysis } = {};
      if (timeColumn) {
        for (const column of numericColumns) {
          if (column !== timeColumn) {
            const trendResult = await statisticalService.performTrendAnalysis(
              data,
              timeColumn,
              column
            );
            trendAnalysis[column] = trendResult;
          }
        }
      }

      setAnalysisData({
        correlationMatrix: correlationResult,
        regressionResults,
        distributionAnalysis,
        trendAnalysis,
        isLoading: false
      });

      if (onAnalysisComplete) {
        onAnalysisComplete({
          correlationMatrix: correlationResult,
          regressionResults,
          distributionAnalysis,
          trendAnalysis
        });
      }

    } catch (error) {
      console.error('Statistical analysis error:', error);
      setAnalysisData({
        isLoading: false,
        error: error instanceof Error ? error.message : 'Analysis failed'
      });
    }
  };

  const renderCorrelationMatrix = () => {
    if (!analysisData.correlationMatrix) return null;

    const { correlation_matrix, column_names, significance_matrix } = analysisData.correlationMatrix;
    
    // Convert to heatmap data
    const heatmapData = correlation_matrix.map((row, i) =>
      row.map((value, j) => ({
        x: column_names[j],
        y: column_names[i],
        value: value,
        significant: significance_matrix[i][j]
      }))
    ).flat();

    return (
      <Card>
        <CardHeader 
          title="Correlation Matrix" 
          action={
            <Tooltip title="Correlation Analysis Information">
              <IconButton>
                <Info />
              </IconButton>
            </Tooltip>
          }
        />
        <CardContent>
          <Box sx={{ mb: 2 }}>
            <Typography variant="body2" color="text.secondary">
              Method: {analysisData.correlationMatrix.method} | 
              Confidence Level: {(analysisData.correlationMatrix.confidence_level * 100).toFixed(0)}% |
              Samples: {analysisData.correlationMatrix.n_samples}
            </Typography>
          </Box>
          
          {/* Custom heatmap visualization would go here */}
          <Box sx={{ height: 300, display: 'flex', alignItems: 'center', justifyContent: 'center' }}>
            <Typography color="text.secondary">
              Correlation Heatmap Visualization
              <br />
              (Custom heatmap component would be implemented here)
            </Typography>
          </Box>
          
          {/* Correlation summary table */}
          <Box sx={{ mt: 2 }}>
            <Typography variant="h6" gutterBottom>Strong Correlations</Typography>
            {correlation_matrix.map((row, i) => 
              row.map((value, j) => {
                if (i < j && Math.abs(value) > 0.5) {
                  return (
                    <Chip
                      key={`${i}-${j}`}
                      label={`${column_names[i]} ↔ ${column_names[j]}: ${value.toFixed(3)}`}
                      color={value > 0 ? 'primary' : 'secondary'}
                      size="small"
                      sx={{ m: 0.5 }}
                    />
                  );
                }
                return null;
              })
            )}
          </Box>
        </CardContent>
      </Card>
    );
  };

  const renderRegressionAnalysis = () => {
    if (!analysisData.regressionResults) return null;

    const regressionKey = Object.keys(analysisData.regressionResults)[0];
    const regression = analysisData.regressionResults[regressionKey];

    if (!regression) return null;

    // Create scatter plot data with regression line
    const scatterData = data.map((row, index) => ({
      x: row[selectedVariable1],
      y: row[selectedVariable2],
      predicted: regression.predicted_values[index] || null
    })).filter(point => point.x !== undefined && point.y !== undefined);

    return (
      <Card>
        <CardHeader 
          title={`Regression Analysis: ${selectedVariable1} vs ${selectedVariable2}`}
          action={
            <Chip 
              label={`R² = ${regression.r_squared.toFixed(3)}`} 
              color={regression.r_squared > 0.7 ? 'success' : regression.r_squared > 0.5 ? 'warning' : 'error'}
            />
          }
        />
        <CardContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={8}>
              <ResponsiveContainer width="100%" height={300}>
                <ScatterChart data={scatterData}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis 
                    dataKey="x" 
                    name={selectedVariable1}
                    label={{ value: selectedVariable1, position: 'insideBottom', offset: -10 }}
                  />
                  <YAxis 
                    dataKey="y" 
                    name={selectedVariable2}
                    label={{ value: selectedVariable2, angle: -90, position: 'insideLeft' }}
                  />
                  <RechartsTooltip 
                    formatter={(value, name) => [value?.toFixed(3), name]}
                  />
                  <Scatter dataKey="y" fill="#8884d8" />
                  {/* Regression line would be added here */}
                </ScatterChart>
              </ResponsiveContainer>
            </Grid>
            
            <Grid item xs={12} md={4}>
              <Typography variant="h6" gutterBottom>Statistics</Typography>
              <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
                <Typography variant="body2">
                  <strong>Equation:</strong> y = {regression.slope.toFixed(3)}x + {regression.intercept.toFixed(3)}
                </Typography>
                <Typography variant="body2">
                  <strong>R-squared:</strong> {regression.r_squared.toFixed(3)}
                </Typography>
                <Typography variant="body2">
                  <strong>P-value:</strong> {regression.p_value.toFixed(6)}
                </Typography>
                <Typography variant="body2">
                  <strong>Std Error:</strong> {regression.std_error.toFixed(3)}
                </Typography>
                
                {regression.p_value < 0.05 ? (
                  <Alert severity="success" size="small">
                    Statistically significant relationship (p &lt; 0.05)
                  </Alert>
                ) : (
                  <Alert severity="warning" size="small">
                    Relationship not statistically significant (p ≥ 0.05)
                  </Alert>
                )}
              </Box>
            </Grid>
          </Grid>
        </CardContent>
      </Card>
    );
  };

  const renderDistributionAnalysis = () => {
    if (!analysisData.distributionAnalysis) return null;

    return (
      <Card>
        <CardHeader title="Distribution Analysis" />
        <CardContent>
          <Grid container spacing={2}>
            {Object.entries(analysisData.distributionAnalysis).map(([variable, analysis]) => (
              <Grid item xs={12} md={6} key={variable}>
                <Paper sx={{ p: 2 }}>
                  <Typography variant="h6" gutterBottom>{variable}</Typography>
                  
                  <Grid container spacing={1}>
                    <Grid item xs={6}>
                      <Typography variant="body2">
                        <strong>Mean:</strong> {analysis.basic_statistics.mean.toFixed(3)}
                      </Typography>
                      <Typography variant="body2">
                        <strong>Median:</strong> {analysis.basic_statistics.median.toFixed(3)}
                      </Typography>
                      <Typography variant="body2">
                        <strong>Std Dev:</strong> {analysis.basic_statistics.std_dev.toFixed(3)}
                      </Typography>
                      <Typography variant="body2">
                        <strong>Skewness:</strong> {analysis.basic_statistics.skewness.toFixed(3)}
                      </Typography>
                    </Grid>
                    
                    <Grid item xs={6}>
                      <Typography variant="body2">
                        <strong>Min:</strong> {analysis.basic_statistics.min.toFixed(3)}
                      </Typography>
                      <Typography variant="body2">
                        <strong>Max:</strong> {analysis.basic_statistics.max.toFixed(3)}
                      </Typography>
                      <Typography variant="body2">
                        <strong>Q1:</strong> {analysis.basic_statistics.q25.toFixed(3)}
                      </Typography>
                      <Typography variant="body2">
                        <strong>Q3:</strong> {analysis.basic_statistics.q75.toFixed(3)}
                      </Typography>
                    </Grid>
                  </Grid>
                  
                  <Divider sx={{ my: 1 }} />
                  
                  <Typography variant="subtitle2" gutterBottom>Normality Tests</Typography>
                  {Object.entries(analysis.normality_tests).map(([testName, testResult]) => (
                    <Box key={testName} sx={{ display: 'flex', alignItems: 'center', gap: 1, mb: 0.5 }}>
                      {testResult.is_normal ? (
                        <CheckCircle color="success" fontSize="small" />
                      ) : (
                        <Warning color="warning" fontSize="small" />
                      )}
                      <Typography variant="body2">
                        {testName}: p = {testResult.p_value.toFixed(4)}
                      </Typography>
                    </Box>
                  ))}
                  
                  <Alert 
                    severity={analysis.recommended_analysis === 'parametric' ? 'success' : 'info'}
                    size="small"
                    sx={{ mt: 1 }}
                  >
                    Recommended: {analysis.recommended_analysis} methods
                  </Alert>
                </Paper>
              </Grid>
            ))}
          </Grid>
        </CardContent>
      </Card>
    );
  };

  const renderTrendAnalysis = () => {
    if (!analysisData.trendAnalysis || Object.keys(analysisData.trendAnalysis).length === 0) {
      return null;
    }

    return (
      <Card>
        <CardHeader title="Trend Analysis" />
        <CardContent>
          <Grid container spacing={2}>
            {Object.entries(analysisData.trendAnalysis).map(([variable, trend]) => (
              <Grid item xs={12} md={6} key={variable}>
                <Paper sx={{ p: 2 }}>
                  <Typography variant="h6" gutterBottom>{variable}</Typography>
                  
                  {/* Trend visualization would go here */}
                  <ResponsiveContainer width="100%" height={200}>
                    <LineChart data={data}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey={timeColumn} />
                      <YAxis />
                      <RechartsTooltip />
                      <Line 
                        type="monotone" 
                        dataKey={variable} 
                        stroke="#8884d8" 
                        strokeWidth={2}
                        dot={false}
                      />
                      {/* Trend line would be overlaid here */}
                    </LineChart>
                  </ResponsiveContainer>
                  
                  <Box sx={{ mt: 1 }}>
                    <Typography variant="body2">
                      <strong>Direction:</strong> {trend.trend_direction}
                    </Typography>
                    <Typography variant="body2">
                      <strong>Slope:</strong> {trend.slope.toFixed(4)}
                    </Typography>
                    <Typography variant="body2">
                      <strong>R-squared:</strong> {trend.r_squared.toFixed(3)}
                    </Typography>
                    {trend.p_value && (
                      <Typography variant="body2">
                        <strong>P-value:</strong> {trend.p_value.toFixed(6)}
                      </Typography>
                    )}
                  </Box>
                </Paper>
              </Grid>
            ))}
          </Grid>
        </CardContent>
      </Card>
    );
  };

  const exportResults = () => {
    if (!analysisData) return;
    
    const results = {
      timestamp: new Date().toISOString(),
      correlation_analysis: analysisData.correlationMatrix,
      regression_analysis: analysisData.regressionResults,
      distribution_analysis: analysisData.distributionAnalysis,
      trend_analysis: analysisData.trendAnalysis
    };
    
    const blob = new Blob([JSON.stringify(results, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `statistical_analysis_${new Date().toISOString().slice(0, 10)}.json`;
    a.click();
    URL.revokeObjectURL(url);
  };

  return (
    <Box sx={{ p: 2 }}>
      <Paper sx={{ p: 2, mb: 2 }}>
        <Typography variant="h4" gutterBottom>
          Statistical Analysis Dashboard
        </Typography>
        
        {/* Control Panel */}
        <Grid container spacing={2} sx={{ mb: 2 }}>
          <Grid item xs={12} md={3}>
            <FormControl fullWidth size="small">
              <InputLabel>Correlation Method</InputLabel>
              <Select
                value={selectedMethod}
                onChange={(e) => setSelectedMethod(e.target.value)}
                label="Correlation Method"
              >
                <MenuItem value="pearson">Pearson</MenuItem>
                <MenuItem value="spearman">Spearman</MenuItem>
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
          
          <Grid item xs={12} md={2}>
            <FormControl fullWidth size="small">
              <InputLabel>Variable 1</InputLabel>
              <Select
                value={selectedVariable1}
                onChange={(e) => setSelectedVariable1(e.target.value)}
                label="Variable 1"
              >
                {numericColumns.map(col => (
                  <MenuItem key={col} value={col}>{col}</MenuItem>
                ))}
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={2}>
            <FormControl fullWidth size="small">
              <InputLabel>Variable 2</InputLabel>
              <Select
                value={selectedVariable2}
                onChange={(e) => setSelectedVariable2(e.target.value)}
                label="Variable 2"
              >
                {numericColumns.map(col => (
                  <MenuItem key={col} value={col}>{col}</MenuItem>
                ))}
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={2}>
            <FormControl fullWidth size="small">
              <InputLabel>Time Column (Optional)</InputLabel>
              <Select
                value={timeColumn}
                onChange={(e) => setTimeColumn(e.target.value)}
                label="Time Column"
              >
                <MenuItem value="">None</MenuItem>
                {selectedColumns.map(col => (
                  <MenuItem key={col} value={col}>{col}</MenuItem>
                ))}
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={1}>
            <Button
              variant="contained"
              onClick={performStatisticalAnalysis}
              disabled={analysisData.isLoading || numericColumns.length === 0}
              startIcon={analysisData.isLoading ? <CircularProgress size={16} /> : <Assessment />}
              fullWidth
            >
              {analysisData.isLoading ? 'Analyzing...' : 'Analyze'}
            </Button>
          </Grid>
        </Grid>
        
        {/* Action Buttons */}
        <Box sx={{ display: 'flex', gap: 1, mb: 2 }}>
          <Button
            variant="outlined"
            startIcon={<Refresh />}
            onClick={performStatisticalAnalysis}
            disabled={analysisData.isLoading}
          >
            Refresh Analysis
          </Button>
          <Button
            variant="outlined"
            startIcon={<Download />}
            onClick={exportResults}
            disabled={!analysisData.correlationMatrix}
          >
            Export Results
          </Button>
        </Box>
      </Paper>

      {/* Error Display */}
      {analysisData.error && (
        <Alert severity="error" sx={{ mb: 2 }}>
          {analysisData.error}
        </Alert>
      )}

      {/* Analysis Results */}
      <Grid container spacing={2}>
        <Grid item xs={12}>
          {renderCorrelationMatrix()}
        </Grid>
        
        <Grid item xs={12}>
          {renderRegressionAnalysis()}
        </Grid>
        
        <Grid item xs={12}>
          {renderDistributionAnalysis()}
        </Grid>
        
        {timeColumn && (
          <Grid item xs={12}>
            {renderTrendAnalysis()}
          </Grid>
        )}
      </Grid>
    </Box>
  );
};

export default StatisticalDashboard;