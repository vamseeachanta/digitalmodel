/**
 * Trend Analysis Component
 * 
 * Provides comprehensive trend analysis and forecasting including:
 * - Time series trend detection
 * - Linear and polynomial trend fitting
 * - Moving average analysis
 * - Seasonal decomposition
 * - Trend forecasting with confidence intervals
 * - Change point detection
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
  Slider,
  TextField,
  FormControlLabel,
  Switch,
  Divider,
  Tooltip,
  IconButton
} from '@mui/material';
import {
  TrendingUp,
  TrendingDown,
  Timeline,
  ShowChart,
  Download,
  Refresh,
  Info,
  Settings,
  PredictionsIcon,
  Analytics
} from '@mui/icons-material';
import {
  ResponsiveContainer,
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip as RechartsTooltip,
  Area,
  AreaChart,
  ComposedChart,
  Bar,
  ReferenceLine,
  Brush,
  Legend
} from 'recharts';
import { StatisticalAnalysisService } from '../../services/statisticalAnalysisService';

// Types
interface TrendResult {
  method: string;
  slope: number;
  intercept?: number;
  r_squared: number;
  p_value?: number;
  trend_strength?: number;
  trend_direction: string;
  predicted_values: number[];
  residuals?: number[];
  confidence_intervals?: {
    lower: number[];
    upper: number[];
  };
}

interface ForecastResult {
  forecasted_values: number[];
  confidence_lower: number[];
  confidence_upper: number[];
  forecast_periods: number;
  forecast_method: string;
}

interface ChangePoint {
  index: number;
  timestamp: any;
  significance: number;
  before_trend: number;
  after_trend: number;
  change_magnitude: number;
}

interface TrendData {
  trends: { [metric: string]: TrendResult };
  forecasts?: { [metric: string]: ForecastResult };
  changePoints?: { [metric: string]: ChangePoint[] };
  seasonality?: { [metric: string]: any };
  isLoading: boolean;
  error?: string;
}

interface Props {
  data: any[];
  timeColumn: string;
  selectedMetrics: string[];
  onTrendAnalysisComplete?: (results: any) => void;
}

const TrendAnalysis: React.FC<Props> = ({
  data,
  timeColumn,
  selectedMetrics,
  onTrendAnalysisComplete
}) => {
  const [trendData, setTrendData] = useState<TrendData>({
    trends: {},
    isLoading: false
  });
  const [trendMethod, setTrendMethod] = useState<string>('linear');
  const [movingAverageWindow, setMovingAverageWindow] = useState<number>(10);
  const [confidenceLevel, setConfidenceLevel] = useState<number>(0.95);
  const [forecastPeriods, setForecastPeriods] = useState<number>(10);
  const [selectedMetric, setSelectedMetric] = useState<string>('');
  const [showConfidenceIntervals, setShowConfidenceIntervals] = useState<boolean>(true);
  const [showForecasts, setShowForecasts] = useState<boolean>(false);
  const [enableChangePointDetection, setEnableChangePointDetection] = useState<boolean>(false);
  const [detrend, setDetrend] = useState<boolean>(false);

  const statisticalService = useMemo(() => new StatisticalAnalysisService(), []);

  const numericMetrics = useMemo(() => {
    if (!data || data.length === 0) return [];
    
    const firstRow = data[0];
    return Object.keys(firstRow).filter(key => 
      typeof firstRow[key] === 'number' && 
      selectedMetrics.includes(key) &&
      key !== timeColumn
    );
  }, [data, selectedMetrics, timeColumn]);

  useEffect(() => {
    if (numericMetrics.length > 0) {
      setSelectedMetric(numericMetrics[0]);
    }
  }, [numericMetrics]);

  const performTrendAnalysis = async () => {
    if (!data || data.length === 0 || !timeColumn || numericMetrics.length === 0) {
      return;
    }

    setTrendData({ trends: {}, isLoading: true });

    try {
      const trends: { [metric: string]: TrendResult } = {};
      const forecasts: { [metric: string]: ForecastResult } = {};
      const changePoints: { [metric: string]: ChangePoint[] } = {};

      for (const metric of numericMetrics) {
        // Perform trend analysis
        const trendResult = await statisticalService.performTrendAnalysis(
          data,
          timeColumn,
          metric,
          trendMethod
        );
        trends[metric] = trendResult;

        // Perform forecasting if enabled
        if (showForecasts) {
          const forecast = await performForecasting(metric, trendResult);
          if (forecast) {
            forecasts[metric] = forecast;
          }
        }

        // Detect change points if enabled
        if (enableChangePointDetection) {
          const changePointsResult = await detectChangePoints(metric);
          if (changePointsResult.length > 0) {
            changePoints[metric] = changePointsResult;
          }
        }
      }

      setTrendData({
        trends,
        forecasts: showForecasts ? forecasts : undefined,
        changePoints: enableChangePointDetection ? changePoints : undefined,
        isLoading: false
      });

      if (onTrendAnalysisComplete) {
        onTrendAnalysisComplete({
          trends,
          forecasts,
          changePoints
        });
      }

    } catch (error) {
      console.error('Trend analysis error:', error);
      setTrendData({
        trends: {},
        isLoading: false,
        error: error instanceof Error ? error.message : 'Analysis failed'
      });
    }
  };

  const performForecasting = async (metric: string, trendResult: TrendResult): Promise<ForecastResult | null> => {
    try {
      // Extract time series data
      const timeSeriesData = data.map((row, index) => ({
        index,
        timestamp: row[timeColumn],
        value: row[metric]
      })).filter(point => !isNaN(point.value));

      if (timeSeriesData.length < 10) return null;

      // Simple linear extrapolation for forecasting
      const lastIndex = timeSeriesData.length - 1;
      const forecastedValues: number[] = [];
      const confidenceLower: number[] = [];
      const confidenceUpper: number[] = [];

      const residualStd = trendResult.residuals ? 
        Math.sqrt(trendResult.residuals.reduce((sum, r) => sum + r * r, 0) / trendResult.residuals.length) : 
        0.1; // Default standard error

      for (let i = 1; i <= forecastPeriods; i++) {
        const forecastIndex = lastIndex + i;
        let forecastValue;

        if (trendMethod === 'linear' && trendResult.intercept !== undefined) {
          forecastValue = trendResult.slope * forecastIndex + trendResult.intercept;
        } else {
          // Use last known trend
          const lastValue = timeSeriesData[lastIndex].value;
          forecastValue = lastValue + (trendResult.slope * i);
        }

        // Calculate confidence intervals (simplified)
        const errorMargin = 1.96 * residualStd * Math.sqrt(1 + i / timeSeriesData.length); // Increases with forecast distance
        
        forecastedValues.push(forecastValue);
        confidenceLower.push(forecastValue - errorMargin);
        confidenceUpper.push(forecastValue + errorMargin);
      }

      return {
        forecasted_values: forecastedValues,
        confidence_lower: confidenceLower,
        confidence_upper: confidenceUpper,
        forecast_periods: forecastPeriods,
        forecast_method: trendMethod
      };

    } catch (error) {
      console.error(`Forecasting error for ${metric}:`, error);
      return null;
    }
  };

  const detectChangePoints = async (metric: string): Promise<ChangePoint[]> => {
    try {
      const values = data.map(row => row[metric]).filter(val => !isNaN(val));
      
      if (values.length < 20) return [];

      const changePoints: ChangePoint[] = [];
      const windowSize = Math.max(5, Math.floor(values.length / 10));

      // Simple change point detection using sliding window variance
      for (let i = windowSize; i < values.length - windowSize; i++) {
        const beforeWindow = values.slice(i - windowSize, i);
        const afterWindow = values.slice(i, i + windowSize);
        
        const beforeMean = beforeWindow.reduce((sum, val) => sum + val, 0) / beforeWindow.length;
        const afterMean = afterWindow.reduce((sum, val) => sum + val, 0) / afterWindow.length;
        
        const beforeVar = beforeWindow.reduce((sum, val) => sum + Math.pow(val - beforeMean, 2), 0) / beforeWindow.length;
        const afterVar = afterWindow.reduce((sum, val) => sum + Math.pow(val - afterMean, 2), 0) / afterWindow.length;
        
        // Calculate change significance
        const meanChange = Math.abs(afterMean - beforeMean);
        const varChange = Math.abs(afterVar - beforeVar);
        const significance = meanChange / (Math.sqrt(beforeVar + afterVar) || 1);
        
        // Threshold for significant change
        if (significance > 1.5) {
          changePoints.push({
            index: i,
            timestamp: data[i][timeColumn],
            significance,
            before_trend: beforeMean,
            after_trend: afterMean,
            change_magnitude: afterMean - beforeMean
          });
        }
      }

      // Sort by significance and return top change points
      return changePoints
        .sort((a, b) => b.significance - a.significance)
        .slice(0, 5);

    } catch (error) {
      console.error(`Change point detection error for ${metric}:`, error);
      return [];
    }
  };

  const prepareChartData = (metric: string) => {
    const trend = trendData.trends[metric];
    const forecast = trendData.forecasts?.[metric];
    
    if (!trend) return [];

    // Base time series data
    const baseData = data.map((row, index) => ({
      index,
      timestamp: row[timeColumn],
      actual: row[metric],
      trend: trend.predicted_values[index],
      residual: trend.residuals?.[index] || 0
    })).filter(point => !isNaN(point.actual));

    // Add forecast data if available
    if (forecast && showForecasts) {
      const lastIndex = baseData.length - 1;
      const lastTimestamp = baseData[lastIndex].timestamp;
      
      for (let i = 0; i < forecast.forecasted_values.length; i++) {
        // Estimate future timestamps (this would depend on your time format)
        const futureTimestamp = typeof lastTimestamp === 'number' ? 
          lastTimestamp + (i + 1) : 
          new Date(new Date(lastTimestamp).getTime() + (i + 1) * 24 * 60 * 60 * 1000);

        baseData.push({
          index: lastIndex + i + 1,
          timestamp: futureTimestamp,
          actual: null,
          trend: null,
          forecast: forecast.forecasted_values[i],
          forecastLower: forecast.confidence_lower[i],
          forecastUpper: forecast.confidence_upper[i],
          residual: 0
        });
      }
    }

    return baseData;
  };

  const renderTrendChart = (metric: string) => {
    const chartData = prepareChartData(metric);
    const trend = trendData.trends[metric];
    
    if (!trend || chartData.length === 0) return null;

    const changePointsForMetric = trendData.changePoints?.[metric] || [];

    return (
      <Card sx={{ mb: 2 }}>
        <CardHeader 
          title={`Trend Analysis: ${metric}`}
          action={
            <Box sx={{ display: 'flex', gap: 1 }}>
              <Chip 
                label={trend.trend_direction}
                color={trend.trend_direction === 'increasing' ? 'success' : 'warning'}
                size="small"
                icon={trend.trend_direction === 'increasing' ? <TrendingUp /> : <TrendingDown />}
              />
              <Chip 
                label={`R² = ${trend.r_squared.toFixed(3)}`}
                color={trend.r_squared > 0.7 ? 'success' : trend.r_squared > 0.5 ? 'warning' : 'error'}
                size="small"
              />
            </Box>
          }
        />
        <CardContent>
          <ResponsiveContainer width="100%" height={400}>
            <ComposedChart data={chartData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="timestamp" 
                type="category"
                scale="point"
              />
              <YAxis />
              <RechartsTooltip 
                formatter={(value: any, name: string) => [
                  value !== null && typeof value === 'number' ? value.toFixed(3) : 'N/A',
                  name
                ]}
                labelFormatter={(label) => `Time: ${label}`}
              />
              <Legend />
              
              {/* Actual values */}
              <Line 
                type="monotone" 
                dataKey="actual" 
                stroke="#8884d8" 
                strokeWidth={2}
                dot={{ r: 2 }}
                name="Actual"
                connectNulls={false}
              />
              
              {/* Trend line */}
              <Line 
                type="monotone" 
                dataKey="trend" 
                stroke="#82ca9d" 
                strokeWidth={2}
                strokeDasharray="5 5"
                dot={false}
                name="Trend"
                connectNulls={false}
              />
              
              {/* Forecast */}
              {showForecasts && (
                <>
                  <Line 
                    type="monotone" 
                    dataKey="forecast" 
                    stroke="#ff7c7c" 
                    strokeWidth={2}
                    strokeDasharray="10 5"
                    dot={{ r: 3 }}
                    name="Forecast"
                    connectNulls={false}
                  />
                  
                  {showConfidenceIntervals && (
                    <>
                      <Area
                        type="monotone"
                        dataKey="forecastUpper"
                        stroke="none"
                        fill="#ff7c7c"
                        fillOpacity={0.2}
                        name="Forecast CI Upper"
                      />
                      <Area
                        type="monotone"
                        dataKey="forecastLower"
                        stroke="none"
                        fill="#ff7c7c"
                        fillOpacity={0.2}
                        name="Forecast CI Lower"
                      />
                    </>
                  )}
                </>
              )}
              
              {/* Change points */}
              {changePointsForMetric.map((changePoint, index) => (
                <ReferenceLine
                  key={index}
                  x={changePoint.timestamp}
                  stroke="red"
                  strokeDasharray="2 2"
                  label={`Change Point ${index + 1}`}
                />
              ))}
              
              <Brush dataKey="timestamp" height={30} />
            </ComposedChart>
          </ResponsiveContainer>
          
          {/* Trend Statistics */}
          <Box sx={{ mt: 2, display: 'flex', gap: 2, flexWrap: 'wrap' }}>
            <Typography variant="body2">
              <strong>Slope:</strong> {trend.slope.toFixed(6)}
            </Typography>
            {trend.intercept !== undefined && (
              <Typography variant="body2">
                <strong>Intercept:</strong> {trend.intercept.toFixed(3)}
              </Typography>
            )}
            <Typography variant="body2">
              <strong>R-squared:</strong> {trend.r_squared.toFixed(3)}
            </Typography>
            {trend.p_value !== undefined && (
              <Typography variant="body2">
                <strong>P-value:</strong> {trend.p_value.toFixed(6)}
              </Typography>
            )}
            {trend.trend_strength !== undefined && (
              <Typography variant="body2">
                <strong>Trend Strength:</strong> {trend.trend_strength.toFixed(3)}
              </Typography>
            )}
          </Box>
          
          {/* Change Points Summary */}
          {changePointsForMetric.length > 0 && (
            <Box sx={{ mt: 2 }}>
              <Typography variant="subtitle2" gutterBottom>Detected Change Points:</Typography>
              {changePointsForMetric.map((changePoint, index) => (
                <Alert key={index} severity="info" sx={{ mb: 1 }}>
                  <Typography variant="body2">
                    <strong>Point {index + 1}:</strong> Significant change detected at {changePoint.timestamp} 
                    (Change: {changePoint.change_magnitude.toFixed(3)}, Significance: {changePoint.significance.toFixed(2)})
                  </Typography>
                </Alert>
              ))}
            </Box>
          )}
        </CardContent>
      </Card>
    );
  };

  const renderResidualAnalysis = (metric: string) => {
    const trend = trendData.trends[metric];
    
    if (!trend || !trend.residuals) return null;

    const residualData = trend.residuals.map((residual, index) => ({
      index,
      residual,
      absResidual: Math.abs(residual)
    }));

    return (
      <Card sx={{ mb: 2 }}>
        <CardHeader title={`Residual Analysis: ${metric}`} />
        <CardContent>
          <ResponsiveContainer width="100%" height={250}>
            <ComposedChart data={residualData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="index" />
              <YAxis />
              <RechartsTooltip />
              <Bar dataKey="residual" fill="#8884d8" opacity={0.6} />
              <Line type="monotone" dataKey="absResidual" stroke="#ff7c7c" strokeWidth={2} />
              <ReferenceLine y={0} stroke="red" strokeDasharray="2 2" />
            </ComposedChart>
          </ResponsiveContainer>
          
          <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>
            Residuals show the difference between actual and predicted values. 
            Patterns in residuals may indicate model inadequacy or non-linear trends.
          </Typography>
        </CardContent>
      </Card>
    );
  };

  const exportResults = () => {
    const results = {
      timestamp: new Date().toISOString(),
      trend_analysis: trendData,
      parameters: {
        method: trendMethod,
        moving_average_window: movingAverageWindow,
        confidence_level: confidenceLevel,
        forecast_periods: forecastPeriods,
        time_column: timeColumn,
        metrics: numericMetrics
      }
    };
    
    const blob = new Blob([JSON.stringify(results, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `trend_analysis_${new Date().toISOString().slice(0, 10)}.json`;
    a.click();
    URL.revokeObjectURL(url);
  };

  return (
    <Box sx={{ p: 2 }}>
      <Paper sx={{ p: 2, mb: 2 }}>
        <Typography variant="h4" gutterBottom>
          Trend Analysis & Forecasting
        </Typography>
        
        {/* Control Panel */}
        <Grid container spacing={2} sx={{ mb: 2 }}>
          <Grid item xs={12} md={3}>
            <FormControl fullWidth size="small">
              <InputLabel>Trend Method</InputLabel>
              <Select
                value={trendMethod}
                onChange={(e) => setTrendMethod(e.target.value)}
                label="Trend Method"
              >
                <MenuItem value="linear">Linear</MenuItem>
                <MenuItem value="moving_average">Moving Average</MenuItem>
                <MenuItem value="polynomial">Polynomial</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={2}>
            <TextField
              fullWidth
              size="small"
              label="MA Window"
              type="number"
              value={movingAverageWindow}
              onChange={(e) => setMovingAverageWindow(parseInt(e.target.value) || 10)}
              disabled={trendMethod !== 'moving_average'}
            />
          </Grid>
          
          <Grid item xs={12} md={2}>
            <TextField
              fullWidth
              size="small"
              label="Forecast Periods"
              type="number"
              value={forecastPeriods}
              onChange={(e) => setForecastPeriods(parseInt(e.target.value) || 10)}
              disabled={!showForecasts}
            />
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
            <Button
              variant="contained"
              onClick={performTrendAnalysis}
              disabled={trendData.isLoading || numericMetrics.length === 0}
              startIcon={trendData.isLoading ? <CircularProgress size={16} /> : <Timeline />}
              fullWidth
            >
              {trendData.isLoading ? 'Analyzing...' : 'Analyze Trends'}
            </Button>
          </Grid>
        </Grid>
        
        {/* Options */}
        <Box sx={{ display: 'flex', gap: 2, flexWrap: 'wrap', mb: 2 }}>
          <FormControlLabel
            control={
              <Switch
                checked={showForecasts}
                onChange={(e) => setShowForecasts(e.target.checked)}
              />
            }
            label="Enable Forecasting"
          />
          
          <FormControlLabel
            control={
              <Switch
                checked={showConfidenceIntervals}
                onChange={(e) => setShowConfidenceIntervals(e.target.checked)}
                disabled={!showForecasts}
              />
            }
            label="Show Confidence Intervals"
          />
          
          <FormControlLabel
            control={
              <Switch
                checked={enableChangePointDetection}
                onChange={(e) => setEnableChangePointDetection(e.target.checked)}
              />
            }
            label="Detect Change Points"
          />
          
          <Button
            variant="outlined"
            startIcon={<Download />}
            onClick={exportResults}
            disabled={Object.keys(trendData.trends).length === 0}
          >
            Export Results
          </Button>
        </Box>
      </Paper>

      {/* Error Display */}
      {trendData.error && (
        <Alert severity="error" sx={{ mb: 2 }}>
          {trendData.error}
        </Alert>
      )}

      {/* Results */}
      {Object.keys(trendData.trends).length > 0 && (
        <Grid container spacing={2}>
          {numericMetrics.map(metric => (
            <Grid item xs={12} key={metric}>
              {renderTrendChart(metric)}
              {renderResidualAnalysis(metric)}
            </Grid>
          ))}
        </Grid>
      )}
    </Box>
  );
};

export default TrendAnalysis;