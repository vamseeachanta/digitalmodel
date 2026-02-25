/**
 * Anomaly Viewer Component
 * 
 * Provides comprehensive anomaly detection and visualization including:
 * - Statistical outlier detection (IQR, Z-score, Modified Z-score)
 * - Engineering limit violations
 * - Multi-variate anomaly detection
 * - Time series anomaly detection
 * - Anomaly severity classification
 * - Interactive anomaly investigation tools
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
  Tabs,
  Tab,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  Badge,
  IconButton,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  List,
  ListItem,
  ListItemText,
  ListItemIcon,
  Tooltip,
  FormControlLabel,
  Switch,
  Slider
} from '@mui/material';
import {
  Warning,
  Error as ErrorIcon,
  CheckCircle,
  Info,
  ExpandMore,
  Visibility,
  Download,
  Refresh,
  FilterList,
  Timeline,
  ScatterPlot,
  Assessment,
  Security,
  TrendingUp
} from '@mui/icons-material';
import {
  ResponsiveContainer,
  ScatterChart,
  Scatter,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip as RechartsTooltip,
  Cell,
  LineChart,
  Line,
  BarChart,
  Bar,
  Heatmap,
  ReferenceLine
} from 'recharts';
import { AnomalyDetectionService } from '../../services/anomalyDetectionService';

// Types
interface AnomalyResult {
  index: number;
  value: number;
  anomaly_type: string;
  detection_method: string;
  severity: string;
  score: number;
  threshold: number;
  description: string;
  recommendations: string[];
}

interface AnomalyData {
  statistical_outliers?: {
    [parameter: string]: {
      [method: string]: {
        anomalies: AnomalyResult[];
        total_outliers: number;
        outlier_percentage: number;
        method: string;
        threshold_info: any;
      };
    };
  };
  engineering_violations?: {
    violations: AnomalyResult[];
    critical_violations: number;
    warning_violations: number;
    total_violations: number;
    violated_parameters: string[];
  };
  multivariate?: {
    anomalies: AnomalyResult[];
    total_anomalies: number;
    anomaly_percentage: number;
    method: string;
    features_used: string[];
  };
  time_series?: {
    time_series_anomalies: {
      [parameter: string]: {
        anomalies: AnomalyResult[];
        total_anomalies: number;
        anomaly_percentage: number;
      };
    };
  };
  summary?: {
    total_anomalies: number;
    critical_anomalies: number;
    high_severity_anomalies: number;
    detection_methods_used: string[];
  };
  isLoading: boolean;
  error?: string;
}

interface Props {
  data: any[];
  timeColumn?: string;
  selectedParameters: string[];
  engineeringLimits?: { [parameter: string]: any };
  onAnomalySelected?: (anomaly: AnomalyResult) => void;
}

const AnomalyViewer: React.FC<Props> = ({
  data,
  timeColumn,
  selectedParameters,
  engineeringLimits,
  onAnomalySelected
}) => {
  const [anomalyData, setAnomalyData] = useState<AnomalyData>({
    isLoading: false
  });
  const [activeTab, setActiveTab] = useState<number>(0);
  const [detectionMethods, setDetectionMethods] = useState<string[]>([
    'iqr', 'z_score', 'engineering_limits', 'multivariate'
  ]);
  const [selectedSeverityFilter, setSelectedSeverityFilter] = useState<string>('all');
  const [selectedMethodFilter, setSelectedMethodFilter] = useState<string>('all');
  const [thresholdFactor, setThresholdFactor] = useState<number>(1.5);
  const [selectedAnomalyDetails, setSelectedAnomalyDetails] = useState<AnomalyResult | null>(null);
  const [showTimeSeriesAnomalies, setShowTimeSeriesAnomalies] = useState<boolean>(false);
  const [contaminationRate, setContaminationRate] = useState<number>(0.1);

  const anomalyService = useMemo(() => new AnomalyDetectionService(), []);

  const numericParameters = useMemo(() => {
    if (!data || data.length === 0) return [];
    
    const firstRow = data[0];
    return Object.keys(firstRow).filter(key => 
      typeof firstRow[key] === 'number' && 
      selectedParameters.includes(key) &&
      key !== timeColumn
    );
  }, [data, selectedParameters, timeColumn]);

  const severityColors = {
    critical: '#d32f2f',
    high: '#f57c00',
    medium: '#1976d2',
    low: '#388e3c'
  };

  const severityIcons = {
    critical: <ErrorIcon />,
    high: <Warning />,
    medium: <Info />,
    low: <CheckCircle />
  };

  const performAnomalyDetection = async () => {
    if (!data || data.length === 0 || numericParameters.length === 0) {
      return;
    }

    setAnomalyData({ isLoading: true });

    try {
      // Add time series detection if time column is available
      const methods = [...detectionMethods];
      if (timeColumn && showTimeSeriesAnomalies) {
        methods.push('time_series');
      }

      // Perform comprehensive anomaly detection
      const results = await anomalyService.comprehensiveAnomalyScan(
        data,
        timeColumn,
        engineeringLimits,
        methods
      );

      setAnomalyData({
        ...results.results,
        summary: results.summary,
        isLoading: false
      });

    } catch (error) {
      console.error('Anomaly detection error:', error);
      setAnomalyData({
        isLoading: false,
        error: error instanceof Error ? error.message : 'Detection failed'
      });
    }
  };

  const getAllAnomalies = (): AnomalyResult[] => {
    const allAnomalies: AnomalyResult[] = [];

    // Statistical outliers
    if (anomalyData.statistical_outliers) {
      Object.values(anomalyData.statistical_outliers).forEach(paramData => {
        Object.values(paramData).forEach(methodData => {
          allAnomalies.push(...methodData.anomalies);
        });
      });
    }

    // Engineering violations
    if (anomalyData.engineering_violations) {
      allAnomalies.push(...anomalyData.engineering_violations.violations);
    }

    // Multivariate anomalies
    if (anomalyData.multivariate) {
      allAnomalies.push(...anomalyData.multivariate.anomalies);
    }

    // Time series anomalies
    if (anomalyData.time_series) {
      Object.values(anomalyData.time_series.time_series_anomalies).forEach(paramData => {
        allAnomalies.push(...paramData.anomalies);
      });
    }

    return allAnomalies;
  };

  const getFilteredAnomalies = (): AnomalyResult[] => {
    let anomalies = getAllAnomalies();

    // Filter by severity
    if (selectedSeverityFilter !== 'all') {
      anomalies = anomalies.filter(a => a.severity === selectedSeverityFilter);
    }

    // Filter by method
    if (selectedMethodFilter !== 'all') {
      anomalies = anomalies.filter(a => a.detection_method === selectedMethodFilter);
    }

    return anomalies.sort((a, b) => b.score - a.score); // Sort by severity score
  };

  const renderAnomalySummary = () => {
    if (!anomalyData.summary) return null;

    const { summary } = anomalyData;
    const allAnomalies = getAllAnomalies();
    const severityCounts = {
      critical: allAnomalies.filter(a => a.severity === 'critical').length,
      high: allAnomalies.filter(a => a.severity === 'high').length,
      medium: allAnomalies.filter(a => a.severity === 'medium').length,
      low: allAnomalies.filter(a => a.severity === 'low').length
    };

    return (
      <Card sx={{ mb: 2 }}>
        <CardHeader 
          title="Anomaly Detection Summary"
          action={
            <Badge badgeContent={summary.total_anomalies} color="error" max={999}>
              <Assessment />
            </Badge>
          }
        />
        <CardContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={3}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h4" color="error.main">
                  {summary.total_anomalies}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Total Anomalies
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12} md={2}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h5" sx={{ color: severityColors.critical }}>
                  {severityCounts.critical}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Critical
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12} md={2}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h5" sx={{ color: severityColors.high }}>
                  {severityCounts.high}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  High
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12} md={2}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h5" sx={{ color: severityColors.medium }}>
                  {severityCounts.medium}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Medium
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12} md={2}>
              <Box sx={{ textAlign: 'center' }}>
                <Typography variant="h5" sx={{ color: severityColors.low }}>
                  {severityCounts.low}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Low
                </Typography>
              </Box>
            </Grid>
            
            <Grid item xs={12}>
              <Box sx={{ display: 'flex', gap: 1, flexWrap: 'wrap', mt: 1 }}>
                <Typography variant="body2" color="text.secondary">
                  Detection Methods Used:
                </Typography>
                {summary.detection_methods_used.map(method => (
                  <Chip key={method} label={method} size="small" variant="outlined" />
                ))}
              </Box>
            </Grid>
          </Grid>
        </CardContent>
      </Card>
    );
  };

  const renderAnomalyScatterPlot = () => {
    const anomalies = getFilteredAnomalies();
    
    if (anomalies.length === 0 || numericParameters.length < 2) return null;

    // Create scatter plot data using first two parameters
    const param1 = numericParameters[0];
    const param2 = numericParameters[1];
    
    const scatterData = data.map((row, index) => {
      const anomaly = anomalies.find(a => a.index === index);
      return {
        x: row[param1],
        y: row[param2],
        index,
        isAnomaly: !!anomaly,
        severity: anomaly?.severity || 'normal',
        anomalyScore: anomaly?.score || 0
      };
    }).filter(point => !isNaN(point.x) && !isNaN(point.y));

    return (
      <Card sx={{ mb: 2 }}>
        <CardHeader title={`Anomaly Scatter Plot: ${param1} vs ${param2}`} />
        <CardContent>
          <ResponsiveContainer width="100%" height={400}>
            <ScatterChart data={scatterData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="x" 
                name={param1}
                label={{ value: param1, position: 'insideBottom', offset: -10 }}
              />
              <YAxis 
                dataKey="y" 
                name={param2}
                label={{ value: param2, angle: -90, position: 'insideLeft' }}
              />
              <RechartsTooltip 
                formatter={(value: any, name: string) => [
                  typeof value === 'number' ? value.toFixed(3) : value,
                  name
                ]}
                labelFormatter={(label, payload) => {
                  if (payload && payload[0]) {
                    const point = payload[0].payload;
                    return `Index: ${point.index}${point.isAnomaly ? ` (${point.severity} anomaly)` : ''}`;
                  }
                  return label;
                }}
              />
              
              {/* Normal points */}
              <Scatter 
                dataKey="y" 
                fill="#8884d8"
                data={scatterData.filter(d => !d.isAnomaly)}
                name="Normal"
              />
              
              {/* Anomaly points by severity */}
              {Object.keys(severityColors).map(severity => (
                <Scatter
                  key={severity}
                  dataKey="y"
                  fill={severityColors[severity as keyof typeof severityColors]}
                  data={scatterData.filter(d => d.severity === severity)}
                  name={`${severity.charAt(0).toUpperCase() + severity.slice(1)} Anomaly`}
                />
              ))}
            </ScatterChart>
          </ResponsiveContainer>
        </CardContent>
      </Card>
    );
  };

  const renderAnomalyTable = () => {
    const anomalies = getFilteredAnomalies();
    
    if (anomalies.length === 0) {
      return (
        <Alert severity="success">
          No anomalies detected with current filters.
        </Alert>
      );
    }

    return (
      <TableContainer component={Paper}>
        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>Index</TableCell>
              <TableCell>Value</TableCell>
              <TableCell>Type</TableCell>
              <TableCell>Method</TableCell>
              <TableCell align="center">Severity</TableCell>
              <TableCell align="right">Score</TableCell>
              <TableCell>Description</TableCell>
              <TableCell align="center">Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {anomalies.slice(0, 100).map((anomaly, idx) => ( // Limit to 100 for performance
              <TableRow 
                key={idx}
                sx={{ 
                  backgroundColor: anomaly.severity === 'critical' ? 'rgba(211, 47, 47, 0.1)' :
                                 anomaly.severity === 'high' ? 'rgba(245, 124, 0, 0.1)' : 'inherit'
                }}
              >
                <TableCell>{anomaly.index}</TableCell>
                <TableCell>{anomaly.value.toFixed(3)}</TableCell>
                <TableCell>
                  <Chip label={anomaly.anomaly_type} size="small" variant="outlined" />
                </TableCell>
                <TableCell>
                  <Chip label={anomaly.detection_method} size="small" />
                </TableCell>
                <TableCell align="center">
                  <Chip
                    icon={severityIcons[anomaly.severity as keyof typeof severityIcons]}
                    label={anomaly.severity}
                    size="small"
                    sx={{ 
                      backgroundColor: severityColors[anomaly.severity as keyof typeof severityColors],
                      color: 'white'
                    }}
                  />
                </TableCell>
                <TableCell align="right">{anomaly.score.toFixed(3)}</TableCell>
                <TableCell>
                  <Tooltip title={anomaly.description}>
                    <Typography variant="body2" noWrap sx={{ maxWidth: 200 }}>
                      {anomaly.description}
                    </Typography>
                  </Tooltip>
                </TableCell>
                <TableCell align="center">
                  <IconButton
                    size="small"
                    onClick={() => setSelectedAnomalyDetails(anomaly)}
                  >
                    <Visibility />
                  </IconButton>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
    );
  };

  const renderTimeSeriesAnomalies = () => {
    if (!anomalyData.time_series || !timeColumn) return null;

    return (
      <Box>
        {Object.entries(anomalyData.time_series.time_series_anomalies).map(([parameter, paramData]) => (
          <Card key={parameter} sx={{ mb: 2 }}>
            <CardHeader 
              title={`Time Series Anomalies: ${parameter}`}
              action={
                <Chip 
                  label={`${paramData.total_anomalies} anomalies (${paramData.anomaly_percentage.toFixed(1)}%)`}
                  color={paramData.total_anomalies > 0 ? 'warning' : 'success'}
                />
              }
            />
            <CardContent>
              {/* Time series chart with anomalies highlighted */}
              <ResponsiveContainer width="100%" height={300}>
                <LineChart data={data}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey={timeColumn} />
                  <YAxis />
                  <RechartsTooltip />
                  <Line 
                    type="monotone" 
                    dataKey={parameter} 
                    stroke="#8884d8" 
                    strokeWidth={2}
                    dot={false}
                  />
                  
                  {/* Add reference lines for anomaly points */}
                  {paramData.anomalies.map((anomaly, idx) => (
                    <ReferenceLine
                      key={idx}
                      x={data[anomaly.index]?.[timeColumn]}
                      stroke={severityColors[anomaly.severity as keyof typeof severityColors]}
                      strokeWidth={2}
                      strokeDasharray="5 5"
                    />
                  ))}
                </LineChart>
              </ResponsiveContainer>
              
              {/* Anomaly details for this parameter */}
              {paramData.anomalies.length > 0 && (
                <Box sx={{ mt: 2 }}>
                  <Typography variant="subtitle2" gutterBottom>Detected Anomalies:</Typography>
                  {paramData.anomalies.slice(0, 5).map((anomaly, idx) => (
                    <Alert key={idx} severity="warning" sx={{ mb: 1 }}>
                      <Typography variant="body2">
                        <strong>Index {anomaly.index}:</strong> {anomaly.description} 
                        (Score: {anomaly.score.toFixed(3)}, Value: {anomaly.value.toFixed(3)})
                      </Typography>
                    </Alert>
                  ))}
                  {paramData.anomalies.length > 5 && (
                    <Typography variant="body2" color="text.secondary">
                      ... and {paramData.anomalies.length - 5} more anomalies
                    </Typography>
                  )}
                </Box>
              )}
            </CardContent>
          </Card>
        ))}
      </Box>
    );
  };

  const renderAnomalyDetailsDialog = () => {
    if (!selectedAnomalyDetails) return null;

    return (
      <Dialog
        open={!!selectedAnomalyDetails}
        onClose={() => setSelectedAnomalyDetails(null)}
        maxWidth="md"
        fullWidth
      >
        <DialogTitle>
          <Box sx={{ display: 'flex', alignItems: 'center', gap: 2 }}>
            {severityIcons[selectedAnomalyDetails.severity as keyof typeof severityIcons]}
            Anomaly Details - Index {selectedAnomalyDetails.index}
          </Box>
        </DialogTitle>
        <DialogContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={6}>
              <Typography variant="h6" gutterBottom>Basic Information</Typography>
              <Typography><strong>Value:</strong> {selectedAnomalyDetails.value.toFixed(6)}</Typography>
              <Typography><strong>Anomaly Type:</strong> {selectedAnomalyDetails.anomaly_type}</Typography>
              <Typography><strong>Detection Method:</strong> {selectedAnomalyDetails.detection_method}</Typography>
              <Typography><strong>Severity:</strong> {selectedAnomalyDetails.severity}</Typography>
              <Typography><strong>Anomaly Score:</strong> {selectedAnomalyDetails.score.toFixed(6)}</Typography>
              <Typography><strong>Threshold:</strong> {selectedAnomalyDetails.threshold.toFixed(6)}</Typography>
            </Grid>
            
            <Grid item xs={12} md={6}>
              <Typography variant="h6" gutterBottom>Context Data</Typography>
              {data[selectedAnomalyDetails.index] && (
                <Box>
                  {Object.entries(data[selectedAnomalyDetails.index]).map(([key, value]) => (
                    <Typography key={key} variant="body2">
                      <strong>{key}:</strong> {typeof value === 'number' ? value.toFixed(3) : String(value)}
                    </Typography>
                  ))}
                </Box>
              )}
            </Grid>
            
            <Grid item xs={12}>
              <Typography variant="h6" gutterBottom>Description</Typography>
              <Alert severity="info">
                {selectedAnomalyDetails.description}
              </Alert>
            </Grid>
            
            <Grid item xs={12}>
              <Typography variant="h6" gutterBottom>Recommendations</Typography>
              <List>
                {selectedAnomalyDetails.recommendations.map((rec, idx) => (
                  <ListItem key={idx}>
                    <ListItemIcon>
                      <Info color="primary" />
                    </ListItemIcon>
                    <ListItemText primary={rec} />
                  </ListItem>
                ))}
              </List>
            </Grid>
          </Grid>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setSelectedAnomalyDetails(null)}>Close</Button>
          {onAnomalySelected && (
            <Button 
              variant="contained" 
              onClick={() => {
                onAnomalySelected(selectedAnomalyDetails);
                setSelectedAnomalyDetails(null);
              }}
            >
              Investigate Further
            </Button>
          )}
        </DialogActions>
      </Dialog>
    );
  };

  const exportResults = () => {
    const results = {
      timestamp: new Date().toISOString(),
      anomaly_detection_results: anomalyData,
      parameters: {
        detection_methods: detectionMethods,
        threshold_factor: thresholdFactor,
        contamination_rate: contaminationRate,
        time_column: timeColumn,
        selected_parameters: selectedParameters
      },
      all_anomalies: getAllAnomalies()
    };
    
    const blob = new Blob([JSON.stringify(results, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `anomaly_detection_${new Date().toISOString().slice(0, 10)}.json`;
    a.click();
    URL.revokeObjectURL(url);
  };

  return (
    <Box sx={{ p: 2 }}>
      <Paper sx={{ p: 2, mb: 2 }}>
        <Typography variant="h4" gutterBottom>
          Anomaly Detection & Analysis
        </Typography>
        
        {/* Control Panel */}
        <Grid container spacing={2} sx={{ mb: 2 }}>
          <Grid item xs={12} md={3}>
            <FormControl fullWidth size="small">
              <InputLabel>Detection Methods</InputLabel>
              <Select
                multiple
                value={detectionMethods}
                onChange={(e) => setDetectionMethods(e.target.value as string[])}
                label="Detection Methods"
                renderValue={(selected) => (
                  <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                    {selected.map((value) => (
                      <Chip key={value} label={value} size="small" />
                    ))}
                  </Box>
                )}
              >
                <MenuItem value="iqr">IQR Method</MenuItem>
                <MenuItem value="z_score">Z-Score</MenuItem>
                <MenuItem value="modified_z_score">Modified Z-Score</MenuItem>
                <MenuItem value="engineering_limits">Engineering Limits</MenuItem>
                <MenuItem value="multivariate">Multivariate</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={2}>
            <Typography variant="body2" gutterBottom>Threshold Factor</Typography>
            <Slider
              value={thresholdFactor}
              onChange={(_, value) => setThresholdFactor(value as number)}
              min={1.0}
              max={3.0}
              step={0.1}
              valueLabelDisplay="auto"
              size="small"
            />
          </Grid>
          
          <Grid item xs={12} md={2}>
            <Typography variant="body2" gutterBottom>Contamination Rate</Typography>
            <Slider
              value={contaminationRate}
              onChange={(_, value) => setContaminationRate(value as number)}
              min={0.01}
              max={0.5}
              step={0.01}
              valueLabelDisplay="auto"
              size="small"
            />
          </Grid>
          
          <Grid item xs={12} md={2}>
            <FormControlLabel
              control={
                <Switch
                  checked={showTimeSeriesAnomalies}
                  onChange={(e) => setShowTimeSeriesAnomalies(e.target.checked)}
                  disabled={!timeColumn}
                />
              }
              label="Time Series Detection"
            />
          </Grid>
          
          <Grid item xs={12} md={3}>
            <Button
              variant="contained"
              onClick={performAnomalyDetection}
              disabled={anomalyData.isLoading || numericParameters.length === 0}
              startIcon={anomalyData.isLoading ? <CircularProgress size={16} /> : <Security />}
              fullWidth
            >
              {anomalyData.isLoading ? 'Detecting...' : 'Detect Anomalies'}
            </Button>
          </Grid>
        </Grid>
        
        {/* Filter Controls */}
        <Grid container spacing={2} sx={{ mb: 2 }}>
          <Grid item xs={12} md={3}>
            <FormControl fullWidth size="small">
              <InputLabel>Severity Filter</InputLabel>
              <Select
                value={selectedSeverityFilter}
                onChange={(e) => setSelectedSeverityFilter(e.target.value)}
                label="Severity Filter"
              >
                <MenuItem value="all">All Severities</MenuItem>
                <MenuItem value="critical">Critical Only</MenuItem>
                <MenuItem value="high">High Only</MenuItem>
                <MenuItem value="medium">Medium Only</MenuItem>
                <MenuItem value="low">Low Only</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={3}>
            <FormControl fullWidth size="small">
              <InputLabel>Method Filter</InputLabel>
              <Select
                value={selectedMethodFilter}
                onChange={(e) => setSelectedMethodFilter(e.target.value)}
                label="Method Filter"
              >
                <MenuItem value="all">All Methods</MenuItem>
                <MenuItem value="iqr">IQR Only</MenuItem>
                <MenuItem value="z_score">Z-Score Only</MenuItem>
                <MenuItem value="engineering_limits">Engineering Limits Only</MenuItem>
                <MenuItem value="multivariate">Multivariate Only</MenuItem>
              </Select>
            </FormControl>
          </Grid>
          
          <Grid item xs={12} md={3}>
            <Button
              variant="outlined"
              startIcon={<Download />}
              onClick={exportResults}
              disabled={!anomalyData.summary}
            >
              Export Results
            </Button>
          </Grid>
          
          <Grid item xs={12} md={3}>
            <Button
              variant="outlined"
              startIcon={<Refresh />}
              onClick={performAnomalyDetection}
              disabled={anomalyData.isLoading}
            >
              Refresh Analysis
            </Button>
          </Grid>
        </Grid>
      </Paper>

      {/* Error Display */}
      {anomalyData.error && (
        <Alert severity="error" sx={{ mb: 2 }}>
          {anomalyData.error}
        </Alert>
      )}

      {/* Results */}
      {anomalyData.summary && (
        <>
          {renderAnomalySummary()}
          
          <Paper sx={{ mb: 2 }}>
            <Tabs 
              value={activeTab} 
              onChange={(_, value) => setActiveTab(value)}
              variant="scrollable"
              scrollButtons="auto"
            >
              <Tab label="Overview" icon={<Assessment />} />
              <Tab label="Scatter Plot" icon={<ScatterPlot />} />
              <Tab label="Anomaly List" icon={<FilterList />} />
              {timeColumn && showTimeSeriesAnomalies && (
                <Tab label="Time Series" icon={<Timeline />} />
              )}
            </Tabs>
            
            <Box sx={{ p: 2 }}>
              {activeTab === 0 && (
                <Box>
                  <Typography variant="h6" gutterBottom>Detection Overview</Typography>
                  <Grid container spacing={2}>
                    {/* Summary cards for each detection method */}
                    {anomalyData.statistical_outliers && (
                      <Grid item xs={12} md={6}>
                        <Card>
                          <CardHeader title="Statistical Outliers" />
                          <CardContent>
                            {Object.entries(anomalyData.statistical_outliers).map(([param, methods]) => (
                              <Accordion key={param}>
                                <AccordionSummary expandIcon={<ExpandMore />}>
                                  <Typography>{param}</Typography>
                                </AccordionSummary>
                                <AccordionDetails>
                                  {Object.entries(methods).map(([method, data]) => (
                                    <Box key={method} sx={{ mb: 1 }}>
                                      <Typography variant="body2">
                                        <strong>{method}:</strong> {data.total_outliers} outliers 
                                        ({data.outlier_percentage.toFixed(1)}%)
                                      </Typography>
                                    </Box>
                                  ))}
                                </AccordionDetails>
                              </Accordion>
                            ))}
                          </CardContent>
                        </Card>
                      </Grid>
                    )}
                    
                    {anomalyData.engineering_violations && (
                      <Grid item xs={12} md={6}>
                        <Card>
                          <CardHeader title="Engineering Violations" />
                          <CardContent>
                            <Typography variant="body2">
                              <strong>Critical Violations:</strong> {anomalyData.engineering_violations.critical_violations}
                            </Typography>
                            <Typography variant="body2">
                              <strong>Warning Violations:</strong> {anomalyData.engineering_violations.warning_violations}
                            </Typography>
                            <Typography variant="body2">
                              <strong>Parameters Violated:</strong> {anomalyData.engineering_violations.violated_parameters.join(', ')}
                            </Typography>
                          </CardContent>
                        </Card>
                      </Grid>
                    )}
                  </Grid>
                </Box>
              )}
              
              {activeTab === 1 && renderAnomalyScatterPlot()}
              {activeTab === 2 && renderAnomalyTable()}
              {activeTab === 3 && renderTimeSeriesAnomalies()}
            </Box>
          </Paper>
        </>
      )}
      
      {renderAnomalyDetailsDialog()}
    </Box>
  );
};

export default AnomalyViewer;