import React, { useMemo } from 'react';
import {
  Box,
  Card,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Chip,
  Tooltip,
  IconButton,
  Paper,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  Grid,
} from '@mui/material';
import {
  ExpandMore as ExpandMoreIcon,
  Info as InfoIcon,
  Functions as FunctionsIcon,
} from '@mui/icons-material';
import { useTheme } from '@mui/material/styles';
import { TimeSeriesData, ComponentGroup, StatisticalOverlay, StatisticalSummary } from './types';

interface TimeSeriesStatisticsProps {
  data: TimeSeriesData[];
  selectedComponents: string[];
  componentGroups: ComponentGroup[];
  enabledStatistics: string[];
  onStatisticsChange?: (statistics: StatisticalOverlay[]) => void;
  showFormulas?: boolean;
}

const TimeSeriesStatistics: React.FC<TimeSeriesStatisticsProps> = ({
  data,
  selectedComponents,
  componentGroups,
  enabledStatistics,
  onStatisticsChange,
  showFormulas = false,
}) => {
  const theme = useTheme();

  // Calculate statistics for all selected components
  const statistics = useMemo(() => {
    const stats: { [componentId: string]: StatisticalSummary } = {};

    selectedComponents.forEach(componentId => {
      const values = data
        .map(point => point[componentId])
        .filter(val => val !== undefined && val !== null && !isNaN(val)) as number[];

      if (values.length === 0) return;

      const n = values.length;
      const sorted = [...values].sort((a, b) => a - b);
      
      // Basic statistics
      const mean = values.reduce((sum, val) => sum + val, 0) / n;
      const variance = values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / n;
      const stdDev = Math.sqrt(variance);
      const min = Math.min(...values);
      const max = Math.max(...values);
      
      // RMS calculation: sqrt(mean(x²))
      const rms = Math.sqrt(values.reduce((sum, val) => sum + val * val, 0) / n);
      
      // Percentiles
      const p95Index = Math.floor(0.95 * (n - 1));
      const p99Index = Math.floor(0.99 * (n - 1));
      const p95 = sorted[p95Index];
      const p99 = sorted[p99Index];
      
      // Median
      const median = n % 2 === 0 
        ? (sorted[n / 2 - 1] + sorted[n / 2]) / 2
        : sorted[Math.floor(n / 2)];
      
      // Skewness and Kurtosis
      const m3 = values.reduce((sum, val) => sum + Math.pow((val - mean) / stdDev, 3), 0) / n;
      const m4 = values.reduce((sum, val) => sum + Math.pow((val - mean) / stdDev, 4), 0) / n;
      const skewness = m3;
      const kurtosis = m4 - 3; // Excess kurtosis
      
      stats[componentId] = {
        componentId,
        count: n,
        mean,
        stdDev,
        variance,
        min,
        max,
        median,
        rms,
        p95,
        p99,
        skewness,
        kurtosis,
        range: max - min,
        cv: stdDev / Math.abs(mean), // Coefficient of variation
      };
    });

    return stats;
  }, [data, selectedComponents]);

  // Generate overlays for enabled statistics
  const overlays = useMemo(() => {
    const result: StatisticalOverlay[] = [];
    
    selectedComponents.forEach(componentId => {
      const stat = statistics[componentId];
      if (!stat) return;

      enabledStatistics.forEach(statType => {
        const value = stat[statType as keyof StatisticalSummary] as number;
        if (value !== undefined && !isNaN(value)) {
          result.push({
            componentId,
            label: `${componentId} ${statType.toUpperCase()}`,
            value,
            type: statType,
          });
        }
      });
    });

    return result;
  }, [statistics, selectedComponents, enabledStatistics]);

  // Update parent with overlays
  React.useEffect(() => {
    onStatisticsChange?.(overlays);
  }, [overlays, onStatisticsChange]);

  // Group statistics by component type
  const groupedStatistics = useMemo(() => {
    const groups: { [groupType: string]: { [componentId: string]: StatisticalSummary } } = {};
    
    componentGroups.forEach(group => {
      groups[group.type] = {};
      group.componentIds.forEach(componentId => {
        if (statistics[componentId]) {
          groups[group.type][componentId] = statistics[componentId];
        }
      });
    });

    return groups;
  }, [statistics, componentGroups]);

  // Statistical formulas for reference
  const formulas = {
    mean: 'x̄ = (1/n) Σx_i',
    stdDev: 'σ = √[(1/n) Σ(x_i - x̄)²]',
    rms: 'x_RMS = √[(1/n) Σx_i²]',
    variance: 'σ² = (1/n) Σ(x_i - x̄)²',
    skewness: 'γ₁ = (1/n) Σ[(x_i - x̄)/σ]³',
    kurtosis: 'γ₂ = (1/n) Σ[(x_i - x̄)/σ]⁴ - 3',
    cv: 'CV = σ/|x̄|',
  };

  const formatValue = (value: number, precision = 4) => {
    if (Math.abs(value) < 1e-10) return '0';
    if (Math.abs(value) > 1e6 || Math.abs(value) < 1e-4) {
      return value.toExponential(precision);
    }
    return value.toFixed(precision);
  };

  const getGroupColor = (groupType: string) => {
    switch (groupType) {
      case 'struts': return theme.palette.primary.main;
      case 'jackets': return theme.palette.secondary.main;
      case 'foundations': return theme.palette.error.main;
      default: return theme.palette.grey[600];
    }
  };

  return (
    <Card sx={{ p: 2 }}>
      <Box sx={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', mb: 2 }}>
        <Typography variant="h6">
          Statistical Analysis
        </Typography>
        
        {showFormulas && (
          <Tooltip title="Show Statistical Formulas">
            <IconButton size="small">
              <FunctionsIcon />
            </IconButton>
          </Tooltip>
        )}
      </Box>

      {Object.keys(statistics).length === 0 ? (
        <Typography variant="body2" color="text.secondary" sx={{ textAlign: 'center', py: 4 }}>
          No components selected for statistical analysis
        </Typography>
      ) : (
        <>
          {/* Summary Cards */}
          <Grid container spacing={2} sx={{ mb: 3 }}>
            <Grid item xs={12} sm={6} md={3}>
              <Paper sx={{ p: 2, textAlign: 'center' }}>
                <Typography variant="h4" color="primary">
                  {selectedComponents.length}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Components
                </Typography>
              </Paper>
            </Grid>
            
            <Grid item xs={12} sm={6} md={3}>
              <Paper sx={{ p: 2, textAlign: 'center' }}>
                <Typography variant="h4" color="secondary">
                  {data.length}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Data Points
                </Typography>
              </Paper>
            </Grid>
            
            <Grid item xs={12} sm={6} md={3}>
              <Paper sx={{ p: 2, textAlign: 'center' }}>
                <Typography variant="h4" color="success.main">
                  {enabledStatistics.length}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Active Statistics
                </Typography>
              </Paper>
            </Grid>
            
            <Grid item xs={12} sm={6} md={3}>
              <Paper sx={{ p: 2, textAlign: 'center' }}>
                <Typography variant="h4" color="warning.main">
                  {overlays.length}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  Chart Overlays
                </Typography>
              </Paper>
            </Grid>
          </Grid>

          {/* Grouped Statistics Tables */}
          {Object.entries(groupedStatistics).map(([groupType, groupStats]) => {
            if (Object.keys(groupStats).length === 0) return null;

            return (
              <Accordion key={groupType} defaultExpanded>
                <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                  <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                    <Chip
                      label={groupType.charAt(0).toUpperCase() + groupType.slice(1)}
                      size="small"
                      sx={{
                        backgroundColor: getGroupColor(groupType),
                        color: theme.palette.getContrastText(getGroupColor(groupType)),
                      }}
                    />
                    <Typography variant="subtitle1">
                      {Object.keys(groupStats).length} Components
                    </Typography>
                  </Box>
                </AccordionSummary>
                
                <AccordionDetails>
                  <TableContainer component={Paper} variant="outlined">
                    <Table size="small" stickyHeader>
                      <TableHead>
                        <TableRow>
                          <TableCell>Component</TableCell>
                          <TableCell align="right">
                            Mean
                            {showFormulas && (
                              <Tooltip title={formulas.mean}>
                                <IconButton size="small">
                                  <InfoIcon fontSize="small" />
                                </IconButton>
                              </Tooltip>
                            )}
                          </TableCell>
                          <TableCell align="right">
                            Std Dev
                            {showFormulas && (
                              <Tooltip title={formulas.stdDev}>
                                <IconButton size="small">
                                  <InfoIcon fontSize="small" />
                                </IconButton>
                              </Tooltip>
                            )}
                          </TableCell>
                          <TableCell align="right">
                            RMS
                            {showFormulas && (
                              <Tooltip title={formulas.rms}>
                                <IconButton size="small">
                                  <InfoIcon fontSize="small" />
                                </IconButton>
                              </Tooltip>
                            )}
                          </TableCell>
                          <TableCell align="right">Min</TableCell>
                          <TableCell align="right">Max</TableCell>
                          <TableCell align="right">P95</TableCell>
                          <TableCell align="right">P99</TableCell>
                          <TableCell align="right">CV</TableCell>
                        </TableRow>
                      </TableHead>
                      
                      <TableBody>
                        {Object.entries(groupStats).map(([componentId, stat]) => (
                          <TableRow key={componentId} hover>
                            <TableCell component="th" scope="row">
                              <Typography variant="body2" fontWeight="medium">
                                {componentId}
                              </Typography>
                            </TableCell>
                            <TableCell align="right">{formatValue(stat.mean)}</TableCell>
                            <TableCell align="right">{formatValue(stat.stdDev)}</TableCell>
                            <TableCell align="right">{formatValue(stat.rms)}</TableCell>
                            <TableCell align="right">{formatValue(stat.min)}</TableCell>
                            <TableCell align="right">{formatValue(stat.max)}</TableCell>
                            <TableCell align="right">{formatValue(stat.p95)}</TableCell>
                            <TableCell align="right">{formatValue(stat.p99)}</TableCell>
                            <TableCell align="right">{formatValue(stat.cv, 3)}</TableCell>
                          </TableRow>
                        ))}
                      </TableBody>
                    </Table>
                  </TableContainer>
                </AccordionDetails>
              </Accordion>
            );
          })}

          {/* Formulas Reference */}
          {showFormulas && (
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                <Typography variant="subtitle1">Statistical Formulas</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <Box sx={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(250px, 1fr))', gap: 2 }}>
                  {Object.entries(formulas).map(([name, formula]) => (
                    <Paper key={name} sx={{ p: 2 }} variant="outlined">
                      <Typography variant="subtitle2" sx={{ mb: 1 }}>
                        {name.charAt(0).toUpperCase() + name.slice(1)}
                      </Typography>
                      <Typography variant="body2" fontFamily="monospace">
                        {formula}
                      </Typography>
                    </Paper>
                  ))}
                </Box>
              </AccordionDetails>
            </Accordion>
          )}
        </>
      )}
    </Card>
  );
};

export default TimeSeriesStatistics;