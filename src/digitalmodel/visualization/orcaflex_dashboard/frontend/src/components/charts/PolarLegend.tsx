/**
 * Custom Legend Component for Polar Plots
 * 
 * Displays engineering units, limits, and dataset information
 * with professional marine engineering styling
 */

import React from 'react';
import {
  Box,
  Typography,
  Chip,
  Divider,
  IconButton,
  Tooltip,
  useTheme,
  alpha
} from '@mui/material';
import {
  Visibility,
  VisibilityOff,
  Info,
  Warning,
  Error,
  CheckCircle
} from '@mui/icons-material';
import { PolarDataSet, PolarLimits } from './PolarDataProcessor';

interface PolarLegendProps {
  datasets: PolarDataSet[];
  limits?: PolarLimits;
  onToggleDataset: (datasetId: string) => void;
  onDatasetInfo?: (datasetId: string) => void;
  showUnits?: boolean;
  showLimits?: boolean;
  showStatistics?: boolean;
  compact?: boolean;
}

interface LegendItemProps {
  dataset: PolarDataSet;
  onToggle: (id: string) => void;
  onInfo?: (id: string) => void;
  compact?: boolean;
}

const severityConfig = {
  safe: {
    icon: CheckCircle,
    color: '#4caf50',
    label: 'Safe'
  },
  caution: {
    icon: Warning,
    color: '#ff9800',
    label: 'Caution'
  },
  critical: {
    icon: Error,
    color: '#f44336',
    label: 'Critical'
  }
};

const LegendItem: React.FC<LegendItemProps> = ({
  dataset,
  onToggle,
  onInfo,
  compact = false
}) => {
  const theme = useTheme();
  const isVisible = dataset.visible !== false;

  const handleToggle = () => {
    onToggle(dataset.id);
  };

  const handleInfo = () => {
    if (onInfo) {
      onInfo(dataset.id);
    }
  };

  const dataStats = React.useMemo(() => {
    const responses = dataset.data.map(d => d.response);
    const min = Math.min(...responses);
    const max = Math.max(...responses);
    const mean = responses.reduce((sum, val) => sum + val, 0) / responses.length;
    
    return { min, max, mean };
  }, [dataset.data]);

  return (
    <Box
      sx={{
        display: 'flex',
        alignItems: 'center',
        gap: 1,
        p: compact ? 0.5 : 1,
        borderRadius: 1,
        backgroundColor: isVisible 
          ? alpha(dataset.color, 0.1) 
          : alpha(theme.palette.grey[500], 0.05),
        border: `1px solid ${isVisible ? dataset.color : theme.palette.grey[300]}`,
        opacity: isVisible ? 1 : 0.5,
        transition: 'all 0.2s ease-in-out'
      }}
    >
      {/* Visibility Toggle */}
      <IconButton
        size="small"
        onClick={handleToggle}
        sx={{
          color: isVisible ? dataset.color : theme.palette.grey[500],
          p: 0.25
        }}
      >
        {isVisible ? <Visibility /> : <VisibilityOff />}
      </IconButton>

      {/* Color Indicator */}
      <Box
        sx={{
          width: compact ? 12 : 16,
          height: compact ? 12 : 16,
          borderRadius: '50%',
          backgroundColor: dataset.color,
          border: `2px solid ${theme.palette.background.paper}`,
          flexShrink: 0
        }}
      />

      {/* Dataset Information */}
      <Box sx={{ flex: 1, minWidth: 0 }}>
        <Typography
          variant={compact ? 'caption' : 'body2'}
          fontWeight={600}
          color={isVisible ? 'text.primary' : 'text.disabled'}
          sx={{ lineHeight: 1.2 }}
        >
          {dataset.label}
        </Typography>
        
        {!compact && (
          <Typography
            variant="caption"
            color="text.secondary"
            sx={{ display: 'block', lineHeight: 1.2 }}
          >
            {dataset.responseType} • {dataset.units}
          </Typography>
        )}

        {!compact && isVisible && (
          <Box sx={{ display: 'flex', gap: 0.5, mt: 0.5, flexWrap: 'wrap' }}>
            <Chip
              label={`Min: ${dataStats.min.toFixed(2)}`}
              size="small"
              variant="outlined"
              sx={{ fontSize: '0.7rem', height: 20 }}
            />
            <Chip
              label={`Max: ${dataStats.max.toFixed(2)}`}
              size="small"
              variant="outlined"
              sx={{ fontSize: '0.7rem', height: 20 }}
            />
            <Chip
              label={`Avg: ${dataStats.mean.toFixed(2)}`}
              size="small"
              variant="outlined"
              sx={{ fontSize: '0.7rem', height: 20 }}
            />
          </Box>
        )}
      </Box>

      {/* Dataset Type Indicator */}
      <Chip
        label={dataset.type.toUpperCase()}
        size="small"
        color={dataset.type === 'response' ? 'primary' : 
               dataset.type === 'rao' ? 'secondary' : 'default'}
        sx={{ fontSize: '0.65rem', height: 20 }}
      />

      {/* Info Button */}
      {onInfo && (
        <IconButton
          size="small"
          onClick={handleInfo}
          sx={{ p: 0.25, color: theme.palette.info.main }}
        >
          <Info fontSize="small" />
        </IconButton>
      )}
    </Box>
  );
};

const LimitsDisplay: React.FC<{ limits: PolarLimits }> = ({ limits }) => {
  const theme = useTheme();

  return (
    <Box sx={{ p: 1, backgroundColor: alpha(theme.palette.warning.main, 0.1) }}>
      <Typography variant="subtitle2" fontWeight={600} color="text.primary">
        Operating Limits
      </Typography>
      
      <Box sx={{ display: 'flex', flexDirection: 'column', gap: 0.5, mt: 1 }}>
        <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
          <Box
            sx={{
              width: 12,
              height: 12,
              borderRadius: '50%',
              backgroundColor: severityConfig.caution.color
            }}
          />
          <Typography variant="caption" color="text.secondary">
            Operational: {limits.operational.toFixed(2)} {limits.units}
          </Typography>
        </Box>
        
        <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
          <Box
            sx={{
              width: 12,
              height: 12,
              borderRadius: '50%',
              backgroundColor: severityConfig.critical.color
            }}
          />
          <Typography variant="caption" color="text.secondary">
            Survival: {limits.survival.toFixed(2)} {limits.units}
          </Typography>
        </Box>
      </Box>
    </Box>
  );
};

const SeverityLegend: React.FC = () => {
  return (
    <Box sx={{ p: 1 }}>
      <Typography variant="subtitle2" fontWeight={600} color="text.primary">
        Severity Levels
      </Typography>
      
      <Box sx={{ display: 'flex', flexDirection: 'column', gap: 0.5, mt: 1 }}>
        {Object.entries(severityConfig).map(([key, config]) => {
          const IconComponent = config.icon;
          return (
            <Box key={key} sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
              <IconComponent sx={{ color: config.color, fontSize: 16 }} />
              <Typography variant="caption" color="text.secondary">
                {config.label}
              </Typography>
            </Box>
          );
        })}
      </Box>
    </Box>
  );
};

export const PolarLegend: React.FC<PolarLegendProps> = ({
  datasets,
  limits,
  onToggleDataset,
  onDatasetInfo,
  showUnits = true,
  showLimits = true,
  showStatistics = true,
  compact = false
}) => {
  const theme = useTheme();

  const visibleDatasets = datasets.filter(ds => ds.visible !== false);
  const totalDataPoints = datasets.reduce((sum, ds) => sum + ds.data.length, 0);

  return (
    <Box
      sx={{
        backgroundColor: theme.palette.background.paper,
        border: `1px solid ${theme.palette.divider}`,
        borderRadius: 2,
        p: compact ? 1 : 2,
        minWidth: compact ? 200 : 300,
        maxHeight: '70vh',
        overflow: 'auto'
      }}
    >
      {/* Header */}
      <Box sx={{ mb: 2 }}>
        <Typography variant="h6" fontWeight={700} color="text.primary">
          Polar Plot Legend
        </Typography>
        
        {showStatistics && (
          <Typography variant="caption" color="text.secondary">
            {visibleDatasets.length} of {datasets.length} datasets visible • 
            {totalDataPoints} data points
          </Typography>
        )}
      </Box>

      {/* Datasets */}
      <Box sx={{ mb: 2 }}>
        <Typography variant="subtitle2" fontWeight={600} color="text.primary" sx={{ mb: 1 }}>
          Datasets
        </Typography>
        
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
          {datasets.map(dataset => (
            <LegendItem
              key={dataset.id}
              dataset={dataset}
              onToggle={onToggleDataset}
              onInfo={onDatasetInfo}
              compact={compact}
            />
          ))}
        </Box>
      </Box>

      {/* Limits */}
      {showLimits && limits && (
        <>
          <Divider sx={{ my: 2 }} />
          <LimitsDisplay limits={limits} />
        </>
      )}

      {/* Severity Legend */}
      <Divider sx={{ my: 2 }} />
      <SeverityLegend />

      {/* Units Information */}
      {showUnits && (
        <>
          <Divider sx={{ my: 2 }} />
          <Box sx={{ p: 1 }}>
            <Typography variant="subtitle2" fontWeight={600} color="text.primary">
              Units & Conventions
            </Typography>
            
            <Box sx={{ mt: 1 }}>
              <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                • Headings: 0° to 345° (15° increments)
              </Typography>
              <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                • 0° = Head seas, 90° = Beam seas
              </Typography>
              <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                • Response: R(θ) = |F(θ)|
              </Typography>
              {datasets.some(ds => ds.type === 'rao') && (
                <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                  • RAO: X(ω,θ)/A(ω)
                </Typography>
              )}
            </Box>
          </Box>
        </>
      )}

      {/* Export Hint */}
      {!compact && (
        <Box sx={{ mt: 2, p: 1, backgroundColor: alpha(theme.palette.info.main, 0.1) }}>
          <Tooltip title="Use the export controls to save plots in various formats">
            <Typography variant="caption" color="info.main" sx={{ display: 'flex', alignItems: 'center', gap: 0.5 }}>
              <Info fontSize="small" />
              Export options available in plot controls
            </Typography>
          </Tooltip>
        </Box>
      )}
    </Box>
  );
};

export default PolarLegend;