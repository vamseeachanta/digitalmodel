'use client';

import React from 'react';
import {
  Card,
  CardContent,
  Typography,
  Box,
  Avatar,
  LinearProgress,
  Chip,
  Divider,
  Stack,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import {
  TrendingUp,
  TrendingDown,
  TrendingFlat,
  Analytics,
  Speed,
  CheckCircle,
  Error,
  Schedule,
  Warning,
} from '@mui/icons-material';

const StyledCard = styled(Card)(({ theme }) => ({
  height: '100%',
  background: `linear-gradient(135deg, ${theme.palette.background.paper} 0%, ${theme.palette.grey[50]} 100%)`,
  border: `1px solid ${theme.palette.divider}`,
  transition: 'transform 0.2s ease-in-out',
  '&:hover': {
    transform: 'translateY(-2px)',
    boxShadow: theme.shadows[4],
  },
}));

const IconAvatar = styled(Avatar)<{ variant?: 'success' | 'error' | 'warning' | 'info' | 'primary' }>(
  ({ theme, variant = 'primary' }) => ({
    width: 48,
    height: 48,
    backgroundColor:
      variant === 'success'
        ? theme.palette.success.light
        : variant === 'error'
        ? theme.palette.error.light
        : variant === 'warning'
        ? theme.palette.warning.light
        : variant === 'info'
        ? theme.palette.info.light
        : theme.palette.primary.light,
    color:
      variant === 'success'
        ? theme.palette.success.main
        : variant === 'error'
        ? theme.palette.error.main
        : variant === 'warning'
        ? theme.palette.warning.main
        : variant === 'info'
        ? theme.palette.info.main
        : theme.palette.primary.main,
  })
);

export interface StatsWidgetData {
  id: string;
  title: string;
  value: string | number;
  subtitle?: string;
  change?: {
    value: number;
    period: string;
    direction: 'up' | 'down' | 'flat';
  };
  progress?: {
    value: number;
    max: number;
    label?: string;
  };
  icon?: React.ReactNode;
  color?: 'success' | 'error' | 'warning' | 'info' | 'primary';
  details?: Array<{
    label: string;
    value: string | number;
    color?: string;
  }>;
  tags?: string[];
}

interface StatsWidgetProps {
  data: StatsWidgetData;
  onClick?: () => void;
  loading?: boolean;
  compact?: boolean;
}

const getTrendIcon = (direction: 'up' | 'down' | 'flat') => {
  switch (direction) {
    case 'up':
      return <TrendingUp fontSize="small" />;
    case 'down':
      return <TrendingDown fontSize="small" />;
    default:
      return <TrendingFlat fontSize="small" />;
  }
};

const getTrendColor = (direction: 'up' | 'down' | 'flat') => {
  switch (direction) {
    case 'up':
      return 'success.main';
    case 'down':
      return 'error.main';
    default:
      return 'text.secondary';
  }
};

const StatsWidget: React.FC<StatsWidgetProps> = ({
  data,
  onClick,
  loading = false,
  compact = false,
}) => {
  const {
    title,
    value,
    subtitle,
    change,
    progress,
    icon,
    color = 'primary',
    details = [],
    tags = [],
  } = data;

  return (
    <StyledCard
      elevation={1}
      onClick={onClick}
      sx={{ cursor: onClick ? 'pointer' : 'default' }}
    >
      <CardContent sx={{ p: compact ? 2 : 3 }}>
        <Box sx={{ display: 'flex', alignItems: 'flex-start', justifyContent: 'space-between', mb: 2 }}>
          <Box sx={{ flexGrow: 1 }}>
            <Typography variant="body2" color="text.secondary" gutterBottom>
              {title}
            </Typography>
            
            <Typography
              variant={compact ? 'h5' : 'h4'}
              component="div"
              fontWeight="bold"
              color="text.primary"
            >
              {loading ? '...' : value}
            </Typography>

            {subtitle && (
              <Typography variant="caption" color="text.secondary">
                {subtitle}
              </Typography>
            )}
          </Box>

          {icon && (
            <IconAvatar variant={color}>
              {icon}
            </IconAvatar>
          )}
        </Box>

        {change && (
          <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, mb: 2 }}>
            <Box sx={{ display: 'flex', alignItems: 'center', color: getTrendColor(change.direction) }}>
              {getTrendIcon(change.direction)}
              <Typography variant="body2" fontWeight={600} sx={{ ml: 0.5 }}>
                {change.value > 0 ? '+' : ''}{change.value}%
              </Typography>
            </Box>
            <Typography variant="caption" color="text.secondary">
              {change.period}
            </Typography>
          </Box>
        )}

        {progress && (
          <Box sx={{ mb: 2 }}>
            <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 1 }}>
              <Typography variant="caption" color="text.secondary">
                {progress.label || 'Progress'}
              </Typography>
              <Typography variant="caption" color="text.secondary">
                {progress.value}/{progress.max}
              </Typography>
            </Box>
            <LinearProgress
              variant="determinate"
              value={(progress.value / progress.max) * 100}
              sx={{
                height: 6,
                borderRadius: 3,
                backgroundColor: 'grey.200',
                '& .MuiLinearProgress-bar': {
                  backgroundColor: color === 'primary' ? 'primary.main' : `${color}.main`,
                  borderRadius: 3,
                },
              }}
            />
          </Box>
        )}

        {details.length > 0 && !compact && (
          <>
            <Divider sx={{ my: 2 }} />
            <Stack spacing={1}>
              {details.map((detail, index) => (
                <Box key={index} sx={{ display: 'flex', justifyContent: 'space-between' }}>
                  <Typography variant="caption" color="text.secondary">
                    {detail.label}
                  </Typography>
                  <Typography
                    variant="caption"
                    fontWeight={600}
                    color={detail.color || 'text.primary'}
                  >
                    {detail.value}
                  </Typography>
                </Box>
              ))}
            </Stack>
          </>
        )}

        {tags.length > 0 && !compact && (
          <Box sx={{ mt: 2, display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
            {tags.map((tag) => (
              <Chip
                key={tag}
                label={tag}
                size="small"
                variant="outlined"
                sx={{ fontSize: '0.7rem', height: 20 }}
              />
            ))}
          </Box>
        )}
      </CardContent>
    </StyledCard>
  );
};

// Predefined widgets for common marine engineering metrics
export const createAnalysisStatsWidget = (
  totalAnalyses: number,
  runningAnalyses: number,
  completedToday: number
): StatsWidgetData => ({
  id: 'analysis-stats',
  title: 'Total Analyses',
  value: totalAnalyses.toLocaleString(),
  subtitle: 'All time',
  progress: {
    value: runningAnalyses,
    max: totalAnalyses,
    label: 'Currently Running',
  },
  change: {
    value: completedToday > 0 ? Math.round((completedToday / totalAnalyses) * 100) : 0,
    period: 'today',
    direction: completedToday > 0 ? 'up' : 'flat',
  },
  icon: <Analytics />,
  color: 'primary',
  details: [
    { label: 'Running', value: runningAnalyses, color: 'primary.main' },
    { label: 'Completed Today', value: completedToday, color: 'success.main' },
  ],
});

export const createPerformanceWidget = (
  avgDuration: number,
  successRate: number,
  failureCount: number
): StatsWidgetData => ({
  id: 'performance-stats',
  title: 'Avg Duration',
  value: `${Math.round(avgDuration)}min`,
  subtitle: 'Analysis time',
  progress: {
    value: successRate,
    max: 100,
    label: 'Success Rate',
  },
  change: {
    value: successRate - 85, // Compare against target of 85%
    period: 'vs target',
    direction: successRate > 85 ? 'up' : successRate < 85 ? 'down' : 'flat',
  },
  icon: <Speed />,
  color: successRate > 85 ? 'success' : 'warning',
  details: [
    { label: 'Success Rate', value: `${successRate}%`, color: 'success.main' },
    { label: 'Failed', value: failureCount, color: 'error.main' },
  ],
});

export const createStatusWidget = (
  completed: number,
  failed: number,
  running: number,
  pending: number
): StatsWidgetData => ({
  id: 'status-breakdown',
  title: 'Analysis Status',
  value: 'Overview',
  subtitle: 'Current state',
  icon: <CheckCircle />,
  color: 'info',
  details: [
    { label: 'Completed', value: completed, color: 'success.main' },
    { label: 'Running', value: running, color: 'primary.main' },
    { label: 'Pending', value: pending, color: 'warning.main' },
    { label: 'Failed', value: failed, color: 'error.main' },
  ],
});

export default StatsWidget;