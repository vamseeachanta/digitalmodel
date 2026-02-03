'use client';

import React from 'react';
import {
  Card,
  CardContent,
  CardActions,
  Typography,
  Button,
  Box,
  Chip,
  IconButton,
  LinearProgress,
  Divider,
  Stack,
  Tooltip,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import {
  PlayArrow,
  Pause,
  Stop,
  Visibility,
  Download,
  Delete,
  Schedule,
  CheckCircle,
  Error,
  Warning,
} from '@mui/icons-material';

const StyledCard = styled(Card)(({ theme }) => ({
  height: '100%',
  display: 'flex',
  flexDirection: 'column',
  transition: 'transform 0.2s ease-in-out, box-shadow 0.2s ease-in-out',
  '&:hover': {
    transform: 'translateY(-4px)',
    boxShadow: theme.shadows[8],
  },
  border: `1px solid ${theme.palette.divider}`,
}));

const StatusIcon = styled(Box)<{ status: AnalysisStatus }>(({ theme, status }) => ({
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  width: 40,
  height: 40,
  borderRadius: '50%',
  backgroundColor:
    status === 'completed'
      ? theme.palette.success.light
      : status === 'running'
      ? theme.palette.primary.light
      : status === 'failed'
      ? theme.palette.error.light
      : theme.palette.warning.light,
  color:
    status === 'completed'
      ? theme.palette.success.main
      : status === 'running'
      ? theme.palette.primary.main
      : status === 'failed'
      ? theme.palette.error.main
      : theme.palette.warning.main,
}));

export type AnalysisStatus = 'pending' | 'running' | 'completed' | 'failed' | 'paused';

export interface AnalysisData {
  id: string;
  name: string;
  description?: string;
  status: AnalysisStatus;
  progress?: number;
  startTime?: Date;
  endTime?: Date;
  duration?: number;
  model?: string;
  type: 'static' | 'dynamic' | 'frequency' | 'fatigue';
  parameters?: Record<string, any>;
  results?: {
    maxTension?: number;
    maxDisplacement?: number;
    utilizationRatio?: number;
    criticalSection?: string;
  };
  tags?: string[];
}

interface AnalysisCardProps {
  analysis: AnalysisData;
  onView?: (analysis: AnalysisData) => void;
  onRun?: (analysis: AnalysisData) => void;
  onPause?: (analysis: AnalysisData) => void;
  onStop?: (analysis: AnalysisData) => void;
  onDownload?: (analysis: AnalysisData) => void;
  onDelete?: (analysis: AnalysisData) => void;
  compact?: boolean;
}

const getStatusIcon = (status: AnalysisStatus) => {
  switch (status) {
    case 'completed':
      return <CheckCircle />;
    case 'running':
      return <Schedule />;
    case 'failed':
      return <Error />;
    case 'paused':
      return <Pause />;
    default:
      return <Warning />;
  }
};

const getAnalysisTypeColor = (type: string) => {
  switch (type) {
    case 'static':
      return 'primary';
    case 'dynamic':
      return 'secondary';
    case 'frequency':
      return 'info';
    case 'fatigue':
      return 'warning';
    default:
      return 'default';
  }
};

const formatDuration = (seconds: number): string => {
  const hours = Math.floor(seconds / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const remainingSeconds = seconds % 60;

  if (hours > 0) {
    return `${hours}h ${minutes}m ${remainingSeconds}s`;
  } else if (minutes > 0) {
    return `${minutes}m ${remainingSeconds}s`;
  } else {
    return `${remainingSeconds}s`;
  }
};

const AnalysisCard: React.FC<AnalysisCardProps> = ({
  analysis,
  onView,
  onRun,
  onPause,
  onStop,
  onDownload,
  onDelete,
  compact = false,
}) => {
  const {
    name,
    description,
    status,
    progress,
    startTime,
    endTime,
    duration,
    model,
    type,
    results,
    tags = [],
  } = analysis;

  const isRunning = status === 'running';
  const canRun = status === 'pending' || status === 'failed' || status === 'paused';
  const hasResults = status === 'completed' && results;

  return (
    <StyledCard elevation={2}>
      <CardContent sx={{ flexGrow: 1, pb: compact ? 2 : 3 }}>
        <Box sx={{ display: 'flex', alignItems: 'flex-start', gap: 2, mb: 2 }}>
          <StatusIcon status={status}>
            {getStatusIcon(status)}
          </StatusIcon>

          <Box sx={{ flexGrow: 1, minWidth: 0 }}>
            <Typography variant="h6" component="h3" noWrap>
              {name}
            </Typography>
            
            <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, mt: 1 }}>
              <Chip
                label={type.charAt(0).toUpperCase() + type.slice(1)}
                size="small"
                color={getAnalysisTypeColor(type) as any}
                variant="outlined"
              />
              <Chip
                label={status.charAt(0).toUpperCase() + status.slice(1)}
                size="small"
                color={
                  status === 'completed' ? 'success' :
                  status === 'running' ? 'primary' :
                  status === 'failed' ? 'error' : 'default'
                }
                variant="filled"
              />
            </Box>
          </Box>
        </Box>

        {!compact && description && (
          <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
            {description}
          </Typography>
        )}

        {model && (
          <Typography variant="caption" color="text.secondary" display="block" sx={{ mb: 1 }}>
            Model: {model}
          </Typography>
        )}

        {isRunning && progress !== undefined && (
          <Box sx={{ mb: 2 }}>
            <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 1 }}>
              <Typography variant="caption" color="text.secondary">
                Progress
              </Typography>
              <Typography variant="caption" color="text.secondary">
                {Math.round(progress)}%
              </Typography>
            </Box>
            <LinearProgress variant="determinate" value={progress} sx={{ height: 6, borderRadius: 3 }} />
          </Box>
        )}

        {!compact && hasResults && (
          <>
            <Divider sx={{ my: 2 }} />
            <Typography variant="subtitle2" color="primary" gutterBottom>
              Key Results
            </Typography>
            <Stack spacing={1}>
              {results.maxTension && (
                <Box sx={{ display: 'flex', justifyContent: 'space-between' }}>
                  <Typography variant="caption">Max Tension:</Typography>
                  <Typography variant="caption" fontWeight={600}>
                    {results.maxTension.toLocaleString()} kN
                  </Typography>
                </Box>
              )}
              {results.maxDisplacement && (
                <Box sx={{ display: 'flex', justifyContent: 'space-between' }}>
                  <Typography variant="caption">Max Displacement:</Typography>
                  <Typography variant="caption" fontWeight={600}>
                    {results.maxDisplacement.toFixed(2)} m
                  </Typography>
                </Box>
              )}
              {results.utilizationRatio && (
                <Box sx={{ display: 'flex', justifyContent: 'space-between' }}>
                  <Typography variant="caption">Utilization Ratio:</Typography>
                  <Typography
                    variant="caption"
                    fontWeight={600}
                    color={results.utilizationRatio > 0.8 ? 'error' : results.utilizationRatio > 0.6 ? 'warning.main' : 'success.main'}
                  >
                    {(results.utilizationRatio * 100).toFixed(1)}%
                  </Typography>
                </Box>
              )}
            </Stack>
          </>
        )}

        {!compact && (startTime || duration) && (
          <Box sx={{ mt: 2, pt: 2, borderTop: 1, borderColor: 'divider' }}>
            {startTime && (
              <Typography variant="caption" color="text.secondary" display="block">
                Started: {startTime.toLocaleString()}
              </Typography>
            )}
            {duration && (
              <Typography variant="caption" color="text.secondary" display="block">
                Duration: {formatDuration(duration)}
              </Typography>
            )}
          </Box>
        )}

        {tags.length > 0 && !compact && (
          <Box sx={{ mt: 2, display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
            {tags.map((tag) => (
              <Chip key={tag} label={tag} size="small" variant="outlined" />
            ))}
          </Box>
        )}
      </CardContent>

      <CardActions sx={{ p: 2, pt: 0, justifyContent: 'space-between' }}>
        <Box>
          {canRun && onRun && (
            <Tooltip title="Run Analysis">
              <IconButton color="primary" onClick={() => onRun(analysis)}>
                <PlayArrow />
              </IconButton>
            </Tooltip>
          )}
          
          {isRunning && onPause && (
            <Tooltip title="Pause Analysis">
              <IconButton color="warning" onClick={() => onPause(analysis)}>
                <Pause />
              </IconButton>
            </Tooltip>
          )}
          
          {isRunning && onStop && (
            <Tooltip title="Stop Analysis">
              <IconButton color="error" onClick={() => onStop(analysis)}>
                <Stop />
              </IconButton>
            </Tooltip>
          )}
        </Box>

        <Box>
          {hasResults && onView && (
            <Button
              size="small"
              startIcon={<Visibility />}
              onClick={() => onView(analysis)}
              variant="outlined"
              sx={{ mr: 1 }}
            >
              View Results
            </Button>
          )}
          
          {hasResults && onDownload && (
            <Tooltip title="Download Results">
              <IconButton size="small" onClick={() => onDownload(analysis)}>
                <Download />
              </IconButton>
            </Tooltip>
          )}
          
          {onDelete && (
            <Tooltip title="Delete Analysis">
              <IconButton size="small" color="error" onClick={() => onDelete(analysis)}>
                <Delete />
              </IconButton>
            </Tooltip>
          )}
        </Box>
      </CardActions>
    </StyledCard>
  );
};

export default AnalysisCard;