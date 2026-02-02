'use client';

import React from 'react';
import {
  Box,
  LinearProgress,
  Typography,
  Paper,
  Stack,
  Fade,
} from '@mui/material';
import { styled } from '@mui/material/styles';

const StyledLinearProgress = styled(LinearProgress)(({ theme }) => ({
  height: 8,
  borderRadius: 4,
  backgroundColor: theme.palette.grey[200],
  '& .MuiLinearProgress-bar': {
    borderRadius: 4,
    background: `linear-gradient(45deg, ${theme.palette.primary.main}, ${theme.palette.primary.light})`,
  },
}));

const ProgressContainer = styled(Paper)(({ theme }) => ({
  padding: theme.spacing(3),
  borderRadius: theme.spacing(2),
  backgroundColor: theme.palette.background.paper,
  border: `1px solid ${theme.palette.divider}`,
}));

interface ProgressBarProps {
  value?: number; // 0-100 for determinate, undefined for indeterminate
  label?: string;
  description?: string;
  showPercentage?: boolean;
  variant?: 'determinate' | 'indeterminate';
  color?: 'primary' | 'secondary' | 'success' | 'error' | 'info' | 'warning';
  size?: 'small' | 'medium' | 'large';
  contained?: boolean;
  stages?: ProgressStage[];
  currentStage?: number;
}

interface ProgressStage {
  label: string;
  description?: string;
  completed?: boolean;
}

const ProgressBar: React.FC<ProgressBarProps> = ({
  value,
  label,
  description,
  showPercentage = true,
  variant = 'determinate',
  color = 'primary',
  size = 'medium',
  contained = false,
  stages = [],
  currentStage = 0,
}) => {
  const progressValue = variant === 'determinate' ? value : undefined;
  
  const getSizeProps = () => {
    switch (size) {
      case 'small':
        return { height: 4, textVariant: 'caption' as const };
      case 'large':
        return { height: 12, textVariant: 'body1' as const };
      default:
        return { height: 8, textVariant: 'body2' as const };
    }
  };

  const { height, textVariant } = getSizeProps();

  const progressBar = (
    <Box sx={{ width: '100%' }}>
      {(label || (showPercentage && progressValue !== undefined)) && (
        <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 1 }}>
          {label && (
            <Typography variant={textVariant} color="text.primary" fontWeight={500}>
              {label}
            </Typography>
          )}
          {showPercentage && progressValue !== undefined && (
            <Typography variant={textVariant} color="text.secondary">
              {Math.round(progressValue)}%
            </Typography>
          )}
        </Box>
      )}

      <StyledLinearProgress
        variant={variant}
        value={progressValue}
        color={color}
        sx={{ height }}
      />

      {description && (
        <Typography
          variant="caption"
          color="text.secondary"
          sx={{ mt: 1, display: 'block' }}
        >
          {description}
        </Typography>
      )}

      {stages.length > 0 && (
        <Box sx={{ mt: 2 }}>
          <Stack spacing={1}>
            {stages.map((stage, index) => (
              <Fade key={index} in timeout={300 * (index + 1)}>
                <Box
                  sx={{
                    display: 'flex',
                    alignItems: 'center',
                    opacity: index <= currentStage ? 1 : 0.5,
                  }}
                >
                  <Box
                    sx={{
                      width: 12,
                      height: 12,
                      borderRadius: '50%',
                      backgroundColor:
                        index < currentStage
                          ? 'success.main'
                          : index === currentStage
                          ? 'primary.main'
                          : 'grey.300',
                      mr: 2,
                      flexShrink: 0,
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center',
                    }}
                  >
                    {index < currentStage && (
                      <Box
                        sx={{
                          width: 6,
                          height: 6,
                          backgroundColor: 'white',
                          borderRadius: '50%',
                        }}
                      />
                    )}
                  </Box>
                  <Box>
                    <Typography
                      variant="body2"
                      fontWeight={index === currentStage ? 600 : 400}
                      color={index <= currentStage ? 'text.primary' : 'text.secondary'}
                    >
                      {stage.label}
                    </Typography>
                    {stage.description && (
                      <Typography variant="caption" color="text.secondary">
                        {stage.description}
                      </Typography>
                    )}
                  </Box>
                </Box>
              </Fade>
            ))}
          </Stack>
        </Box>
      )}
    </Box>
  );

  if (contained) {
    return (
      <ProgressContainer elevation={1}>
        {progressBar}
      </ProgressContainer>
    );
  }

  return progressBar;
};

export default ProgressBar;