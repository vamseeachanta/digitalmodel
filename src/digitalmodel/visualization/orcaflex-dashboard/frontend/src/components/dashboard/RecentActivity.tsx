'use client';

import React from 'react';
import {
  Card,
  CardContent,
  CardHeader,
  Typography,
  List,
  ListItem,
  ListItemAvatar,
  ListItemText,
  Avatar,
  Chip,
  Box,
  Button,
  Divider,
  Tooltip,
  IconButton,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import {
  PlayArrow,
  CheckCircle,
  Error,
  Pause,
  CloudDownload,
  Visibility,
  Schedule,
  MoreVert,
} from '@mui/icons-material';

const StyledCard = styled(Card)(({ theme }) => ({
  height: '100%',
  display: 'flex',
  flexDirection: 'column',
  border: `1px solid ${theme.palette.divider}`,
}));

const ActivityAvatar = styled(Avatar)<{ activityType: ActivityType }>(
  ({ theme, activityType }) => ({
    width: 32,
    height: 32,
    backgroundColor:
      activityType === 'completed'
        ? theme.palette.success.light
        : activityType === 'started'
        ? theme.palette.primary.light
        : activityType === 'failed'
        ? theme.palette.error.light
        : activityType === 'paused'
        ? theme.palette.warning.light
        : theme.palette.info.light,
    color:
      activityType === 'completed'
        ? theme.palette.success.main
        : activityType === 'started'
        ? theme.palette.primary.main
        : activityType === 'failed'
        ? theme.palette.error.main
        : activityType === 'paused'
        ? theme.palette.warning.main
        : theme.palette.info.main,
  })
);

const TimeStamp = styled(Typography)(({ theme }) => ({
  fontSize: '0.75rem',
  color: theme.palette.text.secondary,
  fontWeight: 400,
}));

export type ActivityType = 'started' | 'completed' | 'failed' | 'paused' | 'downloaded' | 'viewed';

export interface ActivityItem {
  id: string;
  type: ActivityType;
  title: string;
  description: string;
  timestamp: Date;
  analysisId?: string;
  analysisName?: string;
  analysisType?: string;
  user?: string;
  metadata?: Record<string, any>;
}

interface RecentActivityProps {
  activities: ActivityItem[];
  maxItems?: number;
  onViewAll?: () => void;
  onActivityClick?: (activity: ActivityItem) => void;
  loading?: boolean;
}

const getActivityIcon = (type: ActivityType) => {
  switch (type) {
    case 'started':
      return <PlayArrow fontSize="small" />;
    case 'completed':
      return <CheckCircle fontSize="small" />;
    case 'failed':
      return <Error fontSize="small" />;
    case 'paused':
      return <Pause fontSize="small" />;
    case 'downloaded':
      return <CloudDownload fontSize="small" />;
    case 'viewed':
      return <Visibility fontSize="small" />;
    default:
      return <Schedule fontSize="small" />;
  }
};

const getActivityColor = (type: ActivityType) => {
  switch (type) {
    case 'completed':
      return 'success';
    case 'started':
      return 'primary';
    case 'failed':
      return 'error';
    case 'paused':
      return 'warning';
    case 'downloaded':
      return 'info';
    case 'viewed':
      return 'secondary';
    default:
      return 'default';
  }
};

const formatTimeAgo = (timestamp: Date): string => {
  const now = new Date();
  const diffInSeconds = Math.floor((now.getTime() - timestamp.getTime()) / 1000);

  if (diffInSeconds < 60) {
    return 'Just now';
  }

  const diffInMinutes = Math.floor(diffInSeconds / 60);
  if (diffInMinutes < 60) {
    return `${diffInMinutes}m ago`;
  }

  const diffInHours = Math.floor(diffInMinutes / 60);
  if (diffInHours < 24) {
    return `${diffInHours}h ago`;
  }

  const diffInDays = Math.floor(diffInHours / 24);
  if (diffInDays < 7) {
    return `${diffInDays}d ago`;
  }

  return timestamp.toLocaleDateString();
};

const RecentActivity: React.FC<RecentActivityProps> = ({
  activities,
  maxItems = 10,
  onViewAll,
  onActivityClick,
  loading = false,
}) => {
  const displayedActivities = activities.slice(0, maxItems);

  if (loading) {
    return (
      <StyledCard elevation={1}>
        <CardHeader
          title="Recent Activity"
          titleTypographyProps={{ variant: 'h6', color: 'primary' }}
        />
        <CardContent>
          <Typography color="text.secondary" textAlign="center" sx={{ py: 4 }}>
            Loading recent activity...
          </Typography>
        </CardContent>
      </StyledCard>
    );
  }

  if (activities.length === 0) {
    return (
      <StyledCard elevation={1}>
        <CardHeader
          title="Recent Activity"
          titleTypographyProps={{ variant: 'h6', color: 'primary' }}
        />
        <CardContent>
          <Typography color="text.secondary" textAlign="center" sx={{ py: 4 }}>
            No recent activity to display
          </Typography>
        </CardContent>
      </StyledCard>
    );
  }

  return (
    <StyledCard elevation={1}>
      <CardHeader
        title="Recent Activity"
        titleTypographyProps={{ variant: 'h6', color: 'primary' }}
        action={
          onViewAll && (
            <Button size="small" onClick={onViewAll}>
              View All
            </Button>
          )
        }
      />
      
      <CardContent sx={{ flexGrow: 1, pt: 0 }}>
        <List disablePadding>
          {displayedActivities.map((activity, index) => (
            <React.Fragment key={activity.id}>
              <ListItem
                alignItems="flex-start"
                onClick={() => onActivityClick && onActivityClick(activity)}
                sx={{
                  cursor: onActivityClick ? 'pointer' : 'default',
                  '&:hover': onActivityClick
                    ? {
                        backgroundColor: 'action.hover',
                        borderRadius: 1,
                      }
                    : {},
                  px: 0,
                  py: 1,
                }}
              >
                <ListItemAvatar sx={{ minWidth: 40 }}>
                  <ActivityAvatar activityType={activity.type}>
                    {getActivityIcon(activity.type)}
                  </ActivityAvatar>
                </ListItemAvatar>
                
                <ListItemText
                  primary={
                    <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, mb: 0.5 }}>
                      <Typography variant="body2" fontWeight={600}>
                        {activity.title}
                      </Typography>
                      {activity.analysisType && (
                        <Chip
                          label={activity.analysisType}
                          size="small"
                          variant="outlined"
                          sx={{ fontSize: '0.7rem', height: 18 }}
                        />
                      )}
                    </Box>
                  }
                  secondary={
                    <Box>
                      <Typography variant="body2" color="text.secondary" gutterBottom>
                        {activity.description}
                      </Typography>
                      
                      <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                        <TimeStamp>
                          {formatTimeAgo(activity.timestamp)}
                        </TimeStamp>
                        
                        {activity.user && (
                          <Typography variant="caption" color="text.secondary">
                            by {activity.user}
                          </Typography>
                        )}
                      </Box>
                      
                      {activity.analysisName && (
                        <Typography variant="caption" color="primary.main" display="block" sx={{ mt: 0.5 }}>
                          {activity.analysisName}
                        </Typography>
                      )}
                    </Box>
                  }
                />
                
                <Tooltip title="More options">
                  <IconButton size="small" sx={{ ml: 1 }}>
                    <MoreVert fontSize="small" />
                  </IconButton>
                </Tooltip>
              </ListItem>
              
              {index < displayedActivities.length - 1 && <Divider component="li" />}
            </React.Fragment>
          ))}
        </List>
        
        {activities.length > maxItems && (
          <Box sx={{ textAlign: 'center', mt: 2 }}>
            <Typography variant="caption" color="text.secondary">
              Showing {maxItems} of {activities.length} activities
            </Typography>
          </Box>
        )}
      </CardContent>
    </StyledCard>
  );
};

// Helper function to create activity items
export const createActivityItem = (
  type: ActivityType,
  analysisName: string,
  analysisType: string,
  user?: string,
  metadata?: Record<string, any>
): ActivityItem => {
  const getTitle = () => {
    switch (type) {
      case 'started':
        return 'Analysis Started';
      case 'completed':
        return 'Analysis Completed';
      case 'failed':
        return 'Analysis Failed';
      case 'paused':
        return 'Analysis Paused';
      case 'downloaded':
        return 'Results Downloaded';
      case 'viewed':
        return 'Results Viewed';
      default:
        return 'Activity';
    }
  };

  const getDescription = () => {
    switch (type) {
      case 'started':
        return `${analysisType} analysis initiated for ${analysisName}`;
      case 'completed':
        return `${analysisType} analysis finished successfully`;
      case 'failed':
        return `${analysisType} analysis encountered an error`;
      case 'paused':
        return `${analysisType} analysis was paused`;
      case 'downloaded':
        return `Results for ${analysisName} were downloaded`;
      case 'viewed':
        return `Results for ${analysisName} were viewed`;
      default:
        return `Activity for ${analysisName}`;
    }
  };

  return {
    id: `${Date.now()}-${Math.random()}`,
    type,
    title: getTitle(),
    description: getDescription(),
    timestamp: new Date(),
    analysisName,
    analysisType,
    user,
    metadata,
  };
};

export default RecentActivity;