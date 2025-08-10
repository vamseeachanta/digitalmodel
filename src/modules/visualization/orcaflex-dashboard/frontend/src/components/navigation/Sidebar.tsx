'use client';

import React, { useState } from 'react';
import { usePathname } from 'next/navigation';
import Link from 'next/link';
import {
  Drawer,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  Typography,
  Box,
  Divider,
  Collapse,
  IconButton,
  Tooltip,
  Badge,
  Avatar,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import {
  Dashboard,
  Analytics,
  Assessment,
  Settings,
  Help,
  ExpandLess,
  ExpandMore,
  PlayArrow,
  History,
  CloudDownload,
  Folder,
  TrendingUp,
  Schedule,
  ChevronLeft,
  ChevronRight,
  AccountCircle,
  ExitToApp,
  Notifications,
} from '@mui/icons-material';

const DRAWER_WIDTH = 280;
const DRAWER_WIDTH_COLLAPSED = 64;

const StyledDrawer = styled(Drawer)(({ theme }) => ({
  '& .MuiDrawer-paper': {
    width: DRAWER_WIDTH,
    boxSizing: 'border-box',
    borderRight: `1px solid ${theme.palette.divider}`,
    backgroundColor: theme.palette.background.paper,
    transition: theme.transitions.create('width', {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.enteringScreen,
    }),
  },
}));

const CollapsedDrawer = styled(Drawer)(({ theme }) => ({
  '& .MuiDrawer-paper': {
    width: DRAWER_WIDTH_COLLAPSED,
    boxSizing: 'border-box',
    borderRight: `1px solid ${theme.palette.divider}`,
    backgroundColor: theme.palette.background.paper,
    transition: theme.transitions.create('width', {
      easing: theme.transitions.easing.sharp,
      duration: theme.transitions.duration.leavingScreen,
    }),
    overflowX: 'hidden',
  },
}));

const LogoContainer = styled(Box)(({ theme }) => ({
  display: 'flex',
  alignItems: 'center',
  padding: theme.spacing(2),
  minHeight: 64,
  borderBottom: `1px solid ${theme.palette.divider}`,
}));

const NavigationItem = styled(ListItem)<{ active?: boolean }>(({ theme, active }) => ({
  borderRadius: theme.spacing(1),
  margin: theme.spacing(0, 1),
  '&:hover': {
    backgroundColor: theme.palette.action.hover,
  },
  ...(active && {
    backgroundColor: theme.palette.primary.main,
    color: theme.palette.primary.contrastText,
    '& .MuiListItemIcon-root': {
      color: theme.palette.primary.contrastText,
    },
    '& .MuiListItemText-primary': {
      color: theme.palette.primary.contrastText,
      fontWeight: 600,
    },
    '&:hover': {
      backgroundColor: theme.palette.primary.dark,
    },
  }),
}));

const UserSection = styled(Box)(({ theme }) => ({
  padding: theme.spacing(2),
  borderTop: `1px solid ${theme.palette.divider}`,
  marginTop: 'auto',
}));

interface NavigationItem {
  id: string;
  label: string;
  icon: React.ReactNode;
  path: string;
  badge?: number;
  children?: NavigationItem[];
}

interface SidebarProps {
  variant?: 'permanent' | 'temporary';
  open?: boolean;
  onClose?: () => void;
  collapsed?: boolean;
  onToggleCollapse?: () => void;
}

const navigationItems: NavigationItem[] = [
  {
    id: 'dashboard',
    label: 'Dashboard',
    icon: <Dashboard />,
    path: '/dashboard',
  },
  {
    id: 'analyses',
    label: 'Analyses',
    icon: <Analytics />,
    path: '/analyses',
    badge: 3,
    children: [
      {
        id: 'new-analysis',
        label: 'New Analysis',
        icon: <PlayArrow />,
        path: '/analyses/new',
      },
      {
        id: 'running',
        label: 'Running',
        icon: <Schedule />,
        path: '/analyses/running',
        badge: 2,
      },
      {
        id: 'history',
        label: 'History',
        icon: <History />,
        path: '/analyses/history',
      },
    ],
  },
  {
    id: 'results',
    label: 'Results',
    icon: <Assessment />,
    path: '/results',
    children: [
      {
        id: 'visualizations',
        label: 'Visualizations',
        icon: <TrendingUp />,
        path: '/results/visualizations',
      },
      {
        id: 'reports',
        label: 'Reports',
        icon: <Folder />,
        path: '/results/reports',
      },
      {
        id: 'downloads',
        label: 'Downloads',
        icon: <CloudDownload />,
        path: '/results/downloads',
      },
    ],
  },
];

const bottomItems: NavigationItem[] = [
  {
    id: 'settings',
    label: 'Settings',
    icon: <Settings />,
    path: '/settings',
  },
  {
    id: 'help',
    label: 'Help & Support',
    icon: <Help />,
    path: '/help',
  },
];

const Sidebar: React.FC<SidebarProps> = ({
  variant = 'permanent',
  open = true,
  onClose,
  collapsed = false,
  onToggleCollapse,
}) => {
  const pathname = usePathname();
  const [expandedItems, setExpandedItems] = useState<string[]>(['analyses', 'results']);

  const handleExpandClick = (itemId: string) => {
    if (collapsed) return;
    
    setExpandedItems((prev) =>
      prev.includes(itemId)
        ? prev.filter((id) => id !== itemId)
        : [...prev, itemId]
    );
  };

  const isItemActive = (item: NavigationItem): boolean => {
    return pathname === item.path || pathname.startsWith(`${item.path}/`);
  };

  const renderNavigationItem = (item: NavigationItem, level = 0) => {
    const isActive = isItemActive(item);
    const hasChildren = item.children && item.children.length > 0;
    const isExpanded = expandedItems.includes(item.id);

    return (
      <React.Fragment key={item.id}>
        <NavigationItem
          button
          active={isActive}
          onClick={() => {
            if (hasChildren) {
              handleExpandClick(item.id);
            }
            if (variant === 'temporary' && onClose) {
              onClose();
            }
          }}
          component={hasChildren ? 'div' : Link}
          href={hasChildren ? undefined : item.path}
          sx={{
            pl: level > 0 ? 4 : 2,
            ...(collapsed && { justifyContent: 'center', px: 1 }),
          }}
        >
          <ListItemIcon sx={{ minWidth: collapsed ? 'auto' : 40 }}>
            <Badge badgeContent={item.badge} color="error" variant="dot">
              {item.icon}
            </Badge>
          </ListItemIcon>
          
          {!collapsed && (
            <>
              <ListItemText primary={item.label} />
              {hasChildren && (isExpanded ? <ExpandLess /> : <ExpandMore />)}
            </>
          )}
        </NavigationItem>

        {hasChildren && !collapsed && (
          <Collapse in={isExpanded} timeout="auto" unmountOnExit>
            <List component="div" disablePadding>
              {item.children!.map((child) => renderNavigationItem(child, level + 1))}
            </List>
          </Collapse>
        )}
      </React.Fragment>
    );
  };

  const drawerContent = (
    <Box sx={{ display: 'flex', flexDirection: 'column', height: '100%' }}>
      <LogoContainer>
        {!collapsed ? (
          <>
            <Avatar
              sx={{
                bgcolor: 'primary.main',
                width: 32,
                height: 32,
                mr: 1.5,
                fontSize: '1rem',
              }}
            >
              OF
            </Avatar>
            <Box sx={{ flexGrow: 1 }}>
              <Typography variant="h6" color="primary" fontWeight="bold">
                OrcaFlex
              </Typography>
              <Typography variant="caption" color="text.secondary">
                Marine Analysis Dashboard
              </Typography>
            </Box>
          </>
        ) : (
          <Avatar
            sx={{
              bgcolor: 'primary.main',
              width: 32,
              height: 32,
              fontSize: '1rem',
              mx: 'auto',
            }}
          >
            OF
          </Avatar>
        )}
        
        {onToggleCollapse && !collapsed && (
          <IconButton onClick={onToggleCollapse} size="small">
            <ChevronLeft />
          </IconButton>
        )}
      </LogoContainer>

      <Box sx={{ flexGrow: 1, py: 1, overflowY: 'auto' }}>
        <List>
          {navigationItems.map((item) => renderNavigationItem(item))}
        </List>

        <Divider sx={{ my: 2 }} />

        <List>
          {bottomItems.map((item) => renderNavigationItem(item))}
        </List>
      </Box>

      {!collapsed && (
        <UserSection>
          <Box sx={{ display: 'flex', alignItems: 'center', mb: 1 }}>
            <Avatar sx={{ width: 32, height: 32, mr: 1.5 }}>
              <AccountCircle />
            </Avatar>
            <Box sx={{ flexGrow: 1 }}>
              <Typography variant="body2" fontWeight={600}>
                Marine Engineer
              </Typography>
              <Typography variant="caption" color="text.secondary">
                engineer@company.com
              </Typography>
            </Box>
            <IconButton size="small">
              <Badge badgeContent={2} color="error" variant="dot">
                <Notifications fontSize="small" />
              </Badge>
            </IconButton>
          </Box>
          
          <Box sx={{ display: 'flex', gap: 1 }}>
            <Tooltip title="Profile Settings">
              <IconButton size="small" sx={{ flex: 1 }}>
                <Settings fontSize="small" />
              </IconButton>
            </Tooltip>
            
            <Tooltip title="Sign Out">
              <IconButton size="small" sx={{ flex: 1 }}>
                <ExitToApp fontSize="small" />
              </IconButton>
            </Tooltip>
          </Box>
        </UserSection>
      )}

      {collapsed && onToggleCollapse && (
        <Box sx={{ p: 1, borderTop: 1, borderColor: 'divider' }}>
          <IconButton onClick={onToggleCollapse} size="small" sx={{ width: '100%' }}>
            <ChevronRight />
          </IconButton>
        </Box>
      )}
    </Box>
  );

  const DrawerComponent = collapsed ? CollapsedDrawer : StyledDrawer;

  return (
    <DrawerComponent
      variant={variant}
      open={open}
      onClose={onClose}
      ModalProps={{
        keepMounted: true, // Better open performance on mobile.
      }}
    >
      {drawerContent}
    </DrawerComponent>
  );
};

export default Sidebar;