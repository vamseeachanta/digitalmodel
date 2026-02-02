'use client';

import React, { useState } from 'react';
import {
  AppBar,
  Box,
  CssBaseline,
  Drawer,
  IconButton,
  List,
  ListItem,
  ListItemButton,
  ListItemIcon,
  ListItemText,
  Toolbar,
  Typography,
  useMediaQuery,
  useTheme,
  Divider,
  Avatar,
  Menu,
  MenuItem,
} from '@mui/material';
import {
  Menu as MenuIcon,
  Dashboard as DashboardIcon,
  Analytics as AnalyticsIcon,
  Assessment as AssessmentIcon,
  CloudUpload as UploadIcon,
  Settings as SettingsIcon,
  Help as HelpIcon,
  AccountCircle as AccountIcon,
} from '@mui/icons-material';
import { useRouter, usePathname } from 'next/navigation';

const drawerWidth = 240;

interface DashboardLayoutProps {
  children: React.ReactNode;
}

interface NavigationItem {
  text: string;
  icon: React.ReactElement;
  href: string;
  disabled?: boolean;
}

const navigationItems: NavigationItem[] = [
  {
    text: 'Dashboard',
    icon: <DashboardIcon />,
    href: '/',
  },
  {
    text: 'Analyses',
    icon: <AnalyticsIcon />,
    href: '/analyses',
  },
  {
    text: 'Results',
    icon: <AssessmentIcon />,
    href: '/results',
  },
  {
    text: 'Upload',
    icon: <UploadIcon />,
    href: '/upload',
  },
];

const secondaryItems: NavigationItem[] = [
  {
    text: 'Settings',
    icon: <SettingsIcon />,
    href: '/settings',
  },
  {
    text: 'Help',
    icon: <HelpIcon />,
    href: '/help',
  },
];

export const DashboardLayout: React.FC<DashboardLayoutProps> = ({ children }) => {
  const theme = useTheme();
  const router = useRouter();
  const pathname = usePathname();
  const isMobile = useMediaQuery(theme.breakpoints.down('md'));
  
  const [mobileOpen, setMobileOpen] = useState(false);
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  
  const handleDrawerToggle = () => {
    setMobileOpen(!mobileOpen);
  };
  
  const handleMenuOpen = (event: React.MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };
  
  const handleMenuClose = () => {
    setAnchorEl(null);
  };
  
  const handleNavigation = (href: string) => {
    router.push(href);
    if (isMobile) {
      setMobileOpen(false);
    }
  };
  
  const isActiveRoute = (href: string) => {
    if (href === '/') {
      return pathname === '/';
    }
    return pathname.startsWith(href);
  };
  
  const drawer = (
    <div>
      <Toolbar>
        <Typography variant=\"h6\" noWrap component=\"div\">
          OrcaFlex Dashboard
        </Typography>
      </Toolbar>
      <Divider />
      
      {/* Main Navigation */}
      <List>
        {navigationItems.map((item) => (
          <ListItem key={item.text} disablePadding>
            <ListItemButton
              onClick={() => handleNavigation(item.href)}
              selected={isActiveRoute(item.href)}
              disabled={item.disabled}
              sx={{
                '&.Mui-selected': {
                  backgroundColor: theme.palette.primary.light,
                  color: theme.palette.primary.contrastText,
                  '&:hover': {
                    backgroundColor: theme.palette.primary.main,
                  },
                  '& .MuiListItemIcon-root': {
                    color: theme.palette.primary.contrastText,
                  },
                },
              }}
            >
              <ListItemIcon
                sx={{
                  color: isActiveRoute(item.href) 
                    ? theme.palette.primary.contrastText 
                    : 'inherit',
                }}
              >
                {item.icon}
              </ListItemIcon>
              <ListItemText primary={item.text} />
            </ListItemButton>
          </ListItem>
        ))}
      </List>
      
      <Divider />
      
      {/* Secondary Navigation */}
      <List>
        {secondaryItems.map((item) => (
          <ListItem key={item.text} disablePadding>
            <ListItemButton
              onClick={() => handleNavigation(item.href)}
              selected={isActiveRoute(item.href)}
              disabled={item.disabled}
            >
              <ListItemIcon>{item.icon}</ListItemIcon>
              <ListItemText primary={item.text} />
            </ListItemButton>
          </ListItem>
        ))}
      </List>
    </div>
  );
  
  return (
    <Box sx={{ display: 'flex' }}>
      <CssBaseline />
      
      {/* App Bar */}
      <AppBar
        position=\"fixed\"
        sx={{
          width: { md: `calc(100% - ${drawerWidth}px)` },
          ml: { md: `${drawerWidth}px` },
        }}
      >
        <Toolbar>
          <IconButton
            color=\"inherit\"
            aria-label=\"open drawer\"
            edge=\"start\"
            onClick={handleDrawerToggle}
            sx={{ mr: 2, display: { md: 'none' } }}
          >
            <MenuIcon />
          </IconButton>
          
          <Typography variant=\"h6\" noWrap component=\"div\" sx={{ flexGrow: 1 }}>
            {navigationItems.find(item => isActiveRoute(item.href))?.text || 'Dashboard'}
          </Typography>
          
          {/* User Menu */}
          <IconButton
            size=\"large\"
            edge=\"end\"
            aria-label=\"account of current user\"
            aria-controls=\"user-menu\"
            aria-haspopup=\"true\"
            onClick={handleMenuOpen}
            color=\"inherit\"
          >
            <AccountIcon />
          </IconButton>
          
          <Menu
            id=\"user-menu\"
            anchorEl={anchorEl}
            anchorOrigin={{
              vertical: 'bottom',
              horizontal: 'right',
            }}
            keepMounted
            transformOrigin={{
              vertical: 'top',
              horizontal: 'right',
            }}
            open={Boolean(anchorEl)}
            onClose={handleMenuClose}
          >
            <MenuItem onClick={handleMenuClose}>
              <Avatar sx={{ mr: 2, width: 24, height: 24 }}>U</Avatar>
              Profile
            </MenuItem>
            <MenuItem onClick={handleMenuClose}>Settings</MenuItem>
            <Divider />
            <MenuItem onClick={handleMenuClose}>Logout</MenuItem>
          </Menu>
        </Toolbar>
      </AppBar>
      
      {/* Drawer */}
      <Box
        component=\"nav\"
        sx={{ width: { md: drawerWidth }, flexShrink: { md: 0 } }}
        aria-label=\"navigation\"
      >
        {/* Mobile drawer */}
        <Drawer
          variant=\"temporary\"
          open={mobileOpen}
          onClose={handleDrawerToggle}
          ModalProps={{
            keepMounted: true, // Better open performance on mobile.
          }}
          sx={{
            display: { xs: 'block', md: 'none' },
            '& .MuiDrawer-paper': {
              boxSizing: 'border-box',
              width: drawerWidth,
            },
          }}
        >
          {drawer}
        </Drawer>
        
        {/* Desktop drawer */}
        <Drawer
          variant=\"permanent\"
          sx={{
            display: { xs: 'none', md: 'block' },
            '& .MuiDrawer-paper': {
              boxSizing: 'border-box',
              width: drawerWidth,
            },
          }}
          open
        >
          {drawer}
        </Drawer>
      </Box>
      
      {/* Main Content */}
      <Box
        component=\"main\"
        sx={{
          flexGrow: 1,
          width: { md: `calc(100% - ${drawerWidth}px)` },
          minHeight: '100vh',
          backgroundColor: theme.palette.background.default,
        }}
      >
        <Toolbar /> {/* Spacer for fixed app bar */}
        {children}
      </Box>
    </Box>
  );
};