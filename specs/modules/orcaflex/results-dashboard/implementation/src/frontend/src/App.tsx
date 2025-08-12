import React from 'react';
import { BrowserRouter as Router } from 'react-router-dom';
import { Provider } from 'react-redux';
import { ThemeProvider, CssBaseline } from '@mui/material';
import { LocalizationProvider } from '@mui/x-date-pickers';
import { AdapterDateFns } from '@mui/x-date-pickers/AdapterDateFns';

import { store } from '@store/index';
import { theme } from '@utils/theme';
import Routes from './routes';
import Layout from '@components/Layout/Layout';
import { WebSocketProvider } from '@services/websocket';

const App: React.FC = () => {
  return (
    <Provider store={store}>
      <ThemeProvider theme={theme}>
        <LocalizationProvider dateAdapter={AdapterDateFns}>
          <CssBaseline />
          <Router>
            <WebSocketProvider>
              <Layout>
                <Routes />
              </Layout>
            </WebSocketProvider>
          </Router>
        </LocalizationProvider>
      </ThemeProvider>
    </Provider>
  );
};

export default App;