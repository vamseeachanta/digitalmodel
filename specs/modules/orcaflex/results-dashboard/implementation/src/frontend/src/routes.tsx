import React from 'react';
import { Routes, Route, Navigate } from 'react-router-dom';

import Dashboard from '@components/Dashboard/Dashboard';
import PolarAnalysis from '@components/Analysis/PolarAnalysis';
import TimeTraceAnalysis from '@components/Analysis/TimeTraceAnalysis';
import Comparison from '@components/Comparison/Comparison';
import Reports from '@components/Reports/Reports';
import Settings from '@components/Settings/Settings';

const AppRoutes: React.FC = () => {
  return (
    <Routes>
      <Route path="/" element={<Navigate to="/dashboard" replace />} />
      <Route path="/dashboard" element={<Dashboard />} />
      <Route path="/analysis/polar" element={<PolarAnalysis />} />
      <Route path="/analysis/time-trace" element={<TimeTraceAnalysis />} />
      <Route path="/comparison" element={<Comparison />} />
      <Route path="/reports" element={<Reports />} />
      <Route path="/settings" element={<Settings />} />
      <Route path="*" element={<Navigate to="/dashboard" replace />} />
    </Routes>
  );
};

export default AppRoutes;