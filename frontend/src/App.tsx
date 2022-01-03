import React, { useCallback, useState } from 'react';
import { Route, Routes } from 'react-router-dom';

import Share from './Share';
import SignalingSocket from './SignallingSocket';
import Start from './Start';

import './style.css';

export default function App() {
  const [socket, _] = useState(new SignalingSocket());
  const getSessionId = useCallback(async () => {
    return await socket.newSession();
  }, [socket]);

  return (
    <Routes>
      <Route path="/" element={<Start getSessionId={getSessionId} />} />
      <Route path="/:shareId" element={<Share />} />
    </Routes>
  );
}
