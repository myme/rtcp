import React, { useEffect } from 'react';
import { useNavigate } from "react-router-dom";
import { useControlSocket } from './ConnectionManager';

import Header from './Header';

export default function New() {
  const controlSocket = useControlSocket();
  const navigate = useNavigate();

  useEffect(() => {
    (async function () {
      if (!controlSocket) return;
      const sessionId = await controlSocket.newSession();
      navigate(`/${sessionId}`);
    })();
  }, [controlSocket]);

  return (
    <>
      <Header />
      <p>
        {'Loading...'}
      </p>
    </>
  );
}
