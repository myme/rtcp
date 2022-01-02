import React, { useCallback } from 'react';
import { useNavigate } from "react-router-dom";
import * as uuid from 'uuid';

import logo from '../favicon.svg';

export default function Start() {
  const navigate = useNavigate();

  const startShare = useCallback(() => {
    const sessionId = uuid.v4();
    navigate(`/${sessionId}`);
  }, []);

  return (
    <>
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
      <button onClick={startShare}>Start a new share</button>
    </>
  );
}
