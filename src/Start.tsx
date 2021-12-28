import React, { useCallback } from 'react';
import { useNavigate } from "react-router-dom";

import logo from '../favicon.svg';

export default function Start() {
  const navigate = useNavigate();

  const startShare = useCallback(() => {
    navigate('/share');
  }, []);

  return (
    <>
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
      <button onClick={startShare}>Start a new share</button>
    </>
  );
}
