import React, { useCallback } from 'react';
import { useNavigate } from "react-router-dom";

import logo from '../favicon.svg';
import ControlSocket from './ControlSocket';

export interface Props {
  socket: ControlSocket,
}

export default function Start(props: Props) {
  const { socket } = props;
  const navigate = useNavigate();

  const startShare = useCallback(async () => {
    const sessionId = await socket.newSession();
    navigate(`/${sessionId}`);
  }, [socket]);

  return (
    <>
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
      <button onClick={startShare}>Start a new share</button>
    </>
  );
}
