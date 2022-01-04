import React, { ChangeEvent, useCallback, useState } from 'react';
import { useNavigate } from "react-router-dom";

import logo from '../favicon.svg';
import ControlSocket from './ControlSocket';

export interface Props {
  socket: ControlSocket,
}

export default function Start(props: Props) {
  const [sessionId, setSessionId] = useState<number>();
  const { socket } = props;
  const navigate = useNavigate();

  const startShare = useCallback(async () => {
    const sessionId = await socket.newSession();
    navigate(`/${sessionId}`);
  }, [socket]);

  const joinShare = useCallback(() => {
    navigate(`/${sessionId}`);
  }, [sessionId]);

  const onJoinSessionIdChange = useCallback((event: ChangeEvent<HTMLInputElement>) => {
    const num = Number(event.target.value);
    setSessionId(!event.target.value || isNaN(num) ? undefined : num);
  }, [setSessionId]);

  console.log('sessionId', sessionId);

  return (
    <>
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
      <p>
        <input type="text" onChange={onJoinSessionIdChange} />
      </p>
      <p>
        <button onClick={joinShare} disabled={typeof sessionId !== 'number'}>
          Join share
        </button>
      </p>
      <p>
        or
      </p>
      <p>
        <button onClick={startShare}>Start a new share</button>
      </p>
    </>
  );
}
