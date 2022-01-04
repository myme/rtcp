import React, { ChangeEvent, useCallback, useState } from 'react';
import { useNavigate } from "react-router-dom";

import logo from '../favicon.svg';

export default function Home() {
  const [sessionId, setSessionId] = useState<number>();
  const navigate = useNavigate();

  const newShare = useCallback(() => {
    navigate(`/new`);
  }, [navigate]);

  const joinShare = useCallback(() => {
    navigate(`/${sessionId}`);
  }, [navigate, sessionId]);

  const onJoinSessionIdChange = useCallback((event: ChangeEvent<HTMLInputElement>) => {
    const num = Number(event.target.value);
    setSessionId(!event.target.value || isNaN(num) ? undefined : num);
  }, [setSessionId]);

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
        <a href="/new" className="button" onClick={newShare}>Start a new share</a>
      </p>
    </>
  );
}
