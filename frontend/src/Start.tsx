import React, { useCallback } from 'react';
import { useNavigate } from "react-router-dom";

import logo from '../favicon.svg';

export interface Props {
  getSessionId(): Promise<string>;
}

export default function Start(props: Props) {
  const { getSessionId } = props;
  const navigate = useNavigate();

  const startShare = useCallback(async () => {
    const sessionId = await getSessionId();
    navigate(`/${sessionId}`);
  }, [getSessionId]);

  return (
    <>
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
      <button onClick={startShare}>Start a new share</button>
    </>
  );
}
