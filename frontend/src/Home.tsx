import React, { useCallback } from 'react';
import { Link, useNavigate } from "react-router-dom";

import logo from '../favicon.svg';
import JoinForm from './JoinForm';

export default function Home() {
  const navigate = useNavigate();

  const joinShare = useCallback((sessionId: string) => {
    navigate(`/${sessionId}`);
  }, [navigate]);

  return (
    <>
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
      <JoinForm joinShare={joinShare} />
      <p>
        or
      </p>
      <p>
        <Link to="/new" className="button">Start a new share</Link>
      </p>
    </>
  );
}
