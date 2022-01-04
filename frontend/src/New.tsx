import React, { useEffect }  from 'react';
import { useNavigate } from "react-router-dom";

import logo from '../favicon.svg';
import ControlSocket from './ControlSocket';

export interface Props {
  socket: ControlSocket,
}

export default function New(props: Props) {
  const { socket } = props;
  const navigate = useNavigate();

  useEffect(() => {
    (async function() {
      const sessionId = await socket.newSession();
      navigate(`/${sessionId}`);
    })();
  }, []);

  return (
    <>
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
      <p>
        {'Loading...'}
      </p>
    </>
  );
}
