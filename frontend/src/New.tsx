import React, { useEffect }  from 'react';
import { useNavigate } from "react-router-dom";

import ControlSocket from './ControlSocket';
import Header from './Header';

export interface Props {
  socket?: ControlSocket,
}

export default function New(props: Props) {
  const { socket } = props;
  const navigate = useNavigate();

  useEffect(() => {
    (async function() {
      if (!socket) {
        return;
      }
      const sessionId = await socket.newSession();
      navigate(`/${sessionId}`);
    })();
  }, [socket]);

  return (
    <>
      <Header />
      <p>
        {'Loading...'}
      </p>
    </>
  );
}
