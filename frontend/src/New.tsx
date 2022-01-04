import React, { useEffect }  from 'react';
import { useNavigate } from "react-router-dom";

import ControlSocket from './ControlSocket';
import Header from './Header';

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
      <Header />
      <p>
        {'Loading...'}
      </p>
    </>
  );
}
