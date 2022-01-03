import React, { useCallback, useMemo, useState } from 'react';
import { Route, Routes } from 'react-router-dom';

import Share, { Share as IShare } from './Share';
import ControlSocket from './ControlSocket';
import PeerConnection, { ConnectionState, Item } from './PeerConnection';
import Start from './Start';

import './style.css';

export default function App() {
  const [connectionState, setConnectionState] = useState<ConnectionState>('disconnected');
  const [shares, setShares] = useState<Array<IShare>>([]);
  const socket = useMemo(() => new ControlSocket(), []);

  const addShare = useCallback((share: IShare) => {
    setShares(shares => shares.concat(share));
  }, [setShares]);

  const peerConnection = useMemo(() => {
    return new PeerConnection({
      onConnectionStateChange: setConnectionState,
      onItem(item: Item) {
        addShare({ direction: '<', item });
      },
    });
  }, [socket, setShares]);

  const getSessionId = useCallback(async () => {
    return await socket.newSession();
  }, [socket]);

  const onSend = useCallback((item: Item) => {
    peerConnection.sendItem(item);
    addShare({ direction: '>', item });
  }, [addShare, peerConnection]);

  return (
    <Routes>
      <Route path="/" element={<Start getSessionId={getSessionId} />} />
      <Route path="/:shareId" element={
        <Share
          connectionState={connectionState}
          shares={shares}
          onSend={onSend} />
      } />
    </Routes>
  );
}
