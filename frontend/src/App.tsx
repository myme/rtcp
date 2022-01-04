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
  const socket = useMemo(() => {
    return new ControlSocket({
      onPeerJoined() {
        peerConnection.sendOffer();
      },
      onBroadcast(message) {
        peerConnection.handleControlMessage(message);
      },
    });
  }, []);

  const addShare = useCallback((share: IShare) => {
    setShares(shares => shares.concat(share));
  }, [setShares]);

  const peerConnection = useMemo(() => {
    return new PeerConnection({
      onConnectionStateChange: setConnectionState,
      onIceCandidate(candidate) {
        socket.broadcast('candidate', { candidate });
      },
      onSessionDescription(type, description) {
        socket.broadcast(type, { description });
      },
      onItem(item) {
        addShare({ direction: '<', item });
      },
    });
  }, [socket, setShares]);

  const onSend = useCallback((item: Item) => {
    peerConnection.sendItem(item);
    addShare({ direction: '>', item });
  }, [addShare, peerConnection]);

  return (
    <Routes>
      <Route path="/" element={<Start socket={socket} />} />
      <Route path="/:shareId" element={
        <Share
          connectionState={connectionState}
          shares={shares}
          socket={socket}
          onSend={onSend} />
      } />
    </Routes>
  );
}
