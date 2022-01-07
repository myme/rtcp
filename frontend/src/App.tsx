import React, { useCallback, useState } from 'react';
import { Route, Routes } from 'react-router-dom';

import ConnectionManager from './ConnectionManager';
import Home from './Home';
import Share, { Share as IShare } from './Share';
import New from './New';
import ControlSocket from './ControlSocket';
import PeerConnection, { ConnectionState, Item } from './PeerConnection';

import './style.css';

export default function App() {
  const [controlSocket, setControlSocket] = useState<ControlSocket>();
  const [peerConnection, setPeerConnection] = useState<PeerConnection>();
  const [connectionState, setConnectionState] = useState<ConnectionState>('pending');
  const [shares, setShares] = useState<IShare[]>([]);

  const addShare = useCallback((share: IShare) => {
    setShares(shares => shares.concat(share));
  }, [setShares]);

  const onRemoveItem = useCallback((index: number) => {
    setShares(shares => shares.filter((_, i) => i !== index));
  }, [setShares]);

  const onSend = useCallback((item: Item) => {
    if (peerConnection) {
      peerConnection.sendItem(item);
      addShare({ direction: '>', item });
    }
  }, [addShare, peerConnection]);

  return (
    <Routes>
      <Route index element={<Home />} />
      <Route element={
        <ConnectionManager
          addShare={addShare}
          setConnectionState={setConnectionState}
          setControlSocket={setControlSocket}
          setPeerConnection={setPeerConnection}
        />
      }>
        <Route path="/new" element={<New socket={controlSocket} />} />
        <Route path="/:shareId" element={
          <Share
            connectionState={connectionState}
            shares={shares}
            socket={controlSocket}
            onRemoveItem={onRemoveItem}
            onSend={onSend}
          />
        } />
      </Route>
    </Routes>
  );
}
