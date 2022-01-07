import React, { useCallback, useState } from 'react';
import { Route, Routes } from 'react-router-dom';
import * as uuid from 'uuid';

import ConnectionManager from './ConnectionManager';
import Home from './Home';
import Share, { Direction, Share as IShare } from './Share';
import New from './New';
import ControlSocket from './ControlSocket';
import PeerConnection, { ConnectionState, Item } from './PeerConnection';

import './style.css';

export default function App() {
  const [controlSocket, setControlSocket] = useState<ControlSocket>();
  const [peerConnection, setPeerConnection] = useState<PeerConnection>();
  const [connectionState, setConnectionState] = useState<ConnectionState>('pending');
  const [shares, setShares] = useState<IShare[]>([]);

  const addShare = useCallback((direction: Direction, item: Item) => {
    const id = uuid.v4();
    const share = { id, direction, item };
    setShares(shares => shares.concat(share));
  }, [setShares]);

  const addIncomingShare = useCallback((item: Item) => {
    addShare('inbound', item);
  }, [addShare]);

  const onCopyItem = useCallback((id: string) => {
    const share = shares.find(share => share.id === id);
    if (!share) {
      return;
    }
    navigator.clipboard.writeText(share.item.value);
  }, [shares]);

  const onRemoveItem = useCallback((id: string) => {
    setShares(shares => shares.filter(share => share.id !== id));
  }, [setShares]);

  const onSend = useCallback((item: Item) => {
    if (peerConnection) {
      peerConnection.sendItem(item);
      addShare('outbound', item);
    }
  }, [addShare, peerConnection]);

  return (
    <Routes>
      <Route index element={<Home />} />
      <Route element={
        <ConnectionManager
          addShare={addIncomingShare}
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
            onCopyItem={onCopyItem}
            onRemoveItem={onRemoveItem}
            onSend={onSend}
          />
        } />
      </Route>
    </Routes>
  );
}
