import React, { useEffect } from 'react';
import { Outlet } from "react-router";

import ControlSocket from './ControlSocket';
import PeerConnection, { ConnectionState } from './PeerConnection';
import { Share } from './Share';

interface Props {
  onAddShare(share: Share): void,
  onShareRemoved(id: string): void,
  setConnectionState(connectionState: ConnectionState): void,
  setControlSocket(controlSocket?: ControlSocket): void;
  setPeerConnection(peerconnection?: PeerConnection): void;
}

export default function ConnectionManager(props: Props) {
  const { onAddShare, onShareRemoved, setConnectionState, setControlSocket, setPeerConnection } = props;

  useEffect(() => {
    const cs = new ControlSocket({
      onPeerJoined() {
        pc.sendOffer();
      },
      onBroadcast(message) {
        pc.handleControlMessage(message);
      },
    });

    const pc = new PeerConnection({
      onConnectionStateChange: setConnectionState,
      onIceCandidate(candidate) {
        cs.broadcast('candidate', { candidate });
      },
      onSessionDescription(type, description) {
        cs.broadcast(type, { description });
      },
      onShare: onAddShare,
      onRemoveShare: onShareRemoved,
    });

    setControlSocket(cs);
    setPeerConnection(pc);

    return () => {
      setControlSocket();
      setPeerConnection();
      // Next tick to let child components send leave message
      setTimeout(() => {
        cs.close();
        pc.close();
        setConnectionState('pending');
      }, 0);
    }
  }, []);

  return <Outlet />;
}
