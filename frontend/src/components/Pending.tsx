import React, { useEffect, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import { QRCodeSVG } from "qrcode.react";

import { Session } from "../ControlSocket";
import { prettifyShareId } from "../utils";
import { getLogger } from "../Logger";

import { useControlSocket } from "./ConnectionManager";
import PinForm from "./PinForm";

const logger = getLogger('Pending');

interface PendingProps {
  session?: Session;
  shareId: string;
}

export default function Pending({ session, shareId }: PendingProps) {
  const controlSocket = useControlSocket();
  const navigate = useNavigate();

  const onConnectWrapper = (sharePin: string) => {
    if (!controlSocket) {
      logger.error("No control socket!");
      return;
    }
    controlSocket.joinSession({ id: shareId, pin: sharePin });
  };

  const autoConnect = useAutoConnect({
    onPinFound(pin) {
      onConnectWrapper(pin);
      navigate(`/${shareId}`);
    },
  });

  if (autoConnect) {
    return null;
  }

  if (!session) {
    return (
      <>
        <h1>{`Connect to share: ${prettifyShareId(shareId)}`}</h1>
        <PinForm onSubmit={onConnectWrapper} />
      </>
    );
  }

  const sessionWithPin = `/${session.id}?pin=${session.pin}`;
  const qrCodeLink = `${window.location.origin}${sessionWithPin}`;

  return (
    <>
      <h1>
        Share ID:
        <Link to={sessionWithPin}>{prettifyShareId(session.id)}</Link>
      </h1>
      <h1>{`PIN: ${session.pin}`}</h1>
      <p>
        <QRCodeSVG className="qr-code" value={qrCodeLink} />
      </p>
    </>
  );
}

function useAutoConnect({ onPinFound }: { onPinFound(pin: string): void }) {
  const [autoConnect, setAutoConnect] = useState(false);

  // Move /shareId?pin=1234 query param into state and redirect to /shareId
  useEffect(() => {
    const pin = new URLSearchParams(window.location.search).get("pin");
    if (!pin) {
      return;
    }
    setAutoConnect(true);
    onPinFound(pin);
  }, []);

  return autoConnect;
}
