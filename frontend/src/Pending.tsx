import React from "react";
import { Link } from "react-router-dom";
import { QRCodeSVG } from "qrcode.react";

import { Session } from "./ControlSocket";
import { useControlSocket } from "./ConnectionManager";
import PinForm from "./PinForm";
import { prettifyShareId } from "./utils";
import { getLogger } from "./Logger";

const logger = getLogger('Pending');

interface PendingProps {
  session?: Session;
  shareId: string;
}

export default function Pending({ session, shareId }: PendingProps) {
  const controlSocket = useControlSocket();

  const onConnectWrapper = (sharePin: string) => {
    if (!controlSocket) {
      logger.error("No control socket!");
      return;
    }
    controlSocket.joinSession({ id: shareId, pin: sharePin });
  };

  if (!session) {
    return (
      <>
        <h1>{`Connect to share: ${prettifyShareId(shareId)}`}</h1>
        <PinForm onSubmit={onConnectWrapper} />
      </>
    );
  }

  return (
    <h1>
      <p>
        Share ID:
        <Link to={`/${session.id}`}>{prettifyShareId(session.id)}</Link>
      </p>
      <p>{`PIN: ${session.pin}`}</p>
      <p>
        <QRCodeSVG value={`${location}`} />
      </p>
    </h1>
  );
}
