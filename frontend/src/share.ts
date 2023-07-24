import { useState } from 'react';
import * as uuid from 'uuid';

import { Item } from "./PeerConnection";

export type Direction = "inbound" | "outbound";

export interface Share {
  direction: Direction;
  id: string;
  item: Item;
}

export function useShare() {
  const [shares, setShares] = useState<Share[]>([]);

  return {
    addIncomingShare(share: Share) {
      setShares(shares => shares.concat(share));
    },

    onCopyItem(id: string) {
      const share = shares.find(share => share.id === id);
      if (!share) return;
      navigator.clipboard.writeText(share.item.value);
    },

    onRemoteShareRemoved(id: string) {
      setShares(shares => shares.filter(share => share.id !== id));
    },

    onRemoveLocalShare(id: string) {
      setShares(shares => shares.filter(share => share.id !== id));
    },

    onResetLocalShares() {
      setShares([]);
    },

    onShare(item: Item) {
      const id = uuid.v4();
      const share: Share = { id, direction: 'outbound', item };
      setShares(shares => shares.concat(share));
      return share;
    },

    shares,
  };
}