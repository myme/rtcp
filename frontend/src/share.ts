import { Item } from "./PeerConnection";

export type Direction = "inbound" | "outbound";

export interface Share {
  direction: Direction;
  id: string;
  item: Item;
}
