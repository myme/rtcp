/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_SIGNALING_SERVER: string,
  readonly VITE_ICE_SERVERS: string,
}

interface ImportMeta {
  readonly env: ImportMetaEnv,
}
