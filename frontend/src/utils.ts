export function prettifyShareId(shareId: string) {
  return shareId.replace(/^(...)(...)$/, "$1 $2");
}
