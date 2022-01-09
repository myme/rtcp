import React, { ChangeEvent, FormEvent, useCallback, useState } from 'react';

interface Props {
  joinShare(sessionId: string): void,
}

export default function JoinForm(props: Props) {
  const shareIdLength = 6;
  const { joinShare } = props;
  const [sessionId, setSessionId] = useState('');

  const onJoinSessionIdChange = useCallback((event: ChangeEvent<HTMLInputElement>) => {
    setSessionId(event.target.value);
  }, [setSessionId]);

  const onFormSubmit = useCallback((event: FormEvent) => {
    event.preventDefault();
    joinShare(sessionId);
  }, [joinShare, sessionId]);

  return (
    <form onSubmit={onFormSubmit}>
      <div className="group">
        <input type="text" placeholder="Share ID" onChange={onJoinSessionIdChange} />
        <button type="submit" disabled={sessionId.length !== shareIdLength}>
          Join share
        </button>
      </div>
    </form>
  );
}
