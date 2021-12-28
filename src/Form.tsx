import React, { useCallback, useState } from "react";

interface Props {
  onSubmit(value: string): void,
}

export default function Form(props: Props): JSX.Element {
  const { onSubmit } = props;
  const [input, setInput] = useState('');

  const submit: React.FormEventHandler = useCallback((event) => {
    event.preventDefault();
    onSubmit(input);
    setInput('');
  }, [input, setInput, onSubmit]);

  const inputChange: React.ChangeEventHandler<HTMLInputElement> = useCallback((event) => {
    setInput(event.target.value);
  }, [setInput]);

  return (
    <form id="client" onSubmit={submit}>
      <input type="text" value={input} onChange={inputChange} />
      <button type="submit">Send</button>
    </form>
  );
}
