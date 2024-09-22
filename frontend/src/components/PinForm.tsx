import React, { useState } from "react";

interface Props {
  onSubmit(pin: string): void;
}

export default function Form({ onSubmit }: Props): JSX.Element {
  const [input, setInput] = useState("");

  const inputChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
    setInput(event.target.value);
  };

  const onSubmitHandler: React.FormEventHandler = (event) => {
    event.preventDefault();
    onSubmit(input);
  };

  return (
    <form id="client" className="inline" onSubmit={onSubmitHandler}>
      <div className="group">
        <input
          type="text"
          placeholder="Pin"
          value={input}
          onChange={inputChange}
          autoFocus
        />
        <button type="submit" disabled={!input.trim().length}>
          Connect
        </button>
      </div>
    </form>
  );
}
