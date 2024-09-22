import React from "react";
import { Link } from "react-router-dom";

interface Props {
  error?: string;
}

export default function Error({ error }: Props) {
  return (
    <>
      <h2>{error || "Something went wrong"}</h2>
      <Link className="button" to="/">
        Home
      </Link>
    </>
  );
}
