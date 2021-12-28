import React from 'react';
import { Link } from "react-router-dom";

export default function Start() {
  return (
    <Link to="/share">
      {'Start a new share'}
    </Link>
  );
}
