import React from "react";
import { createRoot } from "react-dom/client";
import { Playground } from "./playground.jsx";

const container = document.getElementById("root");
if (container) {
  createRoot(container).render(<Playground />);
}
