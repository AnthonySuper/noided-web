import "./noided-dialog-form";
import "./noided-form-fragment";
import "./noided-fragment-redirect";
import { interceptSubmit } from "./intercept-submit";

export function initialize(): void {
  window.addEventListener("submit", (e) => {
    interceptSubmit(e).catch((err) => {
      console.error("Failed to intercept submit", err);
    });
  });
  console.log("noided-web-js initialized");
}
