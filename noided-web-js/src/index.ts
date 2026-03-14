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

  window.addEventListener("popstate", () => {
    // If we've been using pushState to navigate, we should probably just reload
    // or we could get fancy and fetch/morph the page here too.
    // For now, reload is safest to not break back button expectations.
    window.location.reload();
  });

  console.log("noided-web-js initialized");
}
