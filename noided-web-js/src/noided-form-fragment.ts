import { Idiomorph } from "idiomorph/dist/idiomorph.esm.js";
import { parentElements } from "./util";

export default class NoidedFormFragmentElement extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    try {
      this.render();
    } finally {
      this.disconnect();
    }
  }

  render() {
    const formBody = this.querySelector("template");
    if (formBody === null) {
      throw new Error("No child template element!");
    }

    const form = this.parentForm();
    if (form === null) {
      throw new Error("no parent form element!");
    }

    Idiomorph.morph(form, formBody.innerHTML, {
      morphStyle: "outerHTML",
    });
  }

  parentForm() {
    for (const elm of parentElements(this.parentElement)) {
      if (elm instanceof HTMLFormElement) {
        return elm;
      }
    }

    return null;
  }

  disconnect() {
    this.remove();
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "noided-form-fragment": NoidedFormFragmentElement;
  }
}

window.customElements.define("noided-form-fragment", NoidedFormFragmentElement);
