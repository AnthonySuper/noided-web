import { Idiomorph } from "idiomorph/dist/idiomorph.esm.js";
import { parentElements } from "./util";

export default class NoidedFragmentRedirectElement extends HTMLElement {
  static observedAttributes = ["href", "disable-morph"];

  constructor() {
    super();
  }

  async connectedCallback() {
    try {
      await this.render();
    } finally {
      this.disconnect();
    }
  }

  disconnect() {
    try {
      this.remove();
    } catch (e: unknown) { }
  }

  async render() {
    const { href, disableMorph } = this;
    if (href === null) return;
    if (disableMorph || !this.morphToSame()) {
      window.location.href = href;
      return;
    }

    return this.morphPage(href);
  }

  async morphPage(href: string) {
    const page = await fetch(href, {
      headers: {
        Accept: "text/html",
      },
    });

    const text = await page.text();
    const documentParser = new DOMParser();
    const parsed = documentParser.parseFromString(text, "text/html");

    for (const morphableElement of this.parentMorphTargets()) {
      const maybeOther = parsed.getElementById(morphableElement.id);

      if (maybeOther) {
        Idiomorph.morph(morphableElement, maybeOther);
        return;
      }
    }

    Idiomorph.morph(document.documentElement, text);
  }

  *parentMorphTargets() {
    for (const element of parentElements(this.parentElement)) {
      if (
        element.id &&
        element instanceof HTMLElement &&
        element.dataset['redirectMorphTarget']
      ) {
        yield element;
      }
    }
  }

  morphToSame() {
    const { href } = this;

    return href === window.location.href || href === window.location.pathname;
  }

  get href() {
    return this.attributes.getNamedItem("href")?.value ?? null;
  }

  get disableMorph() {
    return this.attributes.getNamedItem("disable-morph") !== null;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "noided-fragment-redirect": NoidedFragmentRedirectElement;
  }
}

window.customElements.define(
  "noided-fragment-redirect",
  NoidedFragmentRedirectElement,
);
