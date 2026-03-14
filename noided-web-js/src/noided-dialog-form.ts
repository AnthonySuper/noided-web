import { parentElements } from "./util";

function hasTriggerDialog(elem: Element) {
  return (
    elem instanceof HTMLElement &&
    "triggerDialog" in elem.dataset
  );
}

function insideBox(event: MouseEvent, target: Element) {
  const { clientY, clientX } = event;
  const rect = target.getBoundingClientRect();
  return (
    rect.top <= clientY &&
    clientY <= rect.top + rect.height &&
    rect.left <= clientX &&
    clientX <= rect.left + rect.width
  );
}

export default class NoidedDialogForm extends HTMLElement {
  static observedAttributes = ["open"];

  constructor() {
    super();
  }

  connectedCallback() {
    this.addEventListener("click", this.listenClick);
  }

  listenClick = (event: MouseEvent) => {
    if (this.isOpen) {
      this.clickWhenOpen(event);
    } else {
      this.clickWhenClosed(event);
    }
  };

  clickWhenClosed(event: MouseEvent) {
    const { target } = event;

    if (!(target instanceof HTMLElement)) {
      return;
    }

    const trigger = Array.from(parentElements(target)).some(hasTriggerDialog);

    if (!trigger) {
      return;
    }

    this.triggerDialog();
  }

  clickWhenOpen(event: MouseEvent) {
    const dialog = this.dialogElement;

    if (dialog === null) {
      return;
    }

    if (event.target === dialog && !insideBox(event, dialog)) {
      this.removeAttribute("open");
    }
  }

  triggerDialog() {
    this.setAttribute("open", "true");
  }

  attributeChangedCallback(
    attrName: string,
    _: string | null,
    newValue: string | null,
  ) {
    if (attrName === "open") {
      this.handleChangeIsOpen(newValue);
    }
  }

  handleChangeIsOpen(value: string | null) {
    const dialogElement = this.querySelector("dialog");
    if (dialogElement === null) {
      return;
    }

    if (value === "true") {
      if (dialogElement.open) {
        return;
      }

      dialogElement.showModal();
    } else if (dialogElement.open) {
      dialogElement.close();
    }
  }

  get isOpen() {
    return this.getAttribute("open") === "true";
  }

  get dialogElement() {
    return this.querySelector("dialog");
  }
}

window.customElements.define("noided-dialog-form", NoidedDialogForm);
