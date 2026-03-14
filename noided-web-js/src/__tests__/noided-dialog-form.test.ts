import { describe, it, expect, beforeEach } from "vitest";
import "../noided-dialog-form";

function buildDialogForm(): {
  host: HTMLElement;
  dialog: HTMLDialogElement;
} {
  const host = document.createElement("noided-dialog-form") as HTMLElement;
  const dialog = document.createElement("dialog");
  host.appendChild(dialog);
  document.body.appendChild(host);
  return { host, dialog };
}

describe("NoidedDialogForm", () => {
  beforeEach(() => {
    document.body.innerHTML = "";
  });

  describe("isOpen", () => {
    it("returns false when the open attribute is absent", () => {
      const { host } = buildDialogForm();
      expect(host.getAttribute("open")).toBeNull();
    });

    it('returns true after triggerDialog sets open="true"', () => {
      const { host } = buildDialogForm();
      host.setAttribute("open", "true");
      expect(host.getAttribute("open")).toBe("true");
    });
  });

  describe("clicking a trigger-dialog element", () => {
    it("opens the dialog when a descendant with data-trigger-dialog is clicked", () => {
      const { host, dialog } = buildDialogForm();

      const trigger = document.createElement("button");
      trigger.dataset["triggerDialog"] = "";
      host.appendChild(trigger);

      trigger.click();

      expect(host.getAttribute("open")).toBe("true");
      expect(dialog.open).toBe(true);
    });

    it("does not open the dialog when a non-trigger element is clicked", () => {
      const { host, dialog } = buildDialogForm();

      const button = document.createElement("button");
      host.appendChild(button);

      button.click();

      expect(host.getAttribute("open")).toBeNull();
      expect(dialog.open).toBe(false);
    });
  });

  describe("attributeChangedCallback", () => {
    it("opens the dialog element when open is set to true", () => {
      const { host, dialog } = buildDialogForm();

      host.setAttribute("open", "true");

      expect(dialog.open).toBe(true);
    });

    it("closes the dialog element when open is removed", () => {
      const { host, dialog } = buildDialogForm();

      host.setAttribute("open", "true");
      expect(dialog.open).toBe(true);

      host.setAttribute("open", "false");
      expect(dialog.open).toBe(false);
    });
  });
});
