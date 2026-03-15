import { describe, it, expect } from "vitest";
import { parentElements } from "../util";

describe("parentElements", () => {
  it("yields nothing for null input", () => {
    const result = [...parentElements(null)];
    expect(result).toEqual([]);
  });

  it("yields the element itself when it has no parent", () => {
    const div = document.createElement("div");
    const result = [...parentElements(div)];
    expect(result).toEqual([div]);
  });

  it("yields the element and its ancestors in document order", () => {
    const grandparent = document.createElement("div");
    const parent = document.createElement("div");
    const child = document.createElement("span");
    grandparent.appendChild(parent);
    parent.appendChild(child);

    const result = [...parentElements(child)];
    expect(result).toEqual([child, parent, grandparent]);
  });

  it("includes the document body when the element is attached to it", () => {
    const div = document.createElement("div");
    document.body.appendChild(div);

    const result = [...parentElements(div)];
    expect(result).toContain(document.body);

    document.body.removeChild(div);
  });
});
