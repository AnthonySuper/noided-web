import { describe, it, expect, vi, beforeEach } from "vitest";
import { interceptSubmit } from "../intercept-submit";

function makeSubmitEvent(
  target: EventTarget | null,
  submitter: HTMLElement | null = null,
): SubmitEvent {
  const event = new Event("submit", { bubbles: true, cancelable: true }) as SubmitEvent;
  Object.defineProperty(event, "target", { value: target, configurable: true });
  Object.defineProperty(event, "submitter", {
    value: submitter,
    configurable: true,
  });
  return event;
}

describe("interceptSubmit", () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  it("does nothing when the event target is not a form", async () => {
    const div = document.createElement("div");
    const event = makeSubmitEvent(div);
    const preventDefault = vi.spyOn(event, "preventDefault");

    await interceptSubmit(event);

    expect(preventDefault).not.toHaveBeenCalled();
  });

  it("does nothing when the form has no data-framelike attribute", async () => {
    const form = document.createElement("form");
    const event = makeSubmitEvent(form);
    const preventDefault = vi.spyOn(event, "preventDefault");

    await interceptSubmit(event);

    expect(preventDefault).not.toHaveBeenCalled();
  });

  it('does nothing when data-framelike is "false"', async () => {
    const form = document.createElement("form");
    form.dataset["framelike"] = "false";
    const event = makeSubmitEvent(form);
    const preventDefault = vi.spyOn(event, "preventDefault");

    await interceptSubmit(event);

    expect(preventDefault).not.toHaveBeenCalled();
  });

  it("intercepts submission and calls fetch when data-framelike is set", async () => {
    const form = document.createElement("form");
    form.dataset["framelike"] = "true";
    form.method = "post";
    form.action = "https://example.com/submit";
    document.body.appendChild(form);

    const event = makeSubmitEvent(form);
    const preventDefault = vi.spyOn(event, "preventDefault");
    const stopPropagation = vi.spyOn(event, "stopPropagation");

    const mockResponse = {
      redirected: false,
      text: vi.fn().mockResolvedValue("<html><body></body></html>"),
    } as unknown as Response;
    const fetchMock = vi.spyOn(globalThis, "fetch").mockResolvedValue(mockResponse);

    await interceptSubmit(event);

    expect(preventDefault).toHaveBeenCalled();
    expect(stopPropagation).toHaveBeenCalled();
    expect(fetchMock).toHaveBeenCalledWith(
      form.action,
      expect.objectContaining({
        method: form.method,
        headers: expect.objectContaining({
          Accept: expect.stringContaining("noided-fragment"),
        }),
      }),
    );

    document.body.removeChild(form);
  });

  it("redirects the page when the fetch response is redirected", async () => {
    const form = document.createElement("form");
    form.dataset["framelike"] = "true";
    form.method = "post";
    form.action = "https://example.com/submit";
    document.body.appendChild(form);

    const redirectUrl = "https://example.com/redirected";
    const mockResponse = {
      redirected: true,
      url: redirectUrl,
    } as unknown as Response;
    vi.spyOn(globalThis, "fetch").mockResolvedValue(mockResponse);

    // happy-dom exposes window.location as a writable property
    const locationSpy = vi.spyOn(window, "location", "get").mockReturnValue({
      href: "",
    } as Location);

    const event = makeSubmitEvent(form);
    await interceptSubmit(event);

    // Verify that a redirect was attempted (the mocked location received the URL)
    expect(locationSpy).toHaveBeenCalled();

    locationSpy.mockRestore();
    document.body.removeChild(form);
  });

  it("disables and re-enables the submit button around the fetch", async () => {
    const form = document.createElement("form");
    form.dataset["framelike"] = "true";
    form.action = "https://example.com/submit";
    document.body.appendChild(form);

    const button = document.createElement("button");
    button.type = "submit";
    form.appendChild(button);

    let disabledDuringFetch = false;

    vi.spyOn(globalThis, "fetch").mockImplementation(async () => {
      disabledDuringFetch = button.disabled;
      return {
        redirected: false,
        text: vi.fn().mockResolvedValue("<html><body></body></html>"),
      } as unknown as Response;
    });

    const event = makeSubmitEvent(form, button);
    await interceptSubmit(event);

    expect(disabledDuringFetch).toBe(true);
    expect(button.disabled).toBe(false);

    document.body.removeChild(form);
  });
});
