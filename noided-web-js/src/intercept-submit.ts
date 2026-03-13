export async function interceptSubmit(event: SubmitEvent) {
  const target = event.target;

  if (!(target instanceof HTMLFormElement)) {
    return;
  }

  const { framelike } = target.dataset;

  if (framelike === "false" || framelike === undefined) {
    return;
  }

  event.preventDefault();
  event.stopPropagation();

  const formData = new FormData(target, event.submitter);
  const action = target.action;
  const method = target.method;
  const submitter =
    event.submitter instanceof HTMLButtonElement ? event.submitter : null;

  await wrapSubmit(target, submitter, async () => {
    const result = await fetch(action, {
      method: method,
      body: formData,
      headers: {
        Accept: "application/vnd.noided-fragment.form;q=0.9, */*;q=0.8",
      },
    });

    if (result.redirected) {
      // We wind up fetching twice here, which is kinda bad, but IDK what else to do lol
      window.location.href = result.url;
      return;
    }

    const bodyParser = new DOMParser();
    const respText = await result.text();
    console.log(respText);
    const doc = bodyParser.parseFromString(respText, "text/html");
    ["noided-form-fragment" as const, "noided-fragment-redirect" as const]
      .flatMap((e) => Array.from(doc.getElementsByTagName(e)))
      .forEach((node) => target.appendChild(node));
  });
}

async function wrapSubmit<T>(
  _form: HTMLFormElement,
  button: HTMLButtonElement | null,
  act: () => Promise<T>,
): Promise<T> {
  button && (button.disabled = true);

  try {
    return await act();
  } finally {
    button && (button.disabled = false);
  }
}
