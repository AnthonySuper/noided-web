export function* parentElements(elm: HTMLElement | null) {
  let otherElm: Element | null = elm;

  while (otherElm != null) {
    yield otherElm;

    otherElm = otherElm.parentElement;
  }

  return null;
}
