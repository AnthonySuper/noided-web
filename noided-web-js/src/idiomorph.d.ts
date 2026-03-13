declare module "idiomorph" {
  export interface Callbacks {
    beforeNodeAdded?: (element: Node) => boolean;
    beforeNodeMorphed?: (oldNode: Node, newNode: Node) => boolean;
    beforeNodeRemoved?: (element: Node) => boolean;
  }

  export interface Configuration {
    morphStyle?: "innerHTML" | "outerHTML";
    ignoreActive?: boolean;
    callbacks?: Callbacks;
  }

  export interface MorphInterface {
    morph(node: Node, newNode: Node | string, options?: Configuration): void;
  }

  export const Idiomorph: MorphInterface;
}
