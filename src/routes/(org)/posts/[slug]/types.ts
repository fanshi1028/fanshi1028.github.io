import type { findOne } from "domutils";

// HACK: the internal Element type is not exported, so we work around it.
export type DomElement = NonNullable<ReturnType<typeof findOne>>;

// HACK: the internal ChildNode type is not exported, so we work around it.
export type ChildNode = DomElement['childNodes'][number]
