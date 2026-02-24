export function isNotNull<T>(a: T | null): a is T {
  return a !== null
}
