export function isNotNull<T>(a: T | null): a is T {
  return a !== null
}

export const error_format =
  (fromFunc: string) => (error: string, key: string, got: any) =>
    `unexpected: ${error}. abort ${fromFunc}.
           got ${key}: ${JSON.stringify(got)}`
