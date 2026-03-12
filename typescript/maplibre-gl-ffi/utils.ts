export function isNotNull<T>(a: T | null): a is T {
  return a !== null
}

export const getGeoJSONFeatures = (data: GeoJSON.GeoJSON) =>
  data.type != 'FeatureCollection'
    ? console.error(
        'unexpected: GeoJSON is not a FeatureCollection. abort getGeoJSONFeatures. \ngot data: %o',
        data
      )
    : data.features
