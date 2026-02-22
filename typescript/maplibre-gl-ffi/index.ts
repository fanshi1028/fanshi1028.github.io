import {
  Map,
  Marker,
  type CameraUpdateTransformFunction,
  type LngLatLike,
} from 'maplibre-gl'

import { hard_surface_soccer_pitch_7 } from './hard_surface_soccer_pitch_7.ts'

const createMap = (
  cid: string,
  transformCameraUpdate?: CameraUpdateTransformFunction
) => {
  const map = new Map({
    container: cid,
    style: 'https://tiles.openfreemap.org/styles/liberty',
    zoom: 12,
    transformCameraUpdate,
  })
  map.on('style.load', () => {
    map.setProjection({
      type: 'globe', // Set projection to globe
    })
  })
  return map
}

const addMarkerAndEaseToLocation = (mapLibre: Map, location: LngLatLike) => {
  new Marker().setLngLat(location).addTo(mapLibre)
  mapLibre.easeTo({ center: location })
}

const getDataURI = (
  data: GeoJSON.GeoJSON,
  callback: (data_url: string) => void
) => {
  if (data.type == 'FeatureCollection') {
    const data_url = data.features[0]?.properties?.['Data_url']
    if (typeof data_url == 'string' && data_url != '') callback(data_url)
  }
}

const getGeoJSONFeatureProperty = (data: GeoJSON.GeoJSON, prop: string) => {
  if (data.type != 'Feature') {
    console.warn('GeoJSON is not a Feature, skipped getGeoJSONFeatureId')
    return
  }
  return data.properties?.[prop]
}

const getGeoJSONFeatures = (data: GeoJSON.GeoJSON) => {
  if (data.type != 'FeatureCollection') {
    console.warn(
      'GeoJSON is not a FeatureCollection, skipped getGeoJSONFeatures'
    )
    return
  }
  return data.features
}

const districtBoudaryLayerId = 'districtBoudaryLayerId'

const addDistrictBoudaryLayer = (map: Map, data: GeoJSON.GeoJSON) => {
  map
    .addSource(districtBoudaryLayerId, { type: 'geojson', data })
    .addLayer({
      id: districtBoudaryLayerId,
      source: districtBoudaryLayerId,
      type: 'line',
      paint: { 'line-color': '#198EC8' },
    })
    .setFilter(districtBoudaryLayerId, ['literal', false])
}

const focusDistrict = (map: Map, areaCode: string) =>
  map.getLayer(districtBoudaryLayerId)
    ? map.setFilter(districtBoudaryLayerId, ['==', 'AREA_CODE', areaCode])
    : console.warn(
        `layer ${districtBoudaryLayerId} not exists yet. skip focusDistrict.`
      )

const getDistrictAreaCode = (data: GeoJSON.GeoJSON): string | undefined => {
  const features = getGeoJSONFeatures(data)
  if (features) {
    // NOTE: assume filtered down to the one district result
    if (features.length == 0) {
      console.warn(
        'unexpected: no districts returned. abort focusDistrictByGeoJSON.'
      )
    } else if (features.length > 1) {
      console.warn(
        'unexpected: multiple districts returned. abort focusDistrictByGeoJSON.'
      )
    } else {
      if (!features[0])
        console.warn(
          'unexpected: district returned has no area_code. abort focusDistrictByGeoJSON.'
        )
      else {
        const code = getGeoJSONFeatureProperty(features[0], 'AREA_CODE')
        if (typeof code != 'string') {
          console.warn(
            `unexpected: district returned non-string AREA_CODE: ${JSON.stringify(code)} . abort focusDistrictByGeoJSON.`
          )
        } else return code
      }
    }
  } else
    console.warn(
      'unexpected: features not found. abort focusDistrictByGeoJSON.'
    )
}
declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap,
  addMarkerAndEaseToLocation,
  focusDistrict,
  getDistrictAreaCode,
  addDistrictBoudaryLayer,
  getDataURI,
  hard_surface_soccer_pitch_7,
}
