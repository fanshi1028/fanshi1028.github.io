import {
  Map,
  Marker,
  type CameraUpdateTransformFunction,
  type LngLatLike,
} from 'maplibre-gl'

import { hard_surface_soccer_pitch_7 } from './hard_surface_soccer_pitch_7.ts'
import { error_format } from './utils.ts'

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

const getGeoJSONFeatureProperty = (data: GeoJSON.GeoJSON, prop: string) =>
  data.type != 'Feature'
    ? console.error(
        error_format('getGeoJSONFeatureProperty')(
          'GeoJSON is not a Feature',
          'data',
          data
        )
      )
    : data.properties?.[prop]

const getGeoJSONFeatures = (data: GeoJSON.GeoJSON) =>
  data.type != 'FeatureCollection'
    ? console.error(
        error_format('getGeoJSONFeatures')(
          'GeoJSON is not a FeatureCollection',
          'data',
          data
        )
      )
    : data.features

const districtBoundaryLayerId = 'districtBoundaryLayerId'

const addDistrictBoundaryLayer = (map: Map, data: GeoJSON.GeoJSON) =>
  map
    .addSource(districtBoundaryLayerId, { type: 'geojson', data })
    .addLayer({
      id: districtBoundaryLayerId,
      source: districtBoundaryLayerId,
      type: 'line',
      paint: { 'line-color': '#198EC8' },
    })
    .setFilter(districtBoundaryLayerId, ['literal', false])

const focusDistrict = (map: Map, areaCode: string) =>
  map.getLayer(districtBoundaryLayerId)
    ? map.setFilter(districtBoundaryLayerId, ['==', 'AREA_CODE', areaCode])
    : console.debug(
        `layer ${districtBoundaryLayerId} not exists yet. skip focusDistrict.`
      )

// NOTE: assume "one" district result!
const getDistrictAreaCode = (data: GeoJSON.GeoJSON): string | undefined => {
  try {
    const _error_format = error_format('getDistrictAreaCode')
    const features = getGeoJSONFeatures(data)
    if (!features || features.length != 1 || !features[0])
      throw _error_format(
        'features should be 1 truthy feature',
        'features',
        features
      )
    const code = getGeoJSONFeatureProperty(features[0], 'AREA_CODE')
    if (typeof code != 'string')
      throw _error_format('feature returned non-string AREA_CODE', 'code', code)
    return code
  } catch (e) {
    console.error(e)
  }
}

declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap,
  addMarkerAndEaseToLocation,
  focusDistrict,
  getDistrictAreaCode,
  addDistrictBoundaryLayer,
  getDataURI,
  hard_surface_soccer_pitch_7,
}
