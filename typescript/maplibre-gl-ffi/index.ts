import {
  Map,
  Marker,
  type GeoJSONSourceSpecification,
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

const addMarkerAndEaseToLocation = (location: LngLatLike, mapLibre: Map) => {
  new Marker().setLngLat(location).addTo(mapLibre)
  mapLibre.easeTo({ center: location })
}

const getDataURI = (
  data: GeoJSONSourceSpecification['data'],
  callback: (data_url: string) => void
) => {
  if (typeof data !== 'string' && data.type == 'FeatureCollection') {
    const data_url = data.features[0]?.properties?.['Data_url']
    if (typeof data_url == 'string' && data_url != '') callback(data_url)
  }
}

const getGeoJSONFeatures = (data: GeoJSONSourceSpecification['data']) => {
  if (typeof data == 'string') {
    console.warn('GeoJSONSourceSpecification data is string, skipped')
    return
  }
  if (data.type != 'FeatureCollection') {
    console.warn(
      'type of GeoJSONSourceSpecification data is not FeatureCollection, skipped'
    )
    return
  }
  return data.features
}

const districtBoudaryLayerId = 'districtBoudaryLayerId'

const addDistrictBoudaryLayer = (
  map: Map,
  data: GeoJSONSourceSpecification['data']
) => {
  map.addSource(districtBoudaryLayerId, { type: 'geojson', data }).addLayer({
    id: districtBoudaryLayerId,
    source: districtBoudaryLayerId,
    type: 'line',
    paint: { 'line-color': '#198EC8' },
  })
  map.setFilter(districtBoudaryLayerId, ['literal', false])
}

const focusDistrict = (map: Map, areaCode: string) =>
  map.setFilter(districtBoudaryLayerId, ['==', 'AREA_CODE', areaCode]) // FIXME or use id to filter?

const focusDistrictByGeoJSON = (
  map: Map,
  data: GeoJSONSourceSpecification['data']
) => {
  const features = getGeoJSONFeatures(data)
  if (features) {
    switch (features.length) {
      case 0:
        console.warn('unexpected: no districts returned. abort.')
        break
      case 1:
        const code = features?.[0]?.properties?.['AREA_CODE']
        if (code)
          map.setFilter(districtBoudaryLayerId, ['==', 'AREA_CODE', code])
        else
          console.warn('unexpected: district returned has no area_code. abort.')
        break
      default:
        console.warn('unexpected: multiple districts returned. abort.')
    }
  }
}
declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap,
  addMarkerAndEaseToLocation,
  focusDistrict,
  focusDistrictByGeoJSON,
  addDistrictBoudaryLayer,
  getDataURI,
  hard_surface_soccer_pitch_7,
}
