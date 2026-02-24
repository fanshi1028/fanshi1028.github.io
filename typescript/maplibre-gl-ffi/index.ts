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


const getGeoJSONFeatures = (data: GeoJSON.GeoJSON) =>
  data.type != 'FeatureCollection'
    ? console.error(
        'unexpected: GeoJSON is not a FeatureCollection. abort getGeoJSONFeatures. \ngot data: %o',
        data
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

const focusDistrict = (map: Map, areaCode: string) => {
  const go = (retry = 5) => {
    if (retry <= 0) {
      console.warn(
        `layer ${districtBoundaryLayerId} not exists yet. no retry left. abort focusDistrict.`
      )
    } else {
      if (map.getLayer(districtBoundaryLayerId))
        map.setFilter(districtBoundaryLayerId, ['==', 'AREA_CODE', areaCode])
      else {
        console.debug(
          `layer ${districtBoundaryLayerId} not exists yet. ${retry} retries left. defer focusDistrict.`
        )
        setTimeout(() => go(retry - 1), 150)
      }
    }
  }
  go()
}

// NOTE: assume "one" district result!
const getDistrict = (data: GeoJSON.GeoJSON): any | undefined => {
  const features = getGeoJSONFeatures(data)
  if (!features || features.length != 1 || !features[0]) {
    console.error(
      'unexpected: features should be 1 truthy feature. abort getDistrict. \ngot features: %o',
      features
    )
    return
  }
  return features[0].properties
}

declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap,
  addMarkerAndEaseToLocation,
  focusDistrict,
  getDistrict,
  addDistrictBoundaryLayer,
  getDataURI,
  hard_surface_soccer_pitch_7,
}
