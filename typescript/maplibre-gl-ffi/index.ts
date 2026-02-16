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

const renderUVIndexGeoJSON = (
  map: Map,
  id: string,
  data: GeoJSONSourceSpecification['data']
) => {
  if (typeof data !== 'string') {
    if (data.type == 'FeatureCollection') {
      // const hi = data.features[0]?.properties
      // Data_url : "https://data.weather.gov.hk/weatherAPI/hko_data/csdi/dataset/latest_15min_uvindex_csdi_0.csv"
      // GmlID : "latest_15min_uvindex.1"
      // OBJECTID : 1 Region_en : "Hong Kong"
      // Region_sc : "香港"
      // Region_uc : "香港"
      // SHAPE_Length : 2.23
      // gml_id : "latest_15min_uvindex.0"
    }
  }
  if (map.getSource(id) == undefined) {
    map.addSource(id, { type: 'geojson', data }).addLayer({
      id,
      source: id,
      type: 'line',
      paint: { 'line-color': '#198EC8' },
    })
  }
}

declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap,
  addMarkerAndEaseToLocation,
  renderUVIndexGeoJSON,
  getDataURI,
  hard_surface_soccer_pitch_7,
}
