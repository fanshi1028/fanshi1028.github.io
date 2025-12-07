import {
  Map,
  Marker,
  type GeoJSONSourceSpecification,
  type CameraUpdateTransformFunction,
  type LngLatLike,
} from 'maplibre-gl'

import hssp7 from './facility-hssp7.json'

const createMap = (
  cid: string,
  transformCameraUpdate?: CameraUpdateTransformFunction
) =>
  new Map({
    container: cid,
    style: 'https://tiles.openfreemap.org/styles/liberty',
    zoom: 12,
    transformCameraUpdate,
  })

const addMarkerAndEaseToLocation = (lng: number, lat: number, mapLbre: Map) => {
  const location: LngLatLike = [lng, lat]
  new Marker().setLngLat(location).addTo(mapLbre)
  mapLbre.easeTo({ center: location })
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
      const hi = data.features[0]?.properties
      // Data_url : "https://data.weather.gov.hk/weatherAPI/hko_data/csdi/dataset/latest_15min_uvindex_csdi_0.csv"
      // GmlID : "latest_15min_uvindex.1"
      // OBJECTID : 1 Region_en : "Hong Kong"
      // Region_sc : "香港"
      // Region_uc : "香港"
      // SHAPE_Length : 2.23
      // gml_id : "latest_15min_uvindex.0"
    }
  }
  map.addSource(id, { type: 'geojson', data }).addLayer({
    id,
    source: id,
    type: 'line',
    paint: { 'line-color': '#198EC8' },
  })
}

const render_hssp7 = (map: Map, data: [lat: number, lng: number][]) => {
  map
    .addSource('hssp7', {
      type: 'geojson',
      data: {
        type: 'FeatureCollection',
        features: data.map(([lat, lng], i) => {
          return {
            type: 'Feature',
            geometry: {
              type: 'Point',
              coordinates: [lng, lat],
            },
            properties: {
              title: hssp7[i]?.Name_cn,
            },
          }
        }),
      },
    })
    .addLayer({
      id: 'hssp7',
      source: 'hssp7',
      type: 'symbol',
      layout: {
        'icon-image': 'soccer',
        'text-field': ['get', 'title'],
        'text-offset': [0, 1.25],
        'text-anchor': 'top',
        'text-font': ['Noto Sans Regular'],
      },
    })
}

declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap,
  addMarkerAndEaseToLocation,
  renderUVIndexGeoJSON,
  getDataURI,
  hssp7,
  render_hssp7,
}
