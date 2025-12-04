import {
  Map,
  Marker,
  type GeoJSONSourceSpecification,
  type CameraUpdateTransformFunction,
  type LngLatLike,
} from 'maplibre-gl'

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

const renderUVIndexGeoJSON = (
  map: Map,
  id: string,
  data: GeoJSONSourceSpecification['data']
) => {
  map.addSource(id, { type: 'geojson', data }).addLayer({
    id,
    source: id,
    type: 'line',
    paint: { 'line-color': '#198EC8' },
  })
}

declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap,
  addMarkerAndEaseToLocation,
  renderUVIndexGeoJSON,
}
