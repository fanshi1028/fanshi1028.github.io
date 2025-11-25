import { Map, Marker, type LngLatLike } from 'maplibre-gl'

const createMap = (cid: string) =>
  new Map({
    container: cid,
    style: 'https://tiles.openfreemap.org/styles/liberty',
    zoom: 12,
  })

const addMarkerAndEaseToLocation = (lng: number, lat: number, mapLbre: Map) => {
  const location: LngLatLike = [lng, lat]
  new Marker().setLngLat(location).addTo(mapLbre)
  mapLbre.easeTo({ center: location })
}

declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = { createMap, addMarkerAndEaseToLocation }
