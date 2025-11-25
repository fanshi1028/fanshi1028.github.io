import { Map, Marker, type LngLatLike } from 'maplibre-gl'

declare global {
  var maplibregl_ffi: unknown
}

globalThis.maplibregl_ffi = {
  createMap(cid: string) {
    return new Map({
      container: cid,
      style: 'https://tiles.openfreemap.org/styles/liberty',
      zoom: 12,
    })
  },
  addMarkerAndEaseToLocation(lng: number, lat: number, mapLbre: Map) {
    const location: LngLatLike = [lng, lat]
    new Marker().setLngLat(location).addTo(mapLbre)
    mapLbre.easeTo({ center: location })
  },
}
