import {
  Map,
  Marker,
  type CameraUpdateTransformFunction,
  type LngLatBoundsLike,
  type LngLatLike,
} from 'maplibre-gl'

import { hard_surface_soccer_pitch_7 } from './sources/hard_surface_soccer_pitch_7.ts'
import {
  addDistrictBoundaryLayer,
  focusDistrict,
  getDistrict,
  unfocusDistrict,
} from './sources/districtBoundary.ts'
import { addWeatherStationsLayer } from './sources/weatherStations.ts'

var map: Map | null = null
var locationMarker: Marker | null = null

const createMap = (
  cid: string,
  transformCameraUpdate?: CameraUpdateTransformFunction
) => {
  map = new Map({
    container: cid,
    style: 'https://tiles.openfreemap.org/styles/liberty',
    transformCameraUpdate,
  })
  map.on('style.load', () => {
    map?.setProjection({
      type: 'globe', // Set projection to globe
    })
    fitTheGlobe()
  })
  return map
}

const addLocationMarkerAndEaseToLocation = (
  mapLibre: Map,
  location: LngLatLike
) => {
  locationMarker = (locationMarker ?? new Marker())
    .setLngLat(location)
    .addTo(mapLibre)
  mapLibre
    .setProjection({ type: 'mercator' })
    .easeTo({ center: location, zoom: 14 })
}

const clearLocation = () => {
  locationMarker?.remove()
  if (map) {
    map.setProjection({ type: 'globe' })?.setMaxBounds()
    unfocusDistrict(map)
    fitTheGlobe()
  }
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


declare global {
  var maplibregl_ffi: unknown
}

function fitTheGlobe() {
  if (map?.getProjection()?.type != 'globe') return

  const { clientWidth: width, clientHeight: height } = map.getContainer()

  // 1. Determine how big (in pixels) we want the globe diameter to be on screen.
  const padding = -20 // visually tweak to fit
  const targetDiameterPx = Math.min(width, height) - padding * 2

  // 2. MapLibre's zoom logic is based on a Mercator projection, which stretches
  // the world as you move away from the equator by a factor of 1/cos(latitude).
  // To keep the globe a constant physical size, we must shrink our target
  // dimensions by cos(latitude) to counteract that internal magnification.
  const lat = map.getCenter().lat
  const latRad = (lat * Math.PI) / 180
  const mercatorScaleCorrection = Math.cos(latRad)

  // 3. Calculate the necessary world circumference (in pixels) to achieve
  // our target diameter. On a sphere, Circumference = Diameter * PI.
  const requiredWorldCircumferencePx =
    targetDiameterPx * Math.PI * mercatorScaleCorrection

  // 4. MapLibre defines Zoom 0 as a world circumference of 512px.
  // Each zoom level doubles the pixel size (exponential growth: 512 * 2^z).
  // We use Math.log2 to convert that pixel growth back into a linear zoom level 'z'.
  const targetZoom = Math.log2(requiredWorldCircumferencePx / 512)

  const currentZoom = map.getZoom()
  const threshold = 0.01

  if (Math.abs(currentZoom - targetZoom) > threshold) {
    map.flyTo({
      zoom: targetZoom,
      // duration: animationDuration,
      essential: true,
    })
  }
}

window.addEventListener('resize', fitTheGlobe)

const cleanupMap = () => {
  window.removeEventListener('resize', fitTheGlobe)
  map?.remove()
}

const hkBounds: LngLatBoundsLike = [
  [113.81, 22.15],
  [114.45, 22.62],
]

const zoomToHK = () =>
  map
    ?.setProjection({ type: 'mercator' }) // NOTE: https://github.com/maplibre/maplibre-gl-js/issues/5474
    ?.fitBounds(hkBounds)
    ?.setMaxBounds(hkBounds)

globalThis.maplibregl_ffi = {
  createMap,
  cleanupMap,
  zoomToHK,
  addLocationMarkerAndEaseToLocation,
  clearLocation,
  focusDistrict,
  getDistrict,
  addDistrictBoundaryLayer,
  addWeatherStationsLayer,
  getDataURI,
  hard_surface_soccer_pitch_7,
  fitTheGlobe,
  getMap() {
    return map
  },
  getLocationMarker() {
    return locationMarker
  },
}
