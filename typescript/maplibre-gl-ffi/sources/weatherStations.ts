import { Map } from 'maplibre-gl'

const source = crypto.randomUUID()

export const addWeatherStationsLayer = (map: Map, data: GeoJSON.GeoJSON) => {
  if (map.getSource(source) === undefined)
    map.addSource(source, { type: 'geojson', data }).addLayer({
      id: source,
      source,
      type: 'symbol',
      layout: {
        'icon-image': 'information',
        'text-field': ['get', 'Name_tc'],
        'text-offset': [0, 1.25],
        'text-anchor': 'top',
        'text-font': ['Noto Sans Regular'],
      },
    })
  // .setFilter(source, ['literal', false])
}

declare global {
  var maplibregl_ffi: unknown
}
