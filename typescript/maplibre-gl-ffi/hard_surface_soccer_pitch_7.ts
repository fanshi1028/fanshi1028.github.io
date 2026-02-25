import { Map, LngLat, type AddLayerObject } from 'maplibre-gl'

import { type Feature } from 'geojson'

import hssp7_data from './facility-hssp7.json'
import { isNotNull } from './utils'

const sourceId = Symbol('facility-hssp7')

let features: Feature[] | null = null

const layerCfg: AddLayerObject = {
  id: sourceId.toString(),
  source: sourceId.toString(),
  type: 'symbol',
  layout: {
    'icon-image': 'soccer',
    'text-field': ['get', 'title'],
    'text-offset': [0, 1.25],
    'text-anchor': 'top',
    'text-font': ['Noto Sans Regular'],
  },
}

export const hard_surface_soccer_pitch_7 = {
  getFeatures() {
    return features
  },
  toggleLayer(
    map: Map,
    // NOTE: help function provided from haskell side to help with reloading the data
    fromWGS84StrPair?: (lngStr: string, latStr: string) => LngLat | null
  ) {
    if (fromWGS84StrPair && features === null) {
      features = hssp7_data
        .map((d) => {
          const coord = fromWGS84StrPair(d.Longitude, d.Latitude)
          return coord
            ? {
                type: 'Feature' as const,
                geometry: {
                  type: 'Point' as const,
                  coordinates: LngLat.convert(coord).toArray(),
                },
                properties: {
                  title: d.Name_en,
                  // title: d.Name_cn,
                },
              }
            : null
        })
        .filter(isNotNull)
      features.length == 0
        ? console.error(
            'hard_surface_soccer_pitch_7 toggleLayer: feature is empty / all null.'
          )
        : map
            .addSource(sourceId.toString(), {
              type: 'geojson',
              data: { type: 'FeatureCollection', features },
            })
            .addLayer(layerCfg)
    } else {
      if (map.getLayer(sourceId.toString()) === undefined)
        map.addLayer(layerCfg)
      else map.removeLayer(sourceId.toString())
    }
  },
}
