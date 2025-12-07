import { Map, LngLat, type LngLatLike, type AddLayerObject } from 'maplibre-gl'

import { type Feature } from 'geojson'

import hssp7_data from './facility-hssp7.json'

function isNotNull<T>(item: T | null): item is T {
  return item !== null
}

const source = 'source-sX6NHU7YLnDLbBuJ'
const layer = 'layer-OC+IXVSzo2f/d5ZO'

let features: Feature[] | null = null

const layerCfg: AddLayerObject = {
  id: layer,
  source,
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
    processCoords?: (
      data: typeof hssp7_data,
      setCoords: (coords: (LngLatLike | null)[]) => void
    ) => void
  ) {
    if (features === null) {
      processCoords?.(hssp7_data, (coords) => {
        features = coords.filter(isNotNull).map((d, i) => {
          return {
            type: 'Feature' as const,
            geometry: {
              type: 'Point' as const,
              coordinates: LngLat.convert(d).toArray(),
            },
            properties: {
              title: hssp7_data[i]?.Name_en,
              // title: hssp7[i]?.Name_cn,
            },
          }
        })

        if (features.length == 0) {
          console.error(
            'hard_surface_soccer_pitch_7 toggleLayer: feature is empty / all null.'
          )
        } else {
          map.addSource(source, {
            type: 'geojson',
            data: { type: 'FeatureCollection', features },
          })
          map.addLayer(layerCfg)
        }
      })
    } else {
      if (!map.getLayer(layer)) map.addLayer(layerCfg)
      else map.removeLayer(layer)
    }
  },
}
