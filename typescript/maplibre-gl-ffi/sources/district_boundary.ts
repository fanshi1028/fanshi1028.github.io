import { Map } from 'maplibre-gl'
import { getGeoJSONFeatures } from '../utils'

const source = crypto.randomUUID()
const fillLayerId = crypto.randomUUID()

let hoveredDistrict: any = null

export const addDistrictBoundaryLayer = (map: Map, data: GeoJSON.GeoJSON) => {
  if (map.getSource(source) === undefined)
    map
      .addSource(source, {
        type: 'geojson',
        data,
        generateId: true,
      })
      .addLayer({
        id: source,
        source: source,
        type: 'line',
        paint: { 'line-color': '#198EC8' },
      })
      .setFilter(source, ['literal', false])
      .addLayer({
        id: fillLayerId,
        source: source,
        type: 'fill',
        paint: {
          'fill-color': '#627BC1',
          'fill-opacity': [
            'case',
            ['boolean', ['feature-state', 'hover'], false],
            0.8,
            0,
          ],
        },
      })

  map.on('mousemove', fillLayerId, (e) => {
    if (e?.features?.length || 0 > 0) {
      if (hoveredDistrict) {
        map.setFeatureState(
          { source: source, id: hoveredDistrict },
          { hover: false }
        )
      }
      hoveredDistrict = e.features?.[0]?.id
      if (hoveredDistrict) {
        map.setFeatureState(
          { source: source, id: hoveredDistrict },
          { hover: true }
        )
      }
    }
  })

  map.on('mouseleave', fillLayerId, () => {
    if (hoveredDistrict) {
      map.setFeatureState(
        { source: source, id: hoveredDistrict },
        { hover: false }
      )
    }
    hoveredDistrict = null
  })
}

export const focusDistrict = (map: Map, areaCode: string) => {
  const go = (retry = 5) => {
    if (retry <= 0) {
      console.warn(
        `layer ${source} not exists yet. no retry left. abort focusDistrict.`
      )
    } else {
      if (map.getLayer(source))
        map.setFilter(source, ['==', 'AREA_CODE', areaCode])
      else {
        console.debug(
          `layer ${source} not exists yet. ${retry} retries left. defer focusDistrict.`
        )
        setTimeout(() => go(retry - 1), 150)
      }
    }
  }
  go()
}

export const unfocusDistrict = (map: Map) =>
  map.setFilter(source, ['literal', false])

// NOTE: assume "one" district result!
export const getDistrict = (data: GeoJSON.GeoJSON): any | undefined => {
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
