/*!
 * KMZ EMR Standard - Configuration and Utilities
 * Copyright (c) 2026 Muhammad Ikhsanudin
 * * This file is part of KMZ EMR Standard.
 * This software is licensed under the PolyForm Strict License 1.0.0.
 * * STRICT PROHIBITION: You may use this software for your own purposes, 
 * but you MAY NOT modify, copy, or distribute this software or any 
 * modified version of it without prior written consent from the Copyright Holder.
 * * A copy of the license is located in the LICENSE file in the root directory, 
 * or can be found at: https://polyformproject.org/licenses/strict/1.0.0/
 */
/* ================= CONFIG & CONSTANTS ================= */
export const TEXT_SIZE = 1.0;
export const SNAP_TOLERANCE_METERS = 5;
export const CABLE_DETECT_TOLERANCE = 5;
export const SMART_ROUTE_SNAP_RADIUS = 7; // snapping cable ke pole terdekat dalam satuan meter
export const SMART_ROUTE_FDT_RADIUS = 1;
export const OFFSET_DISTANCE = 1.0;

export const STRUCTURE_CLUSTER = [
    'BOUNDARY FAT', 'FAT', 'HP COVER', 'HP UNCOVER',
    'EXISTING POLE EMR 7-2.5', 'EXISTING POLE EMR 7-3', 'EXISTING POLE EMR 7-4', 'EXISTING POLE EMR 9-4',
    'EXISTING POLE PARTNER 7-4', 'EXISTING POLE PARTNER 9-4',
    'NEW POLE 7-2.5', 'NEW POLE 7-3', 'NEW POLE 7-4', 'NEW POLE 9-4',
    'DISTRIBUTION CABLE', 'SLACK HANGER', 'SLING WIRE'
];

export const STRUCTURE_SUBFEEDER = [
    'FDT', 'JOINT CLOSURE', 'EXISTING POLE EMR 7-2.5', 'EXISTING POLE EMR 7-3', 'EXISTING POLE EMR 7-4',
    'EXISTING POLE EMR 7-5', 'EXISTING POLE EMR 9-5', 'EXISTING POLE EMR 9-4',
    'EXISTING POLE PARTNER 7-4', 'EXISTING POLE PARTNER 9-4', 'NEW POLE 7-5', 'NEW POLE 9-5',
    'NEW POLE 7-4', 'NEW POLE 9-4', 'CABLE', 'SLACK HANGER'
];

export const RULES_CLUSTER = [
    { folder: '-', placemark: '288C', line: '-', polygon: '-', colorCode: '#AA0000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/cross-hairs.png' },
    { folder: '-', placemark: '144C', line: '-', polygon: '-', colorCode: '#AAFF00', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/cross-hairs.png' },
    { folder: '-', placemark: '96C', line: '-', polygon: '-', colorCode: '#00FFFF', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/cross-hairs.png' },
    { folder: '-', placemark: '72C', line: '-', polygon: '-', colorCode: '#0000FF', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/cross-hairs.png' },
    { folder: '-', placemark: '48C', line: '-', polygon: '-', colorCode: '#AA00FF', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/cross-hairs.png' },
    { folder: 'BOUNDARY FAT', placemark: '-', line: '-', polygon: '-', colorCode: '#009999', styleLink: '-' },
    { folder: 'FAT', placemark: '-', line: '-', polygon: '-', colorCode: '#FFFF00', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/triangle.png' },
    { folder: 'HP COVER', placemark: '-', line: '-', polygon: '-', colorCode: '#00FF00', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/homegardenbusiness.png' },
    { folder: 'HP UNCOVER', placemark: '-', line: '-', polygon: '-', colorCode: '#ff0000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/homegardenbusiness.png' },
    { folder: 'NEW POLE 9-4', placemark: 'NEW POLE 9-4', line: '-', polygon: '-', colorCode: '#FF0000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'NEW POLE 7-4', placemark: 'NEW POLE 7-4', line: '-', polygon: '-', colorCode: '#00FF00', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'NEW POLE 7-3', placemark: 'NEW POLE 7-3', line: '-', polygon: '-', colorCode: '#00FFFF', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'NEW POLE 7-2.5', placemark: 'NEW POLE 7-2.5', line: '-', polygon: '-', colorCode: '#AA00FF', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'EXISTING POLE EMR 7-2.5', placemark: 'EXISTING POLE EMR 7-2.5', line: '-', polygon: '-', colorCode: '#550000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'EXISTING POLE EMR 7-3', placemark: 'EXISTING POLE EMR 7-3', line: '-', polygon: '-', colorCode: '#550000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'EXISTING POLE EMR 7-4', placemark: 'EXISTING POLE EMR 7-4', line: '-', polygon: '-', colorCode: '#550000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'EXISTING POLE EMR 9-4', placemark: 'EXISTING POLE EMR 9-4', line: '-', polygon: '-', colorCode: '#550000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'EXISTING POLE PARTNER 7-4', placemark: 'EXISTING POLE PARTNER 7-4', line: '-', polygon: '-', colorCode: '#550000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'EXISTING POLE PARTNER 9-4', placemark: 'EXISTING POLE PARTNER 9-4', line: '-', polygon: '-', colorCode: '#550000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png' },
    { folder: 'DISTRIBUTION CABLE', placemark: '-', line: '24C', polygon: '-', colorCode: '#00FF00', styleLink: '-' },
    { folder: 'DISTRIBUTION CABLE', placemark: '-', line: '36C', polygon: '-', colorCode: '#FF00FF', styleLink: '-' },
    { folder: 'DISTRIBUTION CABLE', placemark: '-', line: '48C', polygon: '-', colorCode: '#AA00FF', styleLink: '-' },
    { folder: 'DISTRIBUTION CABLE', placemark: '-', line: '72C', polygon: '-', colorCode: '#550000', styleLink: '-' },
    { folder: 'DISTRIBUTION CABLE', placemark: '-', line: '96C', polygon: '-', colorCode: '#FF0000', styleLink: '-' },
    { folder: 'DISTRIBUTION CABLE', placemark: '-', line: '144C', polygon: '-', colorCode: '#FFFF00', styleLink: '-' },
    { folder: 'DISTRIBUTION CABLE', placemark: '-', line: '288C', polygon: '-', colorCode: '#FFAA00', styleLink: '-' },
    { folder: 'SLING WIRE', placemark: '-', line: '-', polygon: '-', colorCode: '#00FFFF', styleLink: '-' },
    { folder: 'FDT', placemark: 'FDT', line: '-', polygon: '-', colorCode: '#AA0000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/cross-hairs.png' },
    { folder: 'SLACK HANGER', placemark: 'SLACK', line: '-', polygon: '-', colorCode: '#ff0000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/target.png' }
];

export const RULES_SUBFEEDER = [
    { folder: 'SLACK HANGER', placemark: 'SLACK', line: '-', polygon: '-', colorCode: '#ff0000', styleLink: 'http://maps.google.com/mapfiles/kml/shapes/target.png' }
];

/* ================= MATH & GEO UTILS ================= */
export function ccw(A, B, C) { return (C.lat - A.lat) * (B.lon - A.lon) > (B.lat - A.lat) * (C.lon - A.lon); }
export function linesIntersect(p1, p2, p3, p4) { return ccw(p1, p3, p4) !== ccw(p2, p3, p4) && ccw(p1, p2, p3) !== ccw(p1, p2, p4); }
export function toRad(val) { return val * Math.PI / 180; }
export function toDeg(val) { return val * 180 / Math.PI; }

export function vincentyDist(lat1, lon1, lat2, lon2) {
    const a = 6378137;
    const b = 6356752.314245;
    const f = 1 / 298.257223563;
    const L = toRad(lon2 - lon1);
    const U1 = Math.atan((1 - f) * Math.tan(toRad(lat1)));
    const U2 = Math.atan((1 - f) * Math.tan(toRad(lat2)));
    const sinU1 = Math.sin(U1), cosU1 = Math.cos(U1);
    const sinU2 = Math.sin(U2), cosU2 = Math.cos(U2);
    let lambda = L, lambdaP, iterLimit = 100;
    let cosSqAlpha, sinSigma, cos2SigmaM, sigma, cosSigma;
    do {
        const sinLambda = Math.sin(lambda), cosLambda = Math.cos(lambda);
        sinSigma = Math.sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) + (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) * (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda));
        if (sinSigma === 0) return 0;
        cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cosLambda;
        sigma = Math.atan2(sinSigma, cosSigma);
        const sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma;
        cosSqAlpha = 1 - sinAlpha * sinAlpha;
        cos2SigmaM = cosSigma - 2 * sinU1 * sinU2 / cosSqAlpha;
        if (isNaN(cos2SigmaM)) cos2SigmaM = 0;
        const C = f / 16 * cosSqAlpha * (4 + f * (4 - 3 * cosSqAlpha));
        lambdaP = lambda;
        lambda = L + (1 - C) * f * sinAlpha * (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)));
    } while (Math.abs(lambda - lambdaP) > 1e-12 && --iterLimit > 0);
    if (iterLimit === 0) return 0;
    const uSq = cosSqAlpha * (a * a - b * b) / (b * b);
    const A = 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)));
    const B = uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)));
    const deltaSigma = B * sinSigma * (cos2SigmaM + B / 4 * (cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM) - B / 6 * cos2SigmaM * (-3 + 4 * sinSigma * sinSigma) * (-3 + 4 * cos2SigmaM * cos2SigmaM)));
    return b * A * (sigma - deltaSigma);
}

export function haversine(pt1, pt2) {
    const R = 6371000;
    const dLat = toRad(pt2.lat - pt1.lat);
    const dLon = toRad(pt2.lon - pt1.lon);
    const a = Math.sin(dLat/2) * Math.sin(dLat/2) + Math.cos(toRad(pt1.lat)) * Math.cos(toRad(pt2.lat)) * Math.sin(dLon/2) * Math.sin(dLon/2);
    return R * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
}

export function isPointInPolygon(p, vs) {
    let inside = false;
    for (let i = 0, j = vs.length - 1; i < vs.length; j = i++) {
        const xi = vs[i].lon, yi = vs[i].lat;
        const xj = vs[j].lon, yj = vs[j].lat;
        const intersect = ((yi > p.lat) !== (yj > p.lat)) && (p.lon < (xj - xi) * (p.lat - yi) / (yj - yi) + xi);
        if (intersect) inside = !inside;
    }
    return inside;
}

export function distToSegment(p, v, w) {
    const l2 = ((v.lon - w.lon)**2 + (v.lat - w.lat)**2);
    if (l2 == 0) return haversine(p, v);
    let t = ((p.lon - v.lon) * (w.lon - v.lon) + (p.lat - v.lat) * (w.lat - v.lat)) / l2;
    t = Math.max(0, Math.min(1, t));
    const projection = { lon: v.lon + t * (w.lon - v.lon), lat: v.lat + t * (w.lat - v.lat) };
    return haversine(p, projection);
}

export function getProjectedPoint(p, v, w) {
    const l2 = ((v.lon - w.lon)**2 + (v.lat - w.lat)**2);
    if (l2 == 0) return { lon: v.lon, lat: v.lat };
    let t = ((p.lon - v.lon) * (w.lon - v.lon) + (p.lat - v.lat) * (w.lat - v.lat)) / l2;
    t = Math.max(0, Math.min(1, t));
    return { lon: v.lon + t * (w.lon - v.lon), lat: v.lat + t * (w.lat - v.lat) };
}

export function getBearing(pt1, pt2) {
    const lat1 = toRad(pt1.lat), lon1 = toRad(pt1.lon);
    const lat2 = toRad(pt2.lat), lon2 = toRad(pt2.lon);
    const y = Math.sin(lon2 - lon1) * Math.cos(lat2);
    const x = Math.cos(lat1) * Math.sin(lat2) - Math.sin(lat1) * Math.cos(lat2) * Math.cos(lon2 - lon1);
    return (toDeg(Math.atan2(y, x)) + 360) % 360;
}

export function getDestinationPoint(pt, distance, bearing) {
    const R = 6371000;
    const d = distance / R;
    const brng = toRad(bearing);
    const lat1 = toRad(pt.lat), lon1 = toRad(pt.lon);
    const lat2 = Math.asin(Math.sin(lat1) * Math.cos(d) + Math.cos(lat1) * Math.sin(d) * Math.cos(brng));
    const lon2 = lon1 + Math.atan2(Math.sin(brng) * Math.sin(d) * Math.cos(lat1), Math.cos(d) - Math.sin(lat1) * Math.sin(lat2));
    return { lat: toDeg(lat2), lon: toDeg(lon2) };
}

export function calculateLineLength(coordString) {
    const parts = coordString.trim().split(/\s+/).filter(p => p.includes(','));
    if (parts.length < 2) return 0;
    const points = parts.map(p => { const [lon, lat] = p.split(',').map(Number); return { lon, lat }; });
    let totalDist = 0;
    for (let i = 0; i < points.length - 1; i++) {
        totalDist += vincentyDist(points[i].lat, points[i].lon, points[i+1].lat, points[i+1].lon);
    }
    return Math.round(totalDist);
}

/* ================= XML HELPERS ================= */
export function createName(xml, text) { const n = xml.createElement('name'); n.textContent = text; return n; }

export function createFolder(xml, name, items) {
    const f = xml.createElement('Folder');
    f.appendChild(createName(xml, name));
    items.forEach(i => f.appendChild(i));
    return f;
}

export function sortFolderChildren(folder, structureArray) {
    const nameNode = folder.querySelector('name');
    const children = Array.from(folder.children).filter(c => c !== nameNode);

    children.sort((a, b) => {
        const nameA = a.querySelector('name')?.textContent || "";
        const nameB = b.querySelector('name')?.textContent || "";

        const idxA = structureArray.indexOf(nameA);
        const idxB = structureArray.indexOf(nameB);

        if (idxA !== -1 && idxB !== -1) return idxA - idxB;
        if (idxA !== -1) return -1;
        if (idxB !== -1) return 1;
        return nameA.localeCompare(nameB);
    });

    children.forEach(c => folder.appendChild(c));
}

/* ================= OPSIONAL: COORDINATE ROUNDING ================= */
// Ubah ke 'true' jika ingin membatasi jumlah digit desimal
export const ENABLE_COORD_ROUNDING = false; 

// Berapa angka di belakang koma? (Standar GPS presisi tinggi biasanya 7 atau 8)
export const ROUND_LAT = 7; 
export const ROUND_LON = 7;