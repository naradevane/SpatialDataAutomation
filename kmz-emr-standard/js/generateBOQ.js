/*!
 * KMZ EMR Standard - BOQ Generation Worker
 * Copyright (c) 2026 Muhammad Ikhsanudin
 * * This file is part of KMZ EMR Standard.
 * This software is licensed under the PolyForm Strict License 1.0.0.
 * * STRICT PROHIBITION: You may use this software for your own purposes, 
 * but you MAY NOT modify, copy, or distribute this software or any 
 * modified version of it without prior written consent from the Copyright Holder.
 * * A copy of the license is located in the LICENSE file in the root directory, 
 * or can be found at: https://polyformproject.org/licenses/strict/1.0.0/
 */

// Fungsi ngitung jarak antar titik (dalam meter)
function getDistance(lat1, lon1, lat2, lon2) {
    const R = 6371000;
    const dLat = (lat2 - lat1) * Math.PI / 180;
    const dLon = (lon2 - lon1) * Math.PI / 180;
    const a = Math.sin(dLat/2) * Math.sin(dLat/2) +
              Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
              Math.sin(dLon/2) * Math.sin(dLon/2);
    return R * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
}

export function extractBOQData(xmlDoc, rootNameRaw, mode = 'cluster') {
    const boqData = {};
    const rootFolder = Array.from(xmlDoc.querySelectorAll('Folder'))
        .find(f => f.querySelector('name')?.textContent === rootNameRaw);
    
    if (!rootFolder) throw new Error("Root folder tidak ditemukan saat ekstraksi BOQ.");

    if (mode === 'subfeeder') {
        // ==========================================
        // LOGIC BOQ SUBFEEDER
        // ==========================================
        const fdtGroupFolders = Array.from(rootFolder.children).filter(child => {
            const n = child.querySelector('name')?.textContent.toUpperCase() || '';
            return child.tagName === 'Folder' && n.match(/FDT\s*(\d+)/i);
        });

        fdtGroupFolders.forEach(fdtFolder => {
            const folderName = fdtFolder.querySelector('name').textContent.toUpperCase();
            const fdtMatch = folderName.match(/FDT\s*(\d+)/i);
            const fdtNum = fdtMatch ? parseInt(fdtMatch[1]) : 1;

            if (!boqData[fdtNum]) {
                boqData[fdtNum] = { fdtCap: '48C', cables: {}, jc: {}, poles: {} };
            }

            // A. FDT Capacity (Cari dari placemark FDT di dalam folder ini)
            const fdtSubFolder = Array.from(fdtFolder.children).find(c => c.querySelector('name')?.textContent.toUpperCase() === 'FDT');
            if (fdtSubFolder) {
                fdtSubFolder.querySelectorAll('Placemark').forEach(pm => {
                    const name = pm.querySelector('name')?.textContent.toUpperCase() || '';
                    const capMatch = name.match(/(\d{2,3})C/);
                    if (capMatch) boqData[fdtNum].fdtCap = capMatch[1] + 'C';
                });
            }

            // B. CABLE Extractor
            const cableFolder = Array.from(fdtFolder.children).find(c => c.querySelector('name')?.textContent.toUpperCase() === 'CABLE');
            if (cableFolder) {
                cableFolder.querySelectorAll('Placemark').forEach(pm => {
                    const cName = pm.querySelector('name')?.textContent.toUpperCase() || '';
                    const cDesc = pm.querySelector('description')?.textContent || '';

                    // Ekstrak Kapasitas Kabel
                    const capMatch = cName.match(/(\d{2,3})C/);
                    const cap = capMatch ? capMatch[1] + 'C' : '48C'; // fallback

                    // Ekstrak Panjang Route
                    let routeLen = 0;
                    const routeMatch = cDesc.match(/Total Route\s*:\s*(\d+)/i);
                    if (routeMatch) {
                        routeLen = parseInt(routeMatch[1]);
                    } else {
                        // Fallback jika belum di-generate auto-desc
                        const mMatch = cName.match(/-\s*(\d+)\s*m/i);
                        if (mMatch) routeLen = parseInt(mMatch[1]);
                    }

                    if (!boqData[fdtNum].cables[cap]) boqData[fdtNum].cables[cap] = { routeLen: 0 };
                    boqData[fdtNum].cables[cap].routeLen += routeLen;
                });
            }

            // C. JOINT CLOSURE Extractor (Skip "EXT")
            const jcFolder = Array.from(fdtFolder.children).find(c => c.querySelector('name')?.textContent.toUpperCase() === 'JOINT CLOSURE');
            if (jcFolder) {
                jcFolder.querySelectorAll('Placemark').forEach(pm => {
                    const name = pm.querySelector('name')?.textContent.toUpperCase() || '';
                    if (!name.includes('EXT')) { // Abaikan kalau ada kata EXT
                        const capMatch = name.match(/(\d{2,3})C/);
                        if (capMatch) {
                            const cap = capMatch[1] + 'C';
                            boqData[fdtNum].jc[cap] = (boqData[fdtNum].jc[cap] || 0) + 1;
                        }
                    }
                });
            }

            // D. POLE Extractor
            fdtFolder.querySelectorAll('Folder').forEach(subF => {
                const subName = subF.querySelector('name')?.textContent.toUpperCase().trim() || '';
                if (subName.includes('POLE') || subName.includes('TIANG')) {
                    const count = subF.querySelectorAll('Placemark').length;
                    if (count > 0) {
                        boqData[fdtNum].poles[subName] = (boqData[fdtNum].poles[subName] || 0) + count;
                    }
                }
            });
        });

        return boqData;

    } else {
        // ==========================================
        // LOGIC BOQ CLUSTER (ORIGINAL)
        // ==========================================
        // 1. Ambil Kapasitas FDT Global
        const globalFdtFolder = Array.from(rootFolder.children).find(f => f.tagName === 'Folder' && f.querySelector('name')?.textContent.toUpperCase() === 'FDT');
        const fdtCapacities = {};
        if (globalFdtFolder) {
            const fdtPlacemarks = globalFdtFolder.querySelectorAll('Placemark');
            fdtPlacemarks.forEach((pm, index) => {
                const fdtNum = index + 1; 
                const name = pm.querySelector('name')?.textContent.toUpperCase() || '';
                const desc = pm.querySelector('description')?.textContent.toUpperCase() || '';
                const fullText = name + ' ' + desc;
                const capMatch = fullText.match(/(\d{2,3})C/);
                fdtCapacities[fdtNum] = capMatch ? capMatch[1] + 'C' : '48C';
            });
        }

        const lineFolders = Array.from(rootFolder.children).filter(child => {
            const n = child.querySelector('name')?.textContent.toUpperCase() || '';
            return child.tagName === 'Folder' && n.includes('LINE');
        });

        lineFolders.forEach(lineFolder => {
            const folderName = lineFolder.querySelector('name').textContent.toUpperCase();
            const fdtMatch = folderName.match(/FDT\s*[-_]?\s*(\d+)/i);
            const lineMatch = folderName.match(/LINE\s*[-_]?\s*([A-Z])/i);
            const fdtNum = fdtMatch ? parseInt(fdtMatch[1]) : 1;
            const lineChar = lineMatch ? lineMatch[1] : 'A';

            if (!boqData[fdtNum]) boqData[fdtNum] = { lines: {}, poles: {}, fdtCap: fdtCapacities[fdtNum] || '48C' };
            if (!boqData[fdtNum].lines[lineChar]) boqData[fdtNum].lines[lineChar] = { cableCap: null, routeLen: 0, slingLen: 0, fatCount: 0, tektokLen: 0 };

            const lineData = boqData[fdtNum].lines[lineChar];

            const cableFolder = Array.from(lineFolder.children).find(c => c.querySelector('name')?.textContent.toUpperCase() === 'DISTRIBUTION CABLE');
            if (cableFolder) {
                const cables = cableFolder.querySelectorAll('Placemark');
                if (cables.length > 1) throw new Error(`❌ ERROR BOQ: Ditemukan ${cables.length} tarikan kabel di folder '${folderName}'. Harap gabungkan (join) kabel di Google Earth menjadi 1 tarikan mutlak.`);
                if (cables.length === 1) {
                    const pm = cables[0];
                    const cName = pm.querySelector('name')?.textContent.toUpperCase() || '';
                    const cDesc = pm.querySelector('description')?.textContent || '';
                    const capMatch = cName.match(/(\d{2,3})C/);
                    if (capMatch) lineData.cableCap = capMatch[1] + 'C';
                    const routeMatch = cDesc.match(/Total Route\s*:\s*(\d+)/i);
                    if (routeMatch) lineData.routeLen = parseInt(routeMatch[1]);

                    let tektokLen = 0;
                    const extData = pm.querySelector('ExtendedData Data[name="tektokLen"] value');
                    if (extData) {
                        tektokLen = parseInt(extData.textContent) || 0;
                    } else {
                        const ls = pm.querySelector('LineString coordinates');
                        if (ls) {
                            const coords = ls.textContent.trim().split(/\s+/).filter(p => p.includes(',')).map(p => {
                                const [lon, lat] = p.split(',').map(Number);
                                return { lon, lat };
                            });
                            const segmentUsage = {};
                            for (let i = 0; i < coords.length - 1; i++) {
                                const p1 = coords[i];
                                const p2 = coords[i+1];
                                const sorted = [p1, p2].sort((a,b) => a.lat - b.lat || a.lon - b.lon);
                                const segKey = `${sorted[0].lat.toFixed(4)}_${sorted[0].lon.toFixed(4)}__${sorted[1].lat.toFixed(4)}_${sorted[1].lon.toFixed(4)}`;
                                if (!segmentUsage[segKey]) {
                                    segmentUsage[segKey] = 1;
                                } else {
                                    if (segmentUsage[segKey] === 1) tektokLen += getDistance(p1.lat, p1.lon, p2.lat, p2.lon);
                                    segmentUsage[segKey]++;
                                }
                            }
                        }
                    }
                    lineData.tektokLen = Math.round(tektokLen);
                }
            }

            const slingFolder = Array.from(lineFolder.children).find(c => c.querySelector('name')?.textContent.toUpperCase() === 'SLING WIRE');
            if (slingFolder) {
                const desc = slingFolder.querySelector('description')?.textContent || '';
                const match = desc.match(/(\d+(?:\.\d+)?)\s*m/i);
                if (match) lineData.slingLen = Math.round(parseFloat(match[1]));
            }

            const fatFolder = Array.from(lineFolder.children).find(c => c.querySelector('name')?.textContent.toUpperCase() === 'FAT');
            if (fatFolder) lineData.fatCount = fatFolder.querySelectorAll('Placemark').length;

            lineFolder.querySelectorAll('Folder').forEach(subF => {
                const subName = subF.querySelector('name')?.textContent.toUpperCase().trim() || '';
                if (subName.includes('POLE') || subName.includes('TIANG')) {
                    const count = subF.querySelectorAll('Placemark').length;
                    if (count > 0) {
                        if (!boqData[fdtNum].poles[subName]) boqData[fdtNum].poles[subName] = 0;
                        boqData[fdtNum].poles[subName] += count;
                    }
                }
            });
        });

        return boqData;
    }
}