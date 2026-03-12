/*!
 * KMZ EMR Standard - Generate HPDB Excel
 * Copyright (c) 2026 Muhammad Ikhsanudin
 * * This file is part of KMZ EMR Standard.
 * This software is licensed under the PolyForm Strict License 1.0.0.
 * * STRICT PROHIBITION: You may use this software for your own purposes, 
 * but you MAY NOT modify, copy, or distribute this software or any 
 * modified version of it without prior written consent from the Copyright Holder.
 * * A copy of the license is located in the LICENSE file in the root directory, 
 * or can be found at: https://polyformproject.org/licenses/strict/1.0.0/
 */
 
/* IMPORT KONFIGURASI YANG DIBUTUHKAN */
import { haversine, ENABLE_COORD_ROUNDING, ROUND_LAT, ROUND_LON } from './config-utils.js';

/* ================= STATE MANAGEMENT (Dihapus karena pakai Worker) ================= */
// Kita biarkan fungsi kosong agar tidak error di main.js saat dipanggil resetHPDBState()
export function resetHPDBState() {}

/* ================= HELPER: COORDINATE FORMATTER ================= */
function fmtCoord(val, digits) {
    if (!ENABLE_COORD_ROUNDING) return val; 
    if (typeof val !== 'number') return val;
    return parseFloat(val.toFixed(digits)); 
}

/* ================= MAIN FUNCTION: EXTRACT & SEND TO WORKER ================= */
export function generateExcelHPDB(xmlDoc, rootNameRaw) {
    return new Promise((resolve, reject) => {
        const fdtGroups = {}; 
        const dashboardData = {}; 

        const rootFolder = Array.from(xmlDoc.querySelectorAll('Folder')).find(f => f.querySelector('name')?.textContent === rootNameRaw);
        if (!rootFolder) return resolve(null);

        // --- 1. PROSES EKSTRAKSI XML KE JSON ---
        const lineFolders = Array.from(rootFolder.children).filter(child => {
            const n = child.querySelector('name')?.textContent || '';
            return child.tagName === 'Folder' && !['OTHERS', 'FDT', 'BOUNDARY CLUSTER'].includes(n);
        });

        lineFolders.forEach(lineFolder => {
            const lineFolderName = lineFolder.querySelector('name').textContent.trim();
            const fdtMatch = lineFolderName.match(/FDT\s*(\d+)/i);
            const fdtID = fdtMatch ? fdtMatch[1] : 'OTHER';
            
            if (!fdtGroups[fdtID]) {
                fdtGroups[fdtID] = [];
                dashboardData[fdtID] = []; 
            }

            const fatFolder = Array.from(lineFolder.children).find(c => c.tagName === 'Folder' && c.querySelector('name')?.textContent === 'FAT');
            const localFatPoints = [];
            if (fatFolder) {
                fatFolder.querySelectorAll('Placemark').forEach(pm => {
                    const pt = pm.querySelector('Point coordinates');
                    if(pt) {
                        const [lon, lat] = pt.textContent.trim().split(',').map(Number);
                        localFatPoints.push({ name: pm.querySelector('name')?.textContent.trim() || 'FAT', lon, lat });
                    }
                });
            }
            
            const localPolePoints = [];
            lineFolder.querySelectorAll('Folder').forEach(f => {
                const n = f.querySelector('name')?.textContent.trim().toUpperCase() || '';
                if (n.includes('POLE') || n.includes('TIANG')) {
                    f.querySelectorAll('Placemark').forEach(pm => {
                        const pt = pm.querySelector('Point coordinates');
                        if(pt) {
                            const [lon, lat] = pt.textContent.trim().split(',').map(Number);
                            localPolePoints.push({ name: pm.querySelector('name')?.textContent.trim() || 'Unknown', lon, lat });
                        }
                    });
                }
            });

            const hpCoverFolder = Array.from(lineFolder.children).find(c => c.tagName === 'Folder' && c.querySelector('name')?.textContent === 'HP COVER');
            if (hpCoverFolder) {
                hpCoverFolder.querySelectorAll('Folder').forEach(sub => {
                    const subName = sub.querySelector('name').textContent.trim(); 
                    const fatCode = subName.slice(-3); 
                    
                    let anchorFat = localFatPoints.find(f => f.name.toUpperCase().includes(fatCode.toUpperCase()));
                    let anchorPole = { name: '', lon: '', lat: '' };
                    
                    if (anchorFat && localPolePoints.length > 0) {
                        let minD = Infinity;
                        localPolePoints.forEach(p => {
                            const d = haversine(p, anchorFat);
                            if (d < minD) { minD = d; anchorPole = p; }
                        });
                    }

                    const hpPlacemarks = sub.querySelectorAll('Placemark');
                    
                    dashboardData[fdtID].push({
                        fatName: subName,
                        count: hpPlacemarks.length
                    });

                    hpPlacemarks.forEach(hp => {
                        const pt = hp.querySelector('Point coordinates');
                        if (pt) {
                            const [hpLon, hpLat] = pt.textContent.trim().split(',').map(Number);
                                fdtGroups[fdtID].push({
                                poleId: anchorPole.name,
                                poleLat: fmtCoord(anchorPole.lat, ROUND_LAT),
                                poleLon: fmtCoord(anchorPole.lon, ROUND_LON),
                                hpName: hp.querySelector('name')?.textContent.trim() || '',
                                lineName: lineFolderName,
                                fatCode: fatCode,
                                fullFatName: subName, // <--- INI WAJIB ADA BROK!
                                hpLat: fmtCoord(hpLat, ROUND_LAT),
                                hpLon: fmtCoord(hpLon, ROUND_LON)
                            });
                        }
                    });
                });
            }
        });

        // --- 2. KIRIM DATA KE WEB WORKER ---
        if (window.Worker) {
            const worker = new Worker('js/hpdb.worker.js');
            
            // Hapus allFdtInfo, balikin jadi cuma ngirim 2 data utama ini:
            worker.postMessage({ fdtGroups, dashboardData }); 

            worker.onmessage = function(e) {
                if (e.data.status === 'success') {
                    // Buat Blob dari file excel (ArrayBuffer) yang dikirim worker
                    const blob = new Blob([e.data.buffer], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
                    resolve(blob);
                } else {
                    reject(e.data.error);
                }
                worker.terminate(); // Matikan worker setelah selesai
            };

            worker.onerror = function(err) {
                reject(err.message);
                worker.terminate();
            };
        } else {
            reject(new Error("Browser lu ga support Web Worker bro."));
        }
    });
}